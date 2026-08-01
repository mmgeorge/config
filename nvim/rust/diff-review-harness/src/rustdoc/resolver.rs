use super::index::{LookupError, RustdocIndex, RustdocItem};
use super::source::{CargoSourceResolver, CargoSourceResolverConfig, RustdocSourceLocation};
use crate::plan::PlanCallableKind;
use anyhow::Context;
use reqwest::Client;
use semver::{Version, VersionReq};
use serde::{Deserialize, Serialize};
use sha2::Digest;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::RwLock;

#[derive(Clone, Debug)]
pub struct RustdocResolverConfig {
    pub crates_io_base: String,
    pub docs_rs_base: String,
    pub cache_dir: PathBuf,
    pub cargo_source: CargoSourceResolverConfig,
}

impl RustdocResolverConfig {
    pub fn production(data_root: &Path) -> anyhow::Result<Self> {
        Ok(Self {
            crates_io_base: "https://crates.io/api/v1".into(),
            docs_rs_base: "https://docs.rs".into(),
            cache_dir: data_root.join("rustdoc-cache"),
            cargo_source: CargoSourceResolverConfig::production()?,
        })
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct RustdocHover {
    pub package: String,
    pub version: String,
    pub path: String,
    pub signature: String,
    pub docs: String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RustdocError {
    Unavailable(String),
    Missing(String),
    Ambiguous(String),
}

impl std::fmt::Display for RustdocError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unavailable(message) | Self::Missing(message) | Self::Ambiguous(message) => {
                formatter.write_str(message)
            }
        }
    }
}

impl std::error::Error for RustdocError {}

pub struct RustdocResolver {
    client: Client,
    config: RustdocResolverConfig,
    document_by_version: RwLock<HashMap<(String, String), Arc<RustdocIndex>>>,
    cargo_source: CargoSourceResolver,
}

impl RustdocResolver {
    pub fn new(config: RustdocResolverConfig) -> anyhow::Result<Self> {
        std::fs::create_dir_all(&config.cache_dir).with_context(|| {
            format!(
                "create Rustdoc cache directory {}",
                config.cache_dir.display()
            )
        })?;
        let client = Client::builder()
            .user_agent("diff-review-harness/0.1")
            .timeout(Duration::from_secs(15))
            .build()
            .context("build Rustdoc HTTP client")?;
        Ok(Self {
            client,
            cargo_source: CargoSourceResolver::new(config.cargo_source.clone()),
            config,
            document_by_version: RwLock::new(HashMap::new()),
        })
    }

    pub async fn resolve_version(
        &self,
        package: &str,
        requirement: &str,
    ) -> Result<String, RustdocError> {
        let requirement = VersionReq::parse(requirement).map_err(|error| {
            RustdocError::Missing(format!(
                "dependency `{package}` has invalid Cargo version requirement `{requirement}`: {error}"
            ))
        })?;
        let url = format!("{}/crates/{package}/versions", self.config.crates_io_base);
        let response = self.client.get(&url).send().await.map_err(|error| {
            RustdocError::Unavailable(format!(
                "could not query published versions for `{package}`: {error}"
            ))
        })?;
        if response.status() == reqwest::StatusCode::NOT_FOUND {
            return Err(RustdocError::Missing(format!(
                "crate `{package}` does not exist"
            )));
        }
        let response = response.error_for_status().map_err(|error| {
            RustdocError::Unavailable(format!(
                "could not query published versions for `{package}`: {error}"
            ))
        })?;
        let payload: CrateVersionResponse = response.json().await.map_err(|error| {
            RustdocError::Unavailable(format!(
                "could not decode published versions for `{package}`: {error}"
            ))
        })?;
        payload
            .versions
            .into_iter()
            .filter(|candidate| !candidate.yanked)
            .filter_map(|candidate| Version::parse(&candidate.num).ok())
            .filter(|candidate| requirement.matches(candidate))
            .max()
            .map(|version| version.to_string())
            .ok_or_else(|| {
                RustdocError::Missing(format!(
                    "crate `{package}` has no non-yanked release matching `{requirement}`"
                ))
            })
    }

    pub(crate) async fn index(
        &self,
        package: &str,
        version: &str,
    ) -> Result<Arc<RustdocIndex>, RustdocError> {
        let key = (package.to_owned(), version.to_owned());
        if let Some(index) = self.document_by_version.read().await.get(&key) {
            return Ok(Arc::clone(index));
        }
        let cache_path = self.cache_path(package, version);
        let cache_hit = cache_path.exists();
        let compressed = if cache_hit {
            tokio::fs::read(&cache_path).await.map_err(|error| {
                RustdocError::Unavailable(format!(
                    "could not read cached Rustdoc for `{package}` {version}: {error}"
                ))
            })?
        } else {
            let url = format!(
                "{}/crate/{package}/{version}/json",
                self.config.docs_rs_base
            );
            let response = self.client.get(&url).send().await.map_err(|error| {
                RustdocError::Unavailable(format!(
                    "could not download Rustdoc for `{package}` {version}: {error}"
                ))
            })?;
            if response.status() == reqwest::StatusCode::NOT_FOUND {
                return Err(RustdocError::Unavailable(format!(
                    "docs.rs has no Rustdoc build for `{package}` {version}"
                )));
            }
            let response = response.error_for_status().map_err(|error| {
                RustdocError::Unavailable(format!(
                    "could not download Rustdoc for `{package}` {version}: {error}"
                ))
            })?;
            let compressed = response.bytes().await.map_err(|error| {
                RustdocError::Unavailable(format!(
                    "could not read Rustdoc response for `{package}` {version}: {error}"
                ))
            })?;
            compressed.to_vec()
        };
        let cache_source = (!cache_hit).then(|| compressed.clone());
        let source =
            tokio::task::spawn_blocking(move || zstd::decode_all(std::io::Cursor::new(compressed)))
                .await
                .map_err(|error| {
                    RustdocError::Unavailable(format!("Rustdoc decompression stopped: {error}"))
                })?
                .map_err(|error| {
                    RustdocError::Unavailable(format!("could not decompress Rustdoc: {error}"))
                })?;
        let index = Arc::new(RustdocIndex::parse(&source).map_err(|error| {
            RustdocError::Unavailable(format!(
                "could not index Rustdoc for `{package}` {version}: {error:#}"
            ))
        })?);
        if let Some(compressed) = cache_source {
            if let Some(parent) = cache_path.parent() {
                tokio::fs::create_dir_all(parent).await.map_err(|error| {
                    RustdocError::Unavailable(format!(
                        "could not create Rustdoc cache for `{package}` {version}: {error}"
                    ))
                })?;
            }
            tokio::fs::write(&cache_path, compressed)
                .await
                .map_err(|error| {
                    RustdocError::Unavailable(format!(
                        "could not cache Rustdoc for `{package}` {version}: {error}"
                    ))
                })?;
        }
        self.document_by_version
            .write()
            .await
            .insert(key, Arc::clone(&index));
        Ok(index)
    }

    pub async fn type_hover(
        &self,
        package: &str,
        version: &str,
        type_name: &str,
    ) -> Result<RustdocHover, RustdocError> {
        Ok(self
            .resolve_type(package, version, type_name)
            .await?
            .hover())
    }

    pub async fn callable_hover(
        &self,
        package_list: &[(String, String)],
        receiver_package: &str,
        receiver_version: &str,
        receiver: &str,
        callable: &str,
        kind: PlanCallableKind,
    ) -> Result<RustdocHover, RustdocError> {
        Ok(self
            .resolve_callable(
                package_list,
                receiver_package,
                receiver_version,
                receiver,
                callable,
                kind,
            )
            .await?
            .hover())
    }

    pub async fn type_source(
        &self,
        package: &str,
        version: &str,
        type_name: &str,
    ) -> Result<RustdocSourceLocation, RustdocError> {
        let resolved = self.resolve_type(package, version, type_name).await?;
        self.source_location(resolved).await
    }

    pub async fn callable_source(
        &self,
        package_list: &[(String, String)],
        receiver_package: &str,
        receiver_version: &str,
        receiver: &str,
        callable: &str,
        kind: PlanCallableKind,
    ) -> Result<RustdocSourceLocation, RustdocError> {
        let resolved = self
            .resolve_callable(
                package_list,
                receiver_package,
                receiver_version,
                receiver,
                callable,
                kind,
            )
            .await?;
        self.source_location(resolved).await
    }

    async fn resolve_type(
        &self,
        package: &str,
        version: &str,
        type_name: &str,
    ) -> Result<ResolvedRustdocItem, RustdocError> {
        let index = self.index(package, version).await?;
        let item = index.type_item(type_name).map_err(map_lookup)?;
        Ok(ResolvedRustdocItem::new(package, version, item))
    }

    async fn resolve_callable(
        &self,
        package_list: &[(String, String)],
        receiver_package: &str,
        receiver_version: &str,
        receiver: &str,
        callable: &str,
        kind: PlanCallableKind,
    ) -> Result<ResolvedRustdocItem, RustdocError> {
        let receiver_index = self.index(receiver_package, receiver_version).await?;
        let resolved_receiver = receiver_index
            .resolve_receiver(receiver)
            .map_err(map_lookup)?;
        let mut match_list = Vec::new();
        let mut unavailable_list = Vec::new();
        for (package, version) in package_list {
            match self.index(package, version).await {
                Ok(index) => match index.callable(&resolved_receiver, callable, kind) {
                    Ok(item) => match_list.push(ResolvedRustdocItem::new(package, version, item)),
                    Err(LookupError::Missing(_)) => {}
                    Err(error) => return Err(map_lookup(error)),
                },
                Err(error @ RustdocError::Unavailable(_)) => unavailable_list.push(error),
                Err(error) => return Err(error),
            }
        }
        match match_list.as_slice() {
            [item] => Ok(item.clone()),
            [] if !unavailable_list.is_empty() => Err(RustdocError::Unavailable(format!(
                "could not conclusively resolve `{receiver}::{callable}` because {} dependency document(s) were unavailable",
                unavailable_list.len()
            ))),
            [] => Err(RustdocError::Missing(format!(
                "callable `{}::{callable}` does not exist in the plan's Rust dependencies",
                resolved_receiver.authored()
            ))),
            _ => Err(RustdocError::Ambiguous(format!(
                "callable `{receiver}::{callable}` resolves in {} dependencies",
                match_list.len()
            ))),
        }
    }

    async fn source_location(
        &self,
        resolved: ResolvedRustdocItem,
    ) -> Result<RustdocSourceLocation, RustdocError> {
        let span = resolved.item.span.as_ref().ok_or_else(|| {
            RustdocError::Missing(format!(
                "source location is unavailable for generated item `{}`",
                resolved.item.path
            ))
        })?;
        self.cargo_source
            .source_location(&resolved.package, &resolved.version, span)
            .await
    }

    fn cache_path(&self, package: &str, version: &str) -> PathBuf {
        let key = format!("{package}/{version}");
        let digest = sha2::Sha256::digest(key.as_bytes());
        self.config
            .cache_dir
            .join(format!("{}.json.zst", hex::encode(digest)))
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct ResolvedRustdocItem {
    package: String,
    version: String,
    item: RustdocItem,
}

impl ResolvedRustdocItem {
    fn new(package: &str, version: &str, item: RustdocItem) -> Self {
        Self {
            package: package.to_owned(),
            version: version.to_owned(),
            item,
        }
    }

    fn hover(self) -> RustdocHover {
        RustdocHover {
            package: self.package,
            version: self.version,
            path: self.item.path,
            signature: self.item.signature,
            docs: self.item.docs,
        }
    }
}

fn map_lookup(error: LookupError) -> RustdocError {
    match error {
        LookupError::Missing(message) => RustdocError::Missing(message),
        LookupError::Ambiguous(message) => RustdocError::Ambiguous(message),
    }
}

#[derive(Debug, Deserialize)]
struct CrateVersionResponse {
    versions: Vec<CrateVersion>,
}

#[derive(Debug, Deserialize)]
struct CrateVersion {
    num: String,
    yanked: bool,
}

#[cfg(test)]
mod test {
    use super::*;
    use tokio::io::{AsyncReadExt, AsyncWriteExt};
    use tokio::net::TcpListener;

    async fn fixture_server(rustdoc: Vec<u8>) -> String {
        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let address = listener.local_addr().unwrap();
        tokio::spawn(async move {
            for _ in 0..2 {
                let (mut stream, _) = listener.accept().await.unwrap();
                let mut request = vec![0; 4096];
                let read = stream.read(&mut request).await.unwrap();
                let request = String::from_utf8_lossy(&request[..read]);
                let body = if request.starts_with("GET /crates/geoparquet/versions ") {
                    br#"{"versions":[{"num":"0.8.0","yanked":false},{"num":"0.8.1","yanked":true},{"num":"0.7.0","yanked":false}]}"#.to_vec()
                } else {
                    rustdoc.clone()
                };
                let response = format!(
                    "HTTP/1.1 200 OK\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
                    body.len()
                );
                stream.write_all(response.as_bytes()).await.unwrap();
                stream.write_all(&body).await.unwrap();
            }
        });
        format!("http://{address}")
    }

    fn rustdoc_fixture() -> Vec<u8> {
        let source = serde_json::to_vec(&serde_json::json!({
            "format_version": 57,
            "paths": {
                "1": { "path": ["parquet", "ReaderBuilder"], "kind": "struct" }
            },
            "index": {
                "1": {
                    "name": "ReaderBuilder",
                    "span": {
                        "filename": "src/lib.rs",
                        "begin": [1, 1],
                        "end": [1, 25]
                    },
                    "docs": "Builds one reader.",
                    "inner": { "struct": { "impls": [2] } }
                },
                "2": {
                    "name": null,
                    "span": null,
                    "docs": null,
                    "inner": {
                        "impl": {
                            "trait": null,
                            "for": { "id": 1, "path": "ReaderBuilder" },
                            "items": [3]
                        }
                    }
                },
                "3": {
                    "name": "metadata",
                    "span": {
                        "filename": "src/lib.rs",
                        "begin": [2, 5],
                        "end": [2, 45]
                    },
                    "docs": "Reads metadata.",
                    "inner": {
                        "function": {
                            "sig": {
                                "inputs": [["self", { "borrowed_ref": { "is_mutable": false, "type": { "generic": "Self" } } }]],
                                "output": { "primitive": "usize" }
                            }
                        }
                    }
                }
            }
        }))
        .unwrap();
        zstd::encode_all(std::io::Cursor::new(source), 1).unwrap()
    }

    #[tokio::test]
    async fn resolves_the_plan_requirement_and_loads_exact_version_rustdoc() {
        let server = fixture_server(rustdoc_fixture()).await;
        let temp = tempfile::tempdir().unwrap();
        let resolver = RustdocResolver::new(RustdocResolverConfig {
            crates_io_base: server.clone(),
            docs_rs_base: server,
            cache_dir: temp.path().join("rustdoc"),
            cargo_source: CargoSourceResolverConfig {
                cargo_executable: temp.path().join("missing-cargo"),
                cargo_home: temp.path().join("cargo"),
            },
        })
        .unwrap();
        let version = resolver.resolve_version("geoparquet", "0.8").await.unwrap();
        assert_eq!(version, "0.8.0");
        let hover = resolver
            .callable_hover(
                &[("geoparquet".into(), version)],
                "geoparquet",
                "0.8.0",
                "ReaderBuilder",
                "metadata",
                PlanCallableKind::Method,
            )
            .await
            .unwrap();
        assert_eq!(hover.signature, "fn metadata(&self) -> usize");
        assert_eq!(hover.docs, "Reads metadata.");
    }

    #[tokio::test]
    async fn does_not_cache_an_invalid_rustdoc_response() {
        let server = fixture_server(b"not zstd".to_vec()).await;
        let temp = tempfile::tempdir().unwrap();
        let cache_dir = temp.path().join("rustdoc");
        let resolver = RustdocResolver::new(RustdocResolverConfig {
            crates_io_base: server.clone(),
            docs_rs_base: server,
            cache_dir: cache_dir.clone(),
            cargo_source: CargoSourceResolverConfig {
                cargo_executable: temp.path().join("missing-cargo"),
                cargo_home: temp.path().join("cargo"),
            },
        })
        .unwrap();

        let error = resolver
            .type_hover("geoparquet", "0.8.0", "ReaderBuilder")
            .await
            .unwrap_err();

        assert!(matches!(error, RustdocError::Unavailable(_)));
        assert_eq!(std::fs::read_dir(cache_dir).unwrap().count(), 0);
    }

    #[tokio::test]
    async fn opens_the_exact_callable_span_from_cargo_source() {
        let server = fixture_server(rustdoc_fixture()).await;
        let temp = tempfile::tempdir().unwrap();
        let cargo_home = temp.path().join("cargo");
        let package_root = cargo_home
            .join("registry")
            .join("src")
            .join("index.crates.io-test")
            .join("geoparquet-0.8.0");
        std::fs::create_dir_all(package_root.join("src")).unwrap();
        std::fs::write(package_root.join(".cargo-ok"), b"complete").unwrap();
        std::fs::write(
            package_root.join("src").join("lib.rs"),
            b"pub struct ReaderBuilder;\n    pub fn metadata(&self) -> usize { 0 }\n",
        )
        .unwrap();
        let resolver = RustdocResolver::new(RustdocResolverConfig {
            crates_io_base: server.clone(),
            docs_rs_base: server,
            cache_dir: temp.path().join("rustdoc"),
            cargo_source: CargoSourceResolverConfig {
                cargo_executable: temp.path().join("missing-cargo"),
                cargo_home,
            },
        })
        .unwrap();

        let location = resolver
            .callable_source(
                &[("geoparquet".into(), "0.8.0".into())],
                "geoparquet",
                "0.8.0",
                "ReaderBuilder",
                "metadata",
                PlanCallableKind::Method,
            )
            .await
            .unwrap();

        assert_eq!(location.package, "geoparquet");
        assert_eq!(location.version, "0.8.0");
        assert_eq!(location.path, package_root.join("src").join("lib.rs"));
        assert_eq!((location.line, location.column), (2, 5));
    }

    #[tokio::test]
    #[ignore = "requires live crates.io and docs.rs services"]
    async fn resolves_live_crates_io_version_and_docs_rs_type() {
        let temp = tempfile::tempdir().unwrap();
        let cargo_home = temp.path().join("cargo");
        let resolver = RustdocResolver::new(RustdocResolverConfig {
            crates_io_base: "https://crates.io/api/v1".into(),
            docs_rs_base: "https://docs.rs".into(),
            cache_dir: temp.path().join("rustdoc"),
            cargo_source: CargoSourceResolverConfig {
                cargo_executable: PathBuf::from("cargo"),
                cargo_home,
            },
        })
        .unwrap();

        let version = resolver.resolve_version("semver", "1").await.unwrap();
        let hover = resolver
            .type_hover("semver", &version, "Version")
            .await
            .unwrap();

        assert_eq!(hover.package, "semver");
        assert_eq!(hover.version, version);
        assert!(hover.path.ends_with("::Version"));
        assert!(hover.signature.contains("struct Version"));
        assert!(!hover.docs.is_empty());
        let location = resolver
            .type_source("semver", &version, "Version")
            .await
            .unwrap();
        assert!(location.path.is_file());
        let source = std::fs::read_to_string(&location.path).unwrap();
        let line = source.lines().nth(location.line - 1).unwrap_or_default();
        assert!(line.contains("Version"));
    }
}

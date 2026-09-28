use super::index::{SourceIndex, SourceItem};
use super::source::{CargoSourceResolver, CargoSourceResolverConfig, RustdocSourceLocation};
use crate::plan::PlanCallableKind;
use anyhow::Context;
use reqwest::Client;
use semver::{Version, VersionReq};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::time::Duration;

#[derive(Clone, Debug)]
/// Registry and Cargo source locations used by the plan declaration resolver.
pub struct RustdocResolverConfig {
    pub crates_io_base: String,
    pub cache_dir: PathBuf,
    pub cargo_source: CargoSourceResolverConfig,
}

impl RustdocResolverConfig {
    pub fn production(data_root: &Path) -> anyhow::Result<Self> {
        Ok(Self {
            crates_io_base: "https://crates.io/api/v1".into(),
            cache_dir: data_root.join("cargo-source-cache"),
            cargo_source: CargoSourceResolverConfig::production()?,
        })
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
/// Signature and documentation extracted from an exact Cargo source declaration.
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

type SourceIndexMap = HashMap<(String, String), Arc<Mutex<SourceIndex>>>;

/// Shares downloaded sources across plan checking, hover, and source navigation.
pub struct RustdocResolver {
    client: Client,
    config: RustdocResolverConfig,
    index_by_version: tokio::sync::Mutex<SourceIndexMap>,
    cargo_source: CargoSourceResolver,
}

impl RustdocResolver {
    pub fn new(config: RustdocResolverConfig) -> anyhow::Result<Self> {
        std::fs::create_dir_all(&config.cache_dir).context("create Cargo source cache")?;
        let client = Client::builder()
            .user_agent("forge-harness/0.1")
            .timeout(Duration::from_secs(15))
            .build()
            .context("build registry HTTP client")?;
        Ok(Self {
            client,
            cargo_source: CargoSourceResolver::new(config.cargo_source.clone()),
            config,
            index_by_version: tokio::sync::Mutex::new(HashMap::new()),
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

    async fn index(
        &self,
        package: &str,
        version: &str,
    ) -> Result<Arc<Mutex<SourceIndex>>, RustdocError> {
        let mut cache = self.index_by_version.lock().await;
        let key = (package.to_owned(), version.to_owned());
        if let Some(index) = cache.get(&key) {
            return Ok(Arc::clone(index));
        }
        let graph = self
            .cargo_source
            .graph(&self.config.cache_dir, package, version)
            .await?;
        let index = Arc::new(Mutex::new(SourceIndex::new(graph)));
        cache.insert(key, Arc::clone(&index));
        Ok(index)
    }

    pub(crate) async fn declaration(
        &self,
        package: &str,
        version: &str,
        receiver: &str,
        callable: Option<(&str, PlanCallableKind)>,
    ) -> Result<SourceItem, RustdocError> {
        let index = self.index(package, version).await?;
        let package = package.to_owned();
        let version = version.to_owned();
        let receiver = receiver.to_owned();
        let callable = callable.map(|(name, kind)| (name.to_owned(), kind));
        tokio::task::spawn_blocking(move || {
            let mut index = index.lock().map_err(|_| {
                RustdocError::Unavailable("Cargo source index lock poisoned".into())
            })?;
            match callable {
                Some((name, kind)) => index.callable(&package, &version, &receiver, &name, kind),
                None => index.type_item(&package, &version, &receiver),
            }
        })
        .await
        .map_err(|error| {
            RustdocError::Unavailable(format!("Cargo source indexing stopped: {error}"))
        })?
    }

    pub async fn type_hover(
        &self,
        package: &str,
        version: &str,
        receiver: &str,
    ) -> Result<RustdocHover, RustdocError> {
        Ok(hover(
            self.declaration(package, version, receiver, None).await?,
        ))
    }

    pub async fn callable_hover(
        &self,
        package: &str,
        version: &str,
        receiver: &str,
        callable: &str,
        kind: PlanCallableKind,
    ) -> Result<RustdocHover, RustdocError> {
        Ok(hover(
            self.declaration(package, version, receiver, Some((callable, kind)))
                .await?,
        ))
    }

    pub async fn type_source(
        &self,
        package: &str,
        version: &str,
        receiver: &str,
    ) -> Result<RustdocSourceLocation, RustdocError> {
        Ok(self
            .declaration(package, version, receiver, None)
            .await?
            .location)
    }

    pub async fn callable_source(
        &self,
        package: &str,
        version: &str,
        receiver: &str,
        callable: &str,
        kind: PlanCallableKind,
    ) -> Result<RustdocSourceLocation, RustdocError> {
        Ok(self
            .declaration(package, version, receiver, Some((callable, kind)))
            .await?
            .location)
    }
}

fn hover(item: SourceItem) -> RustdocHover {
    RustdocHover {
        path: format!("{}::{}", item.location.package.replace('-', "_"), item.path),
        package: item.location.package,
        version: item.location.version,
        signature: item.signature,
        docs: item.docs,
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

    #[tokio::test]
    async fn hover_and_source_share_local_declarations_without_docs_rs() {
        let (directory, index) = super::super::index::test::fixture();
        let resolver = RustdocResolver::new(RustdocResolverConfig {
            crates_io_base: "http://127.0.0.1:1".into(),
            cache_dir: directory.path().join("cache"),
            cargo_source: CargoSourceResolverConfig {
                cargo_executable: "missing-cargo".into(),
                cargo_home: directory.path().join("cargo"),
            },
        })
        .unwrap();
        resolver.index_by_version.lock().await.insert(
            ("facade".into(), "1.0.0".into()),
            Arc::new(Mutex::new(index)),
        );
        let hover = resolver
            .callable_hover(
                "facade",
                "1.0.0",
                "Clock",
                "elapsed",
                PlanCallableKind::Method,
            )
            .await
            .unwrap();
        let location = resolver
            .callable_source(
                "facade",
                "1.0.0",
                "Clock",
                "elapsed",
                PlanCallableKind::Method,
            )
            .await
            .unwrap();
        assert_eq!(hover.package, location.package);
        assert!(hover.signature.contains("f32"));
        assert!(location.path.is_file());
    }

    #[tokio::test]
    #[ignore = "downloads exact Bevy sources through Cargo"]
    async fn resolves_bevy_source_without_docs_rs() {
        let directory = tempfile::tempdir().unwrap();
        let resolver =
            RustdocResolver::new(RustdocResolverConfig::production(directory.path()).unwrap())
                .unwrap();
        for (receiver, callable, kind) in [
            ("App", "new", PlanCallableKind::Function),
            ("Time", "delta_secs", PlanCallableKind::Method),
            ("ButtonInput", "pressed", PlanCallableKind::Method),
            ("Sprite", "from_color", PlanCallableKind::Function),
            ("Transform", "from_xyz", PlanCallableKind::Function),
        ] {
            let hover = resolver
                .callable_hover("bevy", "0.19.1", receiver, callable, kind)
                .await
                .unwrap();
            let location = resolver
                .callable_source("bevy", "0.19.1", receiver, callable, kind)
                .await
                .unwrap();
            assert!(location.path.is_file());
            println!(
                "{receiver}::{callable}: {} at {}:{}",
                hover.signature,
                location.path.display(),
                location.line
            );
        }
    }
}

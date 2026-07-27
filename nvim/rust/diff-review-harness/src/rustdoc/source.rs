use super::{RustdocError, RustdocSourceSpan};
use semver::Version;
use serde::Serialize;
use std::collections::HashMap;
use std::path::{Component, Path, PathBuf};
use std::sync::Arc;
use tokio::process::Command;
use tokio::sync::Mutex;

#[derive(Clone, Debug)]
pub struct CargoSourceResolverConfig {
    pub cargo_executable: PathBuf,
    pub cargo_home: PathBuf,
}

impl CargoSourceResolverConfig {
    pub fn production() -> anyhow::Result<Self> {
        Ok(Self {
            cargo_executable: PathBuf::from("cargo"),
            cargo_home: home::cargo_home()?,
        })
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct RustdocSourceLocation {
    pub package: String,
    pub version: String,
    pub path: PathBuf,
    pub line: usize,
    pub column: usize,
}

pub struct CargoSourceResolver {
    config: CargoSourceResolverConfig,
    package_lock_by_version: Mutex<HashMap<(String, String), Arc<Mutex<()>>>>,
}

impl CargoSourceResolver {
    pub fn new(config: CargoSourceResolverConfig) -> Self {
        Self {
            config,
            package_lock_by_version: Mutex::new(HashMap::new()),
        }
    }

    pub async fn source_location(
        &self,
        package: &str,
        version: &str,
        span: &RustdocSourceSpan,
    ) -> Result<RustdocSourceLocation, RustdocError> {
        validate_package(package)?;
        Version::parse(version).map_err(|error| {
            RustdocError::Missing(format!(
                "dependency `{package}` has invalid resolved version `{version}`: {error}"
            ))
        })?;
        let lock = {
            let mut lock_by_version = self.package_lock_by_version.lock().await;
            Arc::clone(
                lock_by_version
                    .entry((package.to_owned(), version.to_owned()))
                    .or_insert_with(|| Arc::new(Mutex::new(()))),
            )
        };
        let _guard = lock.lock().await;
        let source_root = match self.find_source_root(package, version).await? {
            Some(source_root) => source_root,
            None => {
                self.fetch(package, version).await?;
                self.find_source_root(package, version)
                    .await?
                    .ok_or_else(|| {
                        RustdocError::Missing(format!(
                            "Cargo fetched `{package}` {version} but its extracted source was not found"
                        ))
                    })?
            }
        };
        resolve_span(package, version, &source_root, span).await
    }

    async fn find_source_root(
        &self,
        package: &str,
        version: &str,
    ) -> Result<Option<PathBuf>, RustdocError> {
        let registry_source_root = self.config.cargo_home.join("registry").join("src");
        let package_directory = format!("{package}-{version}");
        tokio::task::spawn_blocking(move || {
            if !registry_source_root.exists() {
                return Ok(None);
            }
            let mut candidate_list = Vec::new();
            let registry_list = std::fs::read_dir(&registry_source_root).map_err(|error| {
                RustdocError::Unavailable(format!(
                    "could not read Cargo registry source cache {}: {error}",
                    registry_source_root.display()
                ))
            })?;
            for registry in registry_list {
                let registry = registry.map_err(|error| {
                    RustdocError::Unavailable(format!(
                        "could not inspect Cargo registry source cache: {error}"
                    ))
                })?;
                let candidate = registry.path().join(&package_directory);
                if candidate.is_dir() && candidate.join(".cargo-ok").is_file() {
                    candidate_list.push(candidate);
                }
            }
            match candidate_list.as_slice() {
                [] => Ok(None),
                [candidate] => Ok(Some(candidate.clone())),
                _ => Err(RustdocError::Ambiguous(format!(
                    "Cargo registry cache contains {} complete copies of `{package_directory}`",
                    candidate_list.len()
                ))),
            }
        })
        .await
        .map_err(|error| {
            RustdocError::Unavailable(format!("Cargo source cache lookup stopped: {error}"))
        })?
    }

    async fn fetch(&self, package: &str, version: &str) -> Result<(), RustdocError> {
        let package_specification = format!("{package}@{version}");
        let output = Command::new(&self.config.cargo_executable)
            .env("CARGO_HOME", &self.config.cargo_home)
            .arg("info")
            .arg(&package_specification)
            .arg("--registry")
            .arg("crates-io")
            .arg("--color")
            .arg("never")
            .output()
            .await
            .map_err(|error| {
                RustdocError::Unavailable(format!(
                    "could not run Cargo to fetch `{package_specification}`: {error}"
                ))
            })?;
        if output.status.success() {
            return Ok(());
        }
        let stderr = String::from_utf8_lossy(&output.stderr).trim().to_owned();
        let stdout = String::from_utf8_lossy(&output.stdout).trim().to_owned();
        let detail = if !stderr.is_empty() {
            stderr
        } else if !stdout.is_empty() {
            stdout
        } else {
            format!("Cargo exited with {}", output.status)
        };
        Err(RustdocError::Unavailable(format!(
            "Cargo could not fetch `{package_specification}`: {detail}"
        )))
    }
}

async fn resolve_span(
    package: &str,
    version: &str,
    source_root: &Path,
    span: &RustdocSourceSpan,
) -> Result<RustdocSourceLocation, RustdocError> {
    let relative_path = normalized_span_path(&span.filename)?;
    let candidate = source_root.join(&relative_path);
    let canonical_source_root = tokio::fs::canonicalize(source_root)
        .await
        .map_err(|error| {
            RustdocError::Unavailable(format!(
                "could not resolve Cargo source root {}: {error}",
                source_root.display()
            ))
        })?;
    let canonical_path = tokio::fs::canonicalize(&candidate).await.map_err(|error| {
        RustdocError::Missing(format!(
            "Rustdoc source `{}` for `{package}` {version} is unavailable: {error}",
            relative_path.display()
        ))
    })?;
    if !canonical_path.starts_with(&canonical_source_root) {
        return Err(RustdocError::Unavailable(format!(
            "Rustdoc source `{}` escapes the Cargo package root",
            span.filename.display()
        )));
    }
    if span.begin.0 == 0 || span.begin.1 == 0 {
        return Err(RustdocError::Unavailable(format!(
            "Rustdoc returned an invalid source position for `{package}` {version}"
        )));
    }
    Ok(RustdocSourceLocation {
        package: package.to_owned(),
        version: version.to_owned(),
        path: candidate,
        line: span.begin.0,
        column: span.begin.1,
    })
}

fn normalized_span_path(filename: &Path) -> Result<PathBuf, RustdocError> {
    let mut normalized = PathBuf::new();
    for component in filename.components() {
        match component {
            Component::Normal(component) => normalized.push(component),
            Component::CurDir => {}
            Component::ParentDir | Component::RootDir | Component::Prefix(_) => {
                return Err(RustdocError::Unavailable(format!(
                    "Rustdoc source path `{}` is not package-relative",
                    filename.display()
                )));
            }
        }
    }
    if normalized.as_os_str().is_empty() {
        return Err(RustdocError::Unavailable(
            "Rustdoc returned an empty source path".into(),
        ));
    }
    Ok(normalized)
}

fn validate_package(package: &str) -> Result<(), RustdocError> {
    if !package.is_empty()
        && package
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_'))
    {
        return Ok(());
    }
    Err(RustdocError::Missing(format!(
        "dependency name `{package}` is not a valid Cargo package name"
    )))
}

#[cfg(test)]
mod test {
    use super::*;

    fn cached_package(temp: &Path, package: &str, version: &str) -> PathBuf {
        let root = temp
            .join("registry")
            .join("src")
            .join("index.crates.io-test")
            .join(format!("{package}-{version}"));
        std::fs::create_dir_all(root.join("src")).unwrap();
        std::fs::write(root.join(".cargo-ok"), b"complete").unwrap();
        std::fs::write(root.join("src").join("lib.rs"), b"pub struct Version;\n").unwrap();
        root
    }

    #[tokio::test]
    async fn resolves_a_rustdoc_span_from_the_existing_cargo_cache() {
        let temp = tempfile::tempdir().unwrap();
        let root = cached_package(temp.path(), "semver", "1.0.27");
        let resolver = CargoSourceResolver::new(CargoSourceResolverConfig {
            cargo_executable: temp.path().join("missing-cargo"),
            cargo_home: temp.path().to_owned(),
        });
        let location = resolver
            .source_location(
                "semver",
                "1.0.27",
                &RustdocSourceSpan {
                    filename: PathBuf::from("src/lib.rs"),
                    begin: (1, 5),
                    end: (1, 18),
                },
            )
            .await
            .unwrap();
        assert_eq!(location.path, root.join("src").join("lib.rs"));
        assert_eq!((location.line, location.column), (1, 5));
    }

    #[tokio::test]
    async fn rejects_source_paths_that_escape_the_cached_package() {
        let temp = tempfile::tempdir().unwrap();
        cached_package(temp.path(), "semver", "1.0.27");
        let resolver = CargoSourceResolver::new(CargoSourceResolverConfig {
            cargo_executable: temp.path().join("missing-cargo"),
            cargo_home: temp.path().to_owned(),
        });
        let error = resolver
            .source_location(
                "semver",
                "1.0.27",
                &RustdocSourceSpan {
                    filename: PathBuf::from("../outside.rs"),
                    begin: (1, 1),
                    end: (1, 1),
                },
            )
            .await
            .unwrap_err();
        assert!(error.to_string().contains("not package-relative"));
    }

    #[tokio::test]
    async fn rejects_ambiguous_registry_cache_entries() {
        let temp = tempfile::tempdir().unwrap();
        cached_package(temp.path(), "semver", "1.0.27");
        let second = temp
            .path()
            .join("registry")
            .join("src")
            .join("second-registry")
            .join("semver-1.0.27");
        std::fs::create_dir_all(&second).unwrap();
        std::fs::write(second.join(".cargo-ok"), b"complete").unwrap();
        let resolver = CargoSourceResolver::new(CargoSourceResolverConfig {
            cargo_executable: temp.path().join("missing-cargo"),
            cargo_home: temp.path().to_owned(),
        });
        let error = resolver
            .find_source_root("semver", "1.0.27")
            .await
            .unwrap_err();
        assert!(matches!(error, RustdocError::Ambiguous(_)));
    }
}

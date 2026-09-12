use std::fs::File;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};

const MAX_CACHE_BYTES: u64 = 16 * 1024;
const DISCOVERY_OVERRIDE: &[&str] = &[
    "EXEPATH",
    "GIT_EXEC_PATH",
    "GIT_CONFIG_SYSTEM",
    "GIT_CONFIG_NOSYSTEM",
    "GIT_ATTR_NOSYSTEM",
];

#[derive(Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct ConfigLocation {
    version: u32,
    system_prefix: PathBuf,
}

/// Seeds gix's process cache from a persisted Windows system prefix before repository reads.
///
/// Cache misses discover and atomically store the prefix. Configuration contents are never cached.
/// Explicit discovery overrides and non-Windows hosts bypass persistence. I/O errors leave gix's
/// normal discovery available and must be reported by the caller without rejecting host startup.
pub fn configure(cache_path: &Path) -> Result<&'static str> {
    if !cfg!(windows) || has_discovery_override(|name| std::env::var_os(name).is_some()) {
        return Ok("bypassed");
    }
    ensure!(
        cache_path.is_absolute(),
        "Git configuration cache path must be absolute"
    );
    let (prefix, cached) = resolve(cache_path, || {
        gix::path::env::system_prefix().map(Path::to_path_buf)
    })?;
    let Some(prefix) = prefix else {
        return Ok("unavailable");
    };
    if cached {
        if let Err(prefix) = gix::path::env::set_system_prefix(prefix) {
            ensure!(
                gix::path::env::system_prefix() == Some(prefix.as_path()),
                "Git system prefix was initialized before its persisted location was loaded"
            );
        }
    }
    Ok(if cached { "hit" } else { "miss" })
}

fn has_discovery_override(mut present: impl FnMut(&str) -> bool) -> bool {
    DISCOVERY_OVERRIDE.iter().any(|name| present(name))
}

fn resolve(
    cache_path: &Path,
    discover: impl FnOnce() -> Option<PathBuf>,
) -> Result<(Option<PathBuf>, bool)> {
    match File::open(cache_path) {
        Ok(file) => {
            let mut bytes = Vec::new();
            file.take(MAX_CACHE_BYTES + 1)
                .read_to_end(&mut bytes)
                .context("read Git configuration location cache")?;
            if bytes.len() <= MAX_CACHE_BYTES as usize
                && let Ok(location) = serde_json::from_slice::<ConfigLocation>(&bytes)
                && location.version == 1
                && location.system_prefix.is_absolute()
                && location.system_prefix.is_dir()
            {
                return Ok((Some(location.system_prefix), true));
            }
        }
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
        Err(error) => return Err(error).context("open Git configuration location cache"),
    }
    let Some(prefix) = discover() else {
        return Ok((None, false));
    };
    ensure!(
        prefix.is_absolute() && prefix.is_dir(),
        "discovered Git system prefix is not an existing absolute directory"
    );
    let parent = cache_path
        .parent()
        .context("Git configuration cache has no parent")?;
    std::fs::create_dir_all(parent).context("create Git configuration cache directory")?;
    let encoded = serde_json::to_vec(&ConfigLocation {
        version: 1,
        system_prefix: prefix.clone(),
    })?;
    ensure!(
        encoded.len() <= MAX_CACHE_BYTES as usize,
        "Git configuration cache exceeds 16 KiB"
    );
    let mut temporary = tempfile::NamedTempFile::new_in(parent)
        .context("create Git configuration cache temporary file")?;
    temporary
        .write_all(&encoded)
        .context("write Git configuration location cache")?;
    temporary
        .persist(cache_path)
        .context("publish Git configuration location cache")?;
    Ok((Some(prefix), false))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn persisted_location_skips_discovery_and_configuration_contents_remain_live() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let prefix = directory.path().join("installation");
        std::fs::create_dir_all(prefix.join("etc"))?;
        let cache = directory.path().join("cache/location.json");
        assert_eq!(
            resolve(&cache, || Some(prefix.clone()))?,
            (Some(prefix.clone()), false)
        );
        std::fs::write(prefix.join("etc/gitconfig"), "[core]\n autocrlf = true\n")?;
        assert_eq!(
            resolve(&cache, || panic!("cache hit launched Git"))?,
            (Some(prefix.clone()), true)
        );
        std::fs::write(prefix.join("etc/gitconfig"), "[core]\n autocrlf = false\n")?;
        assert_eq!(
            resolve(&cache, || panic!(
                "configuration edit invalidated its location"
            ))?,
            (Some(prefix), true)
        );
        assert!(!String::from_utf8(std::fs::read(cache)?)?.contains("autocrlf"));
        Ok(())
    }

    #[test]
    fn invalid_cache_and_removed_installation_are_rediscovered() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let prefix = directory.path().join("current");
        std::fs::create_dir(&prefix)?;
        let cache = directory.path().join("location.json");
        for bytes in [
            b"broken".to_vec(),
            vec![b' '; MAX_CACHE_BYTES as usize + 1],
            serde_json::to_vec(&ConfigLocation {
                version: 9,
                system_prefix: prefix.clone(),
            })?,
            serde_json::to_vec(&ConfigLocation {
                version: 1,
                system_prefix: "relative".into(),
            })?,
            serde_json::to_vec(&ConfigLocation {
                version: 1,
                system_prefix: directory.path().join("removed"),
            })?,
        ] {
            std::fs::write(&cache, bytes)?;
            assert_eq!(
                resolve(&cache, || Some(prefix.clone()))?,
                (Some(prefix.clone()), false)
            );
        }
        std::fs::remove_file(&cache)?;
        assert_eq!(
            resolve(&cache, || Some(prefix.clone()))?,
            (Some(prefix), false)
        );
        Ok(())
    }

    #[test]
    fn explicit_overrides_bypass_the_persisted_default() {
        for override_name in DISCOVERY_OVERRIDE {
            assert!(has_discovery_override(|name| name == *override_name));
        }
        assert!(!has_discovery_override(|_| false));
    }

    #[test]
    fn unavailable_discovery_and_cache_io_failure_are_distinct() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let cache = directory.path().join("location.json");
        assert_eq!(resolve(&cache, || None)?, (None, false));
        assert!(!cache.exists());
        let blocker = directory.path().join("file");
        std::fs::write(&blocker, "not a directory")?;
        assert!(
            resolve(&blocker.join("location.json"), || Some(
                directory.path().to_owned()
            ))
            .is_err()
        );
        Ok(())
    }
}

//! Repository user metadata retains current cache paths and publishes through atomic replacement.

use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};

use crate::lease::RepositoryLease;
use crate::model::GithubRepositoryId;
use crate::remote::RemoteFailure;

pub const MAX_METADATA_BYTES: usize = crate::publication::MAX_CACHE_JSON_BYTES;
pub const MAX_USERS: usize = 100_000;

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct RepositoryUser {
    pub login: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct RepositoryMetadata {
    pub repo: String,
    /// Legacy records omit hostname and inherit identity from their validated cache path.
    #[serde(default)]
    pub hostname: Option<String>,
    pub fetched_at: u64,
    pub contributors: Vec<RepositoryUser>,
    /// Failed sources retained when another source completed successfully.
    #[serde(default)]
    pub failure: Vec<RemoteFailure>,
}

#[derive(Clone, Debug)]
pub struct RepositoryUsers {
    pub contributors: Vec<RepositoryUser>,
    pub failure: Vec<RemoteFailure>,
}

#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct MetadataRequest {
    pub repository: GithubRepositoryId,
    #[serde(default = "default_ttl")]
    pub ttl_seconds: u64,
}

fn default_ttl() -> u64 {
    600
}

#[derive(Clone, Debug)]
pub struct MetadataStore {
    directory: PathBuf,
    repository: GithubRepositoryId,
}

impl MetadataStore {
    /// Validates the current hostname/repos/owner/name layout without moving existing data.
    pub fn new(directory: PathBuf, repository: GithubRepositoryId) -> Result<Self> {
        let directory = std::path::absolute(directory)?;
        let name = repository.repository_name();
        let (owner, name) = name
            .split_once('/')
            .context("metadata repository omits owner")?;
        let owner_directory = directory
            .parent()
            .context("metadata directory omits owner")?;
        let repositories = owner_directory
            .parent()
            .context("metadata directory omits repos")?;
        let host = repositories
            .parent()
            .context("metadata directory omits hostname")?;
        let encoded_host: String = repository
            .hostname()
            .bytes()
            .map(|byte| {
                if byte.is_ascii_alphanumeric() || matches!(byte, b'.' | b'-') {
                    char::from(byte).to_string()
                } else {
                    format!("%{byte:02X}")
                }
            })
            .collect();
        for (path, expected) in [
            (directory.as_path(), name),
            (owner_directory, owner),
            (repositories, "repos"),
            (host, encoded_host.as_str()),
        ] {
            ensure!(
                path.file_name()
                    .and_then(|name| name.to_str())
                    .is_some_and(|name| name.eq_ignore_ascii_case(expected)),
                "metadata cache path differs from its repository identity"
            );
        }
        Ok(Self {
            directory,
            repository,
        })
    }

    pub(crate) fn directory(&self) -> &Path {
        &self.directory
    }

    /// Reads one complete file under deletion exclusion, returning None only for a missing file.
    pub fn read(&self) -> Result<Option<RepositoryMetadata>> {
        let _lease = RepositoryLease::operation(&self.directory)?;
        let path = self.directory.join("metadata.json");
        let file = match fs::File::open(&path) {
            Ok(file) => file,
            Err(failure) if failure.kind() == std::io::ErrorKind::NotFound => return Ok(None),
            Err(failure) => return Err(failure).context("read repository metadata"),
        };
        let mut encoded = Vec::new();
        file.take((MAX_METADATA_BYTES + 1) as u64)
            .read_to_end(&mut encoded)?;
        ensure!(
            encoded.len() <= MAX_METADATA_BYTES,
            "repository metadata exceeds its byte limit"
        );
        let metadata: RepositoryMetadata =
            serde_json::from_slice(&encoded).context("decode repository metadata")?;
        self.validate(&metadata)?;
        Ok(Some(metadata))
    }

    /// Replaces the file after validation and flush. The caller retains metadata refresh ownership.
    pub(crate) fn publish(&self, metadata: &RepositoryMetadata) -> Result<()> {
        self.validate(metadata)?;
        crate::publication::publish_json(&self.directory.join("metadata.json"), metadata)
    }

    fn validate(&self, metadata: &RepositoryMetadata) -> Result<()> {
        ensure!(
            metadata
                .repo
                .eq_ignore_ascii_case(&self.repository.repository_name())
                && metadata
                    .hostname
                    .as_ref()
                    .is_none_or(|host| host.eq_ignore_ascii_case(self.repository.hostname())),
            "repository metadata identity differs from its cache path"
        );
        ensure!(
            metadata.contributors.len() <= MAX_USERS && metadata.failure.len() <= 2,
            "repository metadata exceeds its record limit"
        );
        for user in &metadata.contributors {
            validate_user(user)?;
        }
        Ok(())
    }
}

pub(crate) fn validate_user(user: &RepositoryUser) -> Result<()> {
    ensure!(
        !user.login.is_empty()
            && user.login.len() <= 256
            && !user
                .login
                .chars()
                .any(|character| character.is_whitespace() || character.is_control()),
        "repository user has an invalid login"
    );
    ensure!(
        user.name.as_ref().is_none_or(|name| name.len() <= 1024),
        "repository user name exceeds its byte limit"
    );
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn oversized_publication_preserves_the_previous_file_and_removes_temporary_output() -> Result<()>
    {
        let root = tempfile::tempdir()?;
        let directory = root.path().join("github.com/repos/owner/repo");
        let _lease = RepositoryLease::metadata(&directory)?;
        let store = MetadataStore::new(
            directory.clone(),
            GithubRepositoryId::new("github.com", "owner", "repo").unwrap(),
        )?;
        let mut metadata = RepositoryMetadata {
            repo: "owner/repo".into(),
            hostname: Some("github.com".into()),
            fetched_at: 1,
            contributors: Vec::new(),
            failure: Vec::new(),
        };
        store.publish(&metadata)?;
        let previous = fs::read(directory.join("metadata.json"))?;
        metadata.contributors = (0..9000)
            .map(|number| RepositoryUser {
                login: format!("user-{number}"),
                name: Some("\"".repeat(1024)),
            })
            .collect();
        let failure = store.publish(&metadata).unwrap_err();
        assert!(failure.to_string().contains("byte limit"));
        assert_eq!(fs::read(directory.join("metadata.json"))?, previous);
        assert_eq!(fs::read_dir(directory)?.count(), 1);
        Ok(())
    }
}

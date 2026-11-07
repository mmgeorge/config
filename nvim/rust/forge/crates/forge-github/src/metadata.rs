//! Repository user metadata retains current cache paths and publishes through atomic replacement.

use std::fs;
use std::io::BufReader;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};

use crate::lease::RepositoryLease;
use crate::model::GithubRepositoryId;
use crate::remote::RemoteFailure;

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
        let metadata: RepositoryMetadata =
            serde_json::from_reader(BufReader::new(file)).context("decode repository metadata")?;
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
        for user in &metadata.contributors {
            validate_user(user)?;
        }
        Ok(())
    }
}

pub(crate) fn validate_user(user: &RepositoryUser) -> Result<()> {
    ensure!(
        !user.login.is_empty()
            && !user
                .login
                .chars()
                .any(|character| character.is_whitespace() || character.is_control()),
        "repository user has an invalid login"
    );
    Ok(())
}

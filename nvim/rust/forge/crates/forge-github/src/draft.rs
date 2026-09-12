use std::{
    fs::{self, File, OpenOptions},
    io::Read,
    path::PathBuf,
};

use anyhow::{Context, Result, ensure};
use serde_json::Value;

use crate::recovery::{RecoveryResource, RecoveryStore};
use crate::service::GithubService;

pub(crate) struct DraftStore {
    repository: PathBuf,
    path: PathBuf,
}

impl DraftStore {
    pub(crate) fn new(recovery: &RecoveryStore, resource: &RecoveryResource) -> Result<Self> {
        resource.validate()?;
        if resource.kind == crate::recovery::RecoveryResourceKind::Repository {
            let repository_name = resource.repository.repository_name();
            let (owner, name) = repository_name
                .split_once('/')
                .context("creation repository identity is missing")?;
            let repository = recovery
                .directory()
                .parent()
                .context("recovery parent is missing")?
                .join("creation-drafts")
                .join(crate::recovery::component(resource.repository.hostname()))
                .join(crate::recovery::component(owner))
                .join(crate::recovery::component(name));
            let path = repository.join("creation.json");
            return Ok(Self { repository, path });
        }
        let data = recovery
            .directory()
            .ancestors()
            .nth(4)
            .context("recovery root has no data directory")?;
        let repository = data
            .join("forge/github")
            .join(resource.repository.hostname())
            .join("repos")
            .join(resource.repository.repository_name());
        let path = repository
            .join("reviews")
            .join(resource.number.to_string())
            .join("review.json");
        Ok(Self { repository, path })
    }

    pub(crate) fn read(&self) -> Result<Option<Value>> {
        let _repository = crate::lease::RepositoryLease::operation(&self.repository)?;
        self.read_owned()
    }

    fn read_owned(&self) -> Result<Option<Value>> {
        if !self.path.try_exists()? {
            return Ok(None);
        }
        let mut bytes = Vec::new();
        File::open(&self.path)?
            .take(8 * 1024 * 1024 + 1)
            .read_to_end(&mut bytes)?;
        ensure!(bytes.len() <= 8 * 1024 * 1024, "review draft exceeds 8 MiB");
        let value = serde_json::from_slice(&bytes)
            .context("review draft is malformed and was preserved")?;
        Ok(Some(value))
    }

    pub(crate) fn merge(&self, patch: Value) -> Result<()> {
        let _repository = crate::lease::RepositoryLease::operation(&self.repository)?;
        fs::create_dir_all(&self.repository)?;
        let ownership = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .open(self.repository.join(".forge-review-draft.lock"))?;
        ownership
            .try_lock()
            .context("review draft publication is busy")?;
        let mut current = self.read_owned()?.unwrap_or_else(|| serde_json::json!({}));
        let fields = current
            .as_object_mut()
            .context("existing review draft is not an object")?;
        for (key, value) in patch
            .as_object()
            .context("review draft patch is not an object")?
        {
            if key == "creation" {
                let incoming = value
                    .get("sequence")
                    .and_then(Value::as_u64)
                    .context("creation draft sequence is missing")?;
                ensure!(
                    incoming > 0 && incoming <= (1u64 << 53) - 1,
                    "creation draft sequence is invalid"
                );
                if let Some(existing) = fields.get(key) {
                    let previous = existing
                        .get("sequence")
                        .and_then(Value::as_u64)
                        .context("stored creation draft sequence is missing")?;
                    if previous > incoming {
                        continue;
                    }
                    ensure!(
                        previous != incoming || existing == value,
                        "creation draft sequence was reused for different text"
                    );
                }
            }
            if key == "retired_operation" {
                let retained = fields
                    .entry(key.clone())
                    .or_insert_with(|| serde_json::json!({}))
                    .as_object_mut()
                    .context("retired operation store is malformed")?;
                for (operation_id, record) in value
                    .as_object()
                    .context("retired operations must preserve their captured records")?
                {
                    ensure!(
                        retained
                            .get(operation_id)
                            .is_none_or(|existing| existing == record),
                        "retired operation receipt cannot be replaced"
                    );
                    retained.insert(operation_id.clone(), record.clone());
                }
                continue;
            }
            fields.insert(key.clone(), value.clone());
        }
        ensure!(
            serde_json::to_vec(&current)?.len() <= 8 * 1024 * 1024,
            "combined review draft exceeds 8 MiB"
        );
        crate::publication::publish_json(&self.path, &current)
    }
}

pub(crate) fn validate_patch(resource: &RecoveryResource, draft: &Value) -> Result<()> {
    ensure!(
        serde_json::to_vec(draft)?.len() <= 8 * 1024 * 1024,
        "review draft exceeds 8 MiB"
    );
    ensure!(
        draft.get("repo").and_then(Value::as_str)
            == Some(resource.repository.repository_name().as_str())
            && draft.get("number").and_then(Value::as_u64) == Some(resource.number),
        "review draft has a different resource identity"
    );
    Ok(())
}

impl GithubService {
    pub async fn review_draft(&self, resource: RecoveryResource) -> Result<Option<Value>> {
        let store = DraftStore::new(&self.recovery_store()?, &resource)?;
        self.submit(move || store.read())?
            .await
            .context("review draft read ended without collection")?
    }

    pub async fn review_draft_write(&self, resource: RecoveryResource, draft: Value) -> Result<()> {
        validate_patch(&resource, &draft)?;
        let store = DraftStore::new(&self.recovery_store()?, &resource)?;
        self.submit(move || store.merge(draft))?
            .await
            .context("review draft publication ended without collection")?
    }
}

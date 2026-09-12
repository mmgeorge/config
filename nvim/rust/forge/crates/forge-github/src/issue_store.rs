use std::collections::HashSet;
use std::fs;
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::path::{Path, PathBuf};
use std::thread;
use std::time::{Duration, Instant};
#[cfg(test)]
use std::time::{SystemTime, UNIX_EPOCH};

use anyhow::{Context, Result, anyhow};
use redb::{Database, ReadableTable, TableDefinition};

use crate::completion::snapshot_matches;
use crate::lease::RepositoryLease;
use crate::model::*;
use crate::publication::publish_json;

const ISSUES_TABLE: TableDefinition<&str, &str> = TableDefinition::new("issues");
const ISSUE_DETAILS_TABLE: TableDefinition<&str, &str> = TableDefinition::new("issue_details");
const TERMS_TABLE: TableDefinition<&str, u64> = TableDefinition::new("issue_terms");
const LABELS_TABLE: TableDefinition<&str, u64> = TableDefinition::new("issue_labels");
const SYNC_TABLE: TableDefinition<&str, &str> = TableDefinition::new("sync_state");
const DB_LOCK_RETRY_MS: u64 = 100;
const MAX_ISSUE_REVISION: u64 = 9_007_199_254_740_991;

/// Repository issue storage with no database handle retained between operations.
///
/// Methods block during disk access and lock contention. Async callers must use an owned blocking
/// worker. Each operation opens and closes redb, preserving access between independent processes.
/// Opening failures and database panics return errors without replacing the database.
#[derive(Clone, Debug)]
pub struct IssueStore {
    database_path: PathBuf,
    repo: String,
    lock_timeout: Duration,
}

impl IssueStore {
    /// Validates the repository name without opening a database or creating directories.
    ///
    /// `repo` accepts the existing owner/name and github.com URL forms. `lock_timeout` bounds retry
    /// waits for redb contention, excluding time spent inside native database calls.
    pub fn new(database_path: PathBuf, repo: &str, lock_timeout: Duration) -> Result<Self> {
        Ok(Self {
            database_path,
            repo: normalize_repo(repo)?,
            lock_timeout,
        })
    }

    /// Commits one page, replacing issue indexes and updating sync state in one redb transaction.
    /// Serialization failures precede commit. Callers must reload after a database commit error
    /// before assuming the page was rolled back.
    pub fn upsert_page(&self, scope: SyncScope, page: PageInput) -> Result<UpsertOutput> {
        let _lease = self.operation_lease()?;
        let database = open_database_with_timeout(&self.database_path, self.lock_timeout)?;
        upsert_page(&database, &self.repo, scope, page)
    }

    /// Reads persisted synchronization state, initializing an empty database when absent.
    pub fn read_state(&self) -> Result<RepoSyncState> {
        let _lease = self.operation_lease()?;
        let database = open_database_with_timeout(&self.database_path, self.lock_timeout)?;
        read_state(&database, &self.repo)
    }

    /// Reads one detail record, preserving an explicit `found: false` result for cache misses.
    /// Database and stored-JSON failures return errors instead of cache misses.
    pub fn read_detail(&self, number: u64) -> Result<DetailOutput> {
        let _lease = self.operation_lease()?;
        let database = open_database_with_timeout(&self.database_path, self.lock_timeout)?;
        Ok(detail_output(
            &self.repo,
            number,
            read_detail(&database, &self.repo, number)?,
        ))
    }

    /// Reads details in request order within one transaction, including misses and repeated IDs.
    pub fn read_details(&self, number: &[u64]) -> Result<DetailsOutput> {
        let _lease = self.operation_lease()?;
        let database = open_database_with_timeout(&self.database_path, self.lock_timeout)?;
        Ok(DetailsOutput {
            repo: self.repo.clone(),
            details: read_details(&database, &self.repo, number)?,
        })
    }

    /// Commits a detail record under this repository and the supplied issue number.
    /// Returns the normalized persisted record. Callers must reload after a database commit error
    /// before assuming the previous detail remains current.
    pub fn upsert_detail(&self, number: u64, mut detail: DetailRecord) -> Result<DetailRecord> {
        let _lease = self.operation_lease()?;
        detail.repo = self.repo.clone();
        detail.number = number;
        let database = open_database_with_timeout(&self.database_path, self.lock_timeout)?;
        upsert_detail(&database, &self.repo, number, &detail)?;
        Ok(detail)
    }

    /// Reads completion metadata ordered by descending update time and then ascending issue number.
    /// Issue bodies are excluded. Database and stored-JSON failures return errors.
    pub fn snapshot(&self, state: SnapshotState) -> Result<SnapshotOutput> {
        let _lease = self.operation_lease()?;
        let database = open_database_with_timeout(&self.database_path, self.lock_timeout)?;
        snapshot(&database, &self.repo, state)
    }

    /// Atomically replaces a JSON completion snapshot while retaining database publication ownership.
    ///
    /// Serialization, write, sync, and replacement failures preserve the previous snapshot. The
    /// output must differ from the database path. Success returns the published issue count.
    /// File sync precedes replacement, but directory-entry durability across power loss is not proven.
    pub fn publish_snapshot(&self, state: SnapshotState, output: &Path) -> Result<usize> {
        let _lease = self.operation_lease()?;
        let database = open_database_with_timeout(&self.database_path, self.lock_timeout)?;
        self.validate_snapshot_output(output)?;
        let snapshot = snapshot(&database, &self.repo, state)?;
        publish_json(output, &snapshot)?;
        Ok(snapshot.issue_count)
    }

    /// Repairs missing, legacy, or mismatched completion files before a caller adopts their state.
    /// A database without a committed issue page does not create a successful empty snapshot.
    pub fn reconcile_snapshot(
        &self,
        filter: SnapshotState,
        output: &Path,
    ) -> Result<SnapshotRecovery> {
        let _lease = self.operation_lease()?;
        let database = open_database_with_timeout(&self.database_path, self.lock_timeout)?;
        self.validate_snapshot_output(output)?;
        let state = read_state(&database, &self.repo)?;
        let ready = state.revision > 0
            || state.issue_count > 0
            || state.open_historical_complete
            || state.all_historical_complete;
        let republished = ready && !snapshot_matches(output, &self.repo, filter, state.revision)?;
        if republished {
            publish_json(output, &snapshot(&database, &self.repo, filter)?)?;
        }
        Ok(SnapshotRecovery {
            state,
            ready,
            republished,
        })
    }

    fn validate_snapshot_output(&self, output: &Path) -> Result<()> {
        if output.exists() && fs::canonicalize(output)? == fs::canonicalize(&self.database_path)? {
            return Err(anyhow!(
                "issue snapshot output must not replace its database"
            ));
        }
        Ok(())
    }

    /// Removes the repository cache under exclusive ownership after closing its database.
    ///
    /// Only the current `repository/issues/issues.redb` layout permits repository deletion.
    /// Contention and database failures preserve the directory. Filesystem removal errors can leave
    /// a partially removed directory and are reported without an automatic retry.
    pub fn delete_repository_cache(&self) -> Result<bool> {
        anyhow::ensure!(
            self.database_path.file_name() == Some(std::ffi::OsStr::new("issues.redb"))
                && self.database_path.parent().and_then(Path::file_name)
                    == Some(std::ffi::OsStr::new("issues")),
            "repository deletion requires the current issues/issues.redb layout"
        );
        let directory = self.repository_directory()?;
        let _lease = RepositoryLease::deletion(&directory)?;
        let directory = match fs::canonicalize(&directory) {
            Ok(directory) => directory,
            Err(failure) if failure.kind() == std::io::ErrorKind::NotFound => return Ok(false),
            Err(failure) => return Err(failure).context("resolve repository cache for deletion"),
        };
        let issue_directory = directory.join("issues");
        anyhow::ensure!(
            !issue_directory.join("sync.lock").try_exists()?,
            "Busy: repository issue sync owns its cache"
        );
        if issue_directory.exists() {
            anyhow::ensure!(
                fs::canonicalize(&issue_directory)? == issue_directory,
                "repository issue directory must not redirect deletion ownership"
            );
        }
        if self.database_path.exists() {
            let resolved = fs::canonicalize(&self.database_path)?;
            anyhow::ensure!(
                resolved == issue_directory.join("issues.redb"),
                "repository database must remain inside its cache directory"
            );
            drop(
                open_database_with_timeout(&resolved, Duration::ZERO).context(
                    "Busy or unavailable: repository database cannot be closed for deletion",
                )?,
            );
        }
        fs::remove_dir_all(&directory)
            .with_context(|| format!("remove repository cache {}", directory.display()))?;
        Ok(true)
    }

    fn operation_lease(&self) -> Result<RepositoryLease> {
        RepositoryLease::operation(&self.repository_directory()?)
    }

    pub(crate) fn repository_directory(&self) -> Result<PathBuf> {
        let database = std::path::absolute(&self.database_path)?;
        let parent = database
            .parent()
            .context("issue database needs a parent directory")?;
        if parent.file_name() == Some(std::ffi::OsStr::new("issues")) {
            Ok(parent
                .parent()
                .context("issue cache needs a repository directory")?
                .to_owned())
        } else {
            Ok(parent.to_owned())
        }
    }

    pub(crate) fn validate_repository(&self, repository: &GithubRepositoryId) -> Result<()> {
        anyhow::ensure!(
            self.repo == repository.repository_name(),
            "sync identity differs from its issue store"
        );
        Ok(())
    }
}

#[cfg(test)]
fn open_database(path: &Path) -> Result<Database> {
    open_database_with_timeout(path, Duration::ZERO)
}

#[derive(Debug)]
enum OpenDatabaseError {
    Database(redb::DatabaseError),
    Other(anyhow::Error),
    Panic(String),
}

fn open_database_with_timeout(path: &Path, timeout: Duration) -> Result<Database> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).with_context(|| format!("create {}", parent.display()))?;
    }
    let started = Instant::now();
    loop {
        match try_open_database(path) {
            Ok(database) => return Ok(database),
            Err(OpenDatabaseError::Database(error))
                if is_database_lock_error(&error)
                    && Instant::now().duration_since(started) < timeout =>
            {
                thread::sleep(
                    Duration::from_millis(DB_LOCK_RETRY_MS)
                        .min(timeout.saturating_sub(started.elapsed())),
                );
            }
            Err(OpenDatabaseError::Database(error)) => {
                let waited_ms = Instant::now().duration_since(started).as_millis();
                return Err(anyhow!(error)).with_context(|| {
                    format!(
                        "open {} after waiting {}ms for database lock",
                        path.display(),
                        waited_ms
                    )
                });
            }
            Err(OpenDatabaseError::Panic(message)) => {
                return Err(anyhow!(
                    "redb panicked while opening {}, database retained for exclusive recovery: {}",
                    path.display(),
                    message
                ));
            }
            Err(OpenDatabaseError::Other(error)) => return Err(error),
        }
    }
}

fn try_open_database(path: &Path) -> std::result::Result<Database, OpenDatabaseError> {
    catch_unwind(AssertUnwindSafe(|| {
        let database = Database::create(path).map_err(OpenDatabaseError::Database)?;
        initialize_tables(&database).map_err(OpenDatabaseError::Other)?;
        Ok(database)
    }))
    .map_err(|payload| OpenDatabaseError::Panic(panic_payload_message(payload)))?
}

fn panic_payload_message(payload: Box<dyn std::any::Any + Send>) -> String {
    match payload.downcast::<String>() {
        Ok(message) => *message,
        Err(payload) => match payload.downcast::<&'static str>() {
            Ok(message) => (*message).to_owned(),
            Err(_) => "unknown panic payload".to_owned(),
        },
    }
}

#[cfg(test)]
fn archive_corrupt_database(path: &Path, reason: &str) -> Result<PathBuf> {
    if !path.exists() {
        return Err(anyhow!(
            "redb panicked while opening {}, but no database file exists: {}",
            path.display(),
            reason
        ));
    }

    let timestamp_ms = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis();
    let file_name = path
        .file_name()
        .and_then(|name| name.to_str())
        .ok_or_else(|| anyhow!("database path has no UTF-8 file name: {}", path.display()))?;
    let archived = path.with_file_name(format!("{file_name}.corrupt-{timestamp_ms}"));
    fs::rename(path, &archived)
        .with_context(|| format!("archive corrupt database {}", path.display()))?;
    Ok(archived)
}

fn is_database_lock_error(error: &redb::DatabaseError) -> bool {
    let message = error.to_string();
    message.contains("Database already open") || message.contains("Cannot acquire lock")
}

fn initialize_tables(database: &Database) -> Result<()> {
    let transaction = database.begin_write()?;
    {
        transaction.open_table(ISSUES_TABLE)?;
        transaction.open_table(ISSUE_DETAILS_TABLE)?;
        transaction.open_table(TERMS_TABLE)?;
        transaction.open_table(LABELS_TABLE)?;
        transaction.open_table(SYNC_TABLE)?;
    }
    transaction.commit()?;
    Ok(())
}

fn normalize_repo(repo: &str) -> Result<String> {
    let trimmed = repo
        .trim()
        .trim_start_matches("https://github.com/")
        .trim_end_matches(".git");
    let mut parts = trimmed.split('/');
    let owner = parts.next().unwrap_or("").trim();
    let name = parts.next().unwrap_or("").trim();
    if owner.is_empty() || name.is_empty() || parts.next().is_some() {
        return Err(anyhow!("invalid GitHub repo: {repo}"));
    }
    Ok(format!("{}/{}", owner.to_lowercase(), name.to_lowercase()))
}

fn issue_key(repo: &str, number: u64) -> String {
    format!("{}\0{:020}", repo, number)
}

fn index_key(repo: &str, value: &str, number: u64) -> String {
    format!("{}\0{}\0{:020}", repo, value, number)
}

fn repo_prefix(repo: &str) -> String {
    format!("{}\0", repo)
}

fn sync_key(repo: &str) -> String {
    repo.to_owned()
}

fn upsert_page(
    database: &Database,
    repo: &str,
    scope: SyncScope,
    mut page: PageInput,
) -> Result<UpsertOutput> {
    for issue in &mut page.issues {
        issue.repo = repo.to_owned();
    }

    let transaction = database.begin_write()?;
    let mut previous_issues = Vec::new();
    {
        let issues = transaction.open_table(ISSUES_TABLE)?;
        for issue in &page.issues {
            let key = issue_key(repo, issue.number);
            if let Some(previous) = issues.get(key.as_str())? {
                let decoded: IssueRecord = serde_json::from_str(previous.value())
                    .with_context(|| format!("decode cached issue #{}", issue.number))?;
                previous_issues.push(decoded);
            }
        }
    }
    {
        let mut terms = transaction.open_table(TERMS_TABLE)?;
        let mut labels = transaction.open_table(LABELS_TABLE)?;
        for issue in &previous_issues {
            remove_issue_indexes(repo, issue, &mut terms, &mut labels)?;
        }
        for issue in &page.issues {
            write_issue_indexes(repo, issue, &mut terms, &mut labels)?;
        }
    }
    {
        let mut issues = transaction.open_table(ISSUES_TABLE)?;
        for issue in &page.issues {
            let key = issue_key(repo, issue.number);
            let encoded = serde_json::to_string(issue)?;
            issues.insert(key.as_str(), encoded.as_str())?;
        }
    }

    let state = {
        let mut sync = transaction.open_table(SYNC_TABLE)?;
        let mut state = state_from_table(repo, &sync)?;
        anyhow::ensure!(
            state.revision < MAX_ISSUE_REVISION,
            "issue revision exhausted its exact wire integer range"
        );
        state.revision += 1;
        apply_page_state(&mut state, scope, &page);
        state.issue_count = count_repo_issues_in_transaction(repo, &transaction)?;
        let encoded = serde_json::to_string(&state)?;
        let key = sync_key(repo);
        sync.insert(key.as_str(), encoded.as_str())?;
        state
    };
    transaction.commit()?;

    Ok(UpsertOutput {
        repo: repo.to_owned(),
        upserted: page.issues.len(),
        state,
    })
}

fn read_state(database: &Database, repo: &str) -> Result<RepoSyncState> {
    let transaction = database.begin_read()?;
    let sync = transaction.open_table(SYNC_TABLE)?;
    state_from_table(repo, &sync)
}

fn read_detail(database: &Database, repo: &str, number: u64) -> Result<Option<DetailRecord>> {
    let transaction = database.begin_read()?;
    let details = transaction.open_table(ISSUE_DETAILS_TABLE)?;
    let key = issue_key(repo, number);
    if let Some(encoded) = details.get(key.as_str())? {
        let mut detail: DetailRecord = serde_json::from_str(encoded.value())
            .with_context(|| format!("decode cached issue detail #{}", number))?;
        detail.repo = repo.to_owned();
        detail.number = number;
        return Ok(Some(detail));
    }
    Ok(None)
}

fn detail_output(repo: &str, number: u64, detail: Option<DetailRecord>) -> DetailOutput {
    match detail {
        Some(detail) => DetailOutput {
            repo: repo.to_owned(),
            number,
            found: true,
            fetched_at: Some(detail.fetched_at),
            item: Some(detail.item),
        },
        None => DetailOutput {
            repo: repo.to_owned(),
            number,
            found: false,
            fetched_at: None,
            item: None,
        },
    }
}

fn read_details(database: &Database, repo: &str, numbers: &[u64]) -> Result<Vec<DetailOutput>> {
    let transaction = database.begin_read()?;
    let details = transaction.open_table(ISSUE_DETAILS_TABLE)?;
    let mut output = Vec::with_capacity(numbers.len());
    for number in numbers {
        let key = issue_key(repo, *number);
        let detail = if let Some(encoded) = details.get(key.as_str())? {
            let mut detail: DetailRecord = serde_json::from_str(encoded.value())
                .with_context(|| format!("decode cached issue detail #{}", number))?;
            detail.repo = repo.to_owned();
            detail.number = *number;
            Some(detail)
        } else {
            None
        };
        output.push(detail_output(repo, *number, detail));
    }
    Ok(output)
}

fn upsert_detail(
    database: &Database,
    repo: &str,
    number: u64,
    detail: &DetailRecord,
) -> Result<()> {
    let transaction = database.begin_write()?;
    {
        let mut details = transaction.open_table(ISSUE_DETAILS_TABLE)?;
        let key = issue_key(repo, number);
        let encoded = serde_json::to_string(detail)?;
        details.insert(key.as_str(), encoded.as_str())?;
    }
    transaction.commit()?;
    Ok(())
}

fn state_from_table(
    repo: &str,
    sync: &impl ReadableTable<&'static str, &'static str>,
) -> Result<RepoSyncState> {
    let key = sync_key(repo);
    if let Some(encoded) = sync.get(key.as_str())? {
        let mut state: RepoSyncState = serde_json::from_str(encoded.value())?;
        anyhow::ensure!(
            state.revision <= MAX_ISSUE_REVISION,
            "issue revision exceeds its exact wire integer range"
        );
        state.repo = repo.to_owned();
        return Ok(state);
    }
    Ok(RepoSyncState {
        repo: repo.to_owned(),
        ..RepoSyncState::default()
    })
}

fn apply_page_state(state: &mut RepoSyncState, scope: SyncScope, page: &PageInput) {
    let high_water = page.high_water.clone().or_else(|| {
        page.issues
            .iter()
            .filter_map(|issue| issue.updated_at.clone())
            .max()
    });
    match scope {
        SyncScope::Open => {
            state.open_cursor = if page.has_next_page {
                page.cursor.clone()
            } else {
                None
            };
            state.open_total_count = page.total_count;
            if let Some(high_water) = high_water {
                state.open_high_water = max_string(state.open_high_water.take(), high_water);
            }
            if let Some(checked_at) = page.checked_at {
                state.last_open_checked_at = Some(checked_at);
            }
            if page.completed || !page.has_next_page {
                state.open_historical_complete = true;
                state.open_cursor = None;
            }
        }
        SyncScope::All => {
            state.all_cursor = if page.has_next_page {
                page.cursor.clone()
            } else {
                None
            };
            state.all_total_count = page.total_count;
            if let Some(high_water) = high_water {
                state.all_high_water = max_string(state.all_high_water.take(), high_water);
            }
            if let Some(checked_at) = page.checked_at {
                state.last_all_checked_at = Some(checked_at);
            }
            if page.completed || !page.has_next_page {
                state.all_historical_complete = true;
                state.all_cursor = None;
            }
        }
    }
}

fn max_string(current: Option<String>, next: String) -> Option<String> {
    match current {
        Some(current) if current >= next => Some(current),
        _ => Some(next),
    }
}

fn count_repo_issues_in_transaction(
    repo: &str,
    transaction: &redb::WriteTransaction,
) -> Result<u64> {
    let issues = transaction.open_table(ISSUES_TABLE)?;
    let prefix = repo_prefix(repo);
    let mut count = 0;
    for item in issues.range(prefix.as_str()..)? {
        let (key, _) = item?;
        if !key.value().starts_with(&prefix) {
            break;
        }
        count += 1;
    }
    Ok(count)
}

fn snapshot(
    database: &Database,
    repo: &str,
    state_filter: SnapshotState,
) -> Result<SnapshotOutput> {
    let transaction = database.begin_read()?;
    let issues = transaction.open_table(ISSUES_TABLE)?;
    let state = state_from_table(repo, &transaction.open_table(SYNC_TABLE)?)?;
    let prefix = repo_prefix(repo);
    let mut records = Vec::new();
    for item in issues.range(prefix.as_str()..)? {
        let (key, value) = item?;
        if !key.value().starts_with(&prefix) {
            break;
        }
        let issue: IssueRecord = serde_json::from_str(value.value())?;
        if matches!(state_filter, SnapshotState::Open) && !issue.state.eq_ignore_ascii_case("open")
        {
            continue;
        }
        records.push(SnapshotIssueRecord::from(issue));
    }
    records.sort_by(|left, right| {
        right
            .updated_at
            .cmp(&left.updated_at)
            .then_with(|| left.number.cmp(&right.number))
    });
    Ok(SnapshotOutput {
        repo: repo.to_owned(),
        revision: state.revision,
        state: match state_filter {
            SnapshotState::Open => "open",
            SnapshotState::All => "all",
        }
        .to_owned(),
        issue_count: records.len(),
        issues: records,
    })
}

fn remove_issue_indexes(
    repo: &str,
    issue: &IssueRecord,
    terms: &mut redb::Table<'_, &str, u64>,
    labels: &mut redb::Table<'_, &str, u64>,
) -> Result<()> {
    for term in issue_terms(issue) {
        let key = index_key(repo, &term, issue.number);
        terms.remove(key.as_str())?;
    }
    for label in issue_labels(issue) {
        let key = index_key(repo, &label, issue.number);
        labels.remove(key.as_str())?;
    }
    Ok(())
}

fn write_issue_indexes(
    repo: &str,
    issue: &IssueRecord,
    terms: &mut redb::Table<'_, &str, u64>,
    labels: &mut redb::Table<'_, &str, u64>,
) -> Result<()> {
    for term in issue_terms(issue) {
        let key = index_key(repo, &term, issue.number);
        terms.insert(key.as_str(), &issue.number)?;
    }
    for label in issue_labels(issue) {
        let key = index_key(repo, &label, issue.number);
        labels.insert(key.as_str(), &issue.number)?;
    }
    Ok(())
}

fn issue_terms(issue: &IssueRecord) -> HashSet<String> {
    let mut terms = HashSet::new();
    terms.insert(issue.number.to_string());
    collect_terms(&issue.title, &mut terms);
    if let Some(body) = &issue.body {
        collect_terms(body, &mut terms);
    }
    for label in &issue.labels {
        collect_terms(&label.name, &mut terms);
    }
    terms
}

fn issue_labels(issue: &IssueRecord) -> HashSet<String> {
    issue
        .labels
        .iter()
        .map(|label| label.name.trim().to_lowercase())
        .filter(|label| !label.is_empty())
        .collect()
}

fn collect_terms(text: &str, terms: &mut HashSet<String>) {
    let mut current = String::new();
    for character in text.chars() {
        if character.is_alphanumeric() || character == '_' || character == '-' {
            current.extend(character.to_lowercase());
        } else if !current.is_empty() {
            terms.insert(std::mem::take(&mut current));
        }
    }
    if !current.is_empty() {
        terms.insert(current);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn temp_db(name: &str) -> PathBuf {
        let mut path = std::env::temp_dir();
        let thread_name = std::thread::current()
            .name()
            .unwrap_or("test")
            .chars()
            .map(|character| {
                if character.is_ascii_alphanumeric() {
                    character
                } else {
                    '-'
                }
            })
            .collect::<String>();
        path.push(format!(
            "github-issue-index-{}-{}-{}.redb",
            name,
            std::process::id(),
            thread_name
        ));
        let _ = fs::remove_file(&path);
        path
    }

    fn issue(number: u64, title: &str, state: &str, labels: &[&str]) -> IssueRecord {
        IssueRecord {
            repo: String::new(),
            number,
            title: title.to_owned(),
            state: state.to_owned(),
            url: format!("https://github.com/mmgeorge/test-repo/issues/{number}"),
            created_at: Some("2026-06-14T00:00:00Z".to_owned()),
            updated_at: Some(format!("2026-06-14T00:{number:02}:00Z")),
            body: None,
            labels: labels
                .iter()
                .map(|label| LabelRecord {
                    name: (*label).to_owned(),
                    color: None,
                    description: None,
                })
                .collect(),
        }
    }

    #[test]
    fn upsert_page_and_snapshot_open_issues() -> Result<()> {
        let db_path = temp_db("snapshot");
        let database = open_database(&db_path)?;
        upsert_page(
            &database,
            "mmgeorge/test-repo",
            SyncScope::Open,
            PageInput {
                issues: vec![
                    issue(1, "Add issue index", "OPEN", &["enhancement"]),
                    issue(2, "Closed task", "CLOSED", &["bug"]),
                ],
                cursor: None,
                has_next_page: false,
                total_count: Some(2),
                completed: true,
                high_water: None,
                checked_at: Some(123),
            },
        )?;

        let snapshot = snapshot(&database, "mmgeorge/test-repo", SnapshotState::Open)?;
        assert_eq!(snapshot.issue_count, 1);
        assert_eq!(snapshot.issues[0].number, 1);
        let snapshot_json = serde_json::to_value(&snapshot)?;
        assert!(
            snapshot_json["issues"][0].get("body").is_none(),
            "completion snapshot must not include issue bodies"
        );
        assert!(read_state(&database, "mmgeorge/test-repo")?.open_historical_complete);
        let _ = fs::remove_file(db_path);
        Ok(())
    }

    #[test]
    fn upsert_replaces_old_indexes_without_losing_issue() -> Result<()> {
        let db_path = temp_db("replace");
        let database = open_database(&db_path)?;
        upsert_page(
            &database,
            "mmgeorge/test-repo",
            SyncScope::Open,
            PageInput {
                issues: vec![issue(7, "Old label", "OPEN", &["bug"])],
                ..PageInput {
                    issues: Vec::new(),
                    cursor: None,
                    has_next_page: false,
                    total_count: None,
                    completed: true,
                    high_water: None,
                    checked_at: None,
                }
            },
        )?;
        upsert_page(
            &database,
            "mmgeorge/test-repo",
            SyncScope::Open,
            PageInput {
                issues: vec![issue(7, "New label", "OPEN", &["docs"])],
                ..PageInput {
                    issues: Vec::new(),
                    cursor: None,
                    has_next_page: false,
                    total_count: None,
                    completed: true,
                    high_water: None,
                    checked_at: None,
                }
            },
        )?;

        let snapshot = snapshot(&database, "mmgeorge/test-repo", SnapshotState::Open)?;
        assert_eq!(snapshot.issue_count, 1);
        assert_eq!(snapshot.issues[0].title, "New label");
        assert_eq!(snapshot.issues[0].labels[0].name, "docs");
        let _ = fs::remove_file(db_path);
        Ok(())
    }

    #[test]
    fn upsert_and_read_issue_detail() -> Result<()> {
        let db_path = temp_db("detail");
        let database = open_database(&db_path)?;
        let detail = DetailRecord {
            repo: "mmgeorge/test-repo".to_owned(),
            number: 12,
            fetched_at: 1234,
            item: serde_json::json!({
                "repo": "mmgeorge/test-repo",
                "number": 12,
                "title": "Cached issue",
                "body": "Cached body"
            }),
        };

        upsert_detail(&database, "mmgeorge/test-repo", 12, &detail)?;

        let cached = read_detail(&database, "mmgeorge/test-repo", 12)?
            .ok_or_else(|| anyhow!("missing cached detail"))?;
        assert_eq!(cached.fetched_at, 1234);
        assert_eq!(cached.item["title"], "Cached issue");
        assert!(read_detail(&database, "mmgeorge/test-repo", 13)?.is_none());
        let _ = fs::remove_file(db_path);
        Ok(())
    }

    #[test]
    fn read_multiple_issue_details_preserves_requested_order() -> Result<()> {
        let db_path = temp_db("details");
        let database = open_database(&db_path)?;
        let detail = DetailRecord {
            repo: "mmgeorge/test-repo".to_owned(),
            number: 12,
            fetched_at: 1234,
            item: serde_json::json!({
                "repo": "mmgeorge/test-repo",
                "number": 12,
                "title": "Cached issue",
                "body": "Cached body"
            }),
        };

        upsert_detail(&database, "mmgeorge/test-repo", 12, &detail)?;

        let details = read_details(&database, "mmgeorge/test-repo", &[13, 12])?;
        assert_eq!(details.len(), 2);
        assert_eq!(details[0].number, 13);
        assert!(!details[0].found);
        assert_eq!(details[1].number, 12);
        assert!(details[1].found);
        assert_eq!(
            details[1]
                .item
                .as_ref()
                .and_then(|item| item["title"].as_str()),
            Some("Cached issue")
        );
        let _ = fs::remove_file(db_path);
        Ok(())
    }

    #[test]
    fn open_database_waits_for_existing_lock() -> Result<()> {
        let db_path = temp_db("lock");
        let database = open_database(&db_path)?;
        let reopen_path = db_path.clone();
        let opener = thread::spawn(move || {
            open_database_with_timeout(&reopen_path, Duration::from_secs(2)).map(|database| {
                drop(database);
            })
        });

        thread::sleep(Duration::from_millis(250));
        drop(database);
        opener
            .join()
            .map_err(|_| anyhow!("database opener thread panicked"))??;
        let _ = fs::remove_file(db_path);
        Ok(())
    }

    #[test]
    fn panic_payload_message_preserves_string_details() {
        let result = catch_unwind(|| panic!("allocator state corrupt"));
        let payload = result.expect_err("test panic should be captured");
        assert_eq!(panic_payload_message(payload), "allocator state corrupt");
    }

    #[test]
    fn archive_corrupt_database_moves_only_database_file() -> Result<()> {
        let db_path = temp_db("archive");
        let snapshot_path = db_path.with_file_name("open-snapshot.json");
        fs::write(&db_path, b"not a valid redb database")?;
        fs::write(&snapshot_path, b"{\"issues\":[]}")?;

        let archived = archive_corrupt_database(&db_path, "invalid allocator state")?;

        assert!(!db_path.exists(), "corrupt database should be moved away");
        assert!(archived.exists(), "archived corrupt database should exist");
        assert!(
            snapshot_path.exists(),
            "snapshot must not be removed during database recovery"
        );
        assert!(
            archived
                .file_name()
                .and_then(|name| name.to_str())
                .unwrap_or("")
                .starts_with("github-issue-index-archive"),
            "archive should keep original database file name"
        );

        let _ = fs::remove_file(archived);
        let _ = fs::remove_file(snapshot_path);
        Ok(())
    }
}

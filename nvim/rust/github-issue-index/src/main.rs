use std::collections::HashSet;
use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::thread;
use std::time::{Duration, Instant};

use anyhow::{anyhow, Context, Result};
use clap::{Parser, Subcommand, ValueEnum};
use redb::{Database, ReadableTable, TableDefinition};
use serde::{Deserialize, Serialize};

const ISSUES_TABLE: TableDefinition<&str, &str> = TableDefinition::new("issues");
const TERMS_TABLE: TableDefinition<&str, u64> = TableDefinition::new("issue_terms");
const LABELS_TABLE: TableDefinition<&str, u64> = TableDefinition::new("issue_labels");
const SYNC_TABLE: TableDefinition<&str, &str> = TableDefinition::new("sync_state");
const DEFAULT_DB_LOCK_TIMEOUT_MS: u64 = 120_000;
const DB_LOCK_RETRY_MS: u64 = 100;

#[derive(Parser)]
#[command(author, version, about)]
struct Cli {
    #[arg(long, default_value_t = DEFAULT_DB_LOCK_TIMEOUT_MS)]
    db_lock_timeout_ms: u64,
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    UpsertPage {
        #[arg(long)]
        db: PathBuf,
        #[arg(long)]
        repo: String,
        #[arg(long, value_enum, default_value_t = SyncScope::Open)]
        scope: SyncScope,
        #[arg(long)]
        input: Option<PathBuf>,
    },
    Snapshot {
        #[arg(long)]
        db: PathBuf,
        #[arg(long)]
        repo: String,
        #[arg(long, value_enum, default_value_t = SnapshotState::Open)]
        state: SnapshotState,
        #[arg(long)]
        output: Option<PathBuf>,
    },
    State {
        #[arg(long)]
        db: PathBuf,
        #[arg(long)]
        repo: String,
    },
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize, ValueEnum)]
#[serde(rename_all = "lowercase")]
enum SyncScope {
    Open,
    All,
}

#[derive(Clone, Copy, Debug, ValueEnum)]
enum SnapshotState {
    Open,
    All,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
struct LabelRecord {
    name: String,
    #[serde(default)]
    color: Option<String>,
    #[serde(default)]
    description: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
struct IssueRecord {
    repo: String,
    number: u64,
    title: String,
    state: String,
    url: String,
    #[serde(default)]
    created_at: Option<String>,
    #[serde(default)]
    updated_at: Option<String>,
    #[serde(default)]
    body: Option<String>,
    #[serde(default)]
    labels: Vec<LabelRecord>,
}

#[derive(Clone, Debug, Serialize)]
struct SnapshotIssueRecord {
    repo: String,
    number: u64,
    title: String,
    state: String,
    url: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    created_at: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    updated_at: Option<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    labels: Vec<LabelRecord>,
}

impl From<IssueRecord> for SnapshotIssueRecord {
    fn from(issue: IssueRecord) -> Self {
        Self {
            repo: issue.repo,
            number: issue.number,
            title: issue.title,
            state: issue.state,
            url: issue.url,
            created_at: issue.created_at,
            updated_at: issue.updated_at,
            labels: issue.labels,
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
struct PageInput {
    #[serde(default)]
    issues: Vec<IssueRecord>,
    #[serde(default)]
    cursor: Option<String>,
    #[serde(default)]
    has_next_page: bool,
    #[serde(default)]
    total_count: Option<u64>,
    #[serde(default)]
    completed: bool,
    #[serde(default)]
    high_water: Option<String>,
    #[serde(default)]
    checked_at: Option<i64>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
struct RepoSyncState {
    repo: String,
    #[serde(default)]
    open_cursor: Option<String>,
    #[serde(default)]
    all_cursor: Option<String>,
    #[serde(default)]
    open_historical_complete: bool,
    #[serde(default)]
    all_historical_complete: bool,
    #[serde(default)]
    open_high_water: Option<String>,
    #[serde(default)]
    all_high_water: Option<String>,
    #[serde(default)]
    open_total_count: Option<u64>,
    #[serde(default)]
    all_total_count: Option<u64>,
    #[serde(default)]
    last_open_checked_at: Option<i64>,
    #[serde(default)]
    last_all_checked_at: Option<i64>,
    #[serde(default)]
    issue_count: u64,
}

#[derive(Debug, Serialize)]
struct UpsertOutput {
    repo: String,
    upserted: usize,
    state: RepoSyncState,
}

#[derive(Debug, Serialize)]
struct SnapshotOutput {
    repo: String,
    state: String,
    issue_count: usize,
    issues: Vec<SnapshotIssueRecord>,
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    let db_lock_timeout = Duration::from_millis(cli.db_lock_timeout_ms);
    match cli.command {
        Command::UpsertPage {
            db,
            repo,
            scope,
            input,
        } => {
            let repo = normalize_repo(&repo)?;
            let page = read_page(input.as_deref())?;
            let database = open_database_with_timeout(&db, db_lock_timeout)?;
            let output = upsert_page(&database, &repo, scope, page)?;
            print_json(&output)?;
        }
        Command::Snapshot {
            db,
            repo,
            state,
            output,
        } => {
            let repo = normalize_repo(&repo)?;
            let database = open_database_with_timeout(&db, db_lock_timeout)?;
            let snapshot = snapshot(&database, &repo, state)?;
            if let Some(path) = output {
                write_json_file(&path, &snapshot)?;
            } else {
                print_json(&snapshot)?;
            }
        }
        Command::State { db, repo } => {
            let repo = normalize_repo(&repo)?;
            let database = open_database_with_timeout(&db, db_lock_timeout)?;
            let state = read_state(&database, &repo)?;
            print_json(&state)?;
        }
    }
    Ok(())
}

fn print_json<T: Serialize>(value: &T) -> Result<()> {
    println!("{}", serde_json::to_string(value)?);
    Ok(())
}

fn write_json_file<T: Serialize>(path: &Path, value: &T) -> Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).with_context(|| format!("create {}", parent.display()))?;
    }
    let encoded = serde_json::to_vec(value)?;
    fs::write(path, encoded).with_context(|| format!("write {}", path.display()))?;
    Ok(())
}

fn read_page(path: Option<&Path>) -> Result<PageInput> {
    let mut text = String::new();
    match path {
        Some(path) => {
            text = fs::read_to_string(path).with_context(|| format!("read {}", path.display()))?;
        }
        None => {
            io::stdin()
                .read_to_string(&mut text)
                .context("read stdin")?;
        }
    }
    serde_json::from_str(&text).context("decode page JSON")
}

#[cfg(test)]
fn open_database(path: &Path) -> Result<Database> {
    open_database_with_timeout(path, Duration::ZERO)
}

fn open_database_with_timeout(path: &Path, timeout: Duration) -> Result<Database> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).with_context(|| format!("create {}", parent.display()))?;
    }
    let started = Instant::now();
    loop {
        match Database::create(path) {
            Ok(database) => {
                initialize_tables(&database)?;
                return Ok(database);
            }
            Err(error)
                if is_database_lock_error(&error)
                    && Instant::now().duration_since(started) < timeout =>
            {
                thread::sleep(Duration::from_millis(DB_LOCK_RETRY_MS));
            }
            Err(error) => {
                let waited_ms = Instant::now().duration_since(started).as_millis();
                return Err(anyhow!(error)).with_context(|| {
                    format!(
                        "open {} after waiting {}ms for database lock",
                        path.display(),
                        waited_ms
                    )
                });
            }
        }
    }
}

fn is_database_lock_error(error: &redb::DatabaseError) -> bool {
    let message = error.to_string();
    message.contains("Database already open") || message.contains("Cannot acquire lock")
}

fn initialize_tables(database: &Database) -> Result<()> {
    let transaction = database.begin_write()?;
    {
        transaction.open_table(ISSUES_TABLE)?;
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

fn state_from_table(
    repo: &str,
    sync: &impl ReadableTable<&'static str, &'static str>,
) -> Result<RepoSyncState> {
    let key = sync_key(repo);
    if let Some(encoded) = sync.get(key.as_str())? {
        let mut state: RepoSyncState = serde_json::from_str(encoded.value())?;
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
}

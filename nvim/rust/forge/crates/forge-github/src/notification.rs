use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashSet;
use std::{future::Future, path::PathBuf, pin::Pin, sync::Arc};

use crate::model::{GithubRepositoryId, normalize_hostname};
use crate::remote::{GithubRemote, RemoteFailure};
use crate::service::GithubService;

pub const NOTIFICATION_PAGE_SIZE: usize = 100;

pub trait GithubNotificationRemote: GithubRemote {
    fn notification_detail(
        &self,
        _request: NotificationDetailRequest,
    ) -> Pin<
        Box<
            dyn Future<Output = std::result::Result<NotificationDetail, RemoteFailure>> + Send + '_,
        >,
    > {
        Box::pin(async {
            Err(RemoteFailure {
                kind: crate::remote::RemoteFailureKind::InvalidResponse,
                message: "notification detail reads are unavailable".into(),
            })
        })
    }
    fn notification_page(
        &self,
        request: NotificationReadRequest,
    ) -> Pin<
        Box<dyn Future<Output = std::result::Result<NotificationPage, RemoteFailure>> + Send + '_>,
    >;
}

impl GithubService {
    pub async fn notification_actor(
        &self,
        directory: PathBuf,
        remote: Arc<dyn GithubNotificationRemote>,
        repository: GithubRepositoryId,
    ) -> Result<crate::remote::RemoteActor> {
        self.run_remote(directory, "notification actor", move |_| async move {
            remote
                .read_actor(repository)
                .await
                .map_err(anyhow::Error::from)
        })
        .await
    }

    pub async fn notification_detail(
        &self,
        directory: PathBuf,
        remote: Arc<dyn GithubNotificationRemote>,
        request: NotificationDetailRequest,
    ) -> Result<NotificationDetail> {
        request.endpoint()?;
        self.run_remote(directory, "notification detail", move |_| async move {
            remote
                .notification_detail(request)
                .await
                .map_err(anyhow::Error::from)
        })
        .await
    }
    /// Reads one global participating page while retaining shared remote job admission.
    pub async fn notification_page(
        &self,
        directory: PathBuf,
        remote: Arc<dyn GithubNotificationRemote>,
        mut request: NotificationReadRequest,
    ) -> Result<NotificationPage> {
        request.validate()?;
        self.run_remote(directory, "notification read", move |_| async move {
            remote
                .notification_page(request)
                .await
                .map_err(anyhow::Error::from)
        })
        .await
    }
}

/// One participating-notification page across every repository on a selected host.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct NotificationReadRequest {
    pub hostname: String,
    pub cursor: Option<u32>,
}

impl NotificationReadRequest {
    /// Validates and normalizes host routing and the bounded REST page cursor.
    pub fn validate(&mut self) -> Result<()> {
        self.hostname = normalize_hostname(&self.hostname).map_err(anyhow::Error::msg)?;
        ensure!(
            self.cursor.is_none_or(|page| (1..=100_001).contains(&page)),
            "invalid notification page cursor"
        );
        Ok(())
    }

    pub fn endpoint(&self) -> String {
        format!(
            "notifications?all=true&participating=true&per_page=100&page={}",
            self.cursor.unwrap_or(1)
        )
    }
}

/// Captured notification identity and subject routes from one validated host response.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct NotificationRecord {
    pub id: String,
    pub repository: GithubRepositoryId,
    pub title: String,
    pub kind: String,
    pub unread: bool,
    pub reason: String,
    pub updated_at: String,
    pub subject_endpoint: Option<String>,
    pub comment_endpoint: Option<String>,
    pub number: Option<u64>,
    pub browser_url: Option<String>,
}

#[derive(Clone, Debug, Serialize)]
pub struct NotificationPage {
    pub record: Vec<NotificationRecord>,
    pub next_cursor: Option<u32>,
    pub complete: bool,
}

/// Decodes at most 100 records without treating malformed records as missing results.
pub fn decode_page(bytes: &[u8], request: &NotificationReadRequest) -> Result<NotificationPage> {
    ensure!(
        bytes.len() <= 8 * 1024 * 1024,
        "notification page exceeds 8 MiB"
    );
    let raw: Vec<Value> = serde_json::from_slice(bytes)?;
    ensure!(
        raw.len() <= NOTIFICATION_PAGE_SIZE,
        "notification page exceeds 100 records"
    );
    let mut identity = HashSet::new();
    let mut record = Vec::with_capacity(raw.len());
    for raw in raw {
        let notification = decode_record(raw, &request.hostname)?;
        ensure!(
            identity.insert(notification.id.clone()),
            "notification page repeats an identity"
        );
        record.push(notification);
    }
    let next_cursor = if record.len() == NOTIFICATION_PAGE_SIZE {
        Some(
            request
                .cursor
                .unwrap_or(1)
                .checked_add(1)
                .context("notification cursor overflow")?,
        )
    } else {
        None
    };
    Ok(NotificationPage {
        complete: next_cursor.is_none(),
        record,
        next_cursor,
    })
}

fn decode_record(raw: Value, hostname: &str) -> Result<NotificationRecord> {
    let raw_id = raw
        .get("id")
        .and_then(Value::as_str)
        .context("notification identity is invalid")?;
    let id = raw
        .get("id")
        .and_then(Value::as_str)
        .and_then(|id| id.parse::<u64>().ok())
        .filter(|id| *id > 0)
        .context("notification identity is invalid")?;
    ensure!(
        id.to_string() == raw_id,
        "notification identity is not canonical decimal"
    );
    let id = raw_id.to_owned();
    let full_name = text(&raw, "/repository/full_name", 201)?;
    let (owner, name) = full_name
        .split_once('/')
        .context("notification repository omits owner")?;
    let repository = GithubRepositoryId::new(hostname, owner, name).map_err(anyhow::Error::msg)?;
    let subject_endpoint = endpoint(&raw, "/subject/url", &repository)?;
    let comment_endpoint = endpoint(&raw, "/subject/latest_comment_url", &repository)?;
    let number = subject_endpoint.as_ref().and_then(|path| {
        let suffix = path.strip_prefix(&format!("repos/{}/", repository.repository_name()))?;
        let (kind, number) = suffix.split_once('/')?;
        if !matches!(kind, "issues" | "pulls") {
            return None;
        }
        number.parse::<u64>().ok().filter(|number| *number > 0)
    });
    let kind = text(&raw, "/subject/type", 128)?.to_owned();
    let browser_url = number
        .map(|number| {
            format!(
                "https://{}/{}/{}/{number}",
                repository.hostname(),
                repository.repository_name(),
                if kind == "PullRequest" {
                    "pull"
                } else {
                    "issues"
                }
            )
        })
        .or_else(|| {
            subject_endpoint
                .as_ref()
                .map(|route| format!("{}{route}", api_prefix(&repository)))
        });
    Ok(NotificationRecord {
        id,
        repository,
        title: text(&raw, "/subject/title", 64 * 1024)?.to_owned(),
        kind,
        unread: raw
            .get("unread")
            .and_then(Value::as_bool)
            .context("notification unread state is missing")?,
        reason: text(&raw, "/reason", 256)?.to_owned(),
        updated_at: text(&raw, "/updated_at", 128)?.to_owned(),
        subject_endpoint,
        comment_endpoint,
        number,
        browser_url,
    })
}

fn text<'value>(value: &'value Value, path: &str, limit: usize) -> Result<&'value str> {
    let text = value
        .pointer(path)
        .and_then(Value::as_str)
        .with_context(|| format!("notification field {path} is missing"))?;
    ensure!(
        text.len() <= limit && !text.contains('\0'),
        "notification field {path} is invalid"
    );
    Ok(text)
}

fn api_prefix(repository: &GithubRepositoryId) -> String {
    if repository.hostname() == "github.com" {
        "https://api.github.com/".to_owned()
    } else {
        format!("https://{}/api/v3/", repository.hostname())
    }
}

fn endpoint(value: &Value, path: &str, repository: &GithubRepositoryId) -> Result<Option<String>> {
    let Some(value) = value.pointer(path).filter(|value| !value.is_null()) else {
        return Ok(None);
    };
    let value = value
        .as_str()
        .context("notification endpoint is not text")?;
    if value.is_empty() {
        return Ok(None);
    }
    let prefix = api_prefix(repository);
    let route = value
        .strip_prefix(&prefix)
        .context("notification endpoint belongs to another host")?;
    let repository_prefix = format!("repos/{}/", repository.repository_name());
    ensure!(
        route.len() <= 2048
            && route
                .get(..repository_prefix.len())
                .is_some_and(|prefix| prefix.eq_ignore_ascii_case(&repository_prefix))
            && route.split('/').all(|part| {
                !part.is_empty()
                    && part != "."
                    && part != ".."
                    && part.bytes().all(|byte| {
                        byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_' | b'.')
                    })
            }),
        "notification endpoint escapes its repository"
    );
    Ok(Some(format!(
        "{}{}",
        repository_prefix,
        &route[repository_prefix.len()..]
    )))
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn record(repository: &str, id: &str) -> Value {
        json!({"id":id,"repository":{"full_name":repository},"unread":true,
            "reason":"participating","updated_at":"2026-09-07T00:00:00Z",
            "subject":{"type":"Issue","title":"Exact title","url":format!("https://api.github.com/repos/{repository}/issues/7"),"latest_comment_url":null}})
    }

    #[test]
    fn participating_scope_preserves_multiple_repositories_and_rejects_foreign_routes() {
        let mut request = NotificationReadRequest {
            hostname: "GitHub.COM.".into(),
            cursor: None,
        };
        request.validate().unwrap();
        assert_eq!(
            request.endpoint(),
            "notifications?all=true&participating=true&per_page=100&page=1"
        );
        let page = decode_page(
            &serde_json::to_vec(&vec![
                record("owner/first", "1"),
                record("other/second", "2"),
            ])
            .unwrap(),
            &request,
        )
        .unwrap();
        assert_eq!(page.record.len(), 2);
        assert_eq!(page.record[1].repository.repository_name(), "other/second");
        assert_eq!(
            page.record[0].browser_url.as_deref(),
            Some("https://github.com/owner/first/issues/7")
        );
        assert!(page.complete);
        let mut foreign = record("owner/first", "1");
        foreign["subject"]["url"] = json!("https://evil.example/repos/owner/first/issues/7");
        assert!(decode_page(&serde_json::to_vec(&vec![foreign]).unwrap(), &request).is_err());
        assert!(
            decode_page(
                &serde_json::to_vec(&vec![
                    record("owner/first", "1"),
                    record("owner/first", "1")
                ])
                .unwrap(),
                &request
            )
            .is_err()
        );
    }

    #[test]
    fn full_page_requires_explicit_next_demand_and_malformed_results_are_errors() {
        let request = NotificationReadRequest {
            hostname: "github.com".into(),
            cursor: Some(3),
        };
        let records = (1..=100)
            .map(|id| record("owner/repo", &id.to_string()))
            .collect::<Vec<_>>();
        let page = decode_page(&serde_json::to_vec(&records).unwrap(), &request).unwrap();
        assert_eq!(page.next_cursor, Some(4));
        assert!(!page.complete);
        assert!(decode_page(b"{}", &request).is_err());
        assert!(
            NotificationReadRequest {
                hostname: "--hostname".into(),
                cursor: None
            }
            .validate()
            .is_err()
        );
    }
}

#[derive(Clone, Debug)]
pub struct NotificationDetailRequest {
    pub record: NotificationRecord,
    pub latest_comment: bool,
}
impl NotificationDetailRequest {
    pub fn endpoint(&self) -> Result<String> {
        let selected = if self.latest_comment {
            self.record
                .comment_endpoint
                .as_ref()
                .or(self.record.subject_endpoint.as_ref())
        } else {
            self.record.subject_endpoint.as_ref()
        };
        let selected = selected.context("notification has no API source")?;
        let prefix = api_prefix(&self.record.repository);
        endpoint(
            &serde_json::json!({"endpoint": format!("{prefix}{selected}")}),
            "/endpoint",
            &self.record.repository,
        )?
        .context("notification has no API source")
    }
    pub fn is_comment(&self) -> bool {
        self.latest_comment && self.record.comment_endpoint.is_some()
    }
}
#[derive(Clone, Debug, Serialize)]
pub struct NotificationDetail {
    pub body: String,
    pub author: Option<String>,
    pub comments: Option<u64>,
    pub comment: bool,
}
pub fn decode_detail(
    bytes: &[u8],
    request: &NotificationDetailRequest,
) -> Result<NotificationDetail> {
    ensure!(
        bytes.len() <= 2 * 1024 * 1024,
        "notification detail exceeds 2 MiB"
    );
    request.endpoint()?;
    let raw: Value = serde_json::from_slice(bytes)?;
    ensure!(raw.is_object(), "notification detail is not an object");
    let body = match raw.get("body") {
        None | Some(Value::Null) => String::new(),
        Some(Value::String(body)) => body.clone(),
        _ => anyhow::bail!("notification body is not text"),
    };
    ensure!(
        body.len() <= 1024 * 1024 && !body.contains('\0'),
        "notification body exceeds bounds"
    );
    let author = raw
        .pointer("/user/login")
        .and_then(Value::as_str)
        .map(str::to_owned);
    ensure!(
        author
            .as_ref()
            .is_none_or(|author| author.len() <= 256 && !author.contains('\0')),
        "notification author exceeds bounds"
    );
    let comments = match raw.get("comments") {
        None | Some(Value::Null) => None,
        Some(value) => Some(
            value
                .as_u64()
                .context("notification comment count is invalid")?,
        ),
    };
    Ok(NotificationDetail {
        body,
        author,
        comments,
        comment: request.is_comment(),
    })
}

#[cfg(test)]
mod detail_tests {
    use super::*;
    #[test]
    fn exact_large_thread_identity_and_captured_detail_routes() {
        let request = NotificationReadRequest {
            hostname: "github.com".into(),
            cursor: None,
        };
        let bytes = br#"[{"id":"9007199254740993","repository":{"full_name":"owner/repo"},"unread":true,"reason":"mention","updated_at":"today","subject":{"type":"Issue","title":"title","url":"https://api.github.com/repos/owner/repo/issues/7","latest_comment_url":"https://api.github.com/repos/owner/repo/issues/comments/9"}}]"#;
        let record = decode_page(bytes, &request).unwrap().record.remove(0);
        assert_eq!(
            serde_json::to_value(&record).unwrap()["id"],
            "9007199254740993"
        );
        let mut detail = NotificationDetailRequest {
            record,
            latest_comment: true,
        };
        assert_eq!(
            detail.endpoint().unwrap(),
            "repos/owner/repo/issues/comments/9"
        );
        let body = decode_detail(
            br#"{"body":"exact\r\ntext","user":{"login":"person"},"comments":4}"#,
            &detail,
        )
        .unwrap();
        assert!(body.comment);
        assert_eq!(body.body, "exact\r\ntext");
        assert_eq!(body.comments, Some(4));
        let other = String::from_utf8_lossy(bytes)
            .replace("\"Issue\"", "\"Release\"")
            .replace("issues/7", "releases/7");
        let other = decode_page(other.as_bytes(), &request)
            .unwrap()
            .record
            .remove(0);
        assert_eq!(other.number, None);
        assert_eq!(
            other.browser_url.as_deref(),
            Some("https://api.github.com/repos/owner/repo/releases/7")
        );
        detail.record.comment_endpoint = Some("repos/other/repo/issues/comments/9".into());
        assert!(detail.endpoint().is_err());
        assert!(
            decode_page(
                &String::from_utf8_lossy(bytes)
                    .replace("9007199254740993", "01")
                    .into_bytes(),
                &request
            )
            .is_err()
        );
    }
}

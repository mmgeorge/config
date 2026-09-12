use std::fs::{self, File};
use std::io::Write;
use std::time::Duration;

fn main() {
    let argument: Vec<_> = std::env::args_os().collect();
    if argument
        .get(1)
        .is_some_and(|argument| argument == "--hold-pipe")
    {
        fs::write("descendant.ready", "ready").unwrap();
        let started = std::time::Instant::now();
        while !std::path::Path::new("release-descendant").exists()
            && started.elapsed() < Duration::from_secs(2)
        {
            std::thread::sleep(Duration::from_millis(5));
        }
        return;
    }
    let id = std::process::id();
    if std::path::Path::new("issue-edit-current.json").exists()
        && argument.get(1).is_some_and(|value| value == "api")
        && argument.get(4).is_some_and(|value| value != "user")
    {
        assert_eq!(argument[1], "api");
        assert_eq!(argument[2], "--hostname");
        assert_eq!(argument[3], "enterprise.example");
        assert_eq!(argument[4], "--method");
        let method = argument[5].to_str().unwrap();
        if method == "GET" {
            assert_eq!(argument.len(), 7);
            assert_eq!(argument[6], "repos/owner/repo/issues/7");
            std::io::stdout()
                .write_all(&fs::read("issue-edit-current.json").unwrap())
                .unwrap();
            return;
        }
        assert_eq!(argument.len(), 9);
        assert_eq!(argument[7], "--input");
        let stage = match method {
            "PATCH" => {
                assert_eq!(argument[6], "repos/owner/repo/issues/7");
                "fields"
            }
            "DELETE" => {
                assert_eq!(argument[6], "repos/owner/repo/issues/7/assignees");
                "remove"
            }
            "POST" => {
                assert_eq!(argument[6], "repos/owner/repo/issues/7/assignees");
                "add"
            }
            _ => panic!("unexpected issue edit method {method}"),
        };
        assert_eq!(
            fs::read(&argument[8]).unwrap(),
            fs::read(format!("issue-edit-{stage}-expected.json")).unwrap()
        );
        if stage == "add" && std::path::Path::new("issue-edit-reject-add").exists() {
            eprintln!("HTTP 403 Forbidden");
            std::process::exit(1);
        }
        fs::copy(
            format!("issue-edit-{stage}-result.json"),
            "issue-edit-current.json",
        )
        .unwrap();
        fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open("issue-edit-log")
            .unwrap()
            .write_all(format!("{stage}\n").as_bytes())
            .unwrap();
        if fs::read_to_string("mode").unwrap() == "issue_edit_uncertain" {
            std::io::stdout().write_all(b"{interrupted").unwrap();
            return;
        }
        std::io::stdout()
            .write_all(&fs::read("issue-edit-current.json").unwrap())
            .unwrap();
        return;
    }
    if std::path::Path::new("notification-page.json").exists() {
        assert_eq!(argument.len(), 5);
        assert_eq!(argument[1], "api");
        assert_eq!(argument[2], "--hostname");
        assert_eq!(argument[3], "enterprise.example");
        assert_eq!(
            argument[4],
            "notifications?all=true&participating=true&per_page=100&page=1"
        );
        std::io::stdout()
            .write_all(&fs::read("notification-page.json").unwrap())
            .unwrap();
        return;
    }
    if std::path::Path::new("submission-parent.json").exists()
        && argument.get(4).is_some_and(|value| value != "user")
    {
        assert_eq!(argument[1], "api");
        assert_eq!(argument[2], "--hostname");
        assert_eq!(argument[3], "enterprise.example");
        if argument[4] == "--method" && argument[5] == "POST" {
            assert_eq!(argument.len(), 9);
            assert_eq!(argument[6], "repos/owner/repo/pulls/7/reviews/9/events");
            assert_eq!(argument[7], "--input");
            fs::copy(&argument[8], "submission-write.json").unwrap();
            fs::copy("submission-after.json", "submission-comments.json").unwrap();
            fs::copy("submission-result.json", "submission-review.json").unwrap();
            std::io::stdout()
                .write_all(&fs::read("submission-result.json").unwrap())
                .unwrap();
            return;
        }
        let endpoint = if argument[4] == "--method" {
            assert_eq!(argument.len(), 7);
            assert_eq!(argument[5], "GET");
            argument[6].to_str().unwrap()
        } else {
            assert_eq!(argument.len(), 5);
            argument[4].to_str().unwrap()
        };
        let file = match endpoint {
            "repos/owner/repo/pulls/7" => "submission-parent.json",
            "repos/owner/repo/pulls/7/reviews/9" => "submission-review.json",
            "repos/owner/repo/pulls/7/reviews/9/comments?per_page=100&page=1" => {
                "submission-comments.json"
            }
            _ => panic!("unexpected submission read {endpoint}"),
        };
        std::io::stdout()
            .write_all(&fs::read(file).unwrap())
            .unwrap();
        return;
    }
    if std::path::Path::new("source-identity.json").exists() {
        assert_eq!(argument[1], "api");
        assert_eq!(argument[2], "--hostname");
        assert_eq!(argument[3], "enterprise.example");
        assert_eq!(argument.len(), 7);
        if argument[4] == "graphql" {
            assert_eq!(argument[5], "--input");
            fs::copy(&argument[6], "source-request.json").unwrap();
            std::io::stdout()
                .write_all(&fs::read("source-identity.json").unwrap())
                .unwrap();
        } else {
            assert_eq!(argument[4], "--header");
            assert_eq!(argument[5], "Accept: application/vnd.github.raw+json");
            assert_eq!(
                argument[6].to_str().unwrap(),
                format!("repos/owner/repo/git/blobs/{}", "b".repeat(40))
            );
            fs::write("source-blob-read", "read").unwrap();
            std::io::stdout()
                .write_all(&fs::read("source-blob").unwrap())
                .unwrap();
        }
        return;
    }
    if argument.get(1).is_some_and(|argument| argument == "repo") {
        assert_eq!(argument.len(), 5);
        assert_eq!(argument[2], "view");
        assert_eq!(argument[3], "--json");
        assert_eq!(argument[4], "id,nameWithOwner,url");
        let host = fs::read_to_string("host").unwrap();
        let repository = fs::read_to_string("repository").unwrap_or_else(|_| "owner/repo".into());
        write!(std::io::stdout(), "{{\"id\":\"REPOSITORY_fixture\",\"nameWithOwner\":\"{repository}\",\"url\":\"https://{host}/{repository}\"}}").unwrap();
        return;
    }
    if argument.get(4).is_some_and(|argument| argument == "user") {
        assert_eq!(argument.len(), 5);
        std::io::stdout()
            .write_all(br#"{"login":"fixture","node_id":"ACTOR_fixture"}"#)
            .unwrap();
        return;
    }
    if argument
        .get(4)
        .is_some_and(|argument| argument == "--method")
    {
        assert_eq!(argument[1], "api");
        assert_eq!(argument[2], "--hostname");
        assert_eq!(
            argument[3].to_str().unwrap(),
            fs::read_to_string("host").unwrap()
        );
        let method = argument[5].to_str().unwrap();
        let endpoint = argument[6].to_str().unwrap();
        let repository = fs::read_to_string("repository").unwrap_or_else(|_| "owner/repo".into());
        assert!(
            endpoint == format!("repos/{repository}")
                || endpoint.starts_with(&format!("repos/{repository}/"))
        );
        if endpoint == format!("repos/{repository}") && method == "GET" {
            std::io::stdout()
                .write_all(br#"{"node_id":"REPOSITORY_fixture"}"#)
                .unwrap();
            return;
        }
        if endpoint.contains("/git/ref/heads/") && method == "GET" {
            let branch = endpoint.rsplit('/').next().unwrap();
            assert!(branch == "main" || branch == "feature-create");
            let head = fs::read_to_string("creation-head").unwrap();
            write!(std::io::stdout(), "{{\"ref\":\"refs/heads/{branch}\",\"object\":{{\"type\":\"commit\",\"sha\":\"{head}\"}}}}").unwrap();
            return;
        }
        if endpoint == format!("repos/{repository}/pulls") && method == "POST" {
            assert_eq!(argument.len(), 9);
            assert_eq!(argument[7], "--input");
            assert_eq!(
                fs::read(&argument[8]).unwrap(),
                fs::read("expected-pr-creation.json").unwrap()
            );
            fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open("pr-creation-log")
                .unwrap()
                .write_all(b"write\n")
                .unwrap();
            std::io::stdout()
                .write_all(&fs::read("pr-creation-result.json").unwrap())
                .unwrap();
            return;
        }
        if endpoint.ends_with("/pulls/7") && method == "GET" {
            std::io::stdout()
                .write_all(br#"{"number":7,"node_id":"PR_fixture"}"#)
                .unwrap();
            return;
        }
        assert!(
            endpoint.ends_with("/pulls/comments/4000000000")
                || endpoint.ends_with("/issues/7/comments")
        );
        if method == "GET" {
            assert_eq!(argument.len(), 7);
            std::io::stdout()
                .write_all(&fs::read("rest-read.json").unwrap())
                .unwrap();
            return;
        }
        assert!(method == "PATCH" || method == "POST");
        assert_eq!(argument.len(), 9);
        assert_eq!(argument[7], "--input");
        fs::copy(&argument[8], format!("request.{id}.json")).unwrap();
        let creation = method == "POST";
        fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(if creation {
                "creation-log"
            } else {
                "comment-mutation-log"
            })
            .unwrap()
            .write_all(b"write\n")
            .unwrap();
        if !creation {
            fs::copy("rest-after.json", "rest-read.json").unwrap();
            fs::copy("comment-after.json", "comment-read.json").unwrap();
        }
        if fs::read_to_string("mode").unwrap().ends_with("_uncertain") {
            std::io::stdout().write_all(b"{interrupted").unwrap();
        } else {
            std::io::stdout()
                .write_all(
                    &fs::read(if creation {
                        "rest-result.json"
                    } else {
                        "rest-after.json"
                    })
                    .unwrap(),
                )
                .unwrap();
        }
        return;
    }
    if argument.get(1).is_some_and(|argument| argument == "issue") {
        assert_eq!(argument.len(), 8);
        assert_eq!(argument[2], "view");
        assert!(argument[3].to_str().unwrap().parse::<u64>().unwrap() > 0);
        assert_eq!(argument[4], "--repo");
        let repository = fs::read_to_string("repository").unwrap_or_else(|_| "owner/repo".into());
        assert_eq!(
            argument[5].to_str().unwrap(),
            format!("{}/{repository}", fs::read_to_string("host").unwrap())
        );
        assert_eq!(argument[6], "--json");
        fs::write(format!("fields.{id}"), argument[7].to_str().unwrap()).unwrap();
    } else if argument
        .get(4)
        .is_some_and(|argument| argument.to_str().unwrap().starts_with("/repos/"))
    {
        assert_eq!(argument.len(), 7);
        assert_eq!(argument[1], "api");
        assert_eq!(argument[2], "--hostname");
        assert_eq!(
            argument[3].to_str().unwrap(),
            fs::read_to_string("host").unwrap()
        );
        assert_eq!(argument[5], "--paginate");
        assert_eq!(argument[6], "--slurp");
        let repository = fs::read_to_string("repository").unwrap_or_else(|_| "owner/repo".into());
        let endpoint = argument[4].to_str().unwrap();
        assert!(
            endpoint.starts_with(&format!("/repos/{repository}/"))
                && endpoint.ends_with("?per_page=100")
        );
        let source = if endpoint.contains("/collaborators?") {
            "collaborators"
        } else {
            "contributors"
        };
        fs::write(format!("endpoint.{id}"), endpoint).unwrap();
        if let Ok(response) = fs::read(format!("{source}.json")) {
            std::io::stdout().write_all(&response).unwrap();
            if let Ok(failure) = fs::read(format!("{source}.failure")) {
                std::io::stderr().write_all(&failure).unwrap();
                std::process::exit(1);
            }
            return;
        }
    } else {
        assert_eq!(argument.len(), 7);
        assert_eq!(argument[1], "api");
        assert_eq!(argument[2], "--hostname");
        assert_eq!(
            argument[3].to_str().unwrap(),
            fs::read_to_string("host").unwrap()
        );
        assert_eq!(argument[4], "graphql");
        assert_eq!(argument[5], "--input");
        fs::copy(&argument[6], format!("request.{id}.json")).unwrap();
        fs::write(format!("input.{id}"), argument[6].to_str().unwrap()).unwrap();
    }
    let alive = File::create(format!("alive.{id}")).unwrap();
    alive.lock().unwrap();
    fs::write(format!("started.{id}"), "ready").unwrap();
    if fs::read_to_string("mode").unwrap().starts_with("creation_") {
        let request = fs::read_to_string(format!("request.{id}.json")).unwrap();
        if request.contains("addComment") {
            for field in ["subjectId", "body", "clientMutationId"] {
                if field == "clientMutationId"
                    && std::path::Path::new("creation-echo-receipt").exists()
                {
                    continue;
                }
                let expected = fs::read_to_string(format!("expected-{field}")).unwrap();
                assert!(request.contains(&format!("\"{field}\":{expected}")));
            }
            fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open("creation-log")
                .unwrap()
                .write_all(b"write\n")
                .unwrap();
            fs::write("created", "yes").unwrap();
            if fs::read_to_string("mode").unwrap() == "creation_uncertain" {
                std::io::stdout().write_all(b"{interrupted").unwrap();
                return;
            }
            let mut response = fs::read_to_string("creation-result.json").unwrap();
            if std::path::Path::new("creation-echo-receipt").exists() {
                let marker = "\"clientMutationId\":\"";
                let receipt = &request[request.find(marker).unwrap() + marker.len()..];
                let receipt = &receipt[..receipt.find('"').unwrap()];
                assert!(
                    receipt
                        .bytes()
                        .all(|byte| byte.is_ascii_alphanumeric() || byte == b'-')
                );
                response = response.replace("__receipt__", receipt);
            }
            std::io::stdout().write_all(response.as_bytes()).unwrap();
        } else {
            std::io::stdout()
                .write_all(&fs::read("creation-parent.json").unwrap())
                .unwrap();
        }
        return;
    }
    if fs::read_to_string("mode").unwrap().starts_with("comment_") {
        let request = fs::read_to_string(format!("request.{id}.json")).unwrap();
        let mutation = request.contains("mutation($input:");
        if mutation {
            let expected = fs::read_to_string("expected-mutation").unwrap();
            assert!(request.contains(&expected));
            let identity = if expected == "updatePullRequestReviewComment" {
                "pullRequestReviewCommentId"
            } else {
                "id"
            };
            assert!(request.contains(&format!("\"{identity}\":\"COMMENT_fixture\"")));
            fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open("comment-mutation-log")
                .unwrap()
                .write_all(b"write\n")
                .unwrap();
            fs::copy("comment-after.json", "comment-read.json").unwrap();
            if fs::read_to_string("mode").unwrap() == "comment_uncertain" {
                std::io::stdout().write_all(b"{interrupted").unwrap();
                return;
            }
        } else {
            assert!(request.contains("databaseId: fullDatabaseId"));
        }
        let mut response = fs::read_to_string(if mutation {
            "comment-result.json"
        } else {
            "comment-read.json"
        })
        .unwrap();
        if mutation && std::path::Path::new("comment-echo-receipt").exists() {
            let marker = "\"clientMutationId\":\"";
            let receipt = &request[request.find(marker).unwrap() + marker.len()..];
            let receipt = &receipt[..receipt.find('"').unwrap()];
            assert!(
                receipt
                    .bytes()
                    .all(|byte| byte.is_ascii_alphanumeric() || byte == b'-')
            );
            response = response.replace("__receipt__", receipt);
        }
        std::io::stdout().write_all(response.as_bytes()).unwrap();
        return;
    }
    if fs::read_to_string("mode").unwrap().starts_with("pr_") {
        let request = fs::read_to_string(format!("request.{id}.json")).unwrap();
        let mut state = fs::read_to_string("pr-state").unwrap_or_else(|_| "CLOSED false".into());
        let mutation = [
            "reopenPullRequest",
            "closePullRequest",
            "convertPullRequestToDraft",
            "markPullRequestReadyForReview",
            "updatePullRequest",
        ]
        .into_iter()
        .find(|mutation| request.contains(mutation));
        if let Some(mutation) = mutation {
            fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open("mutation-log")
                .unwrap()
                .write_all(format!("{mutation}\n").as_bytes())
                .unwrap();
            fs::write("mutation-started", mutation).unwrap();
            let started = std::time::Instant::now();
            while fs::read_to_string("mode").unwrap() == "pr_block"
                && !std::path::Path::new("release-mutation").exists()
                && started.elapsed() < Duration::from_secs(10)
            {
                std::thread::sleep(Duration::from_millis(5));
            }
            let draft = state.ends_with("true");
            state = match mutation {
                "reopenPullRequest" => format!("OPEN {draft}"),
                "closePullRequest" => format!("CLOSED {draft}"),
                "convertPullRequestToDraft" => "OPEN true".into(),
                "updatePullRequest" => {
                    for field in ["title", "body"] {
                        if let Ok(expected) = fs::read_to_string(format!("submitted-{field}.json"))
                        {
                            assert!(request.contains(&format!("\"{field}\":{expected}")));
                            fs::write(format!("pr-{field}.json"), expected).unwrap();
                        } else {
                            assert!(!request.contains(&format!("\"{field}\":")));
                        }
                    }
                    state.clone()
                }
                _ => "OPEN false".into(),
            };
            fs::write("pr-state", &state).unwrap();
            if fs::read_to_string("mode").unwrap() == "pr_uncertain" {
                std::io::stdout().write_all(b"{interrupted").unwrap();
                return;
            }
        }
        let (lifecycle, draft) = state.split_once(' ').unwrap();
        let container = if mutation.is_some() {
            "transition"
        } else {
            "repository"
        };
        let text =
            if request.contains("\"includeText\":true") || mutation == Some("updatePullRequest") {
                let title = fs::read_to_string("pr-title.json")
                    .unwrap_or_else(|_| "\"Initial title\"".into());
                let body = fs::read_to_string("pr-body.json")
                    .unwrap_or_else(|_| "\"Initial body\"".into());
                format!(",\"title\":{title},\"body\":{body}")
            } else {
                String::new()
            };
        write!(std::io::stdout(), "{{\"data\":{{\"{container}\":{{\"pullRequest\":{{\"id\":\"PR_fixture\",\"number\":7,\"state\":\"{lifecycle}\",\"isDraft\":{draft}{text}}}}}}}}}").unwrap();
        return;
    }
    match fs::read_to_string("mode").unwrap().as_str() {
        "block" => loop {
            std::thread::sleep(Duration::from_millis(50));
        },
        "oversized" => {
            let mut output = std::io::stdout().lock();
            let chunk = [b'x'; 8192];
            for _ in 0..1025 {
                if output.write_all(&chunk).is_err() {
                    break;
                }
            }
        }
        "descendant" => {
            let mut child = std::process::Command::new(std::env::current_exe().unwrap())
                .arg("--hold-pipe")
                .spawn()
                .unwrap();
            let _ = child.try_wait();
        }
        "failure" => {
            std::io::stdout()
                .write_all(&fs::read("response.json").unwrap())
                .unwrap();
            std::io::stderr()
                .write_all(b"HTTP 403: API rate limit exceeded")
                .unwrap();
            std::process::exit(1);
        }
        _ => std::io::stdout()
            .write_all(&fs::read("response.json").unwrap())
            .unwrap(),
    }
}

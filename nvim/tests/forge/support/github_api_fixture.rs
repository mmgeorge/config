use std::io::Write;

fn main() -> std::io::Result<()> {
    let arguments = std::env::args().collect::<Vec<_>>();
    if std::fs::read_to_string("mode")? != "review_api" {
        let original = std::env::current_exe()?.with_file_name(if cfg!(windows) {
            "gh-base.exe"
        } else {
            "gh-base"
        });
        let status = std::process::Command::new(original)
            .args(&arguments[1..])
            .status()?;
        std::process::exit(status.code().unwrap_or(1));
    }
    assert_eq!(
        &arguments[1..4],
        ["api", "--hostname", "enterprise.example"]
    );
    if arguments[4] == "--method" && arguments[5] == "POST" {
        assert!(arguments[6].ends_with("/pulls/7/reviews"));
        std::fs::write(
            "submission-write.json",
            std::fs::read(arguments.last().unwrap())?,
        )?;
        return std::io::stdout().write_all(b"{interrupted");
    }
    let endpoint = arguments.last().unwrap();
    let source = if arguments[4] == "graphql" {
        let request = std::fs::read_to_string(endpoint)?;
        if request.contains("\"expression\"") {
            if request.contains(&"b".repeat(40)) {
                "source-head.json"
            } else {
                "source-base.json"
            }
        } else if request.contains("node(id:$thread)") {
            "thread.json"
        } else if request.contains("reviewThreads") {
            "threads.json"
        } else if request.contains("statusCheckRollup") {
            "checks.json"
        } else {
            "editable.json"
        }
    } else if endpoint == "user" {
        "actor.json"
    } else if endpoint.ends_with("/pulls/7") {
        "overview.json"
    } else if endpoint.contains("/compare/") {
        "comparison.json"
    } else if endpoint.contains("/git/blobs/") {
        if endpoint.ends_with(&"d".repeat(40)) {
            "base.rs"
        } else {
            "head.rs"
        }
    } else if endpoint.contains("/files?") {
        "files.json"
    } else if endpoint.contains("/commits?") {
        "commits.json"
    } else if endpoint.contains("/comments?") {
        "comments.json"
    } else {
        panic!("unexpected fixture request: {arguments:?}");
    };
    std::io::stdout().write_all(&std::fs::read(source)?)
}

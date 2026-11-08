use super::*;
use forge_diff::cache::CacheLimits;
use forge_diff::engine::{DiffEngine, DiffRequest};
use forge_diff::source::{Representation, SourcePair};

struct Fixture {
    diff: Arc<DiffEngine>,
    syntax: Arc<SyntaxEngine>,
}

impl Fixture {
    fn new() -> Self {
        let diff = DiffEngine::new(CacheLimits::default(), 2);
        let syntax = SyntaxEngine::new(diff.analysis_pool());
        Self { diff, syntax }
    }

    async fn file(&self, path: &str, old: &str, new: &str) -> ChangedFile {
        let source = |text: &str| {
            SourceVersion::new(text.as_bytes().to_vec(), Representation::GitCanonical).unwrap()
        };
        let analysis = self
            .diff
            .compare(DiffRequest {
                source: SourcePair {
                    old: source(old),
                    new: source(new),
                },
                priority: WorkPriority::Speculative,
            })
            .await
            .unwrap();
        ChangedFile {
            path: path.into(),
            note: String::new(),
            analysis: Some(analysis),
        }
    }
}

#[tokio::test]
async fn global_limit_stops_without_shrinking_the_twenty_line_allowance_or_splitting_utf8() {
    let fixture = Fixture::new();
    let source = format!(
        "fn run() {{\n{}\n}}\n",
        (0..30)
            .map(|position| format!("    step_{position}(\"λ🦀\");\n"))
            .collect::<String>()
    );
    let mut files = vec![fixture.file("source.rs", "fn run() {\n}\n", &source).await];
    let full = prompt(&mut files, &fixture.syntax, 20_000).await;
    for limit in [500, 600, 700, 800, 900, 1000] {
        let output = prompt(&mut files, &fixture.syntax, limit).await;
        assert!(output.len() <= limit);
        assert!(full.starts_with(&output), "limit {limit}: {output}");
        assert!(output.is_empty() || output.ends_with('\n'));
    }
    assert_eq!(
        full.lines()
            .filter(|line| line.starts_with("+     step_"))
            .count(),
        20,
        "{full}"
    );
}

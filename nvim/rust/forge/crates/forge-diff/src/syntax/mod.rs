//! Exact-source syntax trees share native parsing between highlights and structural context.

mod assets;
mod context;
mod query;
mod service;
mod tree;

pub use assets::SyntaxLanguage;
pub use context::HunkContext;
pub use service::{SyntaxEngine, SyntaxLimits, SyntaxRequest, SyntaxUsage};
pub use tree::{
    SyntaxCapture, SyntaxFamily, SyntaxHandle, SyntaxInjection, SyntaxPoint, SyntaxRange,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SyntaxError {
    Query(String),
    Closed,
    Busy,
    ConsumerLimit,
    MemoryLimit,
    CaptureLimit,
    InjectionLimit,
    Cancelled,
    Deadline,
    WorkerFailed,
    Language(String),
    Pool(crate::workers::PoolError),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::{Representation, SourceVersion};
    use crate::workers::{AnalysisPool, PoolLimits, WorkPriority};
    use std::sync::Arc;

    fn engine() -> Arc<SyntaxEngine> {
        SyntaxEngine::new(
            Arc::new(AnalysisPool::new(PoolLimits {
                workers: 1,
                jobs: 2,
                input_bytes: 8 * 1024 * 1024,
            })),
            SyntaxLimits::default(),
        )
    }

    fn request(language: SyntaxLanguage, text: &str) -> SyntaxRequest {
        SyntaxRequest {
            language,
            source: SourceVersion::new(text.as_bytes().to_vec(), Representation::Raw).unwrap(),
            priority: WorkPriority::Foreground,
            deadline: None,
        }
    }

    #[tokio::test]
    async fn markdown_paragraphs_keep_independent_inline_trees_with_explicit_admission() {
        let text = (0..128)
            .map(|index| format!("Paragraph **{index}**.\n\n"))
            .collect::<String>();
        let syntax = engine()
            .analyze(request(SyntaxLanguage::Markdown, &text))
            .await
            .unwrap();
        assert_eq!(syntax.tree_count(), 129);
        let mut limits = SyntaxLimits::default();
        limits.injection_trees = 32;
        let limited = SyntaxEngine::new(
            Arc::new(AnalysisPool::new(PoolLimits {
                workers: 1,
                jobs: 1,
                input_bytes: 8 * 1024 * 1024,
            })),
            limits,
        );
        assert!(matches!(
            limited
                .analyze(request(SyntaxLanguage::Markdown, &text))
                .await,
            Err(SyntaxError::InjectionLimit)
        ));
    }

    #[tokio::test]
    async fn exact_tree_shares_highlights_and_context_without_reconstructing_old_source() {
        let engine = engine();
        let old = engine
            .analyze(request(
                SyntaxLanguage::Rust,
                "fn previous() { let value = 1; }\n",
            ))
            .await
            .unwrap();
        let new = engine
            .analyze(request(
                SyntaxLanguage::Rust,
                "fn current() { let value = 2; }\n",
            ))
            .await
            .unwrap();
        assert_ne!(old.source_identity(), new.source_identity());
        assert_eq!(old.tree_count(), 1);
        for handle in [&old, &new] {
            assert!(
                handle
                    .captures()
                    .iter()
                    .any(|capture| capture.family == SyntaxFamily::Context
                        && capture.name.as_ref() == "scope.name")
            );
            assert!(
                handle
                    .captures()
                    .iter()
                    .any(|capture| capture.family == SyntaxFamily::Highlight
                        && capture.name.as_ref() == "declaration")
            );
        }
        let cached = engine
            .analyze(request(
                SyntaxLanguage::Rust,
                "fn previous() { let value = 1; }\n",
            ))
            .await
            .unwrap();
        assert!(Arc::ptr_eq(&old.0, &cached.0));
        assert_eq!(engine.usage().active_jobs, 0);
    }

    #[tokio::test]
    async fn projected_overlapping_highlights_preserve_query_precedence() {
        for source in [
            "fn main() {\n    println!(\"before\");\n}\n",
            "fn main() {\n    println!(\"after\");\n    println!(\"extra line\");\n}\n",
        ] {
            let handle = engine()
                .analyze(request(SyntaxLanguage::Rust, source))
                .await
                .unwrap();
            for (row, text) in source.lines().enumerate() {
                let mut metadata = forge_buffer::block::BlockMetadata::default();
                crate::projection::append_syntax_row(&mut metadata, &handle, row, 0, text).unwrap();
                let Some(column) = text.find("main").or_else(|| text.find("println")) else {
                    continue;
                };
                let overlapping: Vec<_> = metadata
                    .visible_decoration
                    .iter()
                    .filter(|span| {
                        span.range.start.column <= column && span.range.end.column > column
                    })
                    .collect();
                assert_eq!(overlapping.first().unwrap().capture, "@variable.rust");
                assert_eq!(
                    overlapping.last().unwrap().capture,
                    if row == 0 {
                        "@declaration.rust"
                    } else {
                        "@function.macro.rust"
                    },
                    "overlapping captures changed precedence for {text}"
                );
            }
        }
    }

    #[tokio::test]
    async fn markdown_injection_preserves_absolute_source_coordinates() {
        let engine = engine();
        let handle = engine
            .analyze(request(
                SyntaxLanguage::Markdown,
                "# Header\n\n```rust\nfn nested() {}\n```\n",
            ))
            .await
            .unwrap();
        assert!(
            handle
                .injections()
                .iter()
                .any(|injection| injection.language == "rust" && injection.available)
        );
        assert!(
            handle
                .captures()
                .iter()
                .any(|capture| capture.family == SyntaxFamily::Context
                    && capture.name.as_ref() == "scope.name"
                    && capture.range.start.row == 3),
            "captures={:?}, injections={:?}",
            handle.captures(),
            handle.injections()
        );
        let mut metadata = forge_buffer::block::BlockMetadata::default();
        crate::projection::append_syntax_row(&mut metadata, &handle, 3, 0, "fn nested() {}")
            .unwrap();
        assert!(
            metadata
                .visible_decoration
                .iter()
                .any(|span| span.capture == "@function.rust")
        );
        assert!(
            metadata
                .visible_decoration
                .iter()
                .any(|span| span.capture == "@keyword.function.rust")
        );
    }

    #[tokio::test]
    async fn unknown_injection_is_explicit_and_does_not_guess_a_parser() {
        let handle = engine()
            .analyze(request(
                SyntaxLanguage::Markdown,
                "```not_configured\nhello\n```\n",
            ))
            .await
            .unwrap();
        assert!(
            handle
                .injections()
                .iter()
                .any(|injection| injection.language == "not_configured" && !injection.available)
        );
    }

    #[tokio::test]
    async fn every_configured_language_analyzes_a_real_fragment() {
        let engine = engine();
        for (name, text) in [
            ("rust", "fn main() {}"),
            ("typescript", "const value: number = 1;"),
            ("tsx", "const view = <div>Hi</div>;"),
            ("lua", "local value = 1"),
            ("vim", "let value = 1"),
            ("vimdoc", "*example* example help"),
            ("json", "{\"value\": 1}"),
            ("query", "(identifier) @variable"),
            ("javascript", "const value = 1;"),
            ("css", ".value { color: red; }"),
            ("html", "<div>hello</div>"),
            ("wgsl", "fn main() {}"),
            ("glsl", "void main() {}"),
            ("c_sharp", "class Example {}"),
            ("toml", "value = 1"),
            ("slang", "float main() { return 1.0; }"),
            ("yaml", "value: true"),
            ("nu", "let value = 1"),
            ("markdown", "# Example\n"),
            ("markdown_inline", "**bold**"),
            ("latex", "\\section{Example}"),
            ("cue", "value: 1"),
        ] {
            let language = SyntaxLanguage::from_name(name).unwrap();
            let handle = engine
                .analyze(request(language, text))
                .await
                .unwrap_or_else(|error| panic!("{name}: {error:?}"));
            assert!(
                handle
                    .captures()
                    .iter()
                    .any(|capture| capture.family == SyntaxFamily::Highlight),
                "{name}: {:?}",
                handle.captures()
            );
            assert_eq!(handle.source().text(), text);
        }
    }

    #[tokio::test]
    async fn syntax_acceptance_reports_deterministic_resource_usage_for_all_configured_languages() {
        let engine = engine();
        let started = std::time::Instant::now();
        let mut source_bytes = 0usize;
        let mut capture_count = 0usize;
        for (name, text) in [
            ("rust", "fn main() {}"),
            ("typescript", "const value: number = 1;"),
            ("tsx", "const view = <div>Hi</div>;"),
            ("lua", "local value = 1"),
            ("vim", "let value = 1"),
            ("vimdoc", "*example* example help"),
            ("json", "{\"value\": 1}"),
            ("query", "(identifier) @variable"),
            ("javascript", "const value = 1;"),
            ("css", ".value { color: red; }"),
            ("html", "<div>hello</div>"),
            ("wgsl", "fn main() {}"),
            ("glsl", "void main() {}"),
            ("c_sharp", "class Example {}"),
            ("toml", "value = 1"),
            ("slang", "float main() { return 1.0; }"),
            ("yaml", "value: true"),
            ("nu", "let value = 1"),
            ("markdown", "# Example\n"),
            ("markdown_inline", "**bold**"),
            ("latex", "\\section{Example}"),
            ("cue", "value: 1"),
        ] {
            let handle = engine
                .analyze(request(SyntaxLanguage::from_name(name).unwrap(), text))
                .await
                .unwrap_or_else(|error| panic!("{name}: {error:?}"));
            source_bytes += handle.source().retained_bytes();
            capture_count += handle.captures().len();
        }
        let usage = engine.usage();
        assert_eq!(SyntaxLanguage::ALL.len(), 22);
        assert_eq!(usage.active_jobs, 0);
        assert!(usage.cached_entries > 0 && usage.cached_entries <= 22);
        assert!(capture_count <= SyntaxLimits::default().captures);
        eprintln!(
            "syntax_acceptance={{\"languages\":22,\"source_bytes\":{source_bytes},\"captures\":{capture_count},\"cached_entries\":{},\"retained_bytes\":{},\"elapsed_us\":{}}}",
            usage.cached_entries,
            usage.retained_bytes,
            started.elapsed().as_micros(),
        );
    }

    #[test]
    fn configured_language_aliases_and_paths_preserve_native_identity() {
        for (name, language) in [
            ("rust", SyntaxLanguage::Rust),
            ("typescript", SyntaxLanguage::Typescript),
            ("ts", SyntaxLanguage::Typescript),
            ("tsx", SyntaxLanguage::Tsx),
            ("typescriptreact", SyntaxLanguage::Tsx),
            ("lua", SyntaxLanguage::Lua),
            ("vim", SyntaxLanguage::Vim),
            ("vimdoc", SyntaxLanguage::Vimdoc),
            ("help", SyntaxLanguage::Vimdoc),
            ("json", SyntaxLanguage::Json),
            ("query", SyntaxLanguage::Query),
            ("javascript", SyntaxLanguage::Javascript),
            ("js", SyntaxLanguage::Javascript),
            ("javascriptreact", SyntaxLanguage::Javascript),
            ("css", SyntaxLanguage::Css),
            ("html", SyntaxLanguage::Html),
            ("wgsl", SyntaxLanguage::Wgsl),
            ("wgslx", SyntaxLanguage::Wgsl),
            ("glsl", SyntaxLanguage::Glsl),
            ("frag", SyntaxLanguage::Glsl),
            ("vert", SyntaxLanguage::Glsl),
            ("c_sharp", SyntaxLanguage::CSharp),
            ("cs", SyntaxLanguage::CSharp),
            ("csharp", SyntaxLanguage::CSharp),
            ("toml", SyntaxLanguage::Toml),
            ("slang", SyntaxLanguage::Slang),
            ("shaderslang", SyntaxLanguage::Slang),
            ("yaml", SyntaxLanguage::Yaml),
            ("nu", SyntaxLanguage::Nu),
            ("markdown", SyntaxLanguage::Markdown),
            ("markdown_inline", SyntaxLanguage::MarkdownInline),
            ("latex", SyntaxLanguage::Latex),
            ("tex", SyntaxLanguage::Latex),
            ("cue", SyntaxLanguage::Cue),
        ] {
            assert_eq!(SyntaxLanguage::from_name(name), Some(language), "{name}");
        }
        for (path, language) in [
            ("src/main.rs", SyntaxLanguage::Rust),
            ("readme.md", SyntaxLanguage::Markdown),
            ("queries/highlights.scm", SyntaxLanguage::Query),
            ("config.yml", SyntaxLanguage::Yaml),
            ("view.jsx", SyntaxLanguage::Javascript),
            ("module.mjs", SyntaxLanguage::Javascript),
            ("legacy.cjs", SyntaxLanguage::Javascript),
            ("shader.frag", SyntaxLanguage::Glsl),
            ("shader.vert", SyntaxLanguage::Glsl),
            ("shader.wgslx", SyntaxLanguage::Wgsl),
        ] {
            assert_eq!(SyntaxLanguage::for_path(path), Some(language), "{path}");
        }
        assert_eq!(SyntaxLanguage::ALL.len(), 22);
    }

    #[tokio::test]
    async fn shared_consumers_cancel_independently_and_release_native_admission() {
        use crate::workers::WorkBudget;
        use std::future::Future;
        use std::task::Poll;

        let pool = Arc::new(AnalysisPool::new(PoolLimits {
            workers: 1,
            jobs: 2,
            input_bytes: 1024,
        }));
        let (release, blocked) = std::sync::mpsc::channel();
        let (started, running) = tokio::sync::oneshot::channel();
        pool.submit(
            WorkPriority::Foreground,
            WorkBudget::new(0, None),
            move |_| {
                started.send(()).unwrap();
                blocked
                    .recv_timeout(std::time::Duration::from_secs(2))
                    .unwrap();
            },
        )
        .unwrap();
        running.await.unwrap();
        let engine = SyntaxEngine::new(Arc::clone(&pool), SyntaxLimits::default());
        let mut first = Box::pin(engine.analyze(request(SyntaxLanguage::Rust, "fn shared() {}")));
        let mut second = Box::pin(engine.analyze(request(SyntaxLanguage::Rust, "fn shared() {}")));
        assert!(
            std::future::poll_fn(|context| Poll::Ready(first.as_mut().poll(context)))
                .await
                .is_pending()
        );
        assert!(
            std::future::poll_fn(|context| Poll::Ready(second.as_mut().poll(context)))
                .await
                .is_pending()
        );
        assert_eq!(engine.usage().active_jobs, 1);
        assert_eq!(pool.usage().admitted_jobs, 2);
        drop(first);
        assert_eq!(engine.usage().active_jobs, 1);
        release.send(()).unwrap();
        assert!(second.await.is_ok());
        assert_eq!(pool.usage().admitted_jobs, 0);
    }

    #[tokio::test]
    async fn visible_syntax_waits_for_temporary_shared_pool_saturation() {
        use crate::workers::WorkBudget;
        use std::future::Future;
        use std::task::Poll;

        let pool = Arc::new(AnalysisPool::new(PoolLimits {
            workers: 1,
            jobs: 1,
            input_bytes: 1024,
        }));
        let occupied = pool.reserve(WorkPriority::Foreground, WorkBudget::new(0, None)).unwrap();
        let engine = SyntaxEngine::new(Arc::clone(&pool), SyntaxLimits::default());
        let mut pending = Box::pin(engine.analyze(SyntaxRequest {
            priority: WorkPriority::Visible,
            ..request(SyntaxLanguage::Rust, "fn visible() {}")
        }));
        let before_release = std::future::poll_fn(|context| Poll::Ready(pending.as_mut().poll(context))).await;
        drop(occupied);
        let result = match before_release {
            Poll::Ready(result) => result,
            Poll::Pending => tokio::time::timeout(std::time::Duration::from_secs(5), pending).await.unwrap(),
        };
        assert!(result.is_ok(), "temporary pool saturation permanently failed visible syntax: {:?}", result.err());
        assert_eq!(pool.usage().admitted_jobs, 0);
    }

    #[tokio::test]
    async fn syntax_admission_waits_for_bytes_and_releases_cancelled_waiters() {
        use crate::workers::WorkBudget;
        use std::future::Future;
        use std::task::Poll;
        let pool = Arc::new(AnalysisPool::new(PoolLimits { workers: 1, jobs: 4, input_bytes: 1024 }));
        let occupied = pool.reserve(WorkPriority::Foreground, WorkBudget::new(1024, None)).unwrap();
        let engine = SyntaxEngine::new(Arc::clone(&pool), SyntaxLimits::default());
        let mut cancelled = Box::pin(engine.analyze(request(SyntaxLanguage::Rust, "fn cancelled() {}")));
        assert!(std::future::poll_fn(|context| Poll::Ready(cancelled.as_mut().poll(context))).await.is_pending());
        assert_eq!(engine.usage().retained_bytes, 0);
        assert_eq!(engine.usage().active_jobs, 0);
        drop(cancelled);
        let mut waiting = Box::pin(engine.analyze(request(SyntaxLanguage::Rust, "fn waiting() {}")));
        assert!(std::future::poll_fn(|context| Poll::Ready(waiting.as_mut().poll(context))).await.is_pending());
        drop(occupied);
        let syntax = tokio::time::timeout(std::time::Duration::from_secs(2), waiting).await.unwrap().unwrap();
        assert!(!syntax.captures().is_empty());
        assert_eq!(pool.usage().admitted_jobs, 0);
        assert_eq!(pool.usage().input_bytes, 0);
    }

    #[tokio::test]
    async fn syntax_admission_wait_ends_on_deadline_or_owner_close() {
        use crate::workers::{PoolError, WorkBudget};
        use std::future::Future;
        use std::task::Poll;
        for boundary in ["deadline", "syntax close", "pool close"] {
            let pool = Arc::new(AnalysisPool::new(PoolLimits { workers: 1, jobs: 1, input_bytes: 1024 }));
            let occupied = pool.reserve(WorkPriority::Foreground, WorkBudget::new(0, None)).unwrap();
            let engine = SyntaxEngine::new(Arc::clone(&pool), SyntaxLimits::default());
            let mut source = request(SyntaxLanguage::Rust, "fn waiting() {}");
            if boundary == "deadline" {
                source.deadline = Some(std::time::Instant::now() + std::time::Duration::from_millis(30));
            }
            let mut waiting = Box::pin(engine.analyze(source));
            assert!(std::future::poll_fn(|context| Poll::Ready(waiting.as_mut().poll(context))).await.is_pending());
            match boundary {
                "syntax close" => engine.close(),
                "pool close" => pool.close(),
                _ => {},
            }
            let error = tokio::time::timeout(std::time::Duration::from_secs(2), waiting).await.unwrap().err().unwrap();
            assert_eq!(error, match boundary {
                "syntax close" => SyntaxError::Closed,
                "pool close" => SyntaxError::Pool(PoolError::Closed),
                _ => SyntaxError::Deadline,
            });
            assert_eq!(engine.usage().retained_bytes, 0);
            assert_eq!(pool.usage().admitted_jobs, 1);
            drop(occupied);
        }
    }

    #[tokio::test]
    async fn syntax_admission_rejects_permanently_unavailable_capacity() {
        use crate::workers::PoolError;
        for (workers, jobs, input_bytes, expected) in [
            (0, 1, 1024, PoolError::WorkerUnavailable),
            (1, 0, 1024, PoolError::WorkerUnavailable),
            (1, 1, 1, PoolError::Oversized),
        ] {
            let pool = Arc::new(AnalysisPool::new(PoolLimits { workers, jobs, input_bytes }));
            let engine = SyntaxEngine::new(pool, SyntaxLimits::default());
            let outcome = tokio::time::timeout(std::time::Duration::from_secs(2), engine.analyze(request(SyntaxLanguage::Rust, "fn main() {}"))).await.unwrap();
            assert_eq!(outcome.err(), Some(SyntaxError::Pool(expected)));
            assert_eq!(engine.usage().retained_bytes, 0);
        }
    }

    #[tokio::test]
    async fn ordinary_pinned_sources_do_not_retain_maximum_capture_reservations() {
        let engine = engine();
        let mut pinned = Vec::new();
        for revision in 0..32 {
            let source = format!("fn revision_{revision}() {{ let value = {revision}; }}");
            let syntax = engine
                .analyze(request(SyntaxLanguage::Rust, &source))
                .await
                .unwrap_or_else(|error| panic!("revision {revision}: {error:?}"));
            assert!(!syntax.captures().is_empty());
            pinned.push(syntax);
        }
        let usage = engine.usage();
        eprintln!("32 pinned syntax sources: {} charged bytes", usage.retained_bytes);
        assert!(usage.retained_bytes < 1024 * 1024);
    }

    #[tokio::test]
    async fn pinned_syntax_retains_its_budget_after_cache_eviction() {
        let pool = Arc::new(AnalysisPool::new(PoolLimits {
            workers: 1,
            jobs: 1,
            input_bytes: 1024,
        }));
        let engine = SyntaxEngine::new(
            pool,
            SyntaxLimits {
                retained_bytes: (std::mem::size_of::<SyntaxCapture>()
                    + 2 * std::mem::size_of::<usize>())
                    * 128
                    + 1024,
                captures: 128,
                cached_entries: 1,
                ..SyntaxLimits::default()
            },
        );
        let pinned = engine
            .analyze(request(SyntaxLanguage::Rust, "fn first() {}"))
            .await
            .unwrap();
        assert!(matches!(
            engine
                .analyze(request(SyntaxLanguage::Rust, "fn other() {}"))
                .await,
            Err(SyntaxError::MemoryLimit)
        ));
        assert_eq!(engine.usage().cached_entries, 0);
        assert!(engine.usage().retained_bytes > 0);
        drop(pinned);
        assert_eq!(engine.usage().retained_bytes, 0);
        assert!(
            engine
                .analyze(request(SyntaxLanguage::Rust, "fn other() {}"))
                .await
                .is_ok()
        );
    }

    #[tokio::test]
    async fn capture_limit_returns_explicit_failure_without_retaining_partial_analysis() {
        let pool = Arc::new(AnalysisPool::new(PoolLimits {
            workers: 1,
            jobs: 1,
            input_bytes: 1024,
        }));
        let engine = SyntaxEngine::new(
            pool,
            SyntaxLimits {
                captures: 1,
                ..SyntaxLimits::default()
            },
        );
        assert!(matches!(
            engine
                .analyze(request(SyntaxLanguage::Rust, "fn main() {}"))
                .await,
            Err(SyntaxError::CaptureLimit)
        ));
        assert_eq!(engine.usage().active_jobs, 0);
        assert_eq!(engine.usage().retained_bytes, 0);
        assert_eq!(engine.usage().cached_entries, 0);
    }

    #[tokio::test]
    async fn indexed_row_queries_match_reference_for_multiline_captures() {
        let handle = engine()
            .analyze(request(
                SyntaxLanguage::Rust,
                "fn example() {\n let first = 1;\n let second = 2;\n}\nfn tail() {}\n",
            ))
            .await
            .unwrap();
        for start in 0..8 {
            for end in start..8 {
                let mut expected: Vec<_> = handle
                    .captures()
                    .iter()
                    .enumerate()
                    .filter_map(|(index, capture)| {
                        let capture_end = if capture.range.end.column == 0
                            && capture.range.end.row > capture.range.start.row
                        {
                            capture.range.end.row
                        } else {
                            capture.range.end.row + 1
                        };
                        (start < end && capture.range.start.row < end && capture_end > start)
                            .then_some(index)
                    })
                    .collect();
                let mut actual: Vec<_> = handle
                    .captures_intersecting_rows(start, end)
                    .map(|capture| {
                        handle
                            .captures()
                            .iter()
                            .position(|candidate| std::ptr::eq(candidate, capture))
                            .unwrap()
                    })
                    .collect();
                expected.sort_unstable();
                actual.sort_unstable();
                assert_eq!(actual, expected, "{start}..{end}");
            }
        }
        assert_eq!(handle.captures_intersecting_rows(1, 3).take(1).count(), 1);
    }

    #[tokio::test]
    async fn markdown_url_directive_uses_the_adjusted_capture_range() {
        let handle = engine()
            .analyze(request(
                SyntaxLanguage::MarkdownInline,
                "<https://example.com>",
            ))
            .await
            .unwrap();
        let url = handle
            .captures()
            .iter()
            .find_map(|capture| capture.url)
            .unwrap();
        assert_eq!(
            &handle.source().text()[url.start_byte..url.end_byte],
            "https://example.com"
        );
    }

    #[test]
    fn every_vendored_language_loads_all_query_families() {
        for language in SyntaxLanguage::ALL {
            let grammar = language.grammar();
            let mut parser = tree_sitter::Parser::new();
            parser.set_language(&grammar).unwrap();
            assert!(parser.parse("", None).is_some(), "{}", language.name());
            for kind in ["highlights", "locals", "injections", "diff_context"] {
                let text = query::source(language.name(), kind);
                query::CompiledQuery::new(&grammar, &text)
                    .unwrap_or_else(|error| panic!("{} {kind}: {error:?}", language.name()));
            }
        }
    }
}

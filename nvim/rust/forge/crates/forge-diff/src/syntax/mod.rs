//! Exact-source syntax trees share native parsing between highlights and structural context.

mod assets;
mod context;
mod query;
mod configuration;
mod declaration;
mod declaration_fold;
mod declaration_index;
mod declaration_calls;
pub use declaration_calls::{DeclarationCall, DeclarationCallKind, DeclarationCallable, DeclarationCalls};
pub use declaration_index::{DeclarationIndex, DeclarationIndexTiming, DeclarationImport, DeclarationModule, DeclarationMemberOwner, DeclarationReference, DeclarationRole, DeclarationSymbol, SymbolVisibility};
pub use declaration_fold::{DeclarationFold, DeclarationFolding};
mod visibility;
pub use configuration::ConfigurationFormat;
pub use visibility::DeclarationVisibility;
pub use declaration::{DeclarationComment, DeclarationOverview, DeclarationPosition, DeclarationPresentation};
mod service;
mod tree;

pub use assets::SyntaxLanguage;
pub use context::HunkContext;
pub use service::{SYNTAX_LINE_LIMIT, SyntaxEngine, SyntaxRequest, SyntaxUsage};
pub use tree::{
    SyntaxCapture, SyntaxFamily, SyntaxHandle, SyntaxInjection, SyntaxPoint, SyntaxRange,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SyntaxError {
    Query(String),
    Closed,
    Busy,
    MemoryLimit,
    CaptureLimit,
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
    async fn markdown_paragraphs_keep_all_inline_trees_below_line_limit() {
        let text = (0..1100)
            .map(|index| format!("Paragraph **{index}**.\n\n"))
            .collect::<String>();
        let syntax = engine()
            .analyze(request(SyntaxLanguage::Markdown, &text))
            .await
            .unwrap();
        assert_eq!(syntax.tree_count(), 1101);
        assert!(syntax.captures().iter().any(|capture| capture.range.start.row == 2198));
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
        eprintln!(
            "syntax_acceptance={{\"languages\":22,\"source_bytes\":{source_bytes},\"captures\":{capture_count},\"cached_entries\":{},\"elapsed_us\":{}}}",
            usage.cached_entries,
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
        let engine = SyntaxEngine::new(Arc::clone(&pool));
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
        let engine = SyntaxEngine::new(Arc::clone(&pool));
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
    async fn syntax_uses_workers_when_shared_byte_capacity_is_occupied() {
        use crate::workers::WorkBudget;
        let pool = Arc::new(AnalysisPool::new(PoolLimits { workers: 1, jobs: 4, input_bytes: 1 }));
        let occupied = pool.reserve(WorkPriority::Foreground, WorkBudget::new(1, None)).unwrap();
        let engine = SyntaxEngine::new(Arc::clone(&pool));
        let syntax = tokio::time::timeout(
            std::time::Duration::from_secs(2),
            engine.analyze(request(SyntaxLanguage::Rust, "fn highlighted() {}")),
        ).await.unwrap().unwrap();
        assert!(!syntax.captures().is_empty());
        assert_eq!(pool.usage().input_bytes, 1);
        drop(occupied);
        assert_eq!(pool.usage().admitted_jobs, 0);
    }

    #[tokio::test]
    async fn syntax_admission_wait_ends_on_deadline_or_owner_close() {
        use crate::workers::{PoolError, WorkBudget};
        use std::future::Future;
        use std::task::Poll;
        for boundary in ["deadline", "syntax close", "pool close"] {
            let pool = Arc::new(AnalysisPool::new(PoolLimits { workers: 1, jobs: 1, input_bytes: 1024 }));
            let occupied = pool.reserve(WorkPriority::Foreground, WorkBudget::new(0, None)).unwrap();
            let engine = SyntaxEngine::new(Arc::clone(&pool));
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
        ] {
            let pool = Arc::new(AnalysisPool::new(PoolLimits { workers, jobs, input_bytes }));
            let engine = SyntaxEngine::new(pool);
            let outcome = tokio::time::timeout(std::time::Duration::from_secs(2), engine.analyze(request(SyntaxLanguage::Rust, "fn main() {}"))).await.unwrap();
            assert_eq!(outcome.err(), Some(SyntaxError::Pool(expected)));
        }
    }

    #[tokio::test]
    async fn pinned_syntax_survives_cache_eviction_without_blocking_new_files() {
        let engine = engine();
        let pinned = engine.analyze(request(SyntaxLanguage::Rust, "fn pinned() {}"))
            .await.unwrap();
        for revision in 0..257 {
            let source = format!("fn revision_{revision}() {{ let value = {revision}; }}");
            let syntax = engine.analyze(request(SyntaxLanguage::Rust, &source)).await.unwrap();
            assert!(!syntax.captures().is_empty());
        }
        let reloaded = engine.analyze(request(SyntaxLanguage::Rust, "fn pinned() {}"))
            .await.unwrap();
        assert!(!Arc::ptr_eq(&pinned.0, &reloaded.0));
        assert_eq!(pinned.source_identity(), reloaded.source_identity());
        assert!(!pinned.captures().is_empty());
    }

    #[tokio::test]
    async fn syntax_accepts_ten_thousand_lines_and_skips_larger_sources() {
        let engine = engine();
        for newline in ["\n", "\r\n"] {
            for trailing_newline in [false, true] {
                for lines in [SYNTAX_LINE_LIMIT, SYNTAX_LINE_LIMIT + 1] {
                    let mut source = vec!["// padding"; lines];
                    source[0] = "fn boundary() {}";
                    let mut source = source.join(newline);
                    if trailing_newline { source.push_str(newline); }
                    let syntax = engine.analyze(request(SyntaxLanguage::Rust, &source)).await.unwrap();
                    assert_eq!(syntax.source().text(), source);
                    if lines == SYNTAX_LINE_LIMIT {
                        assert_eq!(syntax.tree_count(), 1);
                        assert!(!syntax.captures().is_empty());
                    } else {
                        assert_eq!(syntax.tree_count(), 0);
                        assert!(syntax.captures().is_empty());
                        assert!(syntax.injections().is_empty());
                        assert!(syntax.hunk_context(0).is_none());
                    }
                }
            }
        }
    }

    #[tokio::test]
    async fn long_lines_and_many_captures_remain_eligible() {
        let engine = engine();
        let source = format!("fn long_line() {{}} // {}", "x".repeat(1024 * 1024));
        let syntax = engine.analyze(request(SyntaxLanguage::Rust, &source)).await.unwrap();
        assert_eq!(syntax.tree_count(), 1);
        assert!(!syntax.captures().is_empty());
        let source = "fn dense() { let first = 1; let second = first + 2; }\n".repeat(SYNTAX_LINE_LIMIT);
        let syntax = engine.analyze(request(SyntaxLanguage::Rust, &source)).await.unwrap();
        assert!(syntax.captures().len() > 65_536);
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

//! Exact-source syntax trees share native parsing between highlights and structural context.

mod algorithm;
mod assets;
pub use algorithm::{
    AlgorithmInvocation, AlgorithmReference, algorithm_reference, algorithm_references,
    validate_algorithm,
};
mod configuration;
mod context;
mod declaration;
mod declaration_contract;
mod query;
pub use declaration_contract::{ContractDeclaration, ContractDifference, DeclarationContract};
mod declaration_calls;
mod declaration_fold;
mod declaration_index;
pub use declaration_calls::{
    DeclarationCall, DeclarationCallKind, DeclarationCallable, DeclarationCalls,
};
pub use declaration_fold::{DeclarationFold, DeclarationFolding};
pub use declaration_index::{
    DeclarationImport, DeclarationIndex, DeclarationIndexTiming, DeclarationMemberOwner,
    DeclarationModule, DeclarationReference, DeclarationRole, DeclarationSymbol, SymbolVisibility,
};
mod visibility;
pub use configuration::ConfigurationFormat;
pub use declaration::{
    DeclarationComment, DeclarationOverview, DeclarationPosition, DeclarationPresentation,
};
pub use visibility::DeclarationVisibility;
mod outline;
mod service;
mod structure;
mod tree;
pub use outline::{SourceOutline, SourceScope};
pub use structure::SourceDeclaration;

pub use assets::SyntaxLanguage;
pub use context::HunkContext;
pub use service::{SyntaxEngine, SyntaxRequest, SyntaxUsage};
pub use tree::{
    SyntaxCapture, SyntaxFamily, SyntaxHandle, SyntaxInjection, SyntaxPoint, SyntaxRange,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SyntaxError {
    Query(String),
    Closed,
    Busy,
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
        SyntaxEngine::new(Arc::new(AnalysisPool::new(PoolLimits {
            workers: 1,
            jobs: 2,
        })))
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
    async fn every_configured_language_analyzes_a_real_fragment() {
        let engine = engine();
        for (name, text) in [
            ("rust", "fn main() {}"),
            ("typescript", "const value: number = 1;"),
            ("tsx", "const view = <div>Hi</div>;"),
            ("lua", "local value = 1"),
            (
                "python",
                "def example(value: int) -> int:\n    return value + 1\n",
            ),
            ("c", "int calculate(int value) { return value + 1; }"),
            (
                "cpp",
                "template<typename Value> Value calculate(Value value) { return value + 1; }",
            ),
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
            ("algorithm", "value ← 1\nif value ≥ 1\n  return value"),
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
    async fn shared_consumers_cancel_independently_and_release_native_admission() {
        use crate::workers::WorkBudget;
        use std::future::Future;
        use std::task::Poll;

        let pool = Arc::new(AnalysisPool::new(PoolLimits {
            workers: 1,
            jobs: 2,
        }));
        let (release, blocked) = std::sync::mpsc::channel();
        let (started, running) = tokio::sync::oneshot::channel();
        pool.submit(
            WorkPriority::Foreground,
            WorkBudget::new(0, None),
            move |_| {
                started.send(()).unwrap();
                blocked.recv().unwrap();
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
        }));
        let occupied = pool
            .reserve(WorkPriority::Foreground, WorkBudget::new(0, None))
            .unwrap();
        let engine = SyntaxEngine::new(Arc::clone(&pool));
        let mut pending = Box::pin(engine.analyze(SyntaxRequest {
            priority: WorkPriority::Visible,
            ..request(SyntaxLanguage::Rust, "fn visible() {}")
        }));
        let before_release =
            std::future::poll_fn(|context| Poll::Ready(pending.as_mut().poll(context))).await;
        drop(occupied);
        let result = match before_release {
            Poll::Ready(result) => result,
            Poll::Pending => pending.await,
        };
        assert!(
            result.is_ok(),
            "temporary pool saturation permanently failed visible syntax: {:?}",
            result.err()
        );
        assert_eq!(pool.usage().admitted_jobs, 0);
    }

    #[tokio::test]
    async fn syntax_admission_rejects_permanently_unavailable_capacity() {
        use crate::workers::PoolError;
        for (workers, jobs, expected) in [
            (0, 1, PoolError::WorkerUnavailable),
            (1, 0, PoolError::WorkerUnavailable),
        ] {
            let pool = Arc::new(AnalysisPool::new(PoolLimits { workers, jobs }));
            let engine = SyntaxEngine::new(pool);
            let outcome = engine
                .analyze(request(SyntaxLanguage::Rust, "fn main() {}"))
                .await;
            assert_eq!(outcome.err(), Some(SyntaxError::Pool(expected)));
        }
    }

    #[tokio::test]
    async fn pinned_syntax_survives_cache_eviction_without_blocking_new_files() {
        let engine = engine();
        let pinned = engine
            .analyze(request(SyntaxLanguage::Rust, "fn pinned() {}"))
            .await
            .unwrap();
        for revision in 0..257 {
            let source = format!("fn revision_{revision}() {{ let value = {revision}; }}");
            let syntax = engine
                .analyze(request(SyntaxLanguage::Rust, &source))
                .await
                .unwrap();
            assert!(!syntax.captures().is_empty());
        }
        let reloaded = engine
            .analyze(request(SyntaxLanguage::Rust, "fn pinned() {}"))
            .await
            .unwrap();
        assert!(!Arc::ptr_eq(&pinned.0, &reloaded.0));
        assert_eq!(pinned.source_identity(), reloaded.source_identity());
        assert!(!pinned.captures().is_empty());
    }
}

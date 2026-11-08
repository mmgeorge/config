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
}

//! Exact-source syntax trees share native parsing between highlights and structural context.

mod algorithm;
mod assets;
pub use algorithm::{
    AlgorithmDiagnostic, AlgorithmInvocation, AlgorithmReference, AlgorithmValidationError,
    algorithm_reference, algorithm_references, validate_algorithm,
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
    DeclarationModule, DeclarationReference, DeclarationRole, DeclarationSymbol, FieldShape,
    SymbolVisibility,
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

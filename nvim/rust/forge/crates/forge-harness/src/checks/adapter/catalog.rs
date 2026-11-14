use super::{
    AdapterContext, AdapterError, BuildAdapter, FormatAdapter, LintAdapter, PathSelection, RunExit,
    RustNextestAdapter, RustNextestOptions, ScriptAdapter, ScriptAdapterConfig, TestAdapter,
    TypeCheckAdapter,
    events::{DiagnosticCategory, DiagnosticSink},
};
use crate::checks::CheckCategory;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

/// Select one compiled-in adapter and its typed configuration.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum AdapterConfig {
    Script(ScriptAdapterConfig),
    RustNextest(RustNextestOptions),
}

impl AdapterConfig {
    /// Validate options and reject categories the adapter cannot implement.
    pub fn validate(&self, category: CheckCategory) -> Result<(), AdapterError> {
        self.build(category).map(|_| ())
    }

    /// Construct the category contract supported by this configuration.
    pub fn build(&self, category: CheckCategory) -> Result<CategoryAdapter, AdapterError> {
        match self {
            Self::RustNextest(options) => {
                if category != CheckCategory::Test {
                    return Err(AdapterError::Protocol(
                        "rust_nextest requires the test category".into(),
                    ));
                }
                Ok(CategoryAdapter::Test(Box::new(RustNextestAdapter::new(
                    options.clone(),
                )?)))
            }
            Self::Script(options) => {
                let adapter = ScriptAdapter::new(options.clone())?;
                match category {
                    CheckCategory::Test => Ok(CategoryAdapter::Test(Box::new(adapter))),
                    CheckCategory::TypeCheck => Ok(CategoryAdapter::Diagnostics(
                        DiagnosticAdapter::TypeCheck(Box::new(adapter)),
                    )),
                    CheckCategory::Lint => Ok(CategoryAdapter::Diagnostics(
                        DiagnosticAdapter::Lint(Box::new(adapter)),
                    )),
                    CheckCategory::Build => Ok(CategoryAdapter::Diagnostics(
                        DiagnosticAdapter::Build(Box::new(adapter)),
                    )),
                    CheckCategory::Format => Ok(CategoryAdapter::Format(Box::new(adapter))),
                    CheckCategory::Custom => Err(AdapterError::Protocol(
                        "the script adapter does not implement this category".into(),
                    )),
                }
            }
        }
    }

    /// Get the fixed invocation shown during admission and check review.
    pub fn display(&self) -> String {
        match self {
            Self::RustNextest(_) => "cargo nextest".into(),
            Self::Script(options) => {
                crate::checks::invocation::display(&options.command[0], &options.command[1..])
            }
        }
    }
}

/// Preserve the category contract at the supervisor dispatch boundary.
pub enum CategoryAdapter {
    Test(Box<dyn TestAdapter>),
    Diagnostics(DiagnosticAdapter),
    Format(Box<dyn FormatAdapter>),
}

/// Dispatch categories that share the diagnostic event contract.
pub enum DiagnosticAdapter {
    TypeCheck(Box<dyn TypeCheckAdapter>),
    Lint(Box<dyn LintAdapter>),
    Build(Box<dyn BuildAdapter>),
}

impl DiagnosticAdapter {
    /// Apply lint fixes while rejecting source changes in other diagnostic categories.
    pub async fn fix(
        &self,
        context: &AdapterContext<'_>,
        selection: &PathSelection,
        events: &mut DiagnosticSink,
    ) -> Result<RunExit, AdapterError> {
        match self {
            Self::Lint(adapter) => adapter.fix(context, selection, events).await,
            Self::TypeCheck(_) | Self::Build(_) => Err(AdapterError::Unsupported("fix".into())),
        }
    }
    /// Get the category needed to validate permitted events.
    pub fn category(&self) -> DiagnosticCategory {
        match self {
            Self::TypeCheck(_) => DiagnosticCategory::TypeCheck,
            Self::Lint(_) => DiagnosticCategory::Lint,
            Self::Build(_) => DiagnosticCategory::Build,
        }
    }

    /// Execute the read-only operation owned by the configured category.
    pub async fn run(
        &self,
        context: &AdapterContext<'_>,
        selection: &PathSelection,
        events: &mut DiagnosticSink,
    ) -> Result<RunExit, AdapterError> {
        match self {
            Self::TypeCheck(adapter) => adapter.typecheck(context, selection, events).await,
            Self::Lint(adapter) => adapter.lint(context, selection, events).await,
            Self::Build(adapter) => adapter.build(context, selection, events).await,
        }
    }
}

/// Describe one compiled-in adapter for configuration discovery.
#[derive(Clone, Debug, Serialize)]
pub struct AdapterCatalogEntry {
    pub kind: &'static str,
    pub categories: &'static [CheckCategory],
    pub summary: &'static str,
    pub use_when: &'static str,
    pub options: schemars::Schema,
}

/// Get the adapter catalog from the types compiled into this Harness build.
pub fn catalog() -> Vec<AdapterCatalogEntry> {
    vec![
        AdapterCatalogEntry {
            kind: "rust_nextest",
            categories: &[CheckCategory::Test],
            summary: "Discovers and runs Rust tests with Cargo and nextest.",
            use_when: "Rust tests use cargo-nextest 0.9.151 or later with libtest-json-plus format 0.1 for static source discovery, exact compiled discovery, and structured per-case results.",
            options: schemars::schema_for!(RustNextestOptions),
        },
        AdapterCatalogEntry {
            kind: "script",
            categories: &[
                CheckCategory::TypeCheck,
                CheckCategory::Test,
                CheckCategory::Format,
                CheckCategory::Lint,
                CheckCategory::Build,
            ],
            summary: "Runs a repository command implementing check adapter protocol version 1.",
            use_when: "The repository supplies a script implementing the selected category's test, diagnostic, or format event protocol.",
            options: schemars::schema_for!(ScriptAdapterConfig),
        },
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Keep configuration validation, runtime construction, and published categories in agreement.
    #[test]
    fn published_categories_match_constructible_contracts() {
        let config = AdapterConfig::Script(ScriptAdapterConfig {
            command: vec!["nu".into(), "--stdin".into(), "adapter.nu".into()],
        });
        let entries = catalog();
        assert_eq!(entries.len(), 2);
        for category in [
            CheckCategory::TypeCheck,
            CheckCategory::Test,
            CheckCategory::Format,
            CheckCategory::Lint,
            CheckCategory::Build,
            CheckCategory::Custom,
        ] {
            assert_eq!(
                config.build(category).is_ok(),
                entries[1].categories.contains(&category)
            );
            assert_eq!(
                AdapterConfig::RustNextest(RustNextestOptions::default())
                    .build(category)
                    .is_ok(),
                entries[0].categories.contains(&category)
            );
        }
    }
}

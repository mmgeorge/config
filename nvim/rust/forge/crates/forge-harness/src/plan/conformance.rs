use anyhow::Result;
use forge_diff::syntax::{ConfigurationFormat, DeclarationContract, DeclarationOverview};

/// Compare required declarations without imposing implementation bodies or layout.
pub(crate) fn compare(path: &str, expected: &str, source: &str) -> Result<Vec<String>> {
    if let Some(format) = ConfigurationFormat::for_path(path) {
        format
            .validate(path, source)
            .map_err(|error| anyhow::anyhow!("{error:?}"))?;
        let equal = match format {
            ConfigurationFormat::Json | ConfigurationFormat::Jsonc => {
                ConfigurationFormat::json_value(expected)
                    .map_err(|error| anyhow::anyhow!("{error:?}"))?
                    == ConfigurationFormat::json_value(source)
                        .map_err(|error| anyhow::anyhow!("{error:?}"))?
            }
            ConfigurationFormat::Toml => {
                toml::from_str::<toml::Value>(expected)? == toml::from_str::<toml::Value>(source)?
            }
            _ => expected.replace("\r\n", "\n").trim() == source.replace("\r\n", "\n").trim(),
        };
        return Ok(if equal {
            Vec::new()
        } else {
            vec!["configuration differs from the accepted values".into()]
        });
    }
    let overview =
        DeclarationOverview::extract(path, source).map_err(|error| anyhow::anyhow!("{error:?}"))?;
    let expected =
        DeclarationContract::parse(path, expected).map_err(|error| anyhow::anyhow!("{error:?}"))?;
    let actual = DeclarationContract::parse(path, &overview)
        .map_err(|error| anyhow::anyhow!("{error:?}"))?;
    Ok(expected.differences(&actual))
}

/// Reuse the declaration resolver's export graph for methods, aliases, and private modules.
pub(crate) fn contract(
    workspace: &std::path::Path,
    path: &str,
    overview: &str,
) -> Result<DeclarationContract> {
    let mut contract =
        DeclarationContract::parse(path, overview).map_err(|error| anyhow::anyhow!("{error:?}"))?;
    let mut design = super::DeclarationDesign::default();
    design.proposed.insert(path.into(), overview.into());
    let exposed = crate::declaration::exposure::symbols(workspace, &design)?;
    let positions = exposed
        .into_iter()
        .filter(|(file, _, _)| file == path)
        .map(|(_, line, column)| (line, column))
        .collect();
    contract.retain_exposed(&positions);
    Ok(contract)
}

#[cfg(test)]
fn compare_in_workspace(
    workspace: &std::path::Path,
    path: &str,
    expected: &str,
    source: &str,
) -> Result<Vec<String>> {
    Ok(inspect_in_workspace(workspace, path, expected, source)?.0)
}

pub(crate) fn inspect_in_workspace(
    workspace: &std::path::Path,
    path: &str,
    expected: &str,
    source: &str,
) -> Result<(Vec<String>, Vec<super::execution::DeclarationMismatch>)> {
    if ConfigurationFormat::for_path(path).is_some() {
        let findings = compare(path, expected, source)?;
        let mut changes = Vec::new();
        if !findings.is_empty() {
            changes.push(declaration_mismatch(path, "Configuration", expected, source, &findings[0])?);
        }
        return Ok((findings, changes));
    }
    let overview =
        DeclarationOverview::extract(path, source).map_err(|error| anyhow::anyhow!("{error:?}"))?;
    let planned =
        DeclarationContract::parse(path, expected).map_err(|error| anyhow::anyhow!("{error:?}"))?;
    let actual = contract(workspace, path, &overview)?;
    let changes = planned.changes(&actual);
    let differences = changes.iter().map(|change| change.message.clone()).collect();
    let changes = changes.into_iter().map(|change|
        declaration_mismatch(path, &change.identity, &change.expected, &change.observed, &change.message)
    ).collect::<Result<Vec<_>>>()?;
    Ok((differences, changes))
}

pub(crate) fn declaration_mismatch(path: &str, name: &str, expected: &str, observed: &str, finding: &str)
    -> Result<super::execution::DeclarationMismatch>
{
    let mut patch = Vec::new();
    super::revision::write_file(path, path, Some(expected), Some(observed), &mut patch)?;
    Ok(super::execution::DeclarationMismatch { path: path.into(), name: name.into(), diff: String::from_utf8(patch)?, finding: format!("{path}: {finding}") })
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn declaration_changes_keep_source_signatures_without_implementation_bodies() {
        let workspace = tempfile::tempdir().unwrap();
        let (findings, changes) = inspect_in_workspace(workspace.path(), "src/lib.rs",
            "#[inline]\npub fn update(value: u32);",
            "#[inline]\npub fn update(mut value: u32) { value += 1; }").unwrap();
        assert_eq!(findings.len(), 1);
        assert_eq!(changes.len(), 1);
        let change = &changes[0];
        assert!(change.diff.contains("-pub fn update(value: u32);"));
        assert!(change.diff.contains("+pub fn update(mut value: u32);"));
        assert!(!change.diff.contains("value += 1"));
        assert!(!change.diff.contains("function_signature_item"));
        assert_eq!(change.finding, format!("src/lib.rs: {}", findings[0]));
        let (_, fields) = inspect_in_workspace(workspace.path(), "src/lib.rs",
            "pub struct State { pub count: u32 }", "pub struct State { pub count: bool }").unwrap();
        assert!(fields.iter().any(|change| change.diff.contains("pub count: u32") && change.diff.contains("pub count: bool")));
    }

    #[test]
    fn helpers_in_private_modules_are_internal_but_new_exports_are_not() {
        let workspace = tempfile::tempdir().unwrap();
        std::fs::create_dir(workspace.path().join("src")).unwrap();
        std::fs::write(
            workspace.path().join("Cargo.toml"),
            r#"[package]
name = "fixture"
version = "0.1.0"
edition = "2024"
"#,
        )
        .unwrap();
        std::fs::write(
            workspace.path().join("src/lib.rs"),
            "mod helpers; pub mod api;",
        )
        .unwrap();
        assert!(
            compare_in_workspace(workspace.path(), "src/helpers.rs", "", "pub fn helper() {}")
                .unwrap()
                .is_empty()
        );
        assert!(
            !compare_in_workspace(workspace.path(), "src/api.rs", "", "pub fn public_api() {}")
                .unwrap()
                .is_empty()
        );
        assert!(
            !compare_in_workspace(
                workspace.path(),
                "src/helpers.rs",
                "pub fn helper(value: u32);",
                "pub fn helper(value: bool) {}"
            )
            .unwrap()
            .is_empty()
        );
        std::fs::write(
            workspace.path().join("src/lib.rs"),
            "mod helpers; pub use helpers::Api;",
        )
        .unwrap();
        assert!(
            !compare_in_workspace(
                workspace.path(),
                "src/helpers.rs",
                "pub struct Api;",
                "pub struct Api; impl Api { pub fn added() {} }"
            )
            .unwrap()
            .is_empty()
        );
        assert!(
            compare_in_workspace(
                workspace.path(),
                "src/helpers.rs",
                "pub struct Api;",
                "pub struct Api; fn helper() {}"
            )
            .unwrap()
            .is_empty()
        );
    }
}

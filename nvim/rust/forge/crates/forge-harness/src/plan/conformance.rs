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

pub(crate) fn compare_in_workspace(
    workspace: &std::path::Path,
    path: &str,
    expected: &str,
    source: &str,
) -> Result<Vec<String>> {
    if ConfigurationFormat::for_path(path).is_some() {
        return compare(path, expected, source);
    }
    let overview =
        DeclarationOverview::extract(path, source).map_err(|error| anyhow::anyhow!("{error:?}"))?;
    let planned =
        DeclarationContract::parse(path, expected).map_err(|error| anyhow::anyhow!("{error:?}"))?;
    let actual = contract(workspace, path, &overview)?;
    Ok(planned.differences(&actual))
}

#[cfg(test)]
mod tests {
    use super::*;
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

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
    let design = super::DeclarationDesign::scratch(std::collections::BTreeMap::from([(
        path.to_owned(),
        overview.to_owned(),
    )]))?;
    let exposed = crate::declaration::exposure::symbols(workspace, &design)?;
    let positions = exposed
        .into_iter()
        .filter(|(file, _, _)| file == path)
        .map(|(_, line, column)| (line, column))
        .collect();
    contract.retain_exposed(&positions);
    Ok(contract)
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
            changes.push(declaration_mismatch(
                path,
                "Configuration",
                expected,
                source,
                &findings[0],
            )?);
        }
        return Ok((findings, changes));
    }
    let overview =
        DeclarationOverview::extract(path, source).map_err(|error| anyhow::anyhow!("{error:?}"))?;
    let planned =
        DeclarationContract::parse(path, expected).map_err(|error| anyhow::anyhow!("{error:?}"))?;
    let actual = contract(workspace, path, &overview)?;
    let changes = planned.changes(&actual);
    let differences = changes
        .iter()
        .map(|change| change.message.clone())
        .collect();
    let changes = changes
        .into_iter()
        .map(|change| {
            declaration_mismatch(
                path,
                &change.identity,
                &change.expected,
                &change.observed,
                &change.message,
            )
        })
        .collect::<Result<Vec<_>>>()?;
    Ok((differences, changes))
}

pub(crate) fn declaration_mismatch(
    path: &str,
    name: &str,
    expected: &str,
    observed: &str,
    finding: &str,
) -> Result<super::execution::DeclarationMismatch> {
    let mut patch = Vec::new();
    super::revision::write_file(path, path, Some(expected), Some(observed), &mut patch)?;
    Ok(super::execution::DeclarationMismatch {
        path: path.into(),
        name: name.into(),
        diff: String::from_utf8(patch)?,
        finding: format!("{path}: {finding}"),
    })
}

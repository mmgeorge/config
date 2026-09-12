use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

pub const CATEGORY_BASH: &str = "bash";
pub const CATEGORY_EDIT: &str = "edit";
pub const CATEGORY_ELEVATE: &str = "elevate";
pub const CATEGORY_MCP: &str = "mcp";
pub const CATEGORY_READ: &str = "read";
pub const CATEGORY_TOOL: &str = "tool";
pub const CATEGORY_WEBFETCH: &str = "webfetch";

const CATEGORY_LIST: &[&str] = &[
    CATEGORY_BASH,
    CATEGORY_EDIT,
    CATEGORY_ELEVATE,
    CATEGORY_MCP,
    CATEGORY_READ,
    CATEGORY_TOOL,
    CATEGORY_WEBFETCH,
];

/// Defines one durable permission action in the Harness policy document.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum PermissionDecision {
    Allow,
    Ask,
    Deny,
}

/// Stores the single Rulesync-shaped permission document owned by Harness.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PermissionDocument {
    #[serde(rename = "$schema", skip_serializing_if = "Option::is_none")]
    pub schema: Option<String>,
    pub permission: BTreeMap<String, BTreeMap<String, PermissionDecision>>,
}

/// Parse and validate one Harness permission document from JSON.
pub fn parse_permission_document(source: &str) -> Result<PermissionDocument> {
    let document: PermissionDocument =
        serde_json::from_str(source).context("decode permissions JSON")?;
    for (category, rule_map) in &document.permission {
        anyhow::ensure!(
            CATEGORY_LIST.contains(&category.as_str()),
            "unsupported permission category: {category}"
        );
        for pattern in rule_map.keys() {
            anyhow::ensure!(
                !pattern.trim().is_empty(),
                "permission category {category} contains an empty pattern"
            );
        }
    }
    Ok(document)
}

/// Serialize one validated permission document into stable pretty JSON.
pub fn serialize_permission_document(document: &PermissionDocument) -> Result<String> {
    let mut source = serde_json::to_string_pretty(document)?;
    source.push('\n');
    Ok(source)
}

/// Replace one exact permission rule in the document.
pub fn set_permission_rule(
    document: &mut PermissionDocument,
    category: &str,
    pattern: &str,
    decision: PermissionDecision,
) -> Result<()> {
    anyhow::ensure!(
        CATEGORY_LIST.contains(&category),
        "unsupported permission category: {category}"
    );
    let pattern = pattern.trim();
    anyhow::ensure!(!pattern.is_empty(), "permission pattern cannot be empty");
    document
        .permission
        .entry(category.to_owned())
        .or_default()
        .insert(pattern.to_owned(), decision);
    Ok(())
}

/// Build the conservative default used before a personal policy exists.
pub fn default_permission_document() -> PermissionDocument {
    PermissionDocument {
        schema: Some(
            "https://github.com/dyoshikawa/rulesync/releases/latest/download/permissions-schema.json"
                .into(),
        ),
        permission: BTreeMap::from([
            (
                CATEGORY_READ.into(),
                BTreeMap::from([("*".into(), PermissionDecision::Allow)]),
            ),
            (
                CATEGORY_EDIT.into(),
                BTreeMap::from([(".".into(), PermissionDecision::Allow)]),
            ),
            (
                CATEGORY_WEBFETCH.into(),
                BTreeMap::from([("*".into(), PermissionDecision::Allow)]),
            ),
            (
                CATEGORY_MCP.into(),
                BTreeMap::from([("*".into(), PermissionDecision::Ask)]),
            ),
        ]),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parses_rulesync_shaped_permissions_and_rejects_unknown_categories() {
        let source = r#"{
          "$schema": "schema",
          "permission": {
            "bash": { "git *": "allow", "git commit": "deny" },
            "mcp": { "github/*": "ask" }
          }
        }"#;
        let document = parse_permission_document(source).unwrap();
        assert_eq!(
            document.permission["bash"]["git commit"],
            PermissionDecision::Deny
        );
        assert!(parse_permission_document(r#"{"permission":{"unknown":{"*":"allow"}}}"#).is_err());
    }

    #[test]
    fn serializes_stable_pretty_json_after_replacing_a_rule() {
        let mut document = default_permission_document();
        set_permission_rule(
            &mut document,
            CATEGORY_BASH,
            "git commit",
            PermissionDecision::Deny,
        )
        .unwrap();
        let source = serialize_permission_document(&document).unwrap();
        assert!(source.ends_with('\n'));
        assert_eq!(parse_permission_document(&source).unwrap(), document);
    }
}

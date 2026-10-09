use std::collections::BTreeMap;

use anyhow::{Context, Result};
use forge_diff::syntax::{DeclarationOverview, DeclarationPosition};

use super::DeclarationDesign;
use crate::declaration::{DeclarationDiagnostic, DeclarationValidationError};

/// Reject newly authored comment openings without imposing new rules on captured source.
pub(super) fn validate(design: &DeclarationDesign) -> Result<()> {
    let mut diagnostic = Vec::new();
    let moved_origin = design
        .moved
        .iter()
        .map(|(origin, destination)| (destination.as_str(), origin.as_str()))
        .collect::<BTreeMap<_, _>>();
    for path in design.changed_paths() {
        let Some(proposed) = design.proposed.get(&path) else {
            continue;
        };
        if !proposed
            .as_bytes()
            .windows(7)
            .any(|word| word.eq_ignore_ascii_case(b"Returns"))
        {
            continue;
        }
        let origin = moved_origin.get(path.as_str()).copied().unwrap_or(&path);
        let mut baseline = BTreeMap::<String, usize>::new();
        if let Some(source) = design.baseline.get(origin) {
            for comment in comments(origin, &source.text)? {
                if starts_with_returns(&comment.text) {
                    *baseline.entry(comment.text).or_default() += 1;
                }
            }
        }
        for comment in comments(&path, proposed)? {
            if !starts_with_returns(&comment.text) {
                continue;
            }
            if let Some(count) = baseline.get_mut(&comment.text)
                && *count > 0
            {
                *count -= 1;
                continue;
            }
            diagnostic.push(DeclarationDiagnostic {
                path: path.clone(),
                line: comment.position.line,
                column: comment.position.column,
                reference: "Returns".into(),
                error: true,
                reason: "Plan comments must not start with 'Returns'. Use 'Get' instead.".into(),
            });
        }
    }
    if diagnostic.is_empty() {
        Ok(())
    } else {
        Err(DeclarationValidationError { diagnostic }.into())
    }
}

struct PlanComment {
    position: DeclarationPosition,
    end_line: u32,
    marker: Option<String>,
    text: String,
}

fn comments(path: &str, text: &str) -> Result<Vec<PlanComment>> {
    let extracted = DeclarationOverview::comments(path, text)
        .map_err(|error| anyhow::anyhow!("{error:?}"))
        .with_context(|| format!("extract plan comments from {path}"))?;
    let mut comment = Vec::<PlanComment>::new();
    for source in extracted {
        let raw = source.text.trim_end_matches(['\r', '\n']);
        let end_line = source.position.line + raw.lines().count().saturating_sub(1) as u32;
        let (marker, body) = comment_body(raw);
        let normalized = body.split_whitespace().collect::<Vec<_>>().join(" ");
        if let Some(previous) = comment.last_mut()
            && marker.is_some()
            && previous.marker.as_deref() == marker
            && previous.end_line + 1 == source.position.line
            && previous.position.column == source.position.column
        {
            if !previous.text.is_empty() && !normalized.is_empty() {
                previous.text.push(' ');
            }
            previous.text.push_str(&normalized);
            previous.end_line = end_line;
        } else {
            comment.push(PlanComment {
                position: source.position,
                end_line,
                marker: marker.map(str::to_owned),
                text: normalized,
            });
        }
    }
    Ok(comment)
}

fn comment_body(raw: &str) -> (Option<&str>, String) {
    if let Some(block) = raw.strip_prefix("<!--") {
        return (None, block.strip_suffix("-->").unwrap_or(block).to_owned());
    }
    if let Some(block) = raw.strip_prefix("/*") {
        let body = block
            .strip_suffix("*/")
            .unwrap_or(block)
            .trim_start_matches(['*', '!']);
        return (
            None,
            body.lines()
                .map(|line| line.trim_start().trim_start_matches('*'))
                .collect::<Vec<_>>()
                .join(" "),
        );
    }
    if let Some(block) = raw.strip_prefix("--[") {
        let equal = block.bytes().take_while(|byte| *byte == b'=').count();
        if block.as_bytes().get(equal) == Some(&b'[') {
            let closing = format!("]{}]", "=".repeat(equal));
            let body = &block[equal + 1..];
            return (None, body.strip_suffix(&closing).unwrap_or(body).to_owned());
        }
    }
    for marker in ["///", "//!", "//", "---", "--", "#"] {
        if let Some(body) = raw.strip_prefix(marker) {
            return (Some(marker), body.to_owned());
        }
    }
    (None, raw.to_owned())
}

fn starts_with_returns(text: &str) -> bool {
    text.get(..7)
        .is_some_and(|word| word.eq_ignore_ascii_case("Returns"))
        && text[7..]
            .chars()
            .next()
            .is_none_or(|character| !character.is_alphanumeric() && character != '_')
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::plan::DeclarationFile;

    fn design(path: &str, proposed: &str, baseline: Option<&str>) -> DeclarationDesign {
        let mut design = DeclarationDesign::default();
        design.proposed.insert(path.into(), proposed.into());
        if let Some(baseline) = baseline {
            design.baseline.insert(
                path.into(),
                DeclarationFile {
                    text: baseline.into(),
                    source_digest: "captured".into(),
                },
            );
        }
        design
    }

    #[test]
    fn rejects_comment_opening_with_actionable_source_diagnostic() {
        let error = validate(&design("src/lib.rs",
            "impl Settings {\n  /// Returns the starter game's finite arena and playable round settings.\n  pub fn standard() -> Self;\n}\n", None)).unwrap_err();
        let diagnostic = &error
            .downcast_ref::<DeclarationValidationError>()
            .unwrap()
            .diagnostic;
        assert_eq!(diagnostic.len(), 1);
        assert_eq!(
            (
                &*diagnostic[0].path,
                diagnostic[0].line,
                diagnostic[0].column
            ),
            ("src/lib.rs", 2, 2)
        );
        assert!(diagnostic[0].reason.contains("Use 'Get' instead"));
    }

    #[test]
    fn accepts_get_later_mentions_and_identifier_prefixes() {
        for comment in [
            "Get the settings.",
            "Collects settings and returns them.",
            "Get the settings. Returns the default value on failure.",
            "ReturnsValue identifies the result.",
        ] {
            validate(&design(
                "lib.rs",
                &format!("/// {comment}\npub fn standard();\n"),
                None,
            ))
            .unwrap();
        }
        assert!(!starts_with_returns("Returnsλ"));
        assert!(!starts_with_returns("Retürns"));
    }

    #[test]
    fn treats_continued_comments_as_one_opening() {
        validate(&design(
            "lib.rs",
            "/// Get the settings.\n/// Returns are validated.\npub fn standard();\n",
            None,
        ))
        .unwrap();
        assert!(
            validate(&design(
                "lib.rs",
                "///\n/// Returns the settings.\npub fn standard();\n",
                None
            ))
            .is_err()
        );
        assert!(
            validate(&design(
                "lib.rs",
                "/// Get the settings.\n\n/// Returns the settings.\npub fn standard();\n",
                None
            ))
            .is_err()
        );
    }

    #[test]
    fn supports_native_comment_forms_and_case_insensitive_word_boundaries() {
        for (path, text) in [
            ("lib.rs", "//! Returns settings.\npub fn standard();\n"),
            ("lib.rs", "/* Returns settings. */\npub fn standard();\n"),
            (
                "lib.rs",
                "/**\n * Returns settings.\n */\npub fn standard();\n",
            ),
            (
                "lib.ts",
                "/** Returns settings. */\nexport function standard(): void;\n",
            ),
            (
                "lib.tsx",
                "// returns settings.\nexport function standard(): void;\n",
            ),
            ("lib.lua", "--- Returns settings.\nfunction standard()\n"),
            (
                "lib.lua",
                "--[=[ Returns settings. ]=]\nfunction standard()\n",
            ),
            ("settings.toml", "# Returns settings.\nvalue = 1\n"),
            ("settings.yaml", "# Returns settings.\nvalue: 1\n"),
            (
                "settings.jsonc",
                "{\n// Returns settings.\n\"value\": 1\n}\n",
            ),
            (
                "App.csproj",
                "<Project>\n<!-- Returns settings. -->\n</Project>\n",
            ),
        ] {
            assert!(
                validate(&design(path, text, None)).is_err(),
                "{path}: {text}"
            );
        }
    }

    #[test]
    fn ignores_comment_markers_inside_literals_and_change_metadata() {
        validate(&design(
            "lib.rs",
            "pub const TEXT: &str = r#\"/// Returns settings.\"#;\n",
            None,
        ))
        .unwrap();
        validate(&design(
            "lib.ts",
            "export const text = `// Returns settings.`;\n",
            None,
        ))
        .unwrap();
        validate(&design(
            "lib.lua",
            "text = [[-- Returns settings.]]\n",
            None,
        ))
        .unwrap();
        validate(&design(
            "App.csproj",
            "<Project><![CDATA[<!-- Returns settings. -->]]></Project>\n",
            None,
        ))
        .unwrap();
        validate(&design(
            "settings.yaml",
            "value: |\n  # Returns settings.\n",
            None,
        ))
        .unwrap();
        let mut design = design("lib.rs", "pub fn standard();\n", None);
        design.proposed_calls.insert(
            "lib.rs".into(),
            vec![crate::plan::FunctionBody {
                owner: "standard".into(),
                change: Some("Returns settings without a network request.".into()),
                call: None,
            }],
        );
        validate(&design).unwrap();
    }

    #[test]
    fn preserves_baseline_comments_and_detects_edits_and_added_copies() {
        let baseline = "/// Returns the settings.\npub fn standard();\n";
        validate(&design(
            "lib.rs",
            "/// Returns the\n/// settings.\npub fn standard() -> u32;\n",
            Some(baseline),
        ))
        .unwrap();
        assert!(
            validate(&design(
                "lib.rs",
                "/// Returns different settings.\npub fn standard();\n",
                Some(baseline)
            ))
            .is_err()
        );
        let proposed = format!("{baseline}\n/// Returns the settings.\npub fn extra();\n");
        let error = validate(&design("lib.rs", &proposed, Some(baseline))).unwrap_err();
        assert_eq!(
            error
                .downcast_ref::<DeclarationValidationError>()
                .unwrap()
                .diagnostic
                .len(),
            1
        );
    }

    #[test]
    fn preserves_comments_in_moved_files() {
        let source = "/// Returns the settings.\npub fn standard();\n";
        let mut design = design("old.rs", source, Some(source));
        design.proposed.remove("old.rs");
        design.proposed.insert("new.rs".into(), source.into());
        design.moved.insert("old.rs".into(), "new.rs".into());
        validate(&design).unwrap();
    }

    #[tokio::test]
    async fn rejects_before_preparing_dependency_sources() {
        let temporary = tempfile::tempdir().unwrap();
        let error = design(
            "lib.rs",
            "/// Returns settings.\npub fn standard() -> unavailable::Settings;\n",
            None,
        )
        .validated(temporary.path())
        .await
        .unwrap_err();
        assert!(error.downcast_ref::<DeclarationValidationError>().is_some());
    }

    #[test]
    fn submission_checks_lint_even_with_cached_validation_and_preserves_editable_draft() {
        let temporary = tempfile::tempdir().unwrap();
        let store =
            crate::plan::PlanFileStore::new(temporary.path().join("data"), temporary.path());
        let mut document = crate::plan::document::test_fixture("lint", "Introduce settings");
        let proposed = concat!(
            "/// Get settings that define the finite arena, initial player position, and playable round duration for the starter game.\n",
            "pub fn existing();\n\n",
            "/// Returns settings.\npub fn standard();\n",
        );
        let mut design = design("lib.rs", proposed, None);
        design.document.objective = "Introduce settings".into();
        design.document.background = "The fixture contains the declarations under review.".into();
        design.document.requirements = vec!["Preserve the declared behavior and ownership.".into()];
        design.document.design = "Provide default settings".into();
        design.validation = Some(crate::declaration::DeclarationValidation {
            fingerprint: crate::declaration::fingerprint(&design),
            checked: 0,
            diagnostic: Vec::new(),
        });
        document.design = Some(design);
        store
            .write_working_document("session", "lint", &document)
            .unwrap();
        let working = store.plan_dir("session", "lint").join("working.json");
        let saved = std::fs::read(&working).unwrap();
        let error = store
            .submit_validated_document_revision(
                "session",
                "lint",
                1,
                document.version,
                document.clone(),
            )
            .unwrap_err();
        assert_eq!(
            error
                .downcast_ref::<DeclarationValidationError>()
                .unwrap()
                .diagnostic[0]
                .line,
            4
        );
        assert_eq!(std::fs::read(&working).unwrap(), saved);
        assert!(
            !store
                .plan_dir("session", "lint")
                .join("revisions/submitted-0001.json")
                .exists()
        );
        let design = document.design.as_mut().unwrap();
        design
            .proposed
            .insert("lib.rs".into(), proposed.replace("/// Returns", "/// Get"));
        design.validation = None;
        store
            .write_working_document("session", "lint", &document)
            .unwrap();
        store
            .submit_document_revision("session", "lint", 1, document.version)
            .unwrap();
        assert!(
            store
                .plan_dir("session", "lint")
                .join("revisions/submitted-0001.json")
                .exists()
        );
    }
}

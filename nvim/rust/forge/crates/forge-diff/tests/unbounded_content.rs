use forge_diff::{
    edit::{EditDirection, EditSelection},
    patch::UnifiedPatch,
    raw::compute_hunks,
    source::{Representation, SourcePair, SourceVersion},
    syntax::{DeclarationCalls, DeclarationIndex, DeclarationOverview},
};

#[test]
fn declaration_owners_accept_complete_source_beyond_former_byte_limits() {
    let source = format!(
        "{}fn final_declaration() {{}}\n",
        " ".repeat(8 * 1024 * 1024 + 1)
    );
    let overview = DeclarationOverview::extract("large.rs", &source).unwrap();
    assert!(overview.contains("final_declaration"));
    DeclarationOverview::parse(
        "large.rs",
        &format!("{}\n{overview}", " ".repeat(1024 * 1024)),
    )
    .unwrap();
    let (overview, callable) =
        DeclarationOverview::extract_with_calls("large.rs", &source).unwrap();
    assert!(overview.contains("final_declaration"));
    assert!(
        callable
            .iter()
            .any(|function| function.owner == "final_declaration")
    );
    let index = DeclarationIndex::extract("large.rs", &source).unwrap();
    assert!(
        index
            .symbol
            .iter()
            .any(|symbol| symbol.name == "final_declaration")
    );
    assert!(
        !DeclarationCalls::extract("large.rs", &source, false)
            .unwrap()
            .is_empty()
    );
}

#[test]
fn declaration_comments_preserve_occurrences_beyond_former_total() {
    let source = "// retained comment\n".repeat(65_537);
    let comments = DeclarationOverview::comments("large.rs", &source).unwrap();
    assert_eq!(comments.len(), 65_537);
    assert_eq!(comments.last().unwrap().position.line, 65_537);
    assert_eq!(comments.last().unwrap().text, "// retained comment");
}

#[test]
fn declaration_index_preserves_symbols_beyond_former_total() {
    let source = (0..65_537)
        .map(|index| format!("const entry_{index}: usize = 0;\n"))
        .collect::<String>();
    let index = DeclarationIndex::extract("large.rs", &source).unwrap();
    assert_eq!(
        index
            .symbol
            .iter()
            .filter(|symbol| symbol.name.starts_with("entry_"))
            .count(),
        65_537
    );
    assert!(
        index
            .symbol
            .iter()
            .any(|symbol| symbol.name == "entry_65536")
    );
}

#[test]
fn saved_patch_paths_preserve_text_beyond_former_byte_limit() {
    let path = "directory/".repeat(2000);
    let source = format!("--- a/{path}\n+++ b/{path}\n@@ -1 +1 @@\n-old\n+new\n");
    let patch = UnifiedPatch::parse(&source).unwrap();
    assert_eq!(patch.file[0].old_path.as_deref(), Some(path.as_str()));
    assert_eq!(patch.file[0].new_path.as_deref(), Some(path.as_str()));
}

#[test]
fn selected_edits_preserve_output_beyond_former_source_limit() {
    let bytes = b"retained\n".repeat(1_000_000);
    let diff = compute_hunks(SourcePair {
        old: SourceVersion::new(Vec::new(), Representation::Raw).unwrap(),
        new: SourceVersion::new(bytes.clone(), Representation::Raw).unwrap(),
    });
    let selected = diff.hunks().iter().map(|hunk| hunk.id).collect::<Vec<_>>();
    let applied = EditSelection::capture(&diff, &selected, EditDirection::Forward)
        .unwrap()
        .apply()
        .unwrap();
    assert_eq!(applied.after().bytes(), bytes);
}

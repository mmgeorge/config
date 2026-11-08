#![cfg(windows)]

use std::path::PathBuf;

use anyhow::Result;

#[test]
#[ignore = "runs in isolated child processes to reset gix's prefix cache"]
fn fresh_host() -> Result<()> {
    let cache = PathBuf::from(std::env::var_os("FORGE_CACHE_TEST_PATH").expect("child cache"));
    let repository =
        PathBuf::from(std::env::var_os("FORGE_CACHE_TEST_REPOSITORY").expect("child repository"));
    let expected = std::env::var("FORGE_CACHE_TEST_VALUE")?;
    assert_eq!(forge_git::config_location::configure(&cache)?, "hit");
    let local = gix::discover(repository)?;
    assert_eq!(
        local
            .config_snapshot()
            .string("forgecachetest.value")
            .expect("system config")
            .to_string(),
        expected
    );
    let index = local.index_or_empty()?;
    let mut attributes = local.attributes_only(
        &index,
        gix::worktree::stack::state::attributes::Source::WorktreeThenIdMapping,
    )?;
    let mut selected = gix::attrs::search::Outcome::default();
    selected.initialize_with_selection(&Default::default(), ["forgecachetest"]);
    attributes
        .at_path("file.txt", None)?
        .matching_attributes(&mut selected);
    let actual = selected.iter_selected().next().expect("selected attribute");
    assert_eq!(
        actual.assignment.state,
        gix::attrs::StateRef::Value(expected.as_str().into())
    );
    Ok(())
}

#![cfg(windows)]

use std::{path::PathBuf, process::Command};

use anyhow::Result;

#[test]
fn fresh_hosts_read_live_system_configuration_and_attributes() -> Result<()> {
    let directory = tempfile::tempdir()?;
    let prefix = directory.path().join("installation");
    std::fs::create_dir_all(prefix.join("etc"))?;
    let repository = directory.path().join("repository");
    gix::init(&repository)?;
    let cache = directory.path().join("location.json");
    std::fs::write(
        &cache,
        serde_json::to_vec(&serde_json::json!({
            "version": 1, "system_prefix": prefix,
        }))?,
    )?;
    for value in ["first", "edited"] {
        std::fs::write(
            prefix.join("etc/gitconfig"),
            format!("[forgecachetest]\n value = {value}\n"),
        )?;
        std::fs::write(
            prefix.join("etc/gitattributes"),
            format!("*.txt forgecachetest={value}\n"),
        )?;
        let mut command = Command::new(std::env::current_exe()?);
        command
            .args(["--ignored", "--exact", "fresh_host", "--nocapture"])
            .env("FORGE_CACHE_TEST_PATH", &cache)
            .env("FORGE_CACHE_TEST_REPOSITORY", &repository)
            .env("FORGE_CACHE_TEST_VALUE", value);
        for name in [
            "EXEPATH",
            "GIT_EXEC_PATH",
            "GIT_CONFIG_SYSTEM",
            "GIT_CONFIG_NOSYSTEM",
            "GIT_ATTR_NOSYSTEM",
            "GIT_CONFIG",
            "GIT_CONFIG_COUNT",
        ] {
            command.env_remove(name);
        }
        let output = command.output()?;
        assert!(
            output.status.success(),
            "{}\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }
    Ok(())
}

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

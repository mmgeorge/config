use forge_git::command as crate_command;

pub fn git(root: &std::path::Path, arguments: &[&str]) -> Vec<u8> {
    let output = crate_command::read_command(
        std::process::Command::new("git")
            .arg("--no-optional-locks")
            .arg("-C")
            .arg(root)
            .args(["-c", "core.fsmonitor=false"])
            .args(arguments),
        crate_command::CommandLimits {
            stdout_bytes: 4096,
            stderr_bytes: 4096,
            timeout: std::time::Duration::from_secs(10),
        },
        || Ok(()),
    )
    .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    output.stdout
}

use super::*;

pub(super) fn discard(
    intent: &GitWriteIntent,
    staged: &PatchTarget,
    unstaged: &PatchTarget,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<std::process::Output> {
    let presence = intent
        .precondition()
        .patch_presence(&unstaged.path, PatchDirection::Discard);
    let unstaged_patch = encode_patch(unstaged, presence.0, presence.1)?;
    let presence = intent
        .precondition()
        .patch_presence(&staged.path, PatchDirection::DiscardStaged);
    let staged_patch = encode_patch(staged, presence.0, presence.1)?;
    for (patch, cached) in [(&unstaged_patch, false), (&staged_patch, true)] {
        let output = apply(intent, patch, cached, true, check)?;
        if !output.status.success() {
            return Ok(output);
        }
    }
    let mut collected = apply(intent, &unstaged_patch, false, false, check)?;
    if !collected.status.success() {
        return Ok(collected);
    }
    let staged_output = discard_staged_patch(intent, staged, check)
        .context("selected unstaged hunks were discarded before staged discard failed")?;
    if !staged_output.status.success() {
        let mut diagnostic =
            b"Selected unstaged hunks were discarded before staged discard failed.\n".to_vec();
        diagnostic.extend(collected.stderr);
        collected.stderr = diagnostic;
    }
    collected.status = staged_output.status;
    collected.stdout.extend(staged_output.stdout);
    collected.stderr.extend(staged_output.stderr);
    collected.stdout.truncate(64 * 1024);
    collected.stderr.truncate(64 * 1024);
    Ok(collected)
}

fn apply(
    intent: &GitWriteIntent,
    patch: &[u8],
    cached: bool,
    verify: bool,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<std::process::Output> {
    check()?;
    let mut command = git_command(&intent.repository)?;
    command.args([
        "apply",
        "--reverse",
        "--unidiff-zero",
        "--whitespace=nowarn",
    ]);
    if cached {
        command.arg("--cached");
    }
    if verify {
        command.arg("--check");
    }
    command.arg("-");
    progress_command(
        &mut command,
        CommandLimits {
            stdout_bytes: 64 * 1024,
            stderr_bytes: 64 * 1024,
            timeout: Duration::from_secs(120),
        },
        Some(patch),
        intent.progress.as_ref(),
        check,
    )
}

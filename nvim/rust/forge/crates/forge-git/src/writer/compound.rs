use super::*;

pub(super) fn discard(
    intent: &WriteExecution<'_>,
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
    for (patch, step) in [
        (&unstaged_patch, ReversePatchStep::CheckWorktree),
        (&staged_patch, ReversePatchStep::CheckIndex),
    ] {
        let output = run_reverse_patch(intent, patch, step, 64 * 1024, check)?;
        if !output.status.success() {
            return Ok(output);
        }
    }
    let mut collected = run_reverse_patch(
        intent,
        &unstaged_patch,
        ReversePatchStep::ApplyWorktree,
        64 * 1024,
        check,
    )?;
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

# Plan creation speedups

This list comes from the test-bevy planning run `4e76e3df` (state `v15`, plan `e999223e`), which used the prompt `/plan out extensive bevy game in this repo. do not ask questions. 8 components and 4 systems.` The turn took 219 s. Tools ran for about 31 s of it, and the remaining 188 s went to model generation between calls. The largest savings therefore come from removing round trips and from avoiding output that the agent has to write twice, not from faster tools.

Offsets below are seconds from turn start. Savings are estimates from this one run.

## 1. Replace an artifact with Delete File plus Add File

At +44 s the agent sent one patch with `*** Delete File: plan:overview.md` followed by `*** Add File: plan:overview.md` and the complete overview. `DeclarationDesign::patch` rejected it with `plan:overview.md cannot be deleted; use *** Revert File to reset it` (`plan/design.rs:804`). The agent then spent 24.7 s generating the same content again as an `Update File` patch, which was accepted at +69 s.

The `plan_edit` description already says that Add File on an artifact replaces its whole text. Treat a Delete File followed by an Add File of the same authored artifact in one patch as that replacement. At minimum, change the error to say that Add File alone replaces the artifact. Estimated saving: about 25 s.

## 2. Make patch context matching tolerant of whitespace

Two edits failed on context lines. At +107.7 s, a `src/main.rs` patch that took 31.5 s to generate failed with `chunk 6 does not match: its line 11 Accesses does not follow the line before it`, and the agent resent it. At +154 s, another patch failed with `its first line ... is not in the file`, which cost two `plan_read` range calls and a retry.

Codex's `apply_patch` retries a failed match while ignoring trailing whitespace, then while ignoring leading and trailing whitespace. Check whether the plan patch matcher has any such fallback, and add one if it does not. Estimated saving: about 15 s, plus fewer re-reads.

## 3. Ask for one patch per file

Between +116 s and +166 s the agent made six sequential `plan_edit` calls to `src/main.rs`, and each one waited 5–9 s for the next model call. Add guidance to the planning prompt or the `plan_edit` description: combine all changes to one file in one patch. A larger patch is more likely to hit a context mismatch, so this pays off most after item 2. Estimated saving: up to about 25 s.

## 4. Put the pseudocode rules in the prompt

The `plan_edit` description tells the agent to load the `academic-pseudocode` skill before writing an Algorithm. At +19.6 s the agent spent a 7.2 s turn plus a 0.7 s shell read loading `SKILL.md`. Moving the rules the Algorithm grammar needs into `plan/prompts/planning.md`, or attaching the skill to planning turns, removes that round trip. Estimated saving: about 8 s.

## 5. Cache baseline classification for workspace reads

Reading an uncaptured workspace file took 1.3 s (`src/main.rs` at +11 s and +74.8 s, `Cargo.toml` at +11 s). In the earlier `v13` runs, `plan_read` averaged about 0.6 s. `DeclarationDesign::capture_baseline` (`plan/design.rs:339`) caches extraction in `SOURCE_OVERVIEW_CACHE`. On every call, though, it still clones the design, builds a `DeclarationResolver::planned`, and runs `calls::classify` (`plan/design.rs:400–421`).

Cache the classified calls as well. The cache key must cover every input that classification reads, which may include other workspace files, not only this file's digest. Estimated saving: 2–3 s per run.

## 6. Check whether reads block behind submit

At +190.8 s the agent read `plan:checks.md` and `src/main.rs` at the same time as `plan_submit`. All three calls took 3.2–3.3 s, while the same reads elsewhere took 0.1–1.3 s. That pattern suggests the reads waited on the same lock as the submission. This is unconfirmed: trace which lock or queue the plan read path shares with `plan_submit` before changing anything. Estimated saving: about 3 s when the agent overlaps reads with submission.

## 7. Profile the accepted submission

The rejected submission took 3.2 s, and the accepted one took 8.6 s, the longest single tool call in the run. The accepted path also runs resolver validation, writes the revision, and renders the review. Profile it to find which step dominates. The saving is unknown until the profile exists.

## Priority

Items 1, 3, and 4 are small changes to patch handling and prompt text, and together they save an estimated 55–60 s, about a quarter of this run. Item 2 is the next most valuable, and it also makes item 3 safer. Items 5–7 save seconds of tool time and matter less than the model round trips above.

# Harness-Owned Agents

Forge owns child admission, identity, task queues, cancellation, and result delivery across Codex,
Copilot, Claude ACP, and Antigravity ACP. The accepted design is [plans/subagent.md](../../../../plans/subagent.md).
Provider-native orchestration cannot serve as a fallback.

## Ownership contract

Each child starts with a fresh provider conversation in the root workspace. `ExecutionOwner`
contains the Harness session and agent identifiers. Provider maps and approval requests use that
owner, while resources and settings use the root session's portable preference scope. Follow-ups
reuse only the child's native history. Children have no root planning or delegation authority.

Admission reserves capacity before one transaction writes the agent, parent delegation, queued
exchange, and task. The root admits at most eight active children. A child serializes follow-ups
and retains at most 32 pending tasks. Informational messages persist without starting idle work.
The active root prompt and idle session controller drain one bounded coordinator request channel.
Model callbacks never reacquire the active prompt's broker lock.

## Public controls

`/agents spawn` selects an enabled global profile and accepts task text. The profile owns provider, model, thinking, and speed. `/agents` exposes Main,
Running, and Done timelines, informational messaging, follow-ups, interruption, and the root tool
toggle. `subagent_tools` defaults off and persists through Forge's portable settings. Manual
spawning remains available with the toggle off.

The enabled root receives six tools:

- `agent_spawn` admits a fresh child task.
- `agent_send` retains information and steers an active child when supported.
- `agent_followup` queues another task on the child's own history.
- `agent_interrupt` cancels active and queued tasks without deleting history.
- `agent_list` returns owned identities and their latest tasks.
- `harness_wait_agent` waits for child activity and returns unreported completed tasks.

## Provider and failure boundaries

Codex sets `agents.enabled=false`. Copilot excludes builtin-qualified native delegation tools.
Claude ACP disallows `Agent` and `Task`. Antigravity ACP does not expose suppression, and the
capability menu reports that limitation. Native lifecycle events cannot create managed children.

A child cannot weaken the root's required sandbox policy. Cleanup failure retains execution
ownership and its capacity slot for retry. Restart marks unfinished tasks interrupted and does
not replay queued tasks. Session format compatibility is independent of preference compatibility,
and hidden incompatible sessions remain stored.

## Verification evidence

`agent/coordinator_test.rs` covers capacity reservations, duplicate admissions, foreign targets,
queue limits, interruption, and request delivery. `broker/agent_test.rs` exercises provider routing
and durable operation receipts through the real broker composition. Broker workflow tests cover
manual follow-ups and automatic root continuation. Approval tests isolate siblings with identical
provider request identifiers. Storage tests check atomic admission and restart recovery.

`agent_picker.lua` checks identity-preserving live selection and the generic spawn composer.
`harness_owned_agents_real.lua` launches a fresh rebuilt host in a temporary workspace and checks
manual spawning, idle inbox retention, follow-up history, and optional model delegation. Pass an
explicit rebuilt binary and configured provider launch descriptors when running outside the
normal Neovim cache environment. Real-provider authentication failures remain separate from
automated passes.

### Recorded execution on 2026-10-11

Every real run used a fresh Neovim host and temporary workspace. Claude used the installed
Claude ACP adapter with refreshed OAuth. Antigravity used the installed ACP server with OAuth.
Codex used `codex app-server`, and Copilot used the embedded Rust SDK with its installed CLI.
The prompts requested bounded responses without file edits. Tests explicitly disabled the OS
sandbox before admitting ACP children, preserving the production policy rejection boundary.

| Root | Child | Model selection | Recorded result |
| --- | --- | --- | --- |
| Claude | Claude | Haiku | Manual spawn, idle inbox, retained follow-up, and model delegation passed. |
| Copilot | Copilot | Haiku | Manual spawn, idle inbox, retained follow-up, and model delegation passed. |
| Codex | Codex | Advertised default `gpt-6.1-sol` | Manual spawn, idle inbox, retained follow-up, and model delegation passed. |
| Antigravity | Antigravity | Flash | Manual workflow and model delegation passed in separate fresh runs. |
| Codex | Claude | Default / Haiku | Manual workflow and model delegation passed. |
| Claude | Antigravity | Haiku / Flash | Manual workflow and model delegation passed. |
| Antigravity | Copilot | Flash / Haiku | Manual workflow and model delegation passed. |
| Copilot | Codex | Haiku / default | Manual workflow, model delegation, two simultaneous children, individual interruption, interrupt-all, a visibility toggle during execution, and reopening cancelled outcomes passed. |

Antigravity initially supplied invalid spawn and wait fields. The private ACP control server
returned structured field diagnostics, and the root corrected its calls and completed the task.
These recoverable errors remained visible on the timeline. The tests did not add aliases for
invalid fields or weaken the six public schemas.

The fixture bounds host startup at 30 seconds, requests and child tasks at 90 seconds, control
settlement at 45 seconds, and shutdown and view updates at 10 seconds. Those limits apply to
each operation. Runs contain multiple bounded operations.

The final automated command uses the actual release target directory:

```text
cargo test --manifest-path Cargo.toml --target-dir D:/.cache/nvim/rust-sidecar/forge/build --release --locked -p forge-harness --lib
```

The command ran in `D:/config/nvim/rust/forge` with a 300-second compilation bound and passed
873 tests, failed zero tests, and retained 10 ignored tests. Test execution took 15.87 seconds.
The targeted `child` filter passed 35 tests in 4.73 seconds, including all 16 provider pairings,
replayed child snapshots, late failed completion during a newer follow-up, persisted provider
tuning, and heading updates without replacing the latest thought.
The backend filter passed 157 tests with seven ignored in 4.12 seconds after removing obsolete
native child-control entry points. The managed replay test additionally verifies that rejected
steering retains its informational message rather than dropping it.

Neovim checks used the installed editor and this command shape with a 30-second bound per test:

```text
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l D:/config/nvim/tests/forge/agent_picker.lua
```

`agent_picker.lua`, `harness_completion.lua`, `forge_completion.lua`,
`harness_controller_native.lua`, `harness_settings.lua`, and `harness_timeline_position.lua`
passed. The controller test's `simulated provider failure` is its expected error-path fixture.
Automated renderer assertions do not replace the interactive terminal inspection.

The final release command ran in the same Cargo directory with a 300-second bound:

```text
cargo build --manifest-path Cargo.toml --target-dir D:/.cache/nvim/rust-sidecar/forge/build --profile release --bin forge --locked --timings
```

The build exited zero in 36.66 seconds with no warnings. The rebuilt executable is
`D:/.cache/nvim/rust-sidecar/forge/build/release/forge.exe`. Its timing artifact is
`D:/.cache/nvim/rust-sidecar/forge/build/cargo-timings/cargo-timing-20261011T101515.8753266Z.html`.

The rebuilt binary passed a final Copilot-to-Codex run through Terminal MCP:

```text
nvim --headless -u NONE -l C:/Users/mattm/AppData/Local/Temp/forge-owned-agents-copilot-codex.lua
```

The wrapper selected Haiku for Copilot, the advertised default model for Codex, and enabled the
fixture's model-tool and control checks. It emitted `HARNESS_AGENTS_MANUAL_PASS`,
`HARNESS_AGENTS_MODEL_PASS`, `HARNESS_AGENTS_CONTROLS_PASS`, and
`HARNESS_AGENTS_RECOVERY_PASS`, then exited zero. The manual child identity was
`06fe65a2-563f-4fc5-a11c-537c4132a29f`.

Interactive Terminal MCP inspection used fresh Claude-to-Claude Haiku hosts with the same
binary. The model run displayed both completed children in the open picker and the root synthesis.
The manual run left the model tools off and reported the fixture result `pass`. The rendered
Main view showed `● Agent fresh-child · claude · haiku` headings and `○` thought previews.
Selecting the child in the picker opened both its original result and retained inbox follow-up.
Pressing Enter on the first Main agent heading also opened that child's full timeline and
selected `523632c7-a073-45d1-8296-b67caa609d32`. Terminal screenshots and the selected identity
confirmed navigation and attribution independently of headless renderer assertions.

Interactive wrappers retain the successful view for 90 seconds and then collect the host.
The temporary wrapper paths describe this recorded run. The reusable fixture remains
`nvim/tests/forge/harness_owned_agents_real.lua`, configured through its documented test globals.

### Sanity-check fixes verified on 2026-10-11

Child approval requests, resolution, and cancellation now pass through registered backend events
to the shared permission popup. The popup labels the requesting child and provider. Its interrupt
action uses the request owner even when a different timeline is selected. Child failure and
interruption cancel only that owner's approvals. The service regression exercises a real mock
runtime, resolves an approval through the control route, observes its wire event, and clears the
running child before releasing execution ownership.

Live picker summaries use registered `agent_updated` events and retain the running exchange when
follow-ups are queued. Explicit waits observe manual-child activity and return completed results.
Interrupted children retain their provider conversation identity. Spawn and follow-up reject
providers unable to enforce the root sandbox. Clear, rollback, phase completion, and automatic
verification respect active child ownership. Changing only agent-tool visibility resumes the same
root exchange, preserving its child-result association.

The following commands ran from `D:/config/nvim/rust/forge` with a 300-second compilation bound:

```text
cargo test --release --locked --manifest-path Cargo.toml --target-dir D:/.cache/nvim/rust-sidecar/forge/build -p forge-harness --lib child_
cargo test --release --locked --manifest-path Cargo.toml --target-dir D:/.cache/nvim/rust-sidecar/forge/build -p forge-harness --lib
cargo build --manifest-path Cargo.toml --target-dir D:/.cache/nvim/rust-sidecar/forge/build --profile release --bin forge --locked --timings
```

All exited zero. The child filter passed 36 tests in 5.35 seconds. The library suite passed 881
tests with 10 ignored in 17.27 seconds. The release build took 71 seconds and compiled `redb`,
`forge-status`, `forge-harness`, `forge-github`, `forge-review`, and `forge`. Its artifact is
`D:/.cache/nvim/rust-sidecar/forge/build/release/forge.exe`. Its timing report is
`D:/.cache/nvim/rust-sidecar/forge/build/cargo-timings/cargo-timing-20261011T172138.2764538Z.html`.

The Neovim `agent_picker`, `harness_controller_native`, `harness_completion`, `harness_settings`,
and `harness_timeline_position` fixtures each exited zero under a 30-second bound. The controller
fixture's `simulated provider failure` remains expected error-path output. `git diff --check`
exited zero with line-ending conversion warnings only.

Fresh runtime hosts used the rebuilt executable. The fixture now waits on subscribed live state
for child transitions and reads a snapshot only after the transition, preventing snapshot polling
from concealing missing events. These commands ran from `D:/config` under a 120-second bound:

```text
nvim --headless -u NONE -l D:/config/nvim/tests/forge/harness_owned_agents_real.lua
nvim --headless -u NONE --cmd "lua vim.g.forge_agent_test_manual = false; vim.g.forge_agent_test_controls = true; vim.g.forge_agent_test_launch = {mock={command={'visible-blocking'},selectable=true}}" -l D:/config/nvim/tests/forge/harness_owned_agents_real.lua
nvim --headless -u NONE -l C:/Users/mattm/AppData/Local/Temp/forge-owned-agents-copilot-codex.lua
```

All exited zero. Mock runs passed manual spawn, inbox delivery, follow-up, concurrent execution,
individual interruption, interrupt-all, and persisted recovery. The real Copilot-to-Codex run
emitted `HARNESS_AGENTS_MANUAL_PASS`, `HARNESS_AGENTS_MODEL_PASS`,
`HARNESS_AGENTS_CONTROLS_PASS`, and `HARNESS_AGENTS_RECOVERY_PASS`. Its manual child ID was
`e7160b30-18bf-471f-90c8-31dca51a8e02`. Approval delivery and popup ownership were verified by
the service and Neovim regression fixtures. This follow-up did not repeat interactive terminal
inspection or every provider pairing against live services.

### Global subagent profiles verified on 2026-10-11

The profile catalog is global at `stdpath("config")/forge/subagents.json`. Unit checks cover
revision-checked saves, external edits, invalid-file retention, enabled profiles, captured tuning
and instructions, permission narrowing, and provider compatibility. The picker checks cover
field edits, profile creation and deletion, default selection, and late callbacks. Spawn and
completion checks use the profile-and-text contract.

The final library command ran from `D:/config/nvim/rust/forge` with a 300-second bound:

```text
cargo test --release --locked --manifest-path Cargo.toml --target-dir D:/.cache/nvim/rust-sidecar/forge/build -p forge-harness --lib --quiet -- --test-threads=4
```

It exited zero with 884 passed and 10 ignored in 28.66 seconds of test execution. An earlier
unrestricted run hit the existing one-millisecond timeout assertion in
`backend::events::tests::cancelled_receive_retains_partial_batch_for_final_drain` under parallel
load. The final run limited concurrency to four test threads.

The `agent_picker`, `subagent_profiles`, `harness_completion`, `harness_controller_native`,
`harness_settings`, and `harness_timeline_position` Neovim fixtures each exited zero with a
30-second bound using this command shape:

```text
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/subagent_profiles.lua
```

The controller fixture's simulated provider failure is expected error-path output.

The final executable build used a 300-second bound and exited zero in 19.88 seconds:

```text
cargo build --manifest-path Cargo.toml --target-dir D:/.cache/nvim/rust-sidecar/forge/build --profile release --bin forge --locked --timings
```

The rebuilt artifact is `D:/.cache/nvim/rust-sidecar/forge/build/release/forge.exe`. The timing report
is `D:/.cache/nvim/rust-sidecar/forge/build/cargo-timings/cargo-timing-20261011T174723.0218254Z.html`.

The integration-test compilation also exited zero in 14.83 seconds with a 300-second bound:

```text
cargo test --release --locked --manifest-path Cargo.toml --target-dir D:/.cache/nvim/rust-sidecar/forge/build -p forge-harness --tests --no-run
```

This compiled the library tests and the `codex_cli`, `codex_real`, `copilot_real`,
`permission_document`, and `trace_clear` integration binaries without running those binaries.

Fresh hosts ran the following commands from `D:/config`, each with a 120-second bound:

```text
nvim --headless -u NONE --cmd "lua vim.g.forge_agent_test_settings = true" -l D:/config/nvim/tests/forge/harness_owned_agents_real.lua
nvim --headless -u NONE --cmd "lua vim.g.forge_agent_test_manual = false; vim.g.forge_agent_test_controls = true; vim.g.forge_agent_test_launch = {mock={command={'visible-blocking'},selectable=true}}" -l D:/config/nvim/tests/forge/harness_owned_agents_real.lua
nvim --headless -u NONE -l C:/Users/mattm/AppData/Local/Temp/forge-owned-agents-copilot-codex.lua
```

All three exited zero. The mock fixtures passed settings, manual spawn, captured profile behavior
after edits, follow-up, cancellation, and recovery. The real Copilot-to-Codex fixture emitted
`HARNESS_AGENTS_MANUAL_PASS`, `HARNESS_AGENTS_MODEL_PASS`, `HARNESS_AGENTS_CONTROLS_PASS`, and
`HARNESS_AGENTS_RECOVERY_PASS`. Its manual child ID was `09cd0f11-4713-4485-bda4-dd3ff0a85559`.
It added a profile after the parent conversation started, then spawned that profile through the
refreshed tool schema. The first live attempt exposed an SDK session-drop ordering bug that
removed the replacement Copilot event router. Releasing the old session before resume corrected
the handoff, and the final run completed in approximately 33 seconds.

The fixtures isolate both configuration and session data in temporary directories. These checks
do not replace interactive visual inspection, and this change did not repeat every provider
pairing against live services. Session format 42 captures the profile fields. Older incompatible
session records remain on disk but are hidden from the session list.

### Full-page profile editing and grouped commands verified on 2026-10-11

`/agents edit [profile]` replaces the profile entry in `/config`. The profile list opens a
full-page form with native editable text, protected labels, inline Left/Right choices, and one
revision-checked save. `/agents spawn [profile] [text]` replaces the standalone `/spawn` command.
The existing `/agents` list and `/agents <name>` navigation remain available.

The following checks ran from `D:/config`, each with a 30-second bound and exit status zero:

```text
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/subagent_profiles.lua
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/harness_completion.lua
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/harness_controller_native.lua
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/harness_settings.lua
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/review_editing.lua
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/editable_native.lua
```

Each completed in under one second. Checks cover grouped command routing, profile completion,
removal from `/config`, multiline instructions, one-save submission, renaming, asynchronous save
failure, retention of newer edits, reload, return navigation, and late model responses. The PR
editing fixture verifies that existing native editing and save behavior still work.

LuaLS ran from `D:/config/nvim` with a 30-second bound and exited zero in 7.51 seconds:

```text
lua-language-server --check D:/config/nvim/lua/forge --configpath D:/config/nvim/.luarc.json --logpath C:/Users/mattm/AppData/Local/Temp/forge-subagent-form-luals-verified
```

The report contains 204 diagnostics elsewhere in the plugin and none in the new form, profile
editor, command completion, shared command specification, or configuration modules. The
controller and settings modules retain existing diagnostics outside the changed behavior.

Interactive Terminal MCP inspection used a dedicated 120-column, 40-row terminal with:

```text
nvim -u NONE --cmd "set rtp+=D:/config/nvim" --cmd "lua vim.g.forge_profile_form_interactive = true" -c "luafile D:/config/nvim/tests/forge/subagent_profiles.lua"
```

The form occupied one full-page window in its own tab. Moving to Provider and pressing Right
displayed the existing arrow control and a dirty marker. Native insertion edited Instructions
in place. Ctrl-S cleared the dirty markers, and q returned to profile selection. This fixture
uses a mocked profile service and does not write the user's global catalog. No Rust source or
compiled input changed in this follow-up, so the existing rebuilt host remains the runtime artifact.

### Profile picker aliases and Enable toggles verified on 2026-10-11

`/agents edit [profile]` provides profile editing and completion. The picker
includes disabled profiles. Left/Right toggles Enable through an immediate revision-checked
save and updates the existing picker without changing its selected profile. The default profile
must remain enabled. Failed saves preserve the displayed state, and pending saves prevent another
mutation, reload, or form entry.

The following commands exited zero under the 30-second bound:

```text
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/subagent_profiles.lua
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/harness_controller_native.lua
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/harness_completion.lua
git diff --check
```

They completed in 0.38, 0.49, 0.36, and 0.37 seconds respectively. Profile checks use the real
picker's Left/Right key mappings against a mocked service, covering successful toggles, retained
selection, the default-profile rejection, stale revisions, and in-flight operation guards. The
controller fixture printed its expected `simulated provider failure` diagnostic. Diff checking
reported only existing CRLF-to-LF conversion warnings. This follow-up did not repeat interactive
visual inspection or change Rust source.

### Open profile configuration verified on 2026-10-11

The edit picker contains only profiles. `oc` closes the picker and opens the global
catalog in a normal editable tab. Pending saves block the hotkey.

`nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/subagent_profiles.lua`
exited zero. The test exercises the real `oc` mapping, opens a temporary
catalog whose path contains spaces, verifies editable buffer contents and a separate tab, and
removes the fixture. The user's global catalog remains untouched.

`git diff --check` exited zero in 0.35 seconds, with existing CRLF-to-LF conversion warnings.
LuaLS checking the Forge plugin with `nvim/.luarc.json` exited zero in 6.91 seconds and reported
204 existing diagnostics outside the profile editor. These checks completed within 30 seconds.

### Shared picker value shortcuts verified on 2026-10-11

Picker value actions resolve `keymaps.picker.previous_value` and `next_value` through the shared
keymap configuration. Defaults preserve Left/Right and add `r` for previous and `h` for next.
Settings, profiles, models, and plan revisions use those commands. Forms retain their existing
bindings. Pickers without value actions do not acquire these aliases.

Each command below exited zero within 30 seconds, completing in under one second:

```text
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/picker.lua
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/subagent_profiles.lua
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/harness_settings.lua
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/harness_plan_picker.lua
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/harness_model_pins.lua
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/picker_buffer_lock.lua
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/harness_approval.lua
```

The tests exercise letter aliases, preserved arrows, custom bindings, disabled bindings, and
removal of stale mappings after picker updates. The picker fixture printed its expected native
policy diagnostic. `git diff --check` exited zero with existing CRLF conversion warnings. LuaLS
exited zero in 6.87 seconds with the same 204 pre-existing diagnostics and none in the shared
picker, configuration, model picker, or profile editor. No interactive visual check was repeated.

### Explicit agent tool commands verified on 2026-10-11

`/agents on` and `/agents off` route explicit booleans through the existing validated
configuration path. Completion offers both commands. Repetition keeps the requested state,
and selected child timelines and active work do not redirect these controls into prompts.

`nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/harness_controller_native.lua`
exited zero in 0.47 seconds, including its expected simulated-provider-failure diagnostic.
`nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/harness_completion.lua`
exited zero in 0.36 seconds. Checks cover repeated on/off routing, the selected-child boundary,
preservation of queued work, composer clearing, and full and partial command completion.

`git diff --check` exited zero in 0.34 seconds with existing CRLF conversion warnings. LuaLS
exited zero in 6.92 seconds with the same 204 existing diagnostics. All checks completed within
30 seconds. This change reuses existing persistence and does not modify Rust code.

### Agent heading speed verified on 2026-10-11

The Rust projection appends `+` for fast and `++` for ultrafast immediately after the child model
name. Standard speed has no suffix. The child selection owns this value independently of later
parent configuration. The child-preview test checks all three tiers in running and completed
states, including timer refreshes of the rendered heading.

The following commands exited zero. The Cargo commands used a 300-second build bound and the
fresh-host fixture used a 30-second bound:

```text
cargo test --manifest-path Cargo.toml --target-dir D:/.cache/nvim/rust-sidecar/forge/build --release --locked -p forge-harness --lib child_preview_keeps_latest_thought_and_inspection_retains_full_history
cargo build --manifest-path Cargo.toml --target-dir D:/.cache/nvim/rust-sidecar/forge/build --release --bin forge --locked --timings
cargo test --manifest-path Cargo.toml --target-dir D:/.cache/nvim/rust-sidecar/forge/build --release --locked -p forge-harness --lib buffer::projection::
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/client_initialize_host.lua
```

Cargo ran from `D:/config/nvim/rust/forge`. The focused test compiled in 31.12 seconds, and all
39 projection tests passed on the subsequent cached run. The release rebuild compiled
`forge-harness` and `forge` in 8.19 seconds and produced
`D:/.cache/nvim/rust-sidecar/forge/build/release/forge.exe`. The fresh-host fixture passed startup,
one application request, and process collection in 0.42 seconds. Existing hosts retain their old
process-owned executable until restarted. This verification did not include manual visual inspection.

### Nested agent folds verified on 2026-10-11

Agent folds now belong to the parent's Thought fold. Running children show the latest non-final
thought and latest tool heading, without tool output or the final response. Completion replaces
that preview with the delegated task's normal timeline and resets live expansion so the fold
closes. A later explicit expansion survives updates. Enter on the first line opens the owned
child timeline independently of fold controls.

The projection test covers late agent references after the parent response, nested fold ownership,
live preview filtering, completed task history, heading speed, timer metadata, and collapse state.
The session test covers streamed output suppression, selecting a running child and returning to
Main, completion collapse, reopening the full history and tool output, and completed child selection.

These commands exited zero, with a 300-second Cargo bound and 30-second Neovim bounds:

```text
cargo test --manifest-path Cargo.toml --target-dir D:/.cache/nvim/rust-sidecar/forge/build --release --locked -p forge-harness --lib buffer::
cargo build --manifest-path Cargo.toml --target-dir D:/.cache/nvim/rust-sidecar/forge/build --release --bin forge --locked --timings
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/harness_owned_agents_real.lua
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/harness_timeline_position.lua
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/client_initialize_host.lua
```

Cargo ran from `D:/config/nvim/rust/forge`. All 114 buffer tests passed in 1.11 seconds after
9.40 seconds of compilation. The final release build compiled `forge-harness` and `forge` in
8.17 seconds and produced `D:/.cache/nvim/rust-sidecar/forge/build/release/forge.exe`.
The real-host child fixture passed in 0.85 seconds, including a mapped Enter key on a completed
heading and retained child follow-up history. Timeline position checks passed in 1.22 seconds,
and fresh-host startup, application request, and collection passed in 0.45 seconds.
The mock child completes before a live heading can be captured, so running selection is covered
by the Rust session test. This verification did not include manual graphical inspection.

### Separate child-result delivery verified on 2026-10-11

The broker delivers completed child reports as attributed parent input and acknowledges each
accepted delivery once. Active delivery runs concurrently with provider polling. Child results
do not produce user-prompt rows. Unsupported or rejected delivery remains pending for the next
provider input. An idle parent stays idle, and an unfinished delegated exchange can continue.
`harness_wait_agent` returns only `message` and `timed_out` and does not consume results.

Regression coverage includes two children with and without an explicit wait, a provider whose
acceptance requires continued polling, duplicate completion, retained idle results, rejected
steering, and suppression of user steering events for child reports. The idle fixture emits a
complete provider-turn lifecycle before testing result retention.

The final commands all exited zero:

```text
cargo test --manifest-path Cargo.toml --target-dir D:/.cache/nvim/rust-sidecar/forge/build --release --locked -p forge-harness --lib idle_child_result_is_retained -- --nocapture
cargo test --manifest-path Cargo.toml --target-dir D:/.cache/nvim/rust-sidecar/forge/build --release --locked -p forge-harness --lib -- --quiet
cargo build --manifest-path Cargo.toml --target-dir D:/.cache/nvim/rust-sidecar/forge/build --release --bin forge --locked --timings
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/harness_owned_agents_real.lua
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/harness_timeline_position.lua
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/client_initialize_host.lua
```

Cargo ran from `D:/config/nvim/rust/forge`. The focused idle test passed in 0.32 seconds after
22.96 seconds of compilation. The complete crate suite passed 889 tests with 10 ignored in
17.26 seconds after 0.26 seconds of cached compilation. The release build completed in
57.55 seconds and produced `D:/.cache/nvim/rust-sidecar/forge/build/release/forge.exe`.
Cargo commands completed within 300 seconds. The child fixture, timeline fixture, and startup
fixture completed in 0.87, 1.20, and 0.45 seconds respectively, each within 30 seconds.
The child fixture used the real host with mock providers. No live-provider or manual graphical
verification was performed for this change. Existing hosts need a restart to load the new binary.

### Harness-owned waiting verified on 2026-10-11

The root exposes five child-management tools. `harness_wait_agent` is removed from the catalog
and dispatch paths. When Main ends a provider turn with unfinished delegated children, Harness
retains the exchange and suspends generation until all its model-delegated children settle.
New user input resumes that exchange immediately without cancelling children, including when
the provider lacks native steering. The existing child-cleanup guard prevents continuation
during cancellation and policy changes. User decisions stop automatic continuation.

Main keeps the existing Working, Planning, Implementing, and other activity labels. Status
rows append the running-agent count independently. Waiting uses the existing spinner with
`Waiting for agents`. Regression tests cover joining two children, early user input, once-only
result delivery, cancellation during child cleanup, user decisions, status transitions, and
composer routing without native steering.

Cargo commands ran from `D:/config/nvim/rust/forge`. These final commands exited zero:

```text
cargo test --manifest-path Cargo.toml --target-dir D:/.cache/nvim/rust-sidecar/forge/build --release --locked -p forge-harness --lib -- --quiet
cargo build --manifest-path Cargo.toml --target-dir D:/.cache/nvim/rust-sidecar/forge/build --release --bin forge --locked --timings
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/harness_submit_steering.lua
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/harness_owned_agents_real.lua
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/harness_timeline_position.lua
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/client_initialize_host.lua
lua-language-server --check D:/config/nvim/lua/forge --configpath D:/config/nvim/.luarc.json --logpath D:/.cache/nvim/subagent-luals-configured
```

The full suite passed 891 tests with 10 ignored in 17.92 seconds including Cargo startup.
One preceding full run failed the unchanged event-stream test
`cancelled_receive_retains_partial_batch_for_final_drain` at its 1 ms timeout assertion.
That test passed in isolation in 0.35 seconds and in the full rerun. The final release build
finished without warnings in 21.12 seconds and produced
`D:/.cache/nvim/rust-sidecar/forge/build/release/forge.exe`. Cargo commands used 120-second bounds.

The steering, child-host, timeline, and startup fixtures finished in 0.04, 0.52, 0.87, and 0.12
seconds respectively, with 30-second bounds. The child-host fixture used mock providers.
LuaLS completed in 7.17 seconds with 204 diagnostics. Comparing file, code, and message against
the previous configured diagnostic record found no additions or removals. No live-provider or
manual graphical verification was performed. Existing hosts need a restart to load the new binary.

The final test-only import cleanup was checked with
`cargo test --manifest-path Cargo.toml --target-dir D:/.cache/nvim/rust-sidecar/forge/build --release --locked -p forge-harness --lib agent::tool_scope -- --quiet`.
Both authority tests passed with exit zero in 37.62 seconds including compilation, within the
120-second bound. `git diff --check` also exited zero.

## Child activity inspection verified on 2026-10-11

Main has two additional read-only tools. `agent_activity` returns recent thoughts and
tool metadata without output or final answers. It inspects the active run before queued
follow-ups. `agent_tool_output` reads a selected call through a stable reference that
validates root, child, exchange, turn, and tool ownership. Reads remain valid after follow-ups
and never execute the tool again. Output pagination preserves Unicode and NUL characters
across stored chunks. Provider activity timestamps remain unchanged by inspection.

The activity tool defaults to five entries, and each call can override the count within 1–20.
Thoughts retain up to 4000 characters. Tool output pages default to 8000
characters and accept up to 32000. The supervision timer and execution timeout are unchanged.

Final Cargo commands ran from `D:/config/nvim/rust/forge` with 120-second process limits:

```text
cargo test --release --locked --target-dir D:/.cache/nvim/rust-sidecar/forge/build -p forge-harness --lib
cargo build --release --locked --target-dir D:/.cache/nvim/rust-sidecar/forge/build --bin forge --timings
```

Both commands exited zero. The library suite passed 892 tests, with 10 optional live-provider
tests ignored. Compilation took 9.18 seconds and test execution took 17.63 seconds. The release
build compiled `forge-harness` and `forge` in 56.22 seconds and produced
`D:/.cache/nvim/rust-sidecar/forge/build/release/forge.exe`. Its timing report is
`D:/.cache/nvim/rust-sidecar/forge/build/cargo-timings/cargo-timing-20261011T192003.5221327Z.html`.

Fresh Neovim checks ran from `D:/config`, each with a 30-second limit:

```text
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/harness_owned_agents_real.lua
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/client_initialize_host.lua
nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/subagent_profiles.lua
```

They exited zero in 0.49, 0.10, and 0.08 seconds respectively. The child-host fixture used mock
providers and the rebuilt executable. LuaLS ran with `--configpath D:/config/nvim/.luarc.json`
and a 60-second limit. It exited zero with 204 repository diagnostics and none in the changed
profile-picker file. `git diff --check` exited zero with a 30-second limit. No live-provider or
manual graphical verification was performed. Existing hosts require a restart for the rebuilt
binary.


### Profile tuning controls verified on 2026-10-11

`/agents edit` cycles Enable, Provider, Model, Thinking, and Speed with Tab selecting
one field and the shared picker value bindings changing it. Enter still opens the full-page
profile editor. Both views use the same catalog-based selection rules. A provider change
selects its first advertised model and resets unsupported thinking or speed to default.
Compatible inheritance remains symbolic and shows its current parent value. Catalog errors,
empty results, stale replies, and failed saves retain the previous complete selection.

The `agent.models` response now contains `models` and `capability` from one adapter. The
service test verifies that this response still bypasses an occupied parent broker, and the
fresh-host fixture consumes the response before spawning and following up with a child.

All commands below exited zero:

- `cargo test --release --locked --target-dir D:/.cache/nvim/rust-sidecar/forge/build -p forge-harness --lib child_model_catalog_bypasses_the_active_root_broker_lock`
  ran with a 120-second limit. Compilation took 42.50 seconds and the test passed in 0.08 seconds.
- `cargo test --release --locked --target-dir D:/.cache/nvim/rust-sidecar/forge/build -p forge-harness --lib`
  ran with a 120-second limit. 892 tests passed and 10 were ignored in 16.76 seconds.
- `cargo build --release --locked --target-dir D:/.cache/nvim/rust-sidecar/forge/build --bin forge --timings`
  ran with a 120-second limit. It compiled forge-harness and forge in 40.85 seconds and produced
  `D:/.cache/nvim/rust-sidecar/forge/build/release/forge.exe`.
  The report is `D:/.cache/nvim/rust-sidecar/forge/build/cargo-timings/cargo-timing-20261011T192601.1013513Z.html`.
- `nvim --headless -u NONE --cmd "set rtp+=D:/config/nvim" -l nvim/tests/forge/subagent_profiles.lua`
  passed in 0.10 seconds with a 30-second limit. Coverage includes both editors, provider-order
  selection, dependent resets, unsupported choices, failed and empty catalogs, and late replies.
- The same Neovim command with `picker.lua`, `picker_buffer_lock.lua`,
  `harness_owned_agents_real.lua`, and `client_initialize_host.lua` passed with 30-second limits.
  The fresh-host child fixture took 0.60 seconds and initialization took 0.26 seconds.
- `lua-language-server --check D:/config/nvim/lua/forge --configpath D:/config/nvim/.luarc.json --checklevel=Warning --logpath D:/tmp/forge-profile-check-final`
  ran with a 30-second limit. It reported 204 existing diagnostics outside the changed modules.
- `git diff --check` passed with a 15-second limit.

These checks use a mock provider and headless Neovim. They do not establish live-provider
availability or manual terminal rendering. Existing Neovim hosts require a restart to adopt
the rebuilt executable.

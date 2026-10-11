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

`/spawn` uses a task name, message, provider, model, and reasoning effort. `/agents` exposes Main,
Running, and Done timelines, informational messaging, follow-ups, interruption, and the root tool
toggle. `subagent_tools` defaults off and persists through Forge's portable settings. Manual
spawning remains available with the toggle off.

The enabled root receives six tools:

- `harness_spawn_agent` admits a fresh child task.
- `harness_send_message` retains information and steers an active child when supported.
- `harness_followup_task` queues another task on the child's own history.
- `harness_interrupt_agent` cancels active and queued tasks without deleting history.
- `harness_list_agents` returns owned identities and their latest tasks.
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

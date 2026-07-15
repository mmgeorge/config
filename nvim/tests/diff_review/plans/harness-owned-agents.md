# Harness-Owned Agents

## Status

Parked. Do not implement Harness-owned subagents until the backend constraints below have been revisited
against the versions that Harness actually launches.

## Objective

Harness-owned agents would give DiffReview one orchestration model across providers. The Rust broker would
own child sessions, lifecycle state, event routing, and execution-mode assignment while each backend translates
that common contract into provider-specific session operations.

The main agent must see exactly one subagent tool family. Exposing both provider-native delegation tools and
Harness delegation tools would create competing orchestration planes. The model could spawn children through
either plane, producing inconsistent permission enforcement, persistence, steering, cancellation, and timeline
events.

## Proposed model-visible contract

```text
harness_spawn_agent
harness_list_agents
harness_read_agent
harness_send_agent_input
harness_wait_agents
harness_cancel_agent
harness_close_agent
```

`harness_read_agent` would expose a compact progress snapshot to the parent model. Harness timeline rendering
must consume broker events directly rather than depend on the model polling this tool.

## Ownership model

```text
Parent model ──calls──▶ Harness agent tools ──route──▶ AgentCoordinator
                                                     ├─▶ Codex child session
                                                     ├─▶ Copilot child session
                                                     └─▶ ACP child session

AgentCoordinator ──owns──▶ child registry, lifecycle, event stream, and execution mode
```

The provider backend would execute children, but `AgentCoordinator` would remain the authoritative owner of
their IDs, status, execution mode, cancellation, steering, and timeline projection.

## Why execution modes motivate Harness ownership

Provider-native children commonly inherit the parent runtime's approval policy and sandbox. That protects the
parent boundary, but it does not let Harness assign a narrower execution mode to an individual child. A read-only
explorer and workspace writer therefore cannot receive distinct Harness ceilings when the provider owns their
lifecycle.

Harness-owned child sessions could select the parent's mode or a narrower mode before starting the backend session.
Every surfaced child approval would pass through the same compiled permission evaluator used by the main Harness
session. The provider sandbox would still enforce the child's capability ceiling. This prevents a child from gaining
authority merely because its provider-native parent already holds it.

This ownership also gives the UI one place to explain effective authority. The agent picker and child timeline
could show the assigned mode, while approval requests could identify both the child and the matching permission rule.
The user-visible result would be predictable least-privilege delegation rather than provider-dependent inheritance.
The complete mode and permission contract lives in `sandbox.md`.

## Backend capability findings

### Codex app-server

Codex implements native delegation as model-visible collaboration tools backed by internal `AgentControl`
operations. Its native family includes spawning, waiting, steering, resuming, and closing child threads.

Codex can remove the complete native collaboration bundle by disabling its multi-agent feature flags when the
app-server starts. It does not currently provide a general per-built-in-tool exclusion mechanism. A future
Harness-owned implementation should disable every multi-agent feature variant recognized by the launched Codex
version, then inject only the Harness tool family.

### Copilot SDK

Copilot exposes native delegation through tools such as `task`, `list_agents`, `read_agent`, and `write_agent`.
Its SDK supports session-level `availableTools` and `excludedTools`, which remove named tools from the model's
advertised tool set. This provides the cleanest path to replacing native delegation with Harness-owned tools.

Copilot also emits structured subagent lifecycle events. A Harness backend should translate those events into
the common child event model rather than infer lifecycle state from assistant prose.

### Agy CLI

Agy exposes `invoke_subagent`, `define_subagent`, `send_message`, and `manage_subagents` to the model. Its
documented `PreToolUse` hook can deny a native delegation call after the model selects it, but no verified CLI or
ACP option currently removes those built-in tools from the root model's advertised schema.

Denial does not solve orchestration ambiguity because the model still sees two delegation systems and can waste
a turn selecting the forbidden one. Until Agy exposes real built-in tool filtering, Harness should adapt Agy's
native children into the common timeline instead of injecting a competing Harness spawn family.

### Generic ACP

ACP does not standardize filtering an agent's internal tool registry. An ACP implementation may expose a
provider-specific capability, but Harness cannot assume one. Generic ACP should use native orchestration when
the agent advertises it and otherwise report subagents as unavailable.

## Required backend capability

Each backend should eventually report one explicit orchestration mode:

```text
harness_owned
native
unavailable
```

- `harness_owned` requires native delegation tools to be absent from the model-visible schema.
- `native` adapts provider lifecycle events into Harness state and documents provider trust limitations.
- `unavailable` hides agent commands and spawning tools from the UI and model.

Harness must never expose native and Harness-owned spawning simultaneously.

## Current recommendation

- Use Harness-owned orchestration for Codex only after verifying feature-disable behavior against the bundled
  app-server version.
- Use Harness-owned orchestration for Copilot SDK sessions after excluding every native delegation tool.
- Keep Agy on native orchestration until it supports genuine built-in tool filtering.
- Gate generic ACP behavior on explicit provider capabilities.
- Preserve native orchestration as the fallback whenever Harness cannot prove that the competing native tool
  family has been removed from the model context.

## Questions to revisit

- Can the launched Codex app-server accept session-scoped feature overrides without changing the user's global
  configuration?
- Does each supported Copilot SDK version apply `excludedTools` to every delegation entry point, including fleet
  mode and automatically inferred custom agents?
- Has Agy added a root-agent tool allowlist or a way to disable subagent tools before model invocation?
- Can an ACP capability extension describe native child lifecycle operations and tool filtering without relying
  on provider names?
- How should Harness persist the effective mode and permission revision when a child session resumes after a Neovim
  restart?

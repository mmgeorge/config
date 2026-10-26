---
root: true
globs:
  - '**/*'
---

# Repository Purpose

This repository stores the user's **dotfiles** and personal developer environment. It covers editor configuration, shell configuration, terminal configuration, Git defaults, Codex state, and Rulesync-managed assistant defaults.

Treat this repository as operational infrastructure rather than a single application. A change here alters the runtime behavior of Neovim, Nushell, WezTerm, Git, Codex, Claude Code, Copilot CLI, and Antigravity CLI across unrelated projects.

# Targeted Verification

- Start with the smallest existing test that exercises the changed behavior. Use
  an exact test name or an individual fixture when available. A changed source
  file selects its package, not a single test, so do not substitute file selection
  for exact-test selection when only one regression needs verification.
- After a compilation failure, fix the compiler error and retry one targeted
  test. Do not queue other tests that compile the same failing target.
- Broaden to related tests after the targeted test passes. Package-wide and
  project-wide suites are exceptional. Run them only for an explicit user request,
  a mandatory repository gate, or an identified cross-module risk that narrower
  tests cannot cover. State the specific risk and why targeted coverage is
  insufficient before running a broad suite. A changed source file, compilation
  failure, or uncertainty about test selection does not meet this bar. Investigate
  the affected behavior and existing tests before expanding coverage.
- Reuse recorded passing results while their inputs remain unchanged. Repeat
  checks only for relevant edits, observed failures, or unresolved coverage.
- Inspect the existing check configuration and executor before changing them.
  Do not add a runner, redesign checks, or disable automatic coverage to compensate
  for selecting an unnecessarily broad test run.

# Cross-Platform Scripting

This repository supports **Windows and macOS**, and every supported environment provides **Nushell**. Treat every shared workflow as cross-platform unless its owning directory or integration explicitly targets Windows.

- Use **Nushell** for new repository-level scripts, automation, launchers, and orchestration that run on both Windows and macOS.
- Use native platform scripting only for platform-native system operations (such as Windows APIs, registry access, or toolchain initialization). On Windows, PowerShell, `cmd`, and batch files serve as narrow adapters for low-overhead startup or OS-specific APIs.
- Do not implement cross-platform logic in platform-specific scripts solely because the file resides in a platform-specific directory. Keep shared orchestration in a `.nu` script and route only the platform-native operation through a narrow adapter.
- Do not duplicate an entire workflow into separate Windows and macOS implementations. Extract common state transitions, Git behavior, validation, and error handling into one Nushell source.
- Validate every new or modified standalone Nushell script with `nu -c 'nu-check path/to/script.nu'`.

# Rulesync Source Layout

This repository uses **Rulesync** as the single source of truth for assistant-facing rules, skills, agents, MCP configuration, hooks, commands, and ignore files. Do not manually edit generated provider files such as `AGENTS.md`, `CLAUDE.md`, `GEMINI.md`, `.agents/`, `.claude/`, `.codex/`, `.copilot/`, or `.gemini/` because Rulesync overwrites them during synchronization.

For new repositories, use Rulesync for agent file management rather than hand-maintaining provider-specific assistant files.

## Local Project Sync

The local `.rulesync/` directory defines configuration specific to this `D:\config` checkout. Its `rulesync.jsonc` synchronizes **rules** and **skills**, so local edits remain limited to project-specific guidance and skills.

- Add local rules under `.rulesync/rules/`.
- Add local skills under `.rulesync/skills/`.
- Do not add local MCP servers, commands, hooks, permissions, or subagents here unless `rulesync.jsonc` explicitly enables those features.
- Keep `.rulesync/mcp.json` empty for this local sync unless this repository requires project-local MCP servers.
- Maintain directory-specific guidance in `.rulesync/rules/` rather than generated `AGENTS.md` files.

Use this dry run before applying local changes:

```sh
rulesync generate --dry-run
```

## Global Defaults Sync

The `rulesync-global/` directory defines baseline dotfile defaults across projects. It targets **Claude Code**, **Codex CLI**, **Copilot CLI**, and **Antigravity CLI**.

- Add global rules under `rulesync-global/.rulesync/rules/`.
- Add global skills under `rulesync-global/.rulesync/skills/`.
- Add global subagents under `rulesync-global/.rulesync/subagents/`.
- Add global MCP servers under `rulesync-global/.rulesync/mcp.json`.
- Add global hooks under `rulesync-global/.rulesync/hooks.json`.
- Add global commands under `rulesync-global/.rulesync/commands/` only for targets that support commands.
- Keep provider-specific generated outputs untracked and ignored.

When adding stdio **MCP servers** that require inherited credentials, declare the environment variable names with `envVars` in `rulesync-global/.rulesync/mcp.json`. Codex generates those as `env_vars` in `~/.codex/config.toml`. The GitHub MCP server must preserve `envVars: ["GITHUB_PERSONAL_ACCESS_TOKEN"]`, or Codex can start `github-mcp-server` without credentials and close the MCP handshake during initialization.

Use this dry run before applying global changes:

```sh
rulesync generate --dry-run --config rulesync-global/rulesync.jsonc --input-roots rulesync-global/.rulesync --global
```

## Feature Placement Rule

Choose the source directory by the target scope. If the behavior applies exclusively to this dotfiles repository, place it in local `.rulesync/`. If the behavior establishes the default across all repositories and provider CLIs, place it in `rulesync-global/.rulesync/`.

When adding a new **skill**, **subagent**, **command**, **hook**, or **MCP server**, update the Rulesync source first and run the matching dry run. Only inspect generated files afterward to verify output structure.

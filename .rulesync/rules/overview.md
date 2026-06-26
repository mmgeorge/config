---
root: true
targets:
  - '*'
globs:
  - '**/*'
---

# Repository Purpose

This repository stores the user's **dotfiles** and personal developer environment. It covers editor configuration, shell configuration, terminal configuration, Git defaults, Codex state, and Rulesync-managed assistant defaults.

Treat this repo as operational infrastructure, not as a single application. A change here can alter the behavior of Neovim, Nushell, WezTerm, Git, Codex, Claude Code, Copilot CLI, and Antigravity CLI across unrelated projects.

# Rulesync Source Layout

This repository uses **Rulesync** as the source of truth for assistant-facing rules, skills, agents, MCP config, hooks, commands, and ignore files. Generated provider files such as `AGENTS.md`, `CLAUDE.md`, `GEMINI.md`, `.agents/`, `.claude/`, `.codex/`, `.copilot/`, and `.gemini/` should not receive hand edits because Rulesync will overwrite them.

For new repositories, prefer [Rulesync](https://github.com/dyoshikawa/rulesync) for agent file management instead of hand-maintaining provider-specific assistant files. This keeps rules, skills, agents, MCP config, commands, hooks, and ignore files in one portable source of truth before provider CLIs generate their native files.

## Local Project Sync

The local `.rulesync/` directory describes behavior for this `D:\config` checkout. Its `rulesync.jsonc` currently syncs only **rules** and **skills**, so local edits should stay limited to project-specific guidance and project-specific skills.

- Add local rules under `.rulesync/rules/`.
- Add local skills under `.rulesync/skills/`.
- Do not add local MCP servers, commands, hooks, permissions, or subagents here unless `rulesync.jsonc` explicitly enables those features.
- Keep `.rulesync/mcp.json` empty for this local sync unless this project needs project-local MCP servers.
- Keep directory-specific guidance in `.rulesync/rules/` rather than generated `AGENTS.md` files. The Neovim guidance that used to live in `nvim/AGENTS.md` now lives in `.rulesync/rules/nvim.md`.

Use this dry run before applying local changes:

```sh
rulesync generate --dry-run
```

## Global Defaults Sync

The `rulesync-global/` directory describes dotfile defaults that should follow the user across projects. It targets **Claude Code**, **Codex CLI**, **Copilot CLI**, and **Antigravity CLI**, so changes there define the baseline assistant experience across provider CLIs.

- Add global rules under `rulesync-global/.rulesync/rules/`.
- Add global skills under `rulesync-global/.rulesync/skills/`.
- Add global subagents under `rulesync-global/.rulesync/subagents/`.
- Add global MCP servers under `rulesync-global/.rulesync/mcp.json`.
- Add global hooks under `rulesync-global/.rulesync/hooks.json`.
- Add global commands under `rulesync-global/.rulesync/commands/` only for targets that support commands.
- Keep provider-specific generated outputs untracked and ignored. Rulesync owns those files.

When adding stdio **MCP servers** that need inherited credentials, declare the environment variable names with `envVars` in `rulesync-global/.rulesync/mcp.json`. Codex generates those as `env_vars` in `~/.codex/config.toml`. The GitHub MCP server must preserve `envVars: ["GITHUB_PERSONAL_ACCESS_TOKEN"]`, or Codex can start `github-mcp-server` without credentials and the MCP handshake can close during initialize.

Use this dry run before applying global changes:

```sh
rulesync generate --dry-run --config rulesync-global/rulesync.jsonc --input-root rulesync-global --global
```

## Feature Placement Rule

Choose the source directory by the intended audience. If the behavior should affect only this dotfiles repo, place it in local `.rulesync/`. If the behavior should become the default for every project and provider CLI, place it in `rulesync-global/.rulesync/`.

When adding a new **skill**, **subagent**, **command**, **hook**, or **MCP server**, update the Rulesync source first and run the matching dry run. Only inspect generated files afterward to verify output shape.

# Testing And Shell Commands

Shell commands are part of the development loop, so they need the same engineering discipline as code changes. A stuck command blocks iteration, hides the real failure, and turns verification into waiting instead of evidence.

- Always run shell commands with an explicit timeout. Use the shortest timeout that fits the command, and never set more than 120 seconds without the user explicitly approving a longer run.
- Treat a timeout as a debugging signal. Do not rerun the same broad command with a longer timeout until you have narrowed the target, added output, or changed the command shape.
- Keep ordinary file reads, searches, diffs, and status checks fast. These should usually finish in seconds, not minutes.
- Do not run watch mode, dev servers, pagers, prompts, or other long-running interactive processes through an unbounded shell command. Use Terminal MCP for interactive TUI work, or start a managed background process only when the workflow requires it and you can stop or reuse it.
- Prefer focused verification first: a single test file, test name, package target, linter target, or generated artifact check. Run the full suite only after the tight loop passes or when the change surface genuinely requires full coverage.
- When a test is slow, optimize the iteration path before accepting the delay. Look for a smaller selector, a lower-level unit test, a fixture-only run, cached setup, or a direct validation command that proves the changed behavior.
- If no focused command exists, say that clearly, run the best bounded command, and report the timeout or runtime as a testability problem worth improving.
- Keep verification output actionable. Capture the command, timeout, exit status, and the failure line or artifact that proves the result.

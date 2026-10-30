---
root: true
globs:
  - '**/*'
---
# Repository Purpose

This repository holds the user's dotfiles: Neovim, Nushell, WezTerm, Git, and the Rulesync sources for every assistant CLI. A change here alters tool behavior across unrelated projects, so treat it as operational infrastructure rather than a single application.

# Cross-Platform Scripting

Every supported environment, Windows and macOS, provides Nushell, so shared workflows belong in one Nushell implementation.

- Write new repository scripts, launchers, and orchestration in Nushell.
- Use PowerShell, `cmd`, or batch files only as narrow adapters for platform-native operations such as Windows APIs, the registry, or toolchain initialization. Do not duplicate a workflow per platform.

# Rulesync Sources

Rulesync generates every provider file, such as `AGENTS.md`, `CLAUDE.md`, `GEMINI.md`, `.agents/`, `.claude/`, `.codex/`, `.copilot/`, and `.gemini/`. Edit the Rulesync source, never the generated file, because the next generation overwrites it.

- **Local scope:** `.rulesync/` holds rules and skills that apply only to this repository. Do not add MCP servers, commands, hooks, permissions, or subagents there unless `rulesync.jsonc` enables them. Dry-run with `rulesync generate --dry-run`.
- **Global scope:** `rulesync-global/.rulesync/` holds rules, skills, subagents, MCP servers, hooks, and commands that load in every repository for Claude Code, Codex CLI, Copilot CLI, and Antigravity CLI. Dry-run with `rulesync generate --dry-run --config rulesync-global/rulesync.jsonc --input-roots rulesync-global/.rulesync --global`.
- **Codex permissions:** Keep the `permissions` feature off the global `codexcli` target. Harness owns Codex approvals, and a generated Codex exec policy can reject an operation before Harness receives its approval request.

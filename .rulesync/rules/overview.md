---
root: true
globs:
  - '**/*'
---
# Repository Purpose

This repository holds the user's dotfiles: Neovim, Nushell, WezTerm, Git, and the Rulesync sources for every assistant CLI. A change here alters tool behavior across unrelated projects, so treat it as operational infrastructure rather than a single application.

# Persisted Data Compatibility

Backward compatibility is not a goal for this personal infrastructure. Preserve user-authored configuration and settings, and try to migrate them gracefully when their formats change; report unsupported settings and migration failures rather than silently resetting them. Treat sessions, transcripts, caches, journals, and other derived state as disposable: never migrate incompatible session data. Select an explicit compatibility-version directory for disposable state so a new version can start while old processes retain their own directories. Provide cleanup that removes stale versions, skips and reports live owners before deleting any dependent files, and leaves persistent settings untouched. Tests must prove fresh state works alongside an active old version, stale state can be removed, and settings survive.

# Cross-Platform Scripting

Every supported environment, Windows and macOS, provides Nushell, so shared workflows belong in one Nushell implementation.

- Write new repository scripts, launchers, and orchestration in Nushell.
- Use PowerShell, `cmd`, or batch files only as narrow adapters for platform-native operations such as Windows APIs, the registry, or toolchain initialization. Do not duplicate a workflow per platform.

# Rulesync Sources

Rulesync generates every provider file, such as `AGENTS.md`, `CLAUDE.md`, `GEMINI.md`, `.agents/`, `.claude/`, `.codex/`, `.copilot/`, and `.gemini/`. Edit the Rulesync source, never the generated file, because the next generation overwrites it.

- **Local scope:** `.rulesync/` holds rules and skills that apply only to this repository. Do not add MCP servers, commands, hooks, permissions, or subagents there unless `rulesync.jsonc` enables them. Dry-run with `rulesync generate --dry-run`.
- **Global scope:** `rulesync-global/.rulesync/` holds rules, skills, subagents, MCP servers, hooks, and commands that load in every repository for Claude Code, Codex CLI, Copilot CLI, and Antigravity CLI. Dry-run with `rulesync generate --dry-run --config rulesync-global/rulesync.jsonc --input-roots rulesync-global/.rulesync --global`.
- **Codex permissions:** Keep the `permissions` feature off the global `codexcli` target. Harness owns Codex approvals, and a generated Codex exec policy can reject an operation before Harness receives its approval request.

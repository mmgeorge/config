---
root: true
globs:
  - '**/*'
---
# Repository Purpose

This repository holds the user's dotfiles: Neovim, Nushell, WezTerm, Git, and the Rulesync sources for every assistant CLI. A change here alters tool behavior across unrelated projects, so treat it as operational infrastructure rather than a single application.

# Persisted Data Compatibility

Backward compatibility is not a goal for this personal infrastructure. Preserve user-authored configuration and settings, and try to migrate them gracefully when their formats change; report unsupported settings and migration failures rather than silently resetting them. Treat sessions, transcripts, caches, journals, and other derived state as disposable: never migrate incompatible session data. Select an explicit compatibility-version directory for disposable state so a new version can start while old processes retain their own directories. Provide cleanup that removes stale versions, skips and reports live owners before deleting any dependent files, and leaves persistent settings untouched. Tests must prove fresh state works alongside an active old version, stale state can be removed, and settings survive.

- Review compatibility whenever changing database tables, columns, constraints, or indexes; serialized records or enum variants; durable commands, queues, receipts, or recovery journals; cache formats; or the meaning of persisted state. Bump the owning disposable-state compatibility version whenever existing state cannot safely satisfy the new reader, writer, or recovery contract, including additive tables required at startup. A purely performance-related index change does not by itself require a bump.
- Finalize the schema and its version together. If an intermediate build has already created state under the proposed version, use another unused version for the completed format rather than trusting or repairing that state in place.
- Validate the version and required schema at the first database-open boundary, before starting owners, recovery, or prompt admission. A matching version number is insufficient: reject missing required tables or other incompatible structure explicitly, without silently resetting or migrating disposable state. Apply the same validation to read-only opens.
- Preserve the actual cause of database-open and startup failures in logs and the UI. An unavailable session must explain the failure and reject execution requests instead of silently accepting prompts that cannot run.
- Cover fresh initialization, rejected old/new versions, and a matching-version database with missing required schema. Keep persistent-settings migration and validation separate from disposable-state versioning.

# Cross-Platform Scripting

Every supported environment, Windows and macOS, provides Nushell, so shared workflows belong in one Nushell implementation.

- Write new repository scripts, launchers, and orchestration in Nushell.
- Use PowerShell, `cmd`, or batch files only as narrow adapters for platform-native operations such as Windows APIs, the registry, or toolchain initialization. Do not duplicate a workflow per platform.

# Rulesync Sources

Rulesync generates every provider file, such as `AGENTS.md`, `CLAUDE.md`, `GEMINI.md`, `.agents/`, `.claude/`, `.codex/`, `.copilot/`, and `.gemini/`. Edit the Rulesync source, never the generated file, because the next generation overwrites it.

- **Local scope:** `.rulesync/` holds rules and skills that apply only to this repository. Do not add MCP servers, commands, hooks, permissions, or subagents there unless `rulesync.jsonc` enables them. Dry-run with `rulesync generate --dry-run`.
- **Global scope:** `rulesync-global/.rulesync/` holds rules, skills, subagents, MCP servers, hooks, and commands that load in every repository for Claude Code, Codex CLI, Copilot CLI, and Antigravity CLI. Dry-run with `rulesync generate --dry-run --config rulesync-global/rulesync.jsonc --input-roots rulesync-global/.rulesync --global`.
- **Codex permissions:** Keep the `permissions` feature off the global `codexcli` target. Harness owns Codex approvals, and a generated Codex exec policy can reject an operation before Harness receives its approval request.

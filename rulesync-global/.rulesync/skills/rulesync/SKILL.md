---
name: rulesync
description: >-
  Generate and synchronize assistant rule configurations (.cursorrules, CLAUDE.md,
  copilot-instructions.md, and agents) across supported coding tools from a single
  `.rulesync/` source. Use when synchronizing assistant rules, importing tool
  configurations, running rulesync CLI commands, or managing multi-tool setups.
targets: ["*"]
---

# Rulesync

Rulesync generates and synchronizes assistant configuration files across supported AI coding tools from a unified source of truth under `.rulesync/`.

## Quick Start

```bash
# Install CLI
npm install -g rulesync

# Initialize configuration and directory structure
rulesync init

# Import existing tool configuration into unified format
rulesync import --targets claudecode
rulesync import --targets cursor
rulesync import --targets copilot

# Generate target tool configurations from unified rules
rulesync generate --targets "*" --features "*"
```

## Core Workflow

1. **Initialize:** `rulesync init` generates `rulesync.jsonc` and the `.rulesync/` directory.
2. **Define Rules and Assets:** Add shared rules under `.rulesync/rules/`, skills under `.rulesync/skills/`, subagents under `.rulesync/subagents/`, and MCP definitions in `.rulesync/mcp.json`.
3. **Generate Configurations:** `rulesync generate` generates target-specific files (`CLAUDE.md`, `.cursorrules`, `.github/copilot-instructions.md`, `.agents/`).
4. **Validate:** `rulesync generate --dry-run` previews file changes, and `rulesync generate --check` validates that generated files match source definitions without writing modifications.

## Key Commands

| Command | Purpose |
| --- | --- |
| `rulesync init` | Initialize repository configuration and source directories |
| `rulesync generate --targets "*" --features "*"` | Generate all configured target files from source rules |
| `rulesync import --targets <tool>` | Import existing target configuration into `.rulesync/` format |
| `rulesync fetch owner/repo --features skills` | Fetch rules or skills from a remote repository |
| `rulesync install` | Install declared remote skill sources |
| `rulesync generate --check` | Verify that generated files match current source rules |
| `rulesync generate --dry-run` | Preview generation output without writing to disk |

## Detailed References

- [Installation](./installation.md) and [Quick Start](./quick-start.md)
- [Configuration](./configuration.md), [Global Mode](./global-mode.md), [Separate Input Root](./separate-input-root.md), [Simulated Features](./simulated-features.md), and [Declarative Sources](./declarative-sources.md)
- [Supported Tools](./supported-tools.md), [CLI Commands](./cli-commands.md), [File Formats](./file-formats.md), and [MCP Server](./mcp-server.md)
- [Programmatic API](./programmatic-api.md)

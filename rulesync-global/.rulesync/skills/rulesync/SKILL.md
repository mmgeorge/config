---
name: rulesync
description: >-
  Configure Rulesync target tools, feature selection, and output scope in
  rulesync.jsonc and rulesync.local.jsonc, and synchronize assistant rules,
  permissions, MCP servers, skills, and other assets from .rulesync/ sources.
  Use when editing Rulesync JSONC configuration, changing which providers or
  features are targeted, running generation or imports, or diagnosing missing,
  stale, or unexpectedly unchanged generated files.
targets: ["*"]
---

# Rulesync

Rulesync generates only the target-feature combinations enabled by its effective
configuration. Source assets under `.rulesync/` and generation settings in
`rulesync.jsonc` jointly determine the output. Editing an asset alone does not
enable its feature for a provider.

## Check Configuration Before Generation

1. Identify the invocation's working directory and selected `rulesync.jsonc`,
   including any explicit `--config` argument. Inspect `rulesync.local.jsonc`
   overrides and CLI target or feature overrides before inferring effective settings.
2. Read the selected configuration and identify the intended provider and feature.
   For object-form `targets`, check that provider's own feature list. For array-form
   `targets`, check both the provider list and the shared `features` list. Consult
   [Configuration](./configuration.md) for supported forms and precedence.
3. Confirm input and output scope through `--input-root`, `global`, `--global`,
   and `outputRoots`. Distinguish repository output from user-level output. In this
   dotfiles repository, inspect `rulesync-global/rulesync.jsonc` for global defaults
   and `.rulesync/rulesync.jsonc` for local generation, verifying the actual selected
   path rather than assuming either configuration controls every invocation.
4. Edit the owning source asset and its target-feature configuration together when
   necessary. Preserve other target selections. Do not enable every feature or
   edit generated provider files to repair an omitted feature.
5. Run a dry run with the same configuration, input root, and scope as the intended
   generation. Inspect skipped-feature warnings, then generate and verify the
   specific destination file and setting. A successful exit or “All files are up
   to date” only describes the selected, supported output.

## Diagnose Missing or Stale Output

Check feature selection before suggesting deletion, a restart, or a tool-version
change. For example, changing `.rulesync/permissions.json` does not update Codex
permissions if `targets.codexcli` omits `permissions`. The `rules` feature and the
`permissions` feature are separate even when generated command policies use the
`.rules` extension.

For object-form configuration, the relevant selection can look like this:

```jsonc
{
  "targets": {
    "codexcli": ["rules", "permissions"]
  }
}
```

Merge the required feature into the existing provider selection. Do not replace
its other features with this example. After confirming selection, check source
discovery, destination scope, installed Rulesync version, and any warning that a
provider does not support the feature or permission category. Disabled output
and unsupported output require different fixes. Do not assume generation removes
obsolete files, especially when deletion is disabled.

Only diagnose cached client rules after verifying the intended generated content
on disk. A client restart cannot correct missing or stale generated output.

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

# Inspect and generate the configured target-feature selection
rulesync generate --dry-run
rulesync generate
```

## Core Workflow

1. **Initialize:** `rulesync init` generates `rulesync.jsonc` and the `.rulesync/` directory.
2. **Select Targets and Define Assets:** Check `rulesync.jsonc` and overrides for the required provider-feature combination, then edit the matching source such as `.rulesync/permissions.json`, `.rulesync/mcp.json`, or `.rulesync/skills/`.
3. **Generate Configurations:** `rulesync generate` generates target-specific files (`CLAUDE.md`, `.cursorrules`, `.github/copilot-instructions.md`, `.agents/`).
4. **Validate:** `rulesync generate --dry-run` previews file changes, and `rulesync generate --check` validates that generated files match source definitions without writing modifications.

## Key Commands

| Command | Purpose |
| --- | --- |
| `rulesync init` | Initialize repository configuration and source directories |
| `rulesync generate` | Generate the configured target-feature combinations |
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

# Contributing to Forge

Forge combines a Neovim plugin with a shared Rust host. Changes must preserve the
component that owns each state transition, and verification must exercise the
boundary being changed. Repository paths in this documentation start at the
dotfiles checkout root unless a command explicitly changes directory.

## Choose the relevant document

Read the [architecture overview](architecture/README.md) first, then select the
documents for the work at hand. Do not read every subsystem chapter for a local change.

| Task | Procedure | Architecture |
| --- | --- | --- |
| Add language support | [Adding a language](adding-language.md) | [Languages](architecture/languages.md), [Planning](architecture/planning.md) |
| Extend package discovery or project checks | [Extending project checks](extending-project-checks.md) | [Planning](architecture/planning.md#project-checks-and-the-harness-checking-gate), [Languages](architecture/languages.md) |
| Update a parser or query | [Updating Tree-sitter](updating-tree-sitter.md) | [Languages](architecture/languages.md), [Buffers and rendering](architecture/buffers-and-rendering.md) |
| Build or verify a change | [Building and testing](building-and-testing.md) | [Host and protocol](architecture/host-and-protocol.md) |
| Change generated buffers, folds, or editing | [Building and testing](building-and-testing.md) | [Buffers and rendering](architecture/buffers-and-rendering.md) |
| Change sessions, tasks, goals, or permissions | [Building and testing](building-and-testing.md) | [Harness](architecture/harness.md) |
| Change a provider adapter | [Building and testing](building-and-testing.md) | [Providers](architecture/providers.md), [Harness](architecture/harness.md) |
| Change plan review or validation | [Building and testing](building-and-testing.md) | [Planning](architecture/planning.md) |
| Change repository reads, writes, or restoration | [Building and testing](building-and-testing.md) | [Git and checkpoints](architecture/git-and-checkpoints.md) |
| Change GitHub synchronization or review editing | [Building and testing](building-and-testing.md) | [GitHub and review](architecture/github-and-review.md) |

User-facing commands are documented separately in [Forge commands](../../docs/forge/commands.md)
and package-aware configuration in [Project checks](../../docs/forge/checks.md).
Read [Agent LSP tools](../../docs/forge/lsp-tools.md) for semantic query operations
and the independent `/config` toggle.

Harness agents run automated validation through the checks interface so execution
and coverage are recorded. Procedure commands identify the intended check content.

## Keep documentation current

Update the affected architecture chapter in the same change when ownership,
data flow, persistence, protocol, resource bounds, or failure handling changes.
Update the relevant procedure when integration steps, build commands, fixtures,
or verification requirements change. Update user documentation when commands or
observable behavior change. Keep this task index and all moved links current.

The implementation and tests remain authoritative. Check source paths and
contracts before reusing a detailed example. Do not append another implementation
history to the architecture overview or duplicate API documentation from source.

Local LLM guidance is maintained in `.rulesync/rules/forge.md` and
`.rulesync/rules/nvim.md`. Edit those sources and regenerate through Rulesync,
rather than editing generated provider instructions.

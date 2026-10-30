# Contributing to Forge

Forge combines a Neovim plugin with a shared Rust host. Changes must preserve the component that owns each state transition, and verification must exercise the boundary being changed. Repository paths in this documentation start at the dotfiles checkout root unless a command explicitly changes directory.

## Choose the relevant document

Read the [architecture overview](architecture/README.md) first, then select the documents for the work at hand. Do not read every subsystem chapter for a local change.

| Task | Procedure | Architecture |
| --- | --- | --- |
| Add language support | [Adding a language](adding-language.md) | [Languages](architecture/languages.md), [Planning](architecture/planning.md) |
| Configure check inputs or repository callbacks | [Extending project checks](extending-project-checks.md) | [Project checks](architecture/project-checks.md), [Languages](architecture/languages.md) |
| Update a parser or query | [Updating Tree-sitter](updating-tree-sitter.md) | [Languages](architecture/languages.md) |
| Build a change or choose Rust checks | [Building and testing](building-and-testing.md) | [Host and protocol](architecture/host-and-protocol.md) |
| Write or run an editor fixture | [Editor tests](editor-tests.md) | [Buffers and rendering](architecture/buffers-and-rendering.md) |
| Find what to run for an area | [Verification by area](verification-by-area.md) | The area's chapter |
| Diagnose an editor that closes unexpectedly | [Debugging](debugging.md) | [Host and protocol](architecture/host-and-protocol.md) |
| Call the native host from Lua | [Lua client API](api/lua-client.md) | [Host and protocol](architecture/host-and-protocol.md) |
| Change generated buffers, folds, or editing | [Building and testing](building-and-testing.md) | [Buffers and rendering](architecture/buffers-and-rendering.md) |
| Change sessions, tasks, or goals | [Building and testing](building-and-testing.md) | [Harness](architecture/harness.md) |
| Change approvals or access scopes | [Building and testing](building-and-testing.md) | [Permissions](architecture/permissions.md) |
| Change child agents | [Building and testing](building-and-testing.md) | [Child agents](architecture/child-agents.md) |
| Change the transcript or status | [Building and testing](building-and-testing.md) | [Timeline](architecture/timeline.md) |
| Change a provider adapter | [Building and testing](building-and-testing.md) | [Providers](architecture/providers.md), [Harness](architecture/harness.md) |
| Classify a new network or capacity error | [Provider retries](provider-retries.md) | [Providers](architecture/providers.md) |
| Change input delivery or add a provider | [Provider submissions](provider-submissions.md) | [Providers](architecture/providers.md) |
| Change plan validation or execution | [Building and testing](building-and-testing.md) | [Planning](architecture/planning.md) |
| Change the plan review interface | [Building and testing](building-and-testing.md) | [Plan review](architecture/plan-review.md) |
| Change repository reads | [Building and testing](building-and-testing.md) | [Git repositories](architecture/git-repositories.md) |
| Change the Status view or index writes | [Building and testing](building-and-testing.md) | [Status](architecture/status.md) |
| Change checkpoints or restoration | [Building and testing](building-and-testing.md) | [Checkpoints](architecture/checkpoints.md) |
| Change generated commit messages | [Building and testing](building-and-testing.md) | [Commit messages](architecture/commit-messages.md) |
| Change GitHub synchronization or review editing | [Building and testing](building-and-testing.md) | [GitHub services](architecture/github-and-review.md) and [Pull request review](architecture/pull-request-review.md) |

User-facing commands are documented separately in [Forge commands](../../docs/forge/commands.md) and typed check configuration in [Project checks](../../docs/forge/checks.md). Read [Agent LSP tools](../../docs/forge/lsp-tools.md) for semantic query operations and the independent `/config` toggle.

Harness agents run automated validation through the checks interface so execution and coverage are recorded. Procedure commands identify the intended check content.

## Keep documentation current

Update the affected architecture chapter in the same change when ownership, data flow, persistence, protocol, resource bounds, or failure handling changes. Update the relevant procedure when integration steps, build commands, fixtures, or verification requirements change. Update user documentation when commands or observable behavior change. Keep this task index and all moved links current.

The implementation and tests remain authoritative. Check source paths and contracts before reusing a detailed example. Do not append another implementation history to the architecture overview or duplicate API documentation from source.

Local LLM guidance is maintained in `.rulesync/rules/forge.md` and `.rulesync/rules/nvim.md`. Edit those sources and regenerate through Rulesync, rather than editing generated provider instructions.

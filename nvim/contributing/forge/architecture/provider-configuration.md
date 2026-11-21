# Provider configuration

Settings that change mid-turn, or credentials copied into provider files, would make a session's behavior depend on when and where it started. Forge keeps settings in one revisioned store, gives each provider an isolated resource profile, and resolves credentials into ephemeral launch payloads. Any generated provider configuration lives in the private profile.

[Providers](providers.md) covers the runtime contract.

## Settings

`SettingsStore` owns revisioned global and workspace layers in `settings.db`, with session overrides and application snapshots in the current versioned history database. Its connection opens the history database as main and attaches `settings.db` after it, because SQLite's `BEGIN IMMEDIATE` locks every writable database in attachment order and every history transaction locks history first; opposite orders in two hosts once waited each other out until the five-second busy timeout failed both. History connections attach `settings.db` read-only, so their write transactions never lock it, and a preference record commits through its own settings connection. A configuration commit that meets contention retries until its command's deadline, and contention that outlasts it fails the command as `command_storage_busy` without closing the session's command queue. Unspecified fields inherit, and provider tuning stays scoped to its backend. New sessions take model, thinking, context, speed, and executor defaults from the more recent of two sources: the workspace preference record for that workspace and provider, which a session's applied configuration writes with the settings revision current at its commit (`HarnessPreference::confirmed_revision`), and the global provider layer, which only an explicit `settings.update` with `provider_settings` writes, stamping its revision (`SettingsSnapshot::workspace_choice_is_newer`). A session configuration never writes the global provider layer except for the Ask model, so a choice in one workspace cannot reach another workspace's new sessions. A workspace record without a confirmed revision, such as the mirror a first session writes, yields to the global layer. `settings.update` and `settings.reset` require the expected revision and apply through task transition admission. A mutation commits its revision and operation receipt together, so a retry with the same operation ID recovers instead of applying twice, and a reused ID with different content is rejected. Configuration timeline records name changed fields, never values.

New sessions resolve saved defaults, and resumed sessions keep their own model and tuning. A session's `/config` Permissions edits (access, directories, sandbox, approval mode, and both timeouts) and its permission commands write that session's layer; only `settings.update` writes the global defaults. A running session reads its timeouts and access from its own record, so another editor's change to the defaults reaches only sessions started afterwards. Configuration authority compares the global settings revision only for a command that applies an explicit `settings_revision`; other configurations merge their explicit fields at the current revision, so one editor's change cannot fail another session's configuration in flight. Native forks copy explicit session overrides into an independent layer.

Executor and Background admission retain a catalog-validated model selection before committing it. Explicit model, reasoning, and service-tier requests must be supported; an empty reasoning catalog permits the provider default, not arbitrary levels. An executor inherits an accelerated tier only when its target model supports it, otherwise it uses standard processing. Both flows clear context-window overrides from the prior model. Background setup stores only default placeholders until validation succeeds, and generation requires the validated selection rather than raw configuration.

Direct field edits merge into the latest settings layer inside an immediate SQLite transaction. They do not reuse a revision read before another request can commit. Whole-layer replacement retains its expected-revision check. Timeout edits serialize persistence and runtime publication per session without acquiring the active task or broker lock.

The global-defaults editor removes JSON nulls for presentation while preserving dictionary markers. Saving a portable setting resends the latest global layer, so empty skill and MCP selections must remain JSON objects; encoding them as arrays rejects the whole update before persistence.

## Resources

`ResourceCatalog` reads MCP definitions from `config_dir/rulesync-global/.rulesync/mcp.json` and the workspace `.rulesync/mcp.json`, and skills from the matching Rulesync skill directories, without prompt bodies or credentials. Each entry carries `Global`, `Workspace`, or `Provider` scope. Applying a native import requires the reviewed fingerprint and preference revision, and imported MCP values keep references into the original file, so Forge never copies literal credentials.

A missing credential or stdio command marks only that server unavailable and logs `mcp.start_skipped`. Each provider receives selected resources through an isolated profile:

| Provider | Isolation |
| --- | --- |
| Claude | Strict MCP configuration and an ephemeral `forge-managed` plugin. |
| Antigravity | A private CLI profile with storage selected by the conversation's lifetime. |
| Copilot | An isolated skill directory, and `session.mcp.startServer` with at most four concurrent starts and a 30-second deadline each. |
| Codex | A private app-server runtime with inherited MCP and plugin resources disabled. |

The Antigravity profile selects a uniquely named Forge main agent whose native tool allowlist excludes the CLI scheduler. It disables customization inheritance for that agent. ACP's slash-command discovery can still find workspace skills, so Forge excludes every managed name through the native manifest and stages enabled `SKILL.md` entrypoints in the private shared-skill directory. The CLI gives those regular files precedence over the workspace and manifest exclusions. The main agent receives each canonical bundle directory for supporting files and scripts. ACP can still discover workspace skills outside the Forge catalog. [Antigravity customization discovery](https://antigravity.google/docs/customization-discovery?tab=cli)

Forge passes selected MCP servers at open or resume. The CLI resolves its sign-in credentials, and the private profile receives supported authentication settings and restrictive native policies from the user's settings file. The CLI writes its profile settings outside the shared native history directory. [Antigravity ACP authentication](https://antigravity.google/docs/ide/extensions/acp/#authentication)

A durable conversation links its native history to Forge's current state version so retiring its private profile preserves its history. Temporary sessions, including workspace model discovery and isolated generation, keep native history inside the private profile. Forge removes that profile after it collects the process.

Forge serializes session resource edits with native open. An unsupported edit is rejected before Forge saves settings or interrupts a turn, including while a retained native open is waiting for a new caller after Stop. The retained attempt owns its resource selection until the adapter establishes its outcome. An exact retry publishes the selection from that attempt.

Detached requests can save global or workspace resource defaults for subsequent opens. An immutable binding keeps its opened selection for pickers, skill grants, later model or access changes, and effective settings confirmation. Resource notices describe that selected generation. An incompatible Antigravity server stays visible with an unavailable reason, while other servers can launch. MCP discovery observations belong to their workspace or exact binding, so opening the first native session discards an earlier workspace observation.

`ResourceProfile` links directories with NTFS junctions or symlinks and rejects traversing names. MCP launch values are redacted from traces. Selected skills receive read and script grants covering one canonical bundle, revoked when the execution ends.

Antigravity applies the initial skill and MCP selections when it opens a binding. Resource queries read those selections from the binding. When the capability manifest marks live resource edits as unsupported, the pickers display the opened selection and address future-default edits to the resource's global or workspace settings layer without a session ID.

Antigravity accepts local stdio and remote Streamable HTTP MCP launches. Forge reports a selected legacy HTTP+SSE server as unavailable during preparation. [Antigravity MCP transports](https://antigravity.google/docs/ide-mcp#antigravity-cli)

## ACP instructions

Antigravity sends base instructions through its negotiated `_meta.antigravity.systemPrompt.append` extension on open and resume. A fresh conversation with native append receives its base instructions through that extension. A resumed conversation also receives those instructions in each ordinary primary prompt, because the native CLI may retain its original system prompt. A conversation without native append receives the same prompt prelude. Bundled `agy acp` can compact silently, so an accepted prompt establishes delivery for that prompt alone. Harness standing sections travel separately, before the user's text. [Turn context](providers.md#turn-context) · [Antigravity compaction reports](https://github.com/google-antigravity/antigravity-cli/issues/1509)

## Models

`ProviderModel` carries the identifier, display label, reasoning choices, context-window tiers, vision capability, and description used by the pickers. The editor submits the identifier and displays the provider's label. The first runtime observation replaces the workspace catalog with one from the live binding, and a model or reasoning change refreshes that binding's catalog. The editor retains the last labels while refreshing and gives pending picker requests the new catalog. Global defaults use workspace discovery and expose Thinking when the provider and catalog support it. When catalog discovery fails during configuration, Forge reports the normalized provider reason and keeps the existing settings active.

Antigravity publishes the model catalog in a session response. A workspace query without a live binding launches a disposable process with its own profile, opens a session with an empty MCP list, and reads the catalog without sending a prompt. Forge then collects the session, process, and profile. Claude's discovery process runs with `--no-session-persistence` so it leaves no transcript.

Complete configuration writes go through `thread_configure` after foreground collection. Its confirmed settings and capabilities publish together after the owner consumes the returned observation watermark. Codex resolves `default` through its catalog and sends the requested model explicitly on thread creation, resume, and turn start. Requested defaults and invocation overrides remain separate from the confirmed model retained on each turn. Only `Turn.ModelResolved(ResolvedModel { model_id, observed_at_ms })` supplies that confirmed identity. A turn begins without one, and providers that never positively confirm a model leave it absent. Copilot takes context capacity from `capabilities.limits.max_context_window_tokens`, never from billing thresholds.

Codex maps each model's `serviceTiers` `priority` ID to Forge's `fast` choice, and selectors offer only tiers the model advertises. `ModelPinStore` keeps pins per backend, changed through `provider.model.pin.set` outside the broker lock.

Known unsupported controls reject before adapter allocation or native changes. Inheritance resolves unavailable access choices against capabilities; explicit changes reject unsupported choices. Copilot requires a discovered default-model entry for omitted model selection. Empty discovery cannot manufacture a model or authorize unverified session creation.

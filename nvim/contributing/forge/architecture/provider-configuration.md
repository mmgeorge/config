# Provider configuration

Settings that change mid-turn, or credentials copied into provider files, would make a session's behavior depend on when and where it started. Forge therefore keeps settings in one revisioned store, gives each provider an isolated resource profile, and resolves credentials only into ephemeral launch payloads. No native configuration file is written.

[Providers](providers.md) covers the runtime contract.

## Settings

`SettingsStore` owns revisioned global, workspace, and session layers in `settings.db`, read from one committed snapshot. Unspecified fields inherit, and provider tuning stays scoped to its backend. `settings.update` and `settings.reset` require the expected revision and apply through task transition admission. A mutation commits its revision and operation receipt together, so a retry with the same operation ID recovers instead of applying twice, and a reused ID with different content is rejected. Configuration timeline records name changed fields, never values.

New sessions resolve saved defaults, and resumed sessions keep their own model and tuning. Native forks copy explicit session overrides into an independent layer.

Executor and Background admission retain a catalog-validated model selection before committing it. Explicit model, reasoning, and service-tier requests must be supported; an empty reasoning catalog permits the provider default, not arbitrary levels. An executor inherits an accelerated tier only when its target model supports it, otherwise it uses standard processing. Both flows clear context-window overrides from the prior model. Background setup stores only default placeholders until validation succeeds, and generation requires the validated selection rather than raw configuration.

Direct field edits merge into the latest settings layer inside an immediate SQLite transaction. They do not reuse a revision read before another request can commit. Whole-layer replacement retains its expected-revision check. Timeout edits serialize persistence and runtime publication per session without acquiring the active task or broker lock.

## Resources

`ResourceCatalog` reads MCP definitions from `config_dir/rulesync-global/.rulesync/mcp.json` and the workspace `.rulesync/mcp.json`, and skills from the matching Rulesync skill directories, without prompt bodies or credentials. Each entry carries `Global`, `Workspace`, or `Provider` scope. Applying a native import requires the reviewed fingerprint and preference revision, and imported MCP values keep references into the original file, so Forge never copies literal credentials.

A missing credential or stdio command marks only that server unavailable and logs `mcp.start_skipped`. Each provider receives selected resources through an isolated profile:

| Provider | Isolation |
| --- | --- |
| Claude | Strict MCP configuration and an ephemeral `forge-managed` plugin. |
| Antigravity | A private `GEMINI_HOME`. |
| Copilot | An isolated skill directory, and `session.mcp.startServer` with at most four concurrent starts and a 30-second deadline each. |
| Codex | A private app-server runtime with inherited MCP and plugin resources disabled. |

`ResourceProfile` links directories with NTFS junctions or symlinks and rejects traversing names. MCP launch values are redacted from traces. Selected skills receive read and script grants covering one canonical bundle, revoked when the execution ends.

Antigravity supports initial resource selection and discovery, but its adapter cannot replace skill or MCP selections on an existing binding. The capability manifest marks those mutation operations unsupported independently of discovery.

## ACP instructions

ACP base instructions use the negotiated session-level `systemPrompt` append capability when available. The retained native configuration carries those instructions through endpoint and resource reloads; a fork supplies its target instructions rather than copying its source's instructions. Resumed conversations also receive a prompt prelude because the native CLI may retain its original system prompt until compaction. Without native append support, the adapter retains base instructions and prepends them to ordinary prompts. Dynamic invocation instructions remain separate. Native fork support is advertised only when target instructions can be preserved; unsupported instruction-bearing forks reject before native dispatch.

## Models

`ProviderModel` is the provider-neutral picker contract: identifier, display label, reasoning choices, context-window tiers, vision capability, and description. The identifier is the value selection sends back to the provider. The label is the provider's display name, which views show in place of the identifier: Claude Code supplies `displayName` from its initialize response, Codex supplies `displayName`, and Copilot supplies `name`. The winbar labels the confirmed model from the session's cached catalog, which `runtime_resolved` loads once a provider session exists. Opening the picker reloads the read-only catalog for an explicit workspace or exact live binding. Discovery cannot open a writable session or apply settings. ACP publishes models only in a session response, so a workspace query without a live binding launches a disposable adapter, opens a session with no tools, MCP servers, or instructions, reads its catalog, and collects the process; Claude's discovery process runs with `--no-session-persistence` so it leaves no transcript. Complete configuration writes go through `thread_configure` after foreground collection; its confirmed settings and capabilities publish together after the owner consumes the returned observation watermark. Codex resolves `default` through its catalog and sends the requested model explicitly on thread creation, resume, and turn start. Requested defaults and invocation overrides remain separate from the confirmed model retained on each turn. Only `Turn.ModelResolved(ResolvedModel { model_id, observed_at_ms })` supplies that confirmed identity. A turn begins without one, and providers that never positively confirm a model leave it absent. Copilot takes context capacity from `capabilities.limits.max_context_window_tokens`, never from billing thresholds.

Codex maps each model's `serviceTiers` `priority` ID to Forge's `fast` choice, and selectors offer only tiers the model advertises. `ModelPinStore` keeps pins per backend, changed through `provider.model.pin.set` outside the broker lock.

Known unsupported controls reject before adapter allocation or native changes. Inheritance resolves unavailable access choices against capabilities; explicit changes reject unsupported choices. Copilot requires a discovered default-model entry for omitted model selection. Empty discovery cannot manufacture a model or authorize unverified session creation.

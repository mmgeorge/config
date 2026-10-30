# Provider configuration

Settings that change mid-turn, or credentials copied into provider files, would make a session's behavior depend on when and where it started. Forge therefore keeps settings in one revisioned store, gives each provider an isolated resource profile, and resolves credentials only into ephemeral launch payloads. No native configuration file is written.

[Providers](providers.md) covers the runtime contract.

## Settings

`SettingsStore` owns revisioned global, workspace, and session layers in `settings.db`, read from one committed snapshot. Unspecified fields inherit, and provider tuning stays scoped to its backend. `settings.update` and `settings.reset` require the expected revision and apply through task transition admission. A mutation commits its revision and operation receipt together, so a retry with the same operation ID recovers instead of applying twice, and a reused ID with different content is rejected. Configuration timeline records name changed fields, never values.

New sessions resolve saved defaults, and resumed sessions keep their own model and tuning. Native forks copy explicit session overrides into an independent layer.

## Resources

`ResourceCatalog` reads MCP definitions from `config_dir/rulesync-global/.rulesync/mcp.json` and the workspace `.rulesync/mcp.json`, and skills from the matching Rulesync skill directories, without prompt bodies or credentials. Each entry carries `Global`, `Workspace`, or `Provider` scope. `/config` also offers an explicit native import. Applying it requires the reviewed fingerprint and preference revision, and imported MCP values keep references into the original file, so Forge never copies literal credentials.

A missing credential or stdio command marks only that server unavailable and logs `mcp.start_skipped`. Each provider receives selected resources through an isolated profile:

| Provider | Isolation |
| --- | --- |
| Claude | Strict MCP configuration and an ephemeral `forge-managed` plugin. |
| Antigravity | A private `GEMINI_HOME`. |
| Copilot | An isolated skill directory, and `session.mcp.startServer` with at most four concurrent starts and a 30-second deadline each. |
| Codex | A private app-server runtime with inherited MCP and plugin resources disabled. |

`ResourceProfile` links directories with NTFS junctions or symlinks and rejects traversing names. MCP launch values are redacted from traces. Selected skills receive read and script grants covering one canonical bundle, revoked when the execution ends.

## Models

`BackendModel` is the provider-neutral picker contract: identifier, reasoning choices, context-window tiers, vision capability, and description. Opening the picker reloads the catalog. Codex resolves `default` through its catalog and sends the resolved model explicitly on thread creation, resume, and turn start, so the displayed and running models match. Copilot takes context capacity from `capabilities.limits.max_context_window_tokens`, never from billing thresholds.

Codex maps each model's `serviceTiers` `priority` ID to Forge's `fast` choice, and selectors offer only tiers the model advertises. `ModelPinStore` keeps pins per backend, changed through `provider.model.pin.set` outside the broker lock.

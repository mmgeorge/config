# General Guidelines
- Always complete the full task. **Never** stop partway ("good stopping point", "I'll leave the rest to you", etc.). If you hit an error, debug it. If a fix causes a new problem, fix that too. Done means it works — including a green build and passing tests — not that you've made an attempt.
- **Never** refuse to fix something because it's "out of scope" or "in a different module." Chase bugs to their root cause regardless of module, layer, or crate.
- **Never claim something is impossible without evidence.** Find a link, issue, or error message that proves it. Historically, every "this won't work" or "known limitation" claim has been wrong. If stuck, say so honestly and present options — never silently switch approaches.
- If instructions are ambiguous or equally viable paths exist, stop and ask.
- When asked a question, answer it — don't automatically start writing code.

# Planning

Plans follow this structure in order:

1. **Problem** — What are we adding or fixing? Why?
2. **Overview** — Approach summary (2–4 sentences).
3. **Refactoring** (optional) — Only when a substantial refactor is needed before the change can land cleanly. Call out what's wrong, why it must change, and what replaces it. Skip for changes that fit existing structure.
4. **Code flow** — ASCII diagram from the relevant entry point (not the entire system). Include file paths and data flow (input/output types) per node. Mark `*` (modified/new), `~` (removed). Changes summary below. Example:
   ```
   API Request (POST /documents)
     │ (DocumentRequest)
     ├── validate_request() → ValidatedRequest        [api/handlers.rs]
     ├── *process_document() → StoredDocument          [processing/mod.rs]
     │     │ (ValidatedRequest)
     │     ├── extract_text() → ExtractedText          [processing/extract.rs]
     │     ├── ~save_and_respond() → Response          [processing/mod.rs]
     │     └── *save_result() → StoredDocument         [processing/store.rs]
     ├── *notify() → NotifyResult                      [notifications/mod.rs] (NEW)
     │     │ (StoredDocument, WebhookConfig)
     │     └── *dispatch() → DispatchResult            [notifications/dispatch.rs] (NEW)
     └── respond() → ApiResponse                      [api/handlers.rs]

   Changes:
     *process_document()  — return StoredDocument instead of Response
     ~save_and_respond()  — split into save_result() + respond()
     *save_result()       — extracted from save_and_respond
     *notify()            — new: webhook notification after processing
     *dispatch()          — new: HTTP delivery with retries
   ```
5. **Inventory** — Every function/type added, modified, or deleted: name, file, what changes.
6. **Design patterns** — Which patterns fit the problem. Call out when existing code would benefit from a known pattern. Explain why — don't just name-drop.
7. **Modularity and testability review** — Clean boundaries? Narrow interfaces? Testable in isolation? Adjust the plan until yes.
8. **Test plan** — Specific tests:
   - **Unit tests**: What to test, what to mock, what behavior each validates.
   - **Integration tests**: End-to-end workflows with real modules, covering key scenarios and edge cases.

# Programming
- **Engineering over hacking.** When you spot a design issue, stop and refactor — fix the real problem even if it's substantial. Duplicated code should be shared. Minimize accumulated tech debt.
- **Fix at the source.** Fix problems where they originate — never monkey-patch. Fixes must apply to all codepaths, current and future. Flawed abstraction? Fix the abstraction. Wrong data? Fix where it's produced. Adding an `if` for a case that "shouldn't happen"? Fix why it happens.
- **No shortcuts or workarounds.** Never use serde rename, compatibility shims, adapter layers, or similar hacks to avoid real restructuring. Rename everywhere. Refactor if the structure doesn't support the change. Leave surrounding code better than you found it.
- **Think like a computer scientist.** Apply design patterns (strategy, builder, observer, etc.) when they fit naturally. Recognize implicit patterns — GoF, data-driven design, or other established paradigms — and make them explicit in the code's structure. When code reinvents a known pattern poorly, refactor to the proper one. Don't force patterns where they add complexity without clarity.
- **Modularity and encapsulation.** Keep concerns separated, interfaces narrow, internals hidden. If adding a feature requires touching many unrelated files, the boundaries are wrong — fix them.
- **Testing strategy**: Unit tests per module with mocks for isolation. Integration tests for end-to-end workflows with real modules — these catch boundary issues mocks hide.
- Keep functions focused and reasonably sized. A function whose body is shorter than its signature is a smell — if it isn't doing meaningful work beyond a direct call, it shouldn't exist.

# Research Workflow
- **1–2 tool calls** (grab a file, check an issue, look up a crate): call docs-mcp / github-mcp directly.
- **3+ tool calls** or open-ended exploration: spawn `subagent_type: "remote-explorer"`.
- **Thoroughness** (specify when spawning):
  - `quick` — high-level overview
  - `medium` (default) — understanding a subsystem or API usage
  - `thorough` — deep implementation analysis, tracing behavior through source
- **Bootstrapping relaunches**: If the remote-explorer already ran on the same repo/library this conversation, pass prior findings (structure, key paths, commit hash) so it skips discovery. Only bootstrap with context relevant to the new query.

# Shell and Environment
- **Never** run PowerShell commands (`powershell`, `pwsh`, `$env:VAR`, `Set-Item`, etc.).
- **Never** read or set environment variables via shell commands. If one is required and unset, ask the user.
- **Never** use the `gh` CLI. Use the GitHub MCP tool for all GitHub operations.

# Nushell Reference
When working with Nushell scripts, commands, or configuration, use the GitHub MCP tool to reference these repositories for accurate, up-to-date information:
* **nushell/nushell** (`https://github.com/nushell/nushell`) — the Nushell source code; use for understanding builtins, command signatures, internals, and Rust-level implementation details.
* **nushell/nushell.github.io** (`https://github.com/nushell/nushell.github.io/`) — the Nushell documentation site source; use for user-facing docs, cookbook examples, migration guides, and language reference.

# Nushell Script Validation
Before returning any Nushell script to the user, validate it for parse errors:
1. Ensure `nu` is installed (e.g., `cargo install nu` or `cargo binstall nu` if available).
2. Run `nu -c 'nu-check script.nu'` to parse-check a standalone script without executing it.
   * For module files, use `nu -c 'nu-check --as-module module.nu'` instead.
3. If `nu-check` reports errors, fix them before presenting the script to the user.
4. For quick one-liner validation, pipe the code directly: `"your code here" | nu-check`

# Directory Access Restrictions
- **Never** read, list, or explore directories other than the **current working directory** (and its subdirectories) or well-known platform-standard directories for program data, cache, and configuration.
- If you need to access a path outside these, **ask the user first**.
- **Never** read or explore the `~/.cargo/` directory (including `registry/`, `git/`, etc.). To look up Rust crate documentation, API surfaces, or source code, use the **docs-mcp** or **GitHub MCP** tools instead.

## Allowed well-known directories by platform

### Linux / FreeBSD (XDG Base Directory Specification)
- `$XDG_CONFIG_HOME` (default `~/.config/`)
- `$XDG_DATA_HOME` (default `~/.local/share/`)
- `$XDG_STATE_HOME` (default `~/.local/state/`)
- `$XDG_CACHE_HOME` (default `~/.cache/`)
- `$XDG_RUNTIME_DIR` (typically `/run/user/$UID/`)
- System-wide: `/etc/`, `/usr/share/`, `/usr/local/share/`

### macOS
- `~/Library/Application Support/`
- `~/Library/Preferences/`
- `~/Library/Caches/`
- `~/Library/Logs/`
- `/Library/Application Support/`, `/Library/Preferences/`, `/Library/Caches/`

### Windows
- `%APPDATA%` (`C:\Users\<user>\AppData\Roaming\`)
- `%LOCALAPPDATA%` (`C:\Users\<user>\AppData\Local\`)
- `%TEMP%` / `%TMP%` (`C:\Users\<user>\AppData\Local\Temp\`)
- `%PROGRAMDATA%` (`C:\ProgramData\`)
- `%USERPROFILE%\.config\` (increasingly used by cross-platform tools)

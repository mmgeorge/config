# Agent LSP tools

Enable **LSP tools** in `/config` to give Harness agents read-only access to
Neovim's project language servers. The setting defaults to off and uses
the existing global, workspace, and session preference layers. Disabling it
revokes agent queries without stopping language servers or disabling plan-review
navigation. Enabled turns receive short guidance recommending semantic queries.

Harness exposes one tool, `harness_lsp`, with an `operation` field:

| Operation | Required fields | Purpose |
| --- | --- | --- |
| `status` | None | List running project servers and supported operations |
| `definition` | `path`, `line`, `column` | Find a symbol's definition |
| `type_definition` | `path`, `line`, `column` | Find its type declaration |
| `implementation` | `path`, `line`, `column` | Find implementations |
| `references` | `path`, `line`, `column` | Find uses, excluding the declaration |
| `hover` | `path`, `line`, `column` | Read type information and documentation |
| `document_symbols` | `path` | Read the file's declaration outline |
| `workspace_symbols` | `query` | Search project symbols by name |
| `calls` | `path`, `line`, `column`, `direction` | Read one level of incoming or outgoing calls |
| `diagnostics` | `path` | Read file errors and warnings |

Paths may be workspace-relative or authorized absolute paths. Lines and columns
start at one. Columns count Unicode code points, not bytes or display cells.
`direction` is `incoming` or `outgoing`. An optional `path` selects the server for
status or workspace search. An optional `query` filters document symbol names by
literal, case-insensitive substring.

List operations accept `limit` from 1 through 50, defaulting to 20, and a
zero-based `offset`. Follow the returned `next_offset` only when more results are
needed. Each page reruns the query against current source. There is no retained
result cache. Replies contain at most 8 KiB, and hover text contains at most 4 KiB.
Locations include one source line or a symbol description rather than complete
function bodies. Truncation and incomplete results are explicit.

Queries use current editor contents, including unsaved buffers. Results identify
unsaved source. Changing source while a request runs invalidates that request.
Supported operations depend on the selected server. A missing server, ambiguity,
timeout, and an empty result are distinct outcomes. At most four agent requests
run concurrently per host.

Opening Harness starts enabled Neovim LSP configurations in the background when
matching real source files are found. It uses the same configurations, root
resolution, and client reuse as the editor. Opening those files in the editor
reuses the server for that project. Startup scans at most 4,096 entries in 128
directories, to depth eight, excluding dependency and build directories. A query
for an explicit project file also prepares its matching configuration.
Discovery resolves source candidates through the configured server's root
callback before starting a client. It prefers the repository's own workspace,
then its closest nested workspace, so a separate tools project does not take
priority over the application. An explicit query into an independent workspace
can still require its own server.

Queries wait up to 60 seconds for startup and initialization, then have a separate
ten-second operation deadline. Initialization does not guarantee that a server
has finished indexing its workspace. There is no standard LSP indexing-complete
signal. Startup failures are reported as failures, never empty results.

Pull diagnostics describe the captured source version. Push-only diagnostics use
Neovim's existing store and report unknown freshness when freshness cannot be
proved. Empty, stale, or unknown diagnostics do not establish a successful
project typecheck. Continue using configured Harness checks for validation.

Harness does not install dependencies, create projects, apply edits, or execute
arbitrary server commands through this tool. Existing project and
dependency source remains subject to the active read policy. Completion, rename,
code actions, and automatic diagnostic injection are outside this feature.

# Agent LSP tools

Enable **LSP tools** in `/config` to expose eleven read-only tools and two rename tools to root and child
agents. Semantic commands require `workspace: { path, language }`, where `path`
is a project directory relative to the session repository and `language` is
`rust`, `typescript` (including JavaScript), or `lua`. Every request uses one LSP.
`lsp_workspace_list` lists project directories and language keys without starting servers; `lsp_source_symbols`
searches parsed source without an LSP. The setting defaults to off. Disabling it revokes agent requests without
stopping editor servers or disabling plan navigation.

Select the Neovim configuration with `require("forge").setup({ lsp = {
workspace_servers = { rust = "rust_analyzer", typescript = "tsgo", lua = "lua_ls" } } })`
when more than one enabled configuration matches a language. These values name
existing configurations; Forge preserves their settings and commands. A sole
matching configuration needs no preference. An ambiguous selection starts none.

All semantic tools, including file-target navigation, diagnostics, status, and
renames, require `workspace`. Paths remain relative to the session repository.
`lsp_context` without workspace accepts only a local file target with no semantic
relationships. Status inspects the selected server without starting it.

Workspace listing returns the exact path and language keys accepted by semantic tools:

```text
nvim                      lua
nvim/rust/forge           rust
nvim/tools/markdown-math  typescript
```

For example, `lsp_symbols({ workspace = { path = "nvim", language = "lua" },
query = "request" })` selects the configured Lua server. The caller never names
that server. Directories have no source coordinates in the listing.

## Choose a tool

Use overview or symbols to discover names and positions, then reuse either with
context or a semantic query. No tool moves the editor cursor.

| Tool | Input | Result |
| --- | --- | --- |
| `lsp_overview` | Directory `path` (default `.`), `depth=2`, `declaration_depth=2`, `visibility=public` | Public declarations, fields, and methods grouped by directory and file |
| `lsp_workspace_list` | Optional directory `path` | Project directories and language keys |
| `lsp_source_symbols` | File/directory `path`, optional `query`, `depth`, `visibility` | Parsed source declarations |
| `lsp_symbols` | Required `workspace` and `query` | Symbols from one selected language server |
| `lsp_definition` | Target selector, `kind=definition` | Definitions, type declarations (`type`), or implementations (`implementation`) |
| `lsp_context` | Target selector, optional `include`, optional `start_line` | Containing declaration, source documentation, and body |
| `lsp_references` | Target selector, `include_declaration=false` | Uses grouped by enclosing declaration, with one source line before and after |
| `lsp_calls` | Target selector, required `direction=incoming` or `outgoing` | One level of callers or callees in an aligned symbol tree |
| `lsp_paths` | `from` and `to` selectors, `max_depth=5`, `max_paths=3`, `max_requests=64` | Complete static call paths in aligned trees |
| `lsp_diagnostics` | Optional file/directory `path`, `severity=all` | File diagnostics, or workspace reports with per-server coverage when path is omitted or a directory |
| `lsp_status` | Optional `path` | Selected server root, initialization state, and supported capabilities |
| `lsp_rename_symbol` | Target selector and required `new_name` | Apply a symbol rename across files |
| `lsp_rename_file` | Required `old_path`, `new_path` | Move a file or directory and update references |

Source paths are relative to the session repository, not the selected project,
or are authorized absolute paths. Lines and columns start
at one. Columns count Unicode code points, not bytes or screen cells. A returned
identifier position can be passed directly to another tool.

Source symbol searches with an explicit file or directory filter captured source
declarations before pagination. Name matching uses a case-insensitive substring of
the short or qualified name. A semantic `lsp_symbols` query searches the selected language
server's workspace symbols. `scope=all` can then include authorized dependency
symbols outside the workspace.
Overview and source-scoped symbol listings parse source directly, without starting
language servers or waiting for indexing.
Context with a local target also reads source directly; requested relationships
and workspace name searches wait for the relevant servers. Optional hover is
included only when its server is already ready. Semantic queries reuse server
readiness within one tool call and check it again on the next call.
Workspace listing excludes nested Git repositories and linked worktrees. List a
nested checkout explicitly to discover its projects. Cargo.toml and package.json
identify Rust and TypeScript/JavaScript project directories. Lua discovery uses
authorized Lua files and the enabled Lua configuration's `root_dir` or
`root_markers`, then deduplicates resolved roots. Root callbacks use a 60-second
deadline. Missing roots and unavailable or ambiguous configurations appear as
explicit discovery warnings. A semantic request starts only the selected configuration.
For static root markers, discovery resolves each source directory once; custom
root callbacks run for each file because their result may depend on that file.
Rust searches request all symbol kinds from rust-analyzer rather than its default
type-only search. Missing paths are rejected before language-server startup and
do not affect later valid requests.

### Target selectors

Definition, context, references, calls, and symbol rename share these selectors. Paths use the
same selector independently for `from` and `to`.

| Selector | Resolution |
| --- | --- |
| `symbol` | Exact short or qualified name among authorized workspace declarations |
| `path`, `symbol` | Exact name within one source file, including private declarations |
| `path`, `line`, `symbol` | Unique identifier spelling on that line, resolved by LSP at its position |
| `path`, `line`, `column` | Exact source occurrence, retaining receiver and expression context |

A column requires a line. A line requires a file and either a symbol or column.
If symbol and column are both supplied, the spelling must cover the column.
Qualified names also must match the declaration resolved by LSP. Name-only lookup
accepts short names only when unique and never selects an approximate match.
Rust uses `Type::method`. TypeScript, JavaScript, and Lua use `Type.method`.
Rust impl blocks supply method qualification but do not create duplicate type targets.

Ambiguous selectors return candidates without running the requested operation:

```text
ambiguous_symbol: 2 matches
  ResourceRegistry::insert  engine/runtime/src/resource_registry.rs:21:10
  ResourceMap::insert       engine/runtime/src/resource_registry.rs:92:10
```

Missing names return `symbol_not_found`. Conflicting names and positions return
`target_mismatch`, including the actual declaration when resolved. Workspace name
lookup examines at most 250 server candidates. Incomplete searches report `partial`
and never imply that a name is absent or unique.

Content tools accept `token_budget`, default 2,000 and maximum 8,000 estimated
tokens. The estimate is UTF-8 byte count divided by four, rounded up, not a model
specific tokenizer. Replies also have a 64 KiB transport ceiling. List tools accept
any positive integer `limit` (default 20) and zero-based `offset`. Internal batches
and the response budget can return fewer rows than requested. Follow `next_offset`
when more results are needed. Context returns `next_line` for a truncated body.
Pass it as `start_line` while preserving the original target position. Pages rerun
against current source and retain no query-result cache.

Directory depth ranges from zero (immediate files) through eight. Declaration depth
ranges from one (top-level declarations) through eight. Visibility is `public`,
`internal`, `private`, or `all` and describes source declarations, not external
reachability. Restricted Rust modifiers remain visible in signatures. Unknown
classification is labeled under `all` and omitted under strict filters. The reply
lists omitted declaration names and positions within its response budget, plus
the total omitted count. Necessary structural parents remain visible for matching
descendants.

Symbols, references, calls, and paths accept `scope=workspace` (default) or `all` to
include authorized dependency results. Ordinary workspace filtering does not
produce permission warnings. `lsp_context.include` optionally requests
`references`, `callers`, `callees`, or `implementations`. Each adds at most five
signatures within the same budget. Related context is not requested by default.

## Output

Callable context uses the source signature and body without repeating server hover
metadata. Other declaration kinds may include supplementary type information.
Definition results that start on declaration attributes point to the identifier.

The location gutter stays aligned across declaration nesting. Source excerpts omit
columns. `>` marks a reference line. Overlapping excerpts merge.
There are no pipe separators or tree-drawing characters.

Overview shows headers and nesting without bodies:

```text
src/
  storage/
    file_store.rs
     12:12   pub struct FileStore
     13:9      pub root: PathBuf
     14:9      pub options: StoreOptions
     20:6    impl FileStore
     21:12     pub fn new(root: PathBuf) -> Self
     30:12     pub fn flush(&mut self) -> Result<()>
```

Symbols use the same header format, without directory expansion:

```text
src/store.rs
      5:11   pub trait Store
      8:8      fn save(&self, session: &Session) -> Result<()>
```

Definitions show each destination's signature and exact identifier position:

```text
src/store.rs
      5:11   pub trait Store
```

Context includes the containing declaration's source:

```text
src/session.rs
     70:4    fn persist_session(store: &dyn Store) -> Result<()> {
     71          let session = prepare_session();
     72          store.save(&session)?;
     73          Ok(())
     74      }
```

References show the enclosing signature once, without repeating the query name
or its known definition:

```text
src/session.rs
     70:4    fn persist_session(store: &dyn Store) -> Result<()>
     71          let session = prepare_session();
>    72          store.save(&session)?;
     73          Ok(())
```

Call trees show the selected symbol at the root and one level of related symbols.
For `direction=incoming`, children call the root:

```text
Store::save         src/store.rs:12:8
  persist_session   src/session.rs:70:4
  shutdown          src/app.rs:40:4
```

For `direction=outgoing`, the root calls its children:

```text
persist_session    src/session.rs:70:4
  prepare_session  src/session.rs:20:4
  Store::save      src/store.rs:12:8
```

Locations identify symbol definitions, not call sites. The language server's
prepared symbol determines the root even when the request points at a call site.
Names include containing declarations when structural analysis resolves them.
The location column aligns across the returned tree. An empty hierarchy shows
only its root. Pagination and scope apply to children, and each page repeats
its authorized root. An empty hierarchy also reports that no incoming or outgoing
calls were found in the selected scope.
This tool does not recursively expand children or show a runtime call stack.

`lsp_paths` follows outgoing LSP call hierarchy edges between resolved callable
endpoints. Both occurrence selectors and declaration selectors are supported:

```json
{
  "from": {"path": "src/app.rs", "symbol": "App::save"},
  "to": {"path": "src/store.rs", "symbol": "Store::write"},
  "max_depth": 5,
  "max_paths": 3
}
```

```text
App::save         src/app.rs:20:8
  Document::save  src/document.rs:40:8
    Store::write  src/store.rs:12:8
```

Each separated tree is one complete path. Depth counts call edges and ranges
from 1 to 8. The path limit ranges from 1 to 10. The native request limit ranges
from 2 to 128 and includes name lookup, endpoint preparation, pagination, and
retries. Search state contains at most 512 routes and lives only for this query.
The query has a 30-second deadline after startup. Cycles are excluded within each
route. Request, route, and output limits report `partial` without emitting unfinished paths.
A deadline expiration reports an explicit timeout.
No-match output states the searched depth. Missing call-hierarchy support reports
an error. Conditions, data flow, and dynamic dispatch can require source review:
a static call path does not establish that the path executes at runtime.

Diagnostics omit the requested filename and report freshness explicitly:

```text
freshness: current

     72:16   error[E0308]: expected &Session, found Session
     72          store.save(session)?;
```

Status reports readiness without claiming indexing is complete:

```text
rust-analyzer  ready  root=.
  overview, symbols, definition, type, implementation, context, references, calls, diagnostics
```

## Source and server lifecycle

Queries use current editor contents, including unsaved buffers. Any contributing
source change invalidates the request. Tree-sitter supplies structural context for
Rust, TypeScript/TSX, JavaScript/JSX/MJS/CJS, and Lua. LSP supplies semantic identity
and relationships. Missing capabilities, failures, and empty results are distinct.

Opening Harness and applying session snapshots do not start language servers.
A file request uses the selected project's configuration and validates that the
client serves the file's language and root. Workspace discovery excludes build
and dependency directories. Lua root resolution reuses Neovim's configured
marker precedence and root callbacks without launching a server.
Queries reuse editor clients already serving the same root, including clients
started by opening a source file. A client for an unrelated sibling project does
not receive a file request.
A server that declines a project root does not produce an initialization-timeout
notification. Source queries wait up to 60 seconds for initialization and any work-done progress
reported by the server, then have a ten-second operation deadline, or 30 seconds for path searches. Status reports
starting or indexing while those states are visible. Servers can index silently,
so readiness still cannot guarantee that every workspace result is complete.
Content-modified replies retry twice within the operation deadline when captured
source remains unchanged.

At most four agent requests run concurrently. Agent structural queries impose no
source-byte or source-line cutoff. Source transfers use ordered UTF-8 chunks
within the existing transport frame size, and transfer state belongs to the active
request. Rendering retains its independent 10,000-line cutoff. Explicit structural
queries use full-source parsing with the existing worker admission, deadline, and
cancellation behavior.

## Failure and partial-result semantics

Errors state the failed operation or target without suggested arguments, retry
instructions, or alternative tools. Valid primary results survive unavailable
optional context. Source changes and revoked authority still invalidate a request.

| Condition | Output |
| --- | --- |
| Missing source | `path_not_found: src/store.rs` |
| Invalid position | `invalid_position: src/store.rs:200:8 is outside the source.` |
| No enclosing declaration | `no_declaration: src/store.rs:20:1` |
| Non-callable path endpoint | `not_callable: Store` |
| Source changed | `source_changed: src/store.rs changed during the request.` |
| Unsupported capability | `unsupported: lua_ls does not support call hierarchy.` |
| Startup failed | `startup_failed: rust_analyzer — <reason>` |
| Server disconnected | `disconnected: rust_analyzer stopped during the request.` |
| Equal server matches | `ambiguous_server: first_server, second_server` |
| Read denied | `access_denied: src/store.rs` |
| Capacity occupied | `busy: All LSP request slots are occupied.` |
| Request limit reached | `partial: LSP request limit reached. Search incomplete.` |
| Route limit reached | `partial: Call-path route limit reached. Search incomplete.` |

Ordinary pagination reports `next_offset` or `next_line`. Empty successful results
name the operation and scope. Partial results never report definitive absence.
Unknown visibility lists are compact and bounded by the output budget:

```text
partial: visibility unknown for 2 omitted declarations
  Store::save   src/store.rs:21:8
  Store::flush  src/store.rs:35:8
```

Diagnostics freshness is `current`, `stale`, or `unknown`. Pull diagnostics describe
the captured source version. Push diagnostics retain unknown freshness when their
version cannot be proved. Empty diagnostics do not establish a successful project
typecheck. Use configured Harness checks for validation.

Omitting `path` from `lsp_diagnostics` queries the single server selected by
`workspace`. A directory `path` narrows those workspace results. Forge uses native
`workspace/diagnostic` when the server advertises it; otherwise it reads that
client's existing published diagnostics in Neovim, including reports for unopened
files. Results retain the server name and resolved project root, even when
`workspace.path` names a nested Cargo project, and respect read authorization.

Published snapshots report `coverage: partial` and `freshness: unknown`. An empty
snapshot means no matching published diagnostics are available; it does not
certify a clean workspace. Unsupported acquisition, failed requests, unavailable
servers, and empty snapshots have distinct output. The query never runs Cargo,
sends a refresh notification, or opens files to request diagnostics. For example,
`rust-analyzer.check.workspace = false` can leave package-scoped reports; Forge
preserves that setting and reports the snapshot's limits. Document queries keep
their existing pull or published-report path. Workspace-symbol queries use the
same server selection and coverage reporting.

## Rename files and symbols

Prefer `lsp_rename_symbol` for a symbol rename and `lsp_rename_file` for a file or directory move. The enabled LSP setting injects the LSP guidance immediately after Harness's system instructions, naming these tools and the semantic search tools. Read-only planning and question-discussion turns cannot invoke rename.

```json
{"workspace":{"path":".","language":"rust"},"path":"src/store.rs","symbol":"Store::save","new_name":"persist"}
```

```json
{"workspace":{"path":".","language":"rust"},"old_path":"src/store.rs","new_path":"src/storage.rs"}
```

Both tools write to disk. They require existing read and write permission for every affected path, including move destinations and directory descendants. Missing permission returns an error without opening an approval prompt. Saved tool denials still apply. Any affected unsaved buffer rejects the whole operation; save or discard those changes before requesting a rename.

Symbol rename uses `prepareRename` when supported and applies the server's complete workspace edit. That edit may include file creation, moves, or deletion. File rename requires a matching `workspace/willRenameFiles` provider and never falls back to a plain move. All matching providers must succeed, and incompatible edits reject the operation. A successful empty response means no reference edits are needed.

The host validates the complete operation sequence, snapshots affected resources, rechecks source freshness, and rolls back filesystem changes on an application failure. Rollback failures identify affected paths. Open clean buffers follow committed contents and names, and the change set is attributed to the native tool in the Harness timeline. Server file-operation notifications are sent only after commit.

Explicit file moves require a vacant destination. Server-returned operations honor their overwrite and ignore options. Cross-filesystem moves, symlink resources, non-file URIs, and nested resource operations returned by file-operation preparation are rejected. Output limits affect the summary only, never the applied edit set.

The tools do not install dependencies or execute arbitrary server commands. Completion and code actions remain outside this interface.

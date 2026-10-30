# ArcGIS Play MCP

*When to read this:* before connecting to, inspecting, editing, running, authenticating, or saving an ArcGIS Play document through `play-mcp`.

## Contents

- Connection model
- Session selection
- Read and edit workflow
- Save and refresh workflow
- Verification
- New documents, authentication, and privacy

## Connection model

The Play MCP bridge operates on live ArcGIS Play browser sessions. It cannot inspect an arbitrary browser tab that has not registered with the bridge.

1. Call `play-mcp:play_get_sessions`.
2. If it returns zero sessions, ask the user to open or reconnect ArcGIS Play.
3. Retry only after the user confirms the page has connected.

The MCP does not expose a create-new-document operation. To build a separate app without replacing current work, have the user open a blank Play app or scratch document first, then replace that connected document's buffers.

## Session selection

Inspect session metadata before reading or editing:

- `documentKind: "app"` identifies an owned app URL.
- `documentKind: "scratch"` identifies file, sample, or scratch-style work.
- Prefer the session whose `owner` matches `authUser` when updating an app.
- Confirm `isSignedIn` and `canSaveApp` before expecting an app save.
- Avoid preview or duplicate sessions when the user names a specific app.

When multiple sessions match ambiguously, ask the user to identify the target.

## Read and edit workflow

1. Call `play-mcp:play_best_practices` before making code changes.
2. Call `play-mcp:play_get` with `what: "both"` to inspect HTML, CSS, TypeScript, editor diagnostics, runtime events, and the active file.
3. Call `play-mcp:play_get_view_state` when current viewpoint, layer state, or map readiness matters.
4. For small documents, replace complete buffers with `play-mcp:play_set_code`.
5. For larger documents, use `play-mcp:play_get_range_info`, `play-mcp:play_find_text`, `play-mcp:play_read_text_range`, and context-verified `play-mcp:play_apply_text_patch` or explicit `play-mcp:play_apply_text_edits`.

Prefer `esri/...` ESM imports. Play rewrites them to the ArcGIS package. Do not add an ArcGIS loader, import map, Calcite CDN, or SDK stylesheet to the panes.

## Save and refresh workflow

After edits:

1. For an owned app edit, call `play-mcp:play_save` when the changes should persist. For an experiment, refresh and verify before saving.
2. If save returns `refreshed: true`, do not refresh again.
3. If save returns `refreshed: false`, call `play-mcp:play_refresh`.
4. Call `play-mcp:play_get` with `what: "diagnostics"` after the active run settles.

For exploratory changes that should not persist yet, refresh first, verify, then save only after the user-visible result works.

## Verification

Require all relevant evidence:

- The run started and reached `view-ready`.
- HTML, CSS, and TypeScript diagnostics contain no unexpected errors.
- Runtime events contain no unexpected error.
- The expected layer appears in `play_get_view_state`.
- The viewpoint, visibility, and layer type match the requested app.
- Schema-dependent status text, popup fields, or interactions complete after layer loading.

`view-ready` proves that the view initialized. It does not override blocking TypeScript diagnostics or a later asynchronous layer failure. Re-read diagnostics after late layer loads when the app performs substantial async work. If diagnostics return a suggested recheck delay, wait that delay and read once more. Do not poll indefinitely.

## New documents, authentication, and privacy

- The user must open a blank Play document before the MCP can build a separate app.
- Do not save an app session whose `owner` differs from `authUser` unless the user explicitly requested a scratch copy.
- If an app session needs authentication, tell the user a browser sign-in interaction is required, invoke `play-mcp:play_auth`, then continue only after authentication succeeds.
- Saving a scratch document may create a new Play file rather than update an existing app.
- ArcGIS Play runs on an Esri-hosted service. Do not place secrets, private tokens, sensitive source, or undisclosed private data in a Play document.
- Report the returned Play URL after a successful save when the user needs the durable document.

## Failure handling

- Zero sessions means disconnected, not an empty app.
- A missing DOM element should throw a specific error instead of silently skipping setup.
- Preserve item, service, CORS, authentication, layer-load, and renderer-field failures.
- If local source and Play disagree, record the exact diagnostic and treat it as runtime version skew. Do not force compilation with unsafe casts.
- Never replace the requested content with a public sample merely to make the view render.

# AI Prompt Library Notes

`require("ai")` is a small, generic prompt library: send a system + user
prompt to an LLM provider, get the response text back via an async callback.
It exists so features like commit-message generation do not depend on
chat-UI plugin internals (it replaced a hack on CodeCompanion's undocumented
background interaction). No streaming and no chat UI; `generate` is one
prompt in, one text out, and `generate_with_tools` adds a bounded
tool-calling loop on top.

`adapters.lua` is separate: it picks which adapter/model each AI feature uses
(CodeCompanion adapter configs for the chat UI, an `ai` model token for
commit generation).

## Public API

| Function | Purpose |
| --- | --- |
| `require("ai").generate(opts, cb)` | One-shot prompt -> text |
| `require("ai").generate_with_tools(opts, cb)` | Prompt with tool calling (native tools, registry tools, MCP) |
| `require("ai").resolve(token)` | Validate/resolve a model token without sending anything |
| `require("ai").set_backend(backend)` / `reset_backend()` | Test seam (HTTP, file reads, clock) |
| `require("ai.inline").inline(opts, on_done?)` | Prompt about a selection, splice the response into the buffer |
| `require("ai.tools").get(name)` | Resolve a registry key to a callable tool |
| `require("ai.tools").registry` | The named-tool registry table |
| `require("ai.mcp").tools(server_names)` | mcphub servers -> AITool list (used by the `mcps` option) |

All callbacks run on the main loop. All functions carry LuaLS annotations;
the shared classes (`AITool`, `AIToolCall`, `AIGenerateWithToolsOpts`, ...)
are defined at the top of `init.lua`.

### generate

```lua
require("ai").generate({
  model = "gemini3-lite,thinking=minimal", -- model token, see below
  system = "You write commit messages.",   -- optional
  prompt = "...",
  temperature = 0.2,                       -- optional, provider default if nil
  max_output_tokens = 300,                 -- optional, provider default if nil
}, function(result)
  -- result: { ok = true, content = "..." } or { ok = false, error = "..." }
end)
```

### generate_with_tools

```lua
require("ai").generate_with_tools({
  model = "gpt-mini",
  system = "You write commit messages.",
  prompt = "Summarize the staged changes with repository context.",
  tools = {
    "git_log",                 -- registry reference (key in ai/tools.lua)
    {                          -- inline one-off tool
      name = "read_todo",
      description = "The project TODO list",
      parameters = { type = "object", properties = {} },
      run = function(args, done)
        done(table.concat(vim.fn.readfile("TODO.md"), "\n"))
      end,
    },
  },
  mcps = { "docs-mcp" },       -- mcphub server names; their tools are offered too
  max_rounds = 8,              -- max model requests before aborting (default 8)
}, function(result)
  -- same result shape as generate
end)
```

Semantics:

- `tools` entries are either a string (a key in `require("ai.tools").registry`)
  or an inline `AITool` table (`name`, `description`, `parameters` JSON
  Schema, `run(args, done)`). `run` is async and must call `done(text)`
  exactly once; extra calls are ignored. Tool names must match
  `^[%w_-]+$` and be unique across tools and MCP servers.
- The loop runs until the model answers with text, an error occurs, or
  `max_rounds` model requests have been made (then it fails with "tool
  rounds exhausted"). Tool failures - unknown tool name, undecodable
  arguments, a `run` that throws - are fed back to the model as the tool
  output (`"Error: ..."`) so it can recover instead of aborting.
- Requested calls run sequentially in the order the model asked for them.

### inline (inline.lua)

Inline editing: run a prompt about the current selection and splice the
model's text back into the buffer. Unlike CodeCompanion's inline interaction
the model always generates the COMPLETE text (no diff formats - those are
error-prone); we do the splicing.

```lua
require("ai.inline").inline({
  type = "replace",          -- "replace" | "before" | "after"
  prompt = function(selected_text)         -- or a plain instruction string
    return "Make this function async:\n" .. selected_text
  end,
  -- model = "gpt-mini",     -- defaults to adapters().inline_edit
  -- range = { start_line = 10, end_line = 20 },  -- defaults to the visual selection
  -- system = "...",         -- extra guidance appended to the placement prompt
})
```

- `replace` swaps the selected lines for the response; `before`/`after`
  insert the response above/below the selection (documentation, trait
  impls, tests, usage examples).
- A string `prompt` is wrapped together with the filetype-fenced selection;
  a function `prompt(selected_text)` builds the entire user message itself.
- The selection is tracked with extmarks while the request is in flight, so
  concurrent edits elsewhere in the buffer do not shift the splice target.
- The splice is one undo step - `u` is the reject button. Code fences around
  the response are stripped.
- The placement-specific system prompt lives in `inline.lua`
  (`system_prompts`); per-call `system` text is appended to it.

User commands (registered in `nvim/lua/plugins/ai.lua`, all range-aware -
select lines first):

| Command | Behavior |
| --- | --- |
| `:'<,'>AIDoc` | Insert a documentation comment above the selection (port of the old CodeCompanion "Generate documentation" inline prompt) |
| `:'<,'>AIReplace <instruction>` | Rewrite the selection per the instruction |
| `:'<,'>AIBefore <instruction>` | Insert generated text above the selection |
| `:'<,'>AIAfter <instruction>` | Insert generated text below the selection (e.g. "implement this trait for struct Foo") |

### Tool registry (tools.lua)

`ai/tools.lua` holds reusable, named tool definitions. Registry entries omit
`name` (the key is the name):

```lua
require("ai.tools").registry.my_tool = {
  description = "...",
  parameters = { type = "object", properties = { ... } },
  run = function(args, done) ... done(text) end,
}
```

Shipped entries: `git_log` (recent commit subjects), `git_diff_stat`
(changed-file summary, staged or unstaged). Keep registry tools small,
read-only, and fast - they run inside interactive flows.

### MCP servers (mcps option)

`mcps = { "<server>", ... }` offers every tool of the named mcphub servers
to the model, namespaced `<server>__<tool>` (sanitized to provider-legal
name characters). `ai/mcp.lua` adapts mcphub.nvim's API
(`get_hub_instance`, `hub:get_servers`, `hub:call_tool`) and flattens MCP
results to text. Requirements: the mcphub.nvim plugin must be set up
(`nvim/lua/plugins/mcphub.lua`) and the servers defined in
`mcphub/servers.json` at the repo root. Servers without `autoApprove` will
prompt for confirmation on every call - fine interactively, wrong for
unattended flows.

## Model Tokens

`<preset>[,key=value...]` - the preset picks the provider and the real model
id; options customize the request:

- `thinking=<minimal|low|medium|high|max>` - the fixed end-user vocabulary,
  valid on every preset. Each preset maps the levels it can to the provider's
  native parameter; depending on the provider a level may send nothing at all
  (copilot has no reasoning knob) or map to the same request value as a
  neighboring level (e.g. `max` aliases `high` on Gemini 3). Only values
  outside the vocabulary are errors.
- `model=<raw id>` - override the model id while keeping the preset's
  provider and thinking mapping.

Examples: `gemini3,thinking=minimal`, `gpt-nano,thinking=minimal`,
`copilot,model=gpt-5-mini`.

Preset names must be unique across providers; `init.lua` indexes them at load.

## Provider Pages

One file per backend under `providers/`. Each module exposes `name`,
`presets` (token name -> `{ model, thinking? }`), `generate(ctx, spec,
request, cb)`, and optionally `reset()` for cached state. The goal is NOT to
support every model - each page keeps a small set of ready presets tuned for
fast, cheap generation, and documents the provider's wire format at the top
of the file.

| Provider | API | Auth | Thinking mapping (minimal/low/medium/high/max) |
| --- | --- | --- | --- |
| `gemini` | `generateContent` (REST, v1beta) | `GEMINI_API_KEY` env via `x-goog-api-key` | Gemini 3: `thinkingLevel` minimal/low/medium/high/high; Gemini 2.5: `thinkingBudget` 0/1024/8192/24576/24576; preset values encode the field by type (string vs number) |
| `openai` | Responses API (`/v1/responses`, recommended over chat completions for new integrations) | `OPENAI_API_KEY` env, bearer | `reasoning.effort` none/low/medium/high, max is `xhigh` on gpt-5.5 and aliases high on gpt-5.4 mini/nano (xhigh unconfirmed there); effort `none` needs gpt-5.1+ |
| `copilot` | Chat completions on `api.githubcopilot.com` (unofficial; protocol shared with CopilotChat.nvim / codecompanion.nvim) | oauth grant from `github-copilot/{hosts,apps}.json` (written by `:Copilot auth` / copilot.lua sign-in), exchanged at `copilot_internal/v2/token` for a cached session token | no API knob - all levels accepted, none sent; pick a non-reasoning model for no thinking |

Official docs:

- Gemini: <https://ai.google.dev/api/generate-content>,
  <https://ai.google.dev/gemini-api/docs/thinking>
- OpenAI: <https://platform.openai.com/docs/api-reference/responses>,
  <https://platform.openai.com/docs/guides/reasoning>
- Copilot has no official HTTP API doc; the reference implementation is
  CopilotChat.nvim `lua/CopilotChat/config/providers.lua`.

Parsing gotchas already handled (keep them when editing):

- Gemini: skip parts with `thought = true`; surface `finishReason` when the
  text is empty (e.g. `MAX_TOKENS` with thinking enabled).
- OpenAI: the `message` output item is not necessarily `output[1]` (reasoning
  items come first) - scan for `type == "message"`; check
  `status == "incomplete"`.
- Copilot: token response `endpoints.api` overrides the base URL
  (business/enterprise hosts); session token refreshes 60s before
  `expires_at`.
- All JSON decoding must pass `luanil` so `"error": null` does not become a
  truthy `vim.NIL`.

Tool-calling wire formats (each provider owns an opaque `history` it
continues between rounds):

- Gemini: tools as `tools = { { functionDeclarations = {...} } }` with
  schemas sanitized (`$schema`/`additionalProperties` stripped). The model's
  content is echoed verbatim into the history - Gemini 3 returns a 400 if a
  functionCall part's `thoughtSignature` is dropped. Results go back as ONE
  `role = "user"` content whose parts are
  `functionResponse = { id, name, response = { result = <text> } }`
  (`response` must be an object; `id` echoes `functionCall.id`).
- OpenAI (Responses, stateless): tools are flat
  `{ type = "function", name, description, parameters }`. Every output item
  (reasoning items included) is replayed verbatim into `input`, then one
  `{ type = "function_call_output", call_id, output = <string> }` per
  result. `instructions` is resent every round.
- Copilot: chat-completions style - nested
  `{ type = "function", ["function"] = {...} }` tools, the assistant
  `tool_calls` message echoed into history, results as
  `{ role = "tool", tool_call_id, content }`; tool requests add the
  `x-initiator: agent` header. Note gpt-5.4-nano (the `copilot-nano`
  preset) is likely not tool-capable - check
  `capabilities.supports.tool_calls` in the `/models` catalog.

## Transport and Test Seam

`http.lua` shells out to `curl` via `vim.system` (fully async, body over
stdin to avoid command-line length limits). Tests and callers must never need
a network: inject a backend with

```lua
require("ai").set_backend({
  request = function(request, cb) ... end, -- AIHttpRequest -> AIHttpResponse
  read_file = function(path) ... end,      -- copilot oauth discovery
  now = function() return 1000 end,        -- copilot session expiry
})
require("ai").reset_backend()
```

Any omitted method falls back to the real implementation. `set_backend` and
`reset_backend` also clear provider caches (the copilot session token).

## Testing

```text
nvim --headless -i NONE --cmd "set shadafile=NONE" -u nvim/init.lua -c "lua vim.loader.enable(false)" -S nvim/tests/ai/generate.lua
nvim --headless -i NONE --cmd "set shadafile=NONE" -u nvim/init.lua -c "lua vim.loader.enable(false)" -S nvim/tests/ai/tools.lua
nvim --headless -i NONE --cmd "set shadafile=NONE" -u nvim/init.lua -c "lua vim.loader.enable(false)" -S nvim/tests/ai/inline.lua
```

`generate.lua` covers token resolution errors, per-provider request shapes
(headers, body, thinking mapping), response parsing (thought parts,
reasoning items), HTTP errors, and the copilot token exchange/cache/refresh
flow. `tools.lua` covers the tool loop per provider (multi-turn request
shapes, thoughtSignature echo, parallel calls), error feedback, max_rounds,
tool validation, registry references, and the mcps option against a fake
mcphub injected via `package.loaded`. `inline.lua` covers splicing for all
three placements, fence stripping, extmark tracking across concurrent
edits, prompt wrapping, and error paths (buffer untouched on failure). All
against mock responses - tests must never need a network. New presets,
providers, or tool plumbing need coverage there.

LuaLS annotations are required (same policy as `nvim/lua/diff_review`):
shared classes (`AIProvider`, `AIModelSpec`, `AIGenerateOpts`, ...) live in
`init.lua`; transport types (`AIHttpRequest`, ...) in `http.lua`.

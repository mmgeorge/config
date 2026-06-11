# AI Prompt Library Notes

`require("ai")` is a small, generic one-shot prompt library: send a system +
user prompt to an LLM provider, get the response text back via an async
callback. It exists so features like commit-message generation do not depend
on chat-UI plugin internals (it replaced a hack on CodeCompanion's
undocumented background interaction). No streaming, no chat history, no
tools - one prompt in, one text out.

`adapters.lua` is separate: it picks which adapter/model each AI feature uses
(CodeCompanion adapter configs for the chat UI, an `ai` model token for
commit generation).

## Public API

```lua
require("ai").generate({
  model = "gemini3-lite,thinking=none",  -- model token, see below
  system = "You write commit messages.", -- optional
  prompt = "...",
  temperature = 0.2,                     -- optional, provider default if nil
  max_output_tokens = 300,               -- optional, provider default if nil
}, function(result)
  -- result: { ok = true, content = "..." } or { ok = false, error = "..." }
  -- always called on the main loop
end)
```

`require("ai").resolve(token)` resolves a model token without sending
anything (used by tests and useful for validating config).

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
```

The test covers token resolution errors, per-provider request shapes
(headers, body, thinking mapping), response parsing (thought parts, reasoning
items), HTTP errors, and the copilot token exchange/cache/refresh flow - all
against mock responses. New presets or providers need request-shape and
parse coverage there.

LuaLS annotations are required (same policy as `nvim/lua/diff_review`):
shared classes (`AIProvider`, `AIModelSpec`, `AIGenerateOpts`, ...) live in
`init.lua`; transport types (`AIHttpRequest`, ...) in `http.lua`.

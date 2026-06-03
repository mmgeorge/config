---@alias DiffReviewAICommitCommand string[]

---@class DiffReviewAICommitAsyncResult
---@field code integer
---@field stdout string
---@field stderr string
---@field output string

---@alias DiffReviewAICommitTextCallback fun(result: DiffReviewAICommitAsyncResult)
---@alias DiffReviewAICommitListCallback fun(output: string[], code: integer, stderr?: string)

---@class DiffReviewAICommitBackend
---@field systemlist_async? fun(command: DiffReviewAICommitCommand, cb: DiffReviewAICommitListCallback)
---@field system_async? fun(command: DiffReviewAICommitCommand, input: string?, cb: DiffReviewAICommitTextCallback)
---@field generate_async? fun(context: string, cb: fun(result: { ok: boolean, message?: string, error?: string }))

---@class DiffReviewAICommitState
---@field state "none"|"generating"|"ready"|"error"
---@field cwd? string
---@field fingerprint? string
---@field message? string
---@field error? string
---@field waiters fun(result: DiffReviewAICommitState)[]

---@class DiffReviewAICommitModule
---@field _backend DiffReviewAICommitBackend?
---@field _state DiffReviewAICommitState?
---@field _request_id integer

---@type DiffReviewAICommitModule
local M = {
  _request_id = 0,
}

local commit_context_limit = {
  stat = 12000,
  diff = 180000,
}

local compact_diff_options = {
  max_hunks = 30,
  max_changed_lines = 800,
  min_hunk_changed_lines = 8,
  hunk_head_lines = 12,
}

local commit_system_prompt = [[You are a factual commit message generator following Conventional Commits.
Respond with ONLY the commit message - no code blocks, no explanation,
commentary, alternatives, or preface.

Format:
<type>: <description>

[body]

Types: feat, fix, docs, style, refactor, perf, test, build, ci, chore, revert

Description rules:
- Imperative mood ("add feature" not "added feature")
- Describe the concrete code change, not intent, benefit, or motivation
- Lowercase first letter, no trailing period
- Max 50 characters
- Prefer specific nouns from the diff over abstract summaries
- Do not add subjective qualifiers or purpose phrases like "for flexibility",
  "for extensibility", "improve", "enhance", "future-proof", or "better"

Examples:
- Bad: feat: refactor parquet data source handling for flexibility
- Bad: refactor: improve parquet extensibility
- Good: refactor: pass parquet data source objects

Body (rare):
- Default to no body
- Never write a body for chore commits
- Only write a body when the subject alone is materially incomplete and
  omits important factual changes
- Do not write a body for benefits, motivation, or generic summary
- If needed, include only essential factual "what changed" details from the
  diff, such as API shape changes, behavior changes, migrations, or tests
- Wrap at 72 chars. Max two lines.]]

---@param backend DiffReviewAICommitBackend?
function M.set_backend(backend)
  M._backend = backend
end

function M.reset_backend()
  M._backend = nil
  M._state = nil
  M._request_id = 0
end

---@param text string
---@return string[] lines
local function text_to_lines(text)
  text = tostring(text or ""):gsub("\r\n", "\n")
  if text:sub(-1) == "\n" then
    text = text:sub(1, -2)
  end
  if text == "" then return {} end
  return vim.split(text, "\n", { plain = true })
end

---@param command DiffReviewAICommitCommand
---@param input string?
---@param cb DiffReviewAICommitTextCallback
local function system_text_async(command, input, cb)
  local backend = M._backend
  if backend and backend.system_async then
    backend.system_async(command, input, cb)
    return
  end

  local ok, process = pcall(vim.system, command, { text = true, stdin = input }, function(result)
    vim.schedule(function()
      local stdout = result.stdout or ""
      local stderr = result.stderr or ""
      cb({
        code = result.code or 0,
        stdout = stdout,
        stderr = stderr,
        output = stdout ~= "" and stdout or stderr,
      })
    end)
  end)
  if not ok then
    vim.schedule(function()
      local message = tostring(process)
      cb({ code = -1, stdout = "", stderr = message, output = message })
    end)
  end
end

---@param command DiffReviewAICommitCommand
---@param cb DiffReviewAICommitListCallback
local function systemlist_async(command, cb)
  local backend = M._backend
  if backend and backend.systemlist_async then
    backend.systemlist_async(command, cb)
    return
  end

  system_text_async(command, nil, function(result)
    cb(text_to_lines(result.stdout), result.code, result.stderr)
  end)
end

---@param text string
---@param limit integer
---@return string text
---@return boolean truncated
local function truncate_at_line(text, limit)
  if #text <= limit then
    return text, false
  end

  local truncated = text:sub(1, limit)
  local last_newline = truncated:match(".*()\n")
  if last_newline and last_newline > 1 then
    truncated = truncated:sub(1, last_newline - 1)
  end
  return truncated, true
end

---@param cwd string
---@param ref string
---@return DiffReviewAICommitCommand
local function diff_command(cwd, ref)
  local command = { "git", "-C", cwd, "diff", "--no-ext-diff", "--no-color" }
  if ref == "staged" then
    command[#command + 1] = "--staged"
  else
    command[#command + 1] = ref
  end
  return command
end

---@param cwd string
---@param ref string
---@param cb fun(fingerprint?: string)
local function changes_fingerprint_async(cwd, ref, cb)
  local command = diff_command(cwd, ref)
  vim.list_extend(command, { "--stat", "--summary" })
  systemlist_async(command, function(output, code)
    if code ~= 0 then
      cb(nil)
      return
    end
    local text = vim.trim(table.concat(output or {}, "\n"))
    cb(text ~= "" and text or nil)
  end)
end

---@param cwd string
---@param ref string
---@param cb fun(context?: string)
local function build_commit_context_async(cwd, ref, cb)
  local stat_command = diff_command(cwd, ref)
  vim.list_extend(stat_command, { "--stat", "--summary" })
  local raw_diff_command = diff_command(cwd, ref)

  systemlist_async(stat_command, function(stat_output, stat_code)
    if stat_code ~= 0 then
      cb(nil)
      return
    end
    systemlist_async(raw_diff_command, function(diff_output, diff_code)
      if diff_code ~= 0 then
        cb(nil)
        return
      end

      local stat = table.concat(stat_output or {}, "\n")
      local diff = table.concat(diff_output or {}, "\n")
      if vim.trim(diff) == "" then
        cb(nil)
        return
      end

      local compacted_diff, diff_compacted, diff_metrics = require("git.diff").compact(diff, compact_diff_options)
      local truncated_stat, stat_truncated = truncate_at_line(stat, commit_context_limit.stat)
      local truncated_diff, diff_truncated = truncate_at_line(compacted_diff, commit_context_limit.diff)
      local truncated = stat_truncated or diff_truncated

      local sections = {
        "Generate a conventional commit message for these changes.",
      }

      if diff_compacted then
        sections[#sections + 1] = (
          "Large diff compacted after counting %d hunks and %d changed lines. "
          .. "The compact diff starts with an overall summary and only includes large hunks."
        ):format(diff_metrics.hunks, diff_metrics.changed)
      end

      if truncated then
        sections[#sections + 1] = "Some context is truncated to avoid exceeding the model request size. "
          .. "Use the diff summary for overall scope, and visible hunks for details."
      end

      if vim.trim(truncated_stat) ~= "" then
        sections[#sections + 1] = "Diff summary:\n```text\n" .. truncated_stat .. "\n```"
      end

      sections[#sections + 1] = "Diff:\n```diff\n" .. truncated_diff .. "\n```"
      cb(table.concat(sections, "\n\n"))
    end)
  end)
end

---@param err any
---@return string
local function format_error(err)
  if type(err) == "string" then
    return err
  end
  if type(err) == "table" then
    for _, key in ipairs({ "message", "error", "stderr", "output", "reason" }) do
      local value = err[key]
      if type(value) == "string" and vim.trim(value) ~= "" then
        return value
      end
    end
    return vim.inspect(err)
  end
  return tostring(err)
end

---@param content any
---@return string?
local function normalize_message(content)
  if type(content) == "table" then
    content = content.content
  end
  if type(content) ~= "string" or vim.trim(content) == "" then
    return nil
  end
  local message = vim.trim(content)
  return message:gsub("^```%w*\n", ""):gsub("\n```$", "")
end

---@param context string
---@param cb fun(result: { ok: boolean, message?: string, error?: string })
local function generate_async(context, cb)
  local backend = M._backend
  if backend and backend.generate_async then
    backend.generate_async(context, cb)
    return
  end

  local ok_background, Background = pcall(require, "codecompanion.interactions.background")
  if not ok_background then
    cb({ ok = false, error = "CodeCompanion background interaction is unavailable" })
    return
  end

  local ok_adapters, adapters = pcall(require, "ai.adapters")
  local adapter_config = ok_adapters and adapters.get().commit or "copilot"
  local background = Background.new({ adapter = adapter_config })
  local inline_output = background
    and background.adapter
    and background.adapter.handlers
    and background.adapter.handlers.inline_output

  if type(inline_output) == "function" and not background.adapter.handlers._commit_safe_inline_output then
    background.adapter.handlers.inline_output = function(self, data, handler_context)
      if type(data) == "string" then
        data = { body = data }
      end
      return inline_output(self, data, handler_context)
    end
    background.adapter.handlers._commit_safe_inline_output = true
  end

  background:ask({
    { role = "system", content = commit_system_prompt },
    { role = "user", content = context },
  }, {
    method = "async",
    parse_handler = "parse_inline",
    silent = true,
    on_done = function(result)
      vim.schedule(function()
        local message = normalize_message(result and result.output)
        if not message then
          cb({ ok = false, error = "No response from model" })
          return
        end
        cb({ ok = true, message = message })
      end)
    end,
    on_error = function(err)
      vim.schedule(function()
        cb({ ok = false, error = format_error(err) })
      end)
    end,
  })
end

---@param state DiffReviewAICommitState
local function notify_waiters(state)
  local waiters = state.waiters or {}
  state.waiters = {}
  for _, waiter in ipairs(waiters) do
    pcall(waiter, state)
  end
end

---@param cwd string
---@param opts? { force?: boolean }
---@param cb? fun(state: DiffReviewAICommitState)
function M.ensure(cwd, opts, cb)
  opts = opts or {}
  if cwd == nil or cwd == "" then
    local state = { state = "none", waiters = {} }
    if cb then cb(state) end
    return
  end

  changes_fingerprint_async(cwd, "HEAD", function(fingerprint)
    if not fingerprint then
      M._state = { state = "none", cwd = cwd, waiters = {} }
      if cb then cb(M._state) end
      return
    end

    local current = M._state
    if not opts.force
      and current
      and current.cwd == cwd
      and current.fingerprint == fingerprint
    then
      if current.state == "generating" and cb then
        current.waiters[#current.waiters + 1] = cb
      elseif cb then
        cb(current)
      end
      return
    end

    M._request_id = M._request_id + 1
    local request_id = M._request_id
    local state = {
      state = "generating",
      cwd = cwd,
      fingerprint = fingerprint,
      waiters = cb and { cb } or {},
    }
    M._state = state

    build_commit_context_async(cwd, "HEAD", function(context)
      if M._request_id ~= request_id or M._state ~= state then return end
      if not context then
        state.state = "none"
        notify_waiters(state)
        return
      end

      generate_async(context, function(result)
        if M._request_id ~= request_id or M._state ~= state then return end
        if result.ok and result.message then
          state.state = "ready"
          state.message = result.message
        else
          state.state = "error"
          state.error = result.error or "Unable to generate commit message"
        end
        notify_waiters(state)
      end)
    end)
  end)
end

---@param cwd string
---@param cb fun(state: DiffReviewAICommitState)
function M.wait_for_message(cwd, cb)
  M.ensure(cwd, nil, cb)
end

---@return DiffReviewAICommitState?
function M.state()
  return M._state
end

---@param message string?
---@return string
function M.subject(message)
  for _, line in ipairs(text_to_lines(message or "")) do
    local trimmed = vim.trim(line)
    if trimmed ~= "" then return trimmed end
  end
  return ""
end

---@param buf integer
---@param cwd string
---@param notify? fun(message: string, level: integer)
function M.populate_commit_buffer_when_ready(buf, cwd, notify)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  if lines[1] and lines[1] ~= "" then return end

  local function apply_when_matching_staged(state)
    if not vim.api.nvim_buf_is_valid(buf) then return end
    if state.state ~= "ready" or not state.message then
      if notify and state.state == "error" then
        notify(state.error or "Unable to generate commit message", vim.log.levels.WARN)
      end
      return
    end

    changes_fingerprint_async(cwd, "staged", function(staged_fingerprint)
      if not vim.api.nvim_buf_is_valid(buf) then return end
      if not staged_fingerprint or staged_fingerprint ~= state.fingerprint then return end

      local current_lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
      if current_lines[1] and current_lines[1] ~= "" then return end
      local message_lines = text_to_lines(state.message)
      if #message_lines == 0 then return end
      local was_modifiable = vim.bo[buf].modifiable
      vim.bo[buf].modifiable = true
      vim.api.nvim_buf_set_lines(buf, 0, 0, false, message_lines)
      vim.bo[buf].modifiable = was_modifiable
      local winid = vim.fn.bufwinid(buf)
      if winid ~= -1 then
        pcall(vim.api.nvim_win_set_cursor, winid, { 1, 0 })
      end
    end)
  end

  M.wait_for_message(cwd, function(state)
    apply_when_matching_staged(state)
  end)
end

return M

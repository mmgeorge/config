local M = {}
local buffer = require("forge.buffer")
local editable = require("forge.editable")
local next_view = 0
local MAX_COUNTER = 9007199254740991
local window_view = {}
local window_baseline = {}
local presentation = require("forge.window_presentation")
local native_option = { number = false, relativenumber = false, signcolumn = "no", foldcolumn = "0",
  statuscolumn = " ", virtualedit = "all", wrap = true, linebreak = true, breakindent = false, conceallevel = 0, concealcursor = "" }

local function release_window(view)
  if view.document_folds then
    presentation.release(view.window, view, function() require("forge.folds").release(view.window) end)
  end
end

local function present(view)
  if not view.document_folds then return end
  require("forge.folds").attach(view.session, view.window)
  local applied = vim.deepcopy(native_option)
  applied.statuscolumn = string.rep(" ", view.margin)
  for name, value in pairs(view.columns) do applied[name] = value end
  if view.conceal then
    applied.conceallevel = view.conceal.level
    applied.concealcursor = view.conceal.cursor
  end
  if view.wrapping then
    applied.breakindent = view.wrapping.indent
    applied.breakindentopt = view.wrapping.options
  end
  for name, value in pairs(applied) do vim.wo[view.window][name] = value end
  for _, name in ipairs({ "foldmethod", "foldexpr", "foldenable", "foldlevel", "foldtext", "fillchars" }) do
    applied[name] = vim.wo[view.window][name]
  end
  presentation.retain(view.window, view, view.baseline, applied)
end

---@class ForgeInputView
---@field id string
---@field window integer
---@field sequence integer
---@field active boolean
---@field document string
---@field effect table<string, boolean>
---@field document_folds boolean Whether this view owns native document fold options.
---@field margin integer Number of fixed leading display cells.
---@field columns table<string, boolean|string> Column options inherited from the invoking window.
---@field conceal? {level: integer, cursor: string} Source concealment options retained by this view.
---@field wrapping? {indent: boolean, options: string} Source continuation indentation retained by this view.
---@field session table
---@field baseline table<string, string|boolean|number>
---@field cursor? integer[]

---@class ForgeDocumentInput
---@field document string
---@field revision integer
---@field view string
---@field sequence integer
---@field action string
---@field block string
---@field position {row: integer, column: integer}
---@field target? string

---@param session table
---@param window integer
---@param options? {exact_source?: boolean, margin?: integer, columns?: table<string, boolean|string>, conceal?: {level: integer, cursor: string}, wrapping?: {indent: boolean, options: string}} Source presentation options retained for this view.
---@return ForgeInputView
function M.open(session, window, options)
  if window == 0 then window = vim.api.nvim_get_current_win() end
  assert(vim.api.nvim_win_is_valid(window) and vim.api.nvim_win_get_buf(window) == session.buffer,
    "input window does not display the document")
  assert(next_view < MAX_COUNTER, "view identity exhausted")
  local margin = options and options.margin or 1
  assert(margin == 0 or margin == 1, "native document margin must be zero or one cell")
  local columns = {}
  for _, name in ipairs({ "number", "relativenumber", "signcolumn", "foldcolumn", "statuscolumn" }) do
    if options and options.columns and options.columns[name] ~= nil then columns[name] = options.columns[name] end
  end
  next_view = next_view + 1
  local baseline = window_baseline[window] or presentation.capture(window)
  window_baseline[window] = nil
  if window_view[window] then release_window(window_view[window]) end
  local view = { id = "view:" .. vim.uv.hrtime() .. ":" .. next_view, document = session.document,
    window = window, sequence = 0, active = true, effect = {}, document_folds = not (options and options.exact_source),
    session = session, baseline = baseline, margin = margin, columns = columns,
    conceal = options and options.conceal and vim.deepcopy(options.conceal),
    wrapping = options and options.wrapping and vim.deepcopy(options.wrapping) }
  window_view[window] = view
  present(view)
  return view
end

---@param session table
---@param view ForgeInputView
---@param action string
---@return ForgeDocumentInput?
---@return string?
---@overload fun(session: ForgeStatusReplica, view: ForgeInputView, action: string): ForgeStatusInput?, string?
function M.capture(session, view, action)
  if session.capture then return session.capture(view, action) end
  if session.status ~= "Applied" or session.applying or not view.active or view.document ~= session.document
    or not vim.api.nvim_win_is_valid(view.window) or vim.api.nvim_win_get_buf(view.window) ~= session.buffer
  then
    return nil, "input view is no longer current"
  end
  if editable.suspend_generated_text(session.editable) then return nil, "local edits require acknowledgement" end
  assert(type(action) == "string" and #action > 0 and #action <= 256 and not action:find("%c"), "invalid input action")
  assert(view.sequence < MAX_COUNTER, "input sequence exhausted")
  local cursor = vim.api.nvim_win_get_cursor(view.window)
  local location = buffer.locate(session, cursor[1] - 1, cursor[2])
  if not location then return nil, "input has no document block" end
  view.sequence = view.sequence + 1
  view.cursor = cursor
  view.effect = {}
  return { document = session.document, revision = session.revision, view = view.id, sequence = view.sequence,
    action = action, block = location.block, position = location.position, target = location.target }
end

---@param view ForgeInputView
function M.close(view)
  view.active = false
  view.effect = {}
  if window_view[view.window] == view then
    release_window(view)
    window_view[view.window] = nil
  end
end

vim.api.nvim_create_autocmd("BufWinLeave", { callback = function(event)
  local window = vim.api.nvim_get_current_win()
  if vim.api.nvim_win_get_buf(window) ~= event.buf then return end
  window_baseline[window] = presentation.capture(window)
  local view = window_view[window]
  if view and view.session.buffer == event.buf then release_window(view) end
end })
vim.api.nvim_create_autocmd("BufWinEnter", { callback = function(event)
  local window = vim.api.nvim_get_current_win()
  if vim.api.nvim_win_get_buf(window) ~= event.buf then return end
  local view = window_view[window]
  if view and view.active and view.session.buffer == event.buf then
    view.baseline = window_baseline[window] or view.baseline
    window_baseline[window] = nil
    present(view)
  end
end })
vim.api.nvim_create_autocmd("WinClosed", { callback = function(event)
  window_view[tonumber(event.match)] = nil
  window_baseline[tonumber(event.match)] = nil
end })

return M

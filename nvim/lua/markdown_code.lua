local M = {}

local markdown_filetypes = {
  GitStatus = true,
  GithubIssue = true,
  markdown = true,
}

local attempted_changetick = {}
local synced_changetick = {}
local language_signature_by_buf = {}
local temp_name_by_buf = {}
local warned = {}

---@param key string
---@param message string
---@param level integer
---@param title string?
local function notify_once(key, message, level, title)
  if warned[key] then return end
  warned[key] = true
  vim.notify(message, level, { title = title or "Markdown" })
end

---@param filetype string?
local function register_markdown_filetype(filetype)
  filetype = tostring(filetype or "")
  if filetype == "" or filetype == "markdown" then return end
  pcall(vim.treesitter.language.register, "markdown", filetype)
end

---@param buf integer
---@return string
local function temp_markdown_name(buf)
  if temp_name_by_buf[buf] then return temp_name_by_buf[buf] end
  local root = vim.fs.joinpath(vim.fn.stdpath("cache"), "markdown-code")
  vim.fn.mkdir(root, "p")
  local name = vim.fs.joinpath(root, tostring(vim.uv.os_getpid()) .. "-" .. tostring(buf) .. ".md")
  temp_name_by_buf[buf] = name
  return name
end

---@param buf integer
---@return boolean
local function needs_temp_name(buf)
  local name = vim.api.nvim_buf_get_name(buf)
  if name == "" then return true end
  if name:find("://", 1, true) then return true end
  return false
end

---@param buf integer
---@param callback fun()
local function with_otter_safe_name(buf, callback)
  local original_name = vim.api.nvim_buf_get_name(buf)
  local changed = false
  if needs_temp_name(buf) then changed = pcall(vim.api.nvim_buf_set_name, buf, temp_markdown_name(buf)) end
  local ok, err = pcall(callback)
  if changed and vim.api.nvim_buf_is_valid(buf) then pcall(vim.api.nvim_buf_set_name, buf, original_name) end
  if not ok then error(err, 0) end
end

---@param buf integer
---@return boolean
local function otter_active(buf)
  local ok, keeper = pcall(require, "otter.keeper")
  return ok and type(keeper) == "table" and type(keeper.rafts) == "table" and keeper.rafts[buf] ~= nil
end

---@param language string
---@return string
local function normalize_fence_language(language)
  language = tostring(language or ""):lower():gsub("[^%w_+-].*$", "")
  local aliases = {
    js = "javascript",
    jsx = "javascriptreact",
    py = "python",
    rs = "rust",
    sh = "bash",
    shell = "bash",
    ts = "typescript",
    tsx = "typescriptreact",
    yml = "yaml",
  }
  return aliases[language] or language
end

---@param buf integer
---@return string
local function fenced_language_signature(buf)
  local languages = {}
  local seen = {}
  for _, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    local language = line:match("^%s*```%s*([^%s`{]+)")
    language = normalize_fence_language(language)
    if language ~= "" and not seen[language] then
      seen[language] = true
      languages[#languages + 1] = language
    end
  end
  table.sort(languages)
  return table.concat(languages, ",")
end

---@param buf integer
---@param otter table
local function deactivate_otter(buf, otter)
  if type(otter.deactivate) ~= "function" then return end
  pcall(function()
    vim.api.nvim_buf_call(buf, function()
      otter.deactivate(false, false)
    end)
  end)
end

---@param buf integer
---@param otter table
---@return boolean
local function sync_otter(buf, otter)
  local tick = vim.api.nvim_buf_get_changedtick(buf)
  if synced_changetick[buf] == tick then return true end
  if type(otter.sync_raft) ~= "function" then return true end
  local ok, err = pcall(otter.sync_raft, buf)
  if ok then
    synced_changetick[buf] = tick
    return true
  end
  notify_once("sync", "Markdown code sync failed: " .. tostring(err), vim.log.levels.WARN)
  return false
end

---@param buf integer
---@param opts? { filetype?: string, register_as_markdown?: boolean, languages?: string[], completion?: boolean, diagnostics?: boolean, tsquery?: string, notify_title?: string }
---@return boolean
function M.activate(buf, opts)
  opts = opts or {}
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return false end

  local filetype = opts.filetype or vim.bo[buf].filetype
  if opts.register_as_markdown or markdown_filetypes[filetype] then register_markdown_filetype(filetype) end

  local ok, otter = pcall(require, "otter")
  if not ok or type(otter) ~= "table" then return false end
  if type(otter.activate) ~= "function" then
    notify_once("api", "otter.nvim is installed but does not expose activate()", vim.log.levels.WARN, opts.notify_title)
    return false
  end

  local signature = fenced_language_signature(buf)
  if otter_active(buf) then
    if signature ~= "" and language_signature_by_buf[buf] ~= nil and language_signature_by_buf[buf] ~= signature then
      deactivate_otter(buf, otter)
    else
      return sync_otter(buf, otter)
    end
  end

  local tick = vim.api.nvim_buf_get_changedtick(buf)
  if attempted_changetick[buf] == tick then return false end
  attempted_changetick[buf] = tick

  local activated = false
  local activate_ok, err = pcall(function()
    vim.api.nvim_buf_call(buf, function()
      with_otter_safe_name(buf, function()
        otter.activate(opts.languages, opts.completion ~= false, opts.diagnostics ~= false, opts.tsquery)
      end)
    end)
    activated = otter_active(buf)
  end)
  if not activate_ok then
    notify_once("activate", "Markdown code activation failed: " .. tostring(err), vim.log.levels.WARN, opts.notify_title)
    return false
  end
  if activated then
    attempted_changetick[buf] = nil
    synced_changetick[buf] = tick
    language_signature_by_buf[buf] = signature
  end
  return activated
end

---@param buf integer
function M.detach(buf)
  attempted_changetick[buf] = nil
  synced_changetick[buf] = nil
  language_signature_by_buf[buf] = nil
  temp_name_by_buf[buf] = nil
end

function M._reset_for_test()
  attempted_changetick = {}
  synced_changetick = {}
  language_signature_by_buf = {}
  temp_name_by_buf = {}
  warned = {}
end

return M

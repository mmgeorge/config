---@class GithubLinkRange
---@field line1 integer
---@field line2 integer

---@class GithubLinkContext
---@field root string
---@field remote_url string
---@field ref string
---@field path string

local gh = require("github.gh")

local M = {}

---@param message string
local function notify_error(message)
  vim.notify(message, vim.log.levels.ERROR, { title = "GithubLink" })
end

---@param value string
---@return string
local function strip(value)
  return vim.trim(tostring(value or ""))
end

---@param path string
---@return string
local function normalize_path(path)
  return vim.fs.normalize(tostring(path or "")):gsub("\\", "/"):gsub("/+$", "")
end

---@param path string
---@return string
local function dirname(path)
  return vim.fn.fnamemodify(path, ":p:h")
end

---@param command string[]
---@param cwd string?
---@param callback fun(ok: boolean, output: string)
local function system_text_async(command, cwd, callback)
  local ok, process_or_error = pcall(vim.system, command, { text = true, cwd = cwd }, function(result)
    vim.schedule(function()
      local stdout = result.stdout or ""
      local stderr = result.stderr or ""
      if (result.code or 0) == 0 then
        callback(true, strip(stdout))
      else
        callback(false, strip(stderr ~= "" and stderr or stdout))
      end
    end)
  end)

  if not ok then
    vim.schedule(function()
      callback(false, tostring(process_or_error))
    end)
  end
end

---@param remote_url string
---@return string?
function M.remote_base(remote_url)
  local url = strip(remote_url):gsub("%.git$", "")
  local host, path = url:match("^git@([^:]+):(.+)$")
  if host and path then return ("https://%s/%s"):format(host, path) end

  host, path = url:match("^ssh://git@([^/]+)/(.+)$")
  if host and path then return ("https://%s/%s"):format(host, path) end

  local scheme, rest = url:match("^(https?)://(.+)$")
  if scheme and rest then return scheme .. "://" .. rest end

  return nil
end

---@param text string
---@param keep_slash boolean
---@return string
local function url_encode(text, keep_slash)
  text = tostring(text or ""):gsub("\\", "/")
  return (text:gsub("([^%w%-%._~/%:])", function(char)
    if keep_slash and char == "/" then return "/" end
    return ("%%%02X"):format(string.byte(char))
  end))
end

---@param root string
---@param file string
---@return string?
function M.relative_path(root, file)
  local normalized_root = normalize_path(root)
  local normalized_file = normalize_path(file)
  local compare_root = normalized_root:lower()
  local compare_file = normalized_file:lower()
  if compare_file == compare_root then return nil end
  local prefix = compare_root .. "/"
  if compare_file:sub(1, #prefix) ~= prefix then return nil end
  return normalized_file:sub(#normalized_root + 2)
end

---@param line1 integer
---@param line2 integer
---@return string
local function line_fragment(line1, line2)
  local start_line = math.max(tonumber(line1) or 1, 1)
  local end_line = math.max(tonumber(line2) or start_line, 1)
  if end_line < start_line then start_line, end_line = end_line, start_line end
  if start_line == end_line then return ("#L%d"):format(start_line) end
  return ("#L%d-L%d"):format(start_line, end_line)
end

---@param kind "blob"|"blame"
---@param context GithubLinkContext
---@param range GithubLinkRange
---@return string?
function M.url(kind, context, range)
  local base = M.remote_base(context.remote_url)
  if not base then return nil end
  local route = kind == "blame" and "blame" or "blob"
  local plain = kind == "blob" and "?plain=1" or ""
  return ("%s/%s/%s/%s%s%s"):format(
    base,
    route,
    url_encode(context.ref, true),
    url_encode(context.path, true),
    plain,
    line_fragment(range.line1, range.line2)
  )
end

---@param opts table?
---@return GithubLinkRange
local function command_range(opts)
  if opts and tonumber(opts.range) and tonumber(opts.range) > 0 then
    return { line1 = tonumber(opts.line1) or 1, line2 = tonumber(opts.line2) or tonumber(opts.line1) or 1 }
  end
  local row = vim.api.nvim_win_get_cursor(0)[1]
  return { line1 = row, line2 = row }
end

---@param root string
---@param callback fun(ok: boolean, remote_url?: string, message?: string)
local function resolve_remote(root, callback)
  system_text_async({ "git", "-C", root, "remote", "get-url", "origin" }, root, function(ok, output)
    if ok and output ~= "" then
      callback(true, output)
      return
    end

    system_text_async({ "git", "-C", root, "remote" }, root, function(remote_ok, remotes)
      if not remote_ok or remotes == "" then
        callback(false, nil, output ~= "" and output or "No git remote configured")
        return
      end
      local remote = vim.split(remotes, "\n", { plain = true, trimempty = true })[1]
      if not remote or remote == "" then
        callback(false, nil, "No git remote configured")
        return
      end
      system_text_async({ "git", "-C", root, "remote", "get-url", remote }, root, function(url_ok, remote_url)
        if not url_ok or remote_url == "" then
          callback(false, nil, remote_url ~= "" and remote_url or "Unable to read git remote URL")
          return
        end
        callback(true, remote_url)
      end)
    end)
  end)
end

---@param root string
---@param callback fun(ok: boolean, ref?: string, message?: string)
local function resolve_ref(root, callback)
  system_text_async({ "git", "-C", root, "branch", "--show-current" }, root, function(ok, branch)
    if ok and branch ~= "" then
      callback(true, branch)
      return
    end
    system_text_async({ "git", "-C", root, "rev-parse", "HEAD" }, root, function(sha_ok, sha)
      if sha_ok and sha ~= "" then
        callback(true, sha)
      else
        callback(false, nil, sha ~= "" and sha or "Unable to resolve current git ref")
      end
    end)
  end)
end

---@param file string
---@param callback fun(ok: boolean, context?: GithubLinkContext, message?: string)
function M.context_async(file, callback)
  file = strip(file)
  if file == "" then
    callback(false, nil, "Current buffer has no file")
    return
  end

  system_text_async({ "git", "-C", dirname(file), "rev-parse", "--show-toplevel" }, nil, function(root_ok, root)
    if not root_ok or root == "" then
      callback(false, nil, root ~= "" and root or "Current file is not in a git repository")
      return
    end

    local path = M.relative_path(root, file)
    if not path then
      callback(false, nil, "Current file is outside the git repository")
      return
    end

    resolve_remote(root, function(remote_ok, remote_url, remote_message)
      if not remote_ok or not remote_url then
        callback(false, nil, remote_message or "Unable to resolve GitHub remote")
        return
      end
      resolve_ref(root, function(ref_ok, ref, ref_message)
        if not ref_ok or not ref then
          callback(false, nil, ref_message or "Unable to resolve git ref")
          return
        end
        callback(true, {
          root = root,
          remote_url = remote_url,
          ref = ref,
          path = path,
        })
      end)
    end)
  end)
end

---@param kind "blob"|"blame"
---@param opts table?
function M.open(kind, opts)
  local file = vim.api.nvim_buf_get_name(0)
  local range = command_range(opts)
  M.context_async(file, function(ok, context, message)
    if not ok or not context then
      notify_error(message or "Unable to build GitHub URL")
      return
    end

    local url = M.url(kind, context, range)
    if not url then
      notify_error("Unable to parse GitHub remote URL")
      return
    end
    if not gh.open_url(url) then
      notify_error("Unable to open GitHub URL")
    end
  end)
end

return M

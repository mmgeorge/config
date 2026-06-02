--- Trouble source: diff_review
--- Shows git diff hunks grouped by file with staging checkboxes.
--- Usage: :Trouble diff_review

local Item = require("trouble.item")

local M = {}

M._hunk_header_ns = vim.api.nvim_create_namespace("diff_review_headers")
M._active_hunk_header_ns = vim.api.nvim_create_namespace("diff_review_active_hunk")
M._status_ns = vim.api.nvim_create_namespace("diff_review_status")
M._hunk_header_priority = 20
M._active_hunk_header_priority = 200

-- Background-only highlight groups for diff lines.
-- Pull bg from existing DiffAdd/DiffDelete so they match the gutter colors.
local function setup_bg_highlights()
  local function get_bg(name)
    local hl = vim.api.nvim_get_hl(0, { name = name, link = false })
    return hl.bg
  end
  local add_bg = get_bg("DiffAdd") or "#002200"
  local del_bg = get_bg("DiffDelete") or "#220000"
  local normal = vim.api.nvim_get_hl(0, { name = "Normal", link = false })
  local header_fg = normal.fg or "#c0c0c0"
  vim.api.nvim_set_hl(0, "DiffReviewAddBg", { bg = add_bg })
  vim.api.nvim_set_hl(0, "DiffReviewDeleteBg", { bg = del_bg })
  vim.api.nvim_set_hl(0, "DiffReviewAddLineNr", { fg = "#50fa7b", bg = add_bg, bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewDeleteLineNr", { fg = "#ff5555", bg = del_bg, bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewContextLineNr", { fg = "#555555" })
  vim.api.nvim_set_hl(0, "DiffReviewContextBg", {})
  -- Fg-only variants for range numbers in headers (no bg)
  vim.api.nvim_set_hl(0, "DiffReviewAddRange", { fg = "#50fa7b" })
  vim.api.nvim_set_hl(0, "DiffReviewDeleteRange", { fg = "#ff5555" })
  vim.api.nvim_set_hl(0, "DiffReviewDirName", {
    fg = "#7f8790",
    nocombine = true,
    ctermfg = 8,
  })
  vim.api.nvim_set_hl(0, "DiffReviewFileName", {
    fg = "#ffffff",
    nocombine = true,
    ctermfg = 15,
  })
  -- Subtle gray bg for hunk header lines in the diff buffer
  vim.api.nvim_set_hl(0, "SnacksDiffHunkHeader", { bg = "#1e1e1e" })
  vim.api.nvim_set_hl(0, "DiffReviewActiveHunkHeader", { bg = "#303446" })
  vim.api.nvim_set_hl(0, "DiffReviewHunkHeader", { fg = header_fg, bg = "#1e1e1e", nocombine = true })
  vim.api.nvim_set_hl(0, "DiffReviewStatusHeader", { fg = "#f8f8f2", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewStatusHint", { fg = "#9ca3af" })
  vim.api.nvim_set_hl(0, "DiffReviewStatusHintKey", { fg = "#f8f8f2", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewStatusFold", { fg = "#9ca3af" })
  vim.api.nvim_set_hl(0, "DiffReviewStatusPath", { fg = "#d4d4d4" })
  vim.api.nvim_set_hl(0, "DiffReviewStatusObjectId", { fg = "#6b7280" })
  vim.api.nvim_set_hl(0, "DiffReviewStatusBranch", { fg = "#f1fa8c", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewStatusRemote", { fg = "#00afff", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewStatusLabel", { fg = "#c0c0c0", bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewHunkContext", {
    fg = header_fg,
    bg = "#1e1e1e",
    underline = true,
    nocombine = true,
  })
end
setup_bg_highlights()

--- Compute added/removed line counts from diff text
---@param diff_text string
---@return number added, number removed
local function count_stats(diff_text)
  local added, removed = 0, 0
  for line in diff_text:gmatch("[^\n]+") do
    local first = line:sub(1, 1)
    if first == "+" and not line:find("^%+%+%+") then
      added = added + 1
    elseif first == "-" and not line:find("^%-%-%-") then
      removed = removed + 1
    end
  end
  return added, removed
end

---@param filename string
---@return number?
local function ensure_file_buffer(filename)
  local buf = vim.fn.bufnr(filename)
  if buf == -1 then
    buf = vim.fn.bufadd(filename)
    if buf == -1 then
      return nil
    end
  end
  if not vim.api.nvim_buf_is_loaded(buf) then
    local ok = pcall(vim.fn.bufload, buf)
    if not ok then
      return nil
    end
  end
  return vim.api.nvim_buf_is_loaded(buf) and buf or nil
end

---@param filename string
---@param contents? string[]
---@return string
local function detect_filetype(filename, contents)
  local args = {
    filename = filename,
    buf = ensure_file_buffer(filename) or 0,
  }
  if contents then
    args.contents = contents
  end
  return vim.filetype.match(args) or ""
end

---@param message string
---@param title? string
local function notify_error(message, title)
  vim.notify(message, vim.log.levels.ERROR, { title = title or "Diff Review" })
end

---@return string? root
---@return string? err
local function git_root()
  local root = vim.fn.systemlist({ "git", "rev-parse", "--show-toplevel" })[1]
  if vim.v.shell_error ~= 0 or not root or root == "" then
    return nil, "Not a git repository"
  end
  return vim.trim(root), nil
end

---@param path string
---@return string
local function normalize_path(path)
  return vim.fs.normalize(vim.fn.fnamemodify(path, ":p")):gsub("\\", "/"):gsub("/$", "")
end

---@param filename string
---@param root string
---@return string? relpath
---@return string? err
local function repo_relative(filename, root)
  local absolute = normalize_path(filename)
  local root_path = normalize_path(root)
  local compare_absolute = absolute
  local compare_root = root_path
  if package.config:sub(1, 1) == "\\" then
    compare_absolute = compare_absolute:lower()
    compare_root = compare_root:lower()
  end
  if compare_absolute == compare_root then
    return ".", nil
  end
  local prefix = compare_root .. "/"
  if compare_absolute:sub(1, #prefix) ~= prefix then
    return nil, ("Path is outside the git root: %s"):format(filename)
  end
  return absolute:sub(#root_path + 2), nil
end

---@param root string
---@param args string[]
---@param input? string
---@return { ok: boolean, code: integer, output: string, root: string, args: string[] }
local function run_git_at_root(root, args, input)
  local command = { "git", "-C", root }
  vim.list_extend(command, args)
  local output
  if input ~= nil then
    output = vim.fn.system(command, input)
  else
    output = vim.fn.system(command)
  end
  return {
    ok = vim.v.shell_error == 0,
    code = vim.v.shell_error,
    output = vim.trim(output or ""),
    root = root,
    args = args,
  }
end

---@param args string[]
---@param input? string
---@return { ok: boolean, code: integer, output: string, root?: string, args: string[] }
local function run_git(args, input)
  local root, root_err = git_root()
  if not root then
    return {
      ok = false,
      code = -1,
      output = root_err or "Unable to find git root",
      args = args,
    }
  end
  return run_git_at_root(root, args, input)
end

---@param failures table[]
---@return string
local function format_failures(failures)
  local lines = {}
  local max_lines = math.min(#failures, 6)
  for index = 1, max_lines do
    local failure = failures[index]
    local label = failure.file and vim.fn.fnamemodify(failure.file, ":.") or "git"
    local detail = failure.message or failure.output or "unknown error"
    if detail == "" then
      detail = ("git exited %d"):format(failure.code or 1)
    end
    detail = detail:gsub("\n+", " ")
    lines[#lines + 1] = ("  %s: %s"):format(label, detail)
  end
  if #failures > max_lines then
    lines[#lines + 1] = ("  ... %d more"):format(#failures - max_lines)
  end
  return table.concat(lines, "\n")
end

---@param title string
---@param failures table[]
function M.notify_git_failures(title, failures)
  if #failures == 0 then return end
  local count = #failures
  local noun = count == 1 and "failure" or "failures"
  notify_error(("%s: %d %s\n%s"):format(title, count, noun, format_failures(failures)))
end

---@param files string[]
---@param args_for_file fun(relpath: string): string[]
---@param title string
---@return { ok: boolean, successes: string[], failures: table[] }
local function run_file_batch(files, args_for_file, title)
  local root, root_err = git_root()
  if not root then
    local failures = { { message = root_err or "Unable to find git root" } }
    M.notify_git_failures(title, failures)
    return { ok = false, successes = {}, failures = failures }
  end

  local successes = {}
  local failures = {}
  local seen = {}
  for _, filename in ipairs(files) do
    if filename and filename ~= "" and not seen[filename] then
      seen[filename] = true
      local relpath, rel_err = repo_relative(filename, root)
      if not relpath then
        failures[#failures + 1] = { file = filename, message = rel_err }
      else
        local result = run_git_at_root(root, args_for_file(relpath))
        if result.ok then
          successes[#successes + 1] = filename
        else
          result.file = filename
          failures[#failures + 1] = result
        end
      end
    end
  end

  if #failures > 0 then
    M.notify_git_failures(title, failures)
  end
  return { ok = #failures == 0, successes = successes, failures = failures }
end

---@param diff string?
---@return boolean
function M.stage_patch(diff)
  if not diff or diff == "" then
    notify_error("No patch to stage")
    return false
  end
  local result = run_git({ "apply", "--cached", "--whitespace=nowarn", "-" }, diff .. "\n")
  if not result.ok then
    notify_error("Stage failed: " .. (result.output ~= "" and result.output or ("git exited " .. result.code)))
    return false
  end
  return true
end

---@param diff string?
---@return boolean
function M.unstage_patch(diff)
  if not diff or diff == "" then
    notify_error("No patch to unstage")
    return false
  end
  local result = run_git({ "apply", "--cached", "--reverse", "--whitespace=nowarn", "-" }, diff .. "\n")
  if not result.ok then
    notify_error("Unstage failed: " .. (result.output ~= "" and result.output or ("git exited " .. result.code)))
    return false
  end
  return true
end

---@param files string[]
---@return { ok: boolean, successes: string[], failures: table[] }
function M.stage_files(files)
  return run_file_batch(files, function(relpath)
    return { "add", "--", relpath }
  end, "Stage failed")
end

---@param files string[]
---@return { ok: boolean, successes: string[], failures: table[] }
function M.unstage_files(files)
  return run_file_batch(files, function(relpath)
    return { "restore", "--staged", "--", relpath }
  end, "Unstage failed")
end

---@param line string
---@return string? status
---@return string? file
local function parse_name_status_line(line)
  local parts = vim.split(line, "\t", { plain = true })
  local status = parts[1]
  local file = parts[2]
  if status and (status:sub(1, 1) == "R" or status:sub(1, 1) == "C") then
    file = parts[3] or file
  end
  if status and file then
    return status, file
  end
  status, file = line:match("^(%S+)%s+(.+)$")
  return status, file
end

--- Parse unified diff output into structured file/hunk data
---@param diff_output string
---@param staged boolean
---@return table[] hunks
local function parse_diff(diff_output, staged)
  local hunks = {}
  local lines = vim.split(diff_output, "\n", { plain = true })
  local file_header = {} ---@type string[]
  local current_file = nil
  local current_hunk_lines = nil ---@type string[]?
  local current_hunk_start = nil ---@type number?
  local current_hunk_context = nil ---@type string?

  local function flush_hunk()
    if current_file and current_hunk_lines and current_hunk_start then
      local full_diff = vim.list_extend(vim.deepcopy(file_header), current_hunk_lines)
      local a, r = count_stats(table.concat(current_hunk_lines, "\n"))
      -- Find the first actual changed line (skip leading context lines)
      -- to jump to the change, not the context before it
      local change_offset = 0
      for i = 2, #current_hunk_lines do -- skip the @@ header at index 1
        local first = current_hunk_lines[i]:sub(1, 1)
        if first == "+" or first == "-" then
          break
        end
        change_offset = change_offset + 1
      end
      hunks[#hunks + 1] = {
        file = current_file,
        pos = current_hunk_start + change_offset,
        context = current_hunk_context,
        diff = table.concat(full_diff, "\n"),
        staged = staged,
        added = a,
        removed = r,
      }
    end
    current_hunk_lines = nil
    current_hunk_start = nil
    current_hunk_context = nil
  end

  for _, line in ipairs(lines) do
    if line:find("^diff ") then
      flush_hunk()
      file_header = { line }
      current_file = nil
    elseif line:find("^%-%-%- ") or line:find("^%+%+%+ ") or line:find("^index ")
        or line:find("^new file") or line:find("^deleted file")
        or line:find("^rename ") or line:find("^similarity")
        or line:find("^old mode") or line:find("^new mode") then
      file_header[#file_header + 1] = line
      if line:find("^%+%+%+ b/") then
        current_file = line:sub(7) -- strip "+++ b/"
      elseif line:find("^%+%+%+ /dev/null") and current_file == nil then
        -- deleted file: get name from --- a/
        -- will be set from the --- line
      elseif line:find("^%-%-%- a/") and current_file == nil then
        current_file = line:sub(7) -- strip "--- a/"
      end
    elseif line:find("^@@") then
      flush_hunk()
      -- Parse @@ -old,count +new,count @@ context
      local new_start, context = line:match("^@@ %-%d+,?%d* %+(%d+),?%d* @@ ?(.*)")
      if not new_start then
        new_start = line:match("^@@ %-%d+,?%d* %+(%d+),?%d* @@")
      end
      current_hunk_start = tonumber(new_start) or 1
      current_hunk_context = (context and context ~= "") and context or nil
      current_hunk_lines = { line }
    elseif current_hunk_lines then
      current_hunk_lines[#current_hunk_lines + 1] = line
    end
  end
  flush_hunk()

  return hunks
end

--- Run git diff and return parsed hunks
---@param cwd string
---@param staged boolean
---@return table[]
local function get_hunks(cwd, staged)
  local args = { "git", "-C", cwd, "-c", "core.quotepath=false",
    "diff", "--no-color", "--no-ext-diff" }
  if staged then
    args[#args + 1] = "--cached"
  end
  local result = vim.fn.systemlist(args)
  if vim.v.shell_error ~= 0 then
    return {}
  end
  return parse_diff(table.concat(result, "\n"), staged)
end

--- Order a single file's hunks by line position and split out the diff
--- patches and their matching staged flags. Keeping line order means a hunk
--- stays put when staged/unstaged — it only folds, never jumps to the end of
--- the file (unstaged and staged hunks would otherwise group separately).
---@param hunks table[] hunks for one file, each with .pos, .staged, .diff
---@return string[] diffs, boolean[] staged_flags
local function order_file_hunks(hunks)
  table.sort(hunks, function(a, b)
    if a.pos ~= b.pos then
      return a.pos < b.pos
    end
    -- Same position: show the unstaged hunk before the staged one
    return not a.staged and b.staged
  end)
  local diffs, flags = {}, {}
  for _, hunk in ipairs(hunks) do
    if hunk.diff then
      diffs[#diffs + 1] = hunk.diff
      flags[#flags + 1] = hunk.staged
    end
  end
  return diffs, flags
end

--- Fetch one file's hunks (unstaged + staged) from git, ordered by line
--- position. Returns the combined diff text and per-hunk staged flags, or
--- (nil, nil) when the file has no hunks.
---@param cwd string git root
---@param filename string absolute path
---@return string? diff_text, boolean[]? staged_flags
local function file_diff_and_flags(cwd, filename)
  local norm = vim.fs.normalize(filename)
  local hunks = {}
  for _, staged in ipairs({ false, true }) do
    for _, hunk in ipairs(get_hunks(cwd, staged)) do
      if vim.fs.normalize(vim.fn.fnamemodify(cwd .. "/" .. hunk.file, ":p")) == norm then
        hunks[#hunks + 1] = hunk
      end
    end
  end
  local diffs, flags = order_file_hunks(hunks)
  if #diffs == 0 then
    return nil, nil
  end
  return table.concat(diffs, "\n"), flags
end

--- Build a synthetic "new file" diff (every line an addition) for an untracked
--- file, so it previews as a single hunk straight from disk — no git, which is
--- both faster and the only way to show content git doesn't track yet.
---@param filename string absolute path
---@param relpath string path relative to the git root (forward slashes)
---@return string? diff_text
local function build_untracked_diff(filename, relpath)
  local ok, lines = pcall(vim.fn.readfile, filename)
  if not ok or type(lines) ~= "table" or #lines == 0 then
    return nil
  end
  -- Skip binary files (a NUL byte in any line).
  for _, line in ipairs(lines) do
    if line:find("\0", 1, true) then
      return nil
    end
  end
  local out = {
    "diff --git a/" .. relpath .. " b/" .. relpath,
    "new file mode 100644",
    "--- /dev/null",
    "+++ b/" .. relpath,
    "@@ -0,0 +1," .. #lines .. " @@",
  }
  for _, line in ipairs(lines) do
    out[#out + 1] = "+" .. line
  end
  return table.concat(out, "\n")
end

--- Compute treesitter-based scope context for a hunk at a given line.
--- Returns a string like "MyClass.my_method" or nil if no context found.
---@param filename string absolute path
---@param line number 1-based line number
---@return string?
function M.compute_hunk_context(filename, line)
  local buf = ensure_file_buffer(filename)
  if not buf then return nil end

  -- Get treesitter language
  local ft = vim.bo[buf].filetype
  if ft == "" then
    ft = detect_filetype(filename)
  end
  local lang = vim.treesitter.language.get_lang(ft)
  if not lang then return nil end

  -- Try to load our diff_context query
  local ok, query = pcall(vim.treesitter.query.get, lang, "diff_context")
  if not ok or not query then return nil end

  -- Get parser and parse
  local pok, parser = pcall(vim.treesitter.get_parser, buf, lang)
  if not pok or not parser then return nil end
  local trees = parser:parse()
  if not trees or #trees == 0 then return nil end

  -- Find all @scope nodes that contain the target line (0-indexed)
  local target = line - 1
  local scopes = {}
  for id, node in query:iter_captures(trees[1]:root(), buf) do
    if query.captures[id] == "scope" then
      local sr, _, er, _ = node:range()
      if sr <= target and er >= target then
        scopes[#scopes + 1] = { node = node, sr = sr, er = er }
      end
    end
  end

  if #scopes == 0 then return nil end

  -- Sort by start row descending (innermost scope first)
  table.sort(scopes, function(a, b) return a.sr > b.sr end)

  -- Limit to 3 levels max
  local max_depth = math.min(#scopes, 3)

  -- Collect names from outermost to innermost
  local names = {}
  for i = max_depth, 1, -1 do
    local scope = scopes[i]
    -- Find @scope.name within this scope's node
    for cid, cnode in query:iter_captures(scope.node, buf) do
      if query.captures[cid] == "scope.name" then
        local name_text = vim.treesitter.get_node_text(cnode, buf)
        if name_text and name_text ~= "" then
          names[#names + 1] = name_text
        end
        break
      end
    end
  end

  if #names == 0 then return nil end
  return table.concat(names, ".")
end

-- Cache for treesitter context per file (cleared on refresh)
M._ts_context_cache = {}

function M.setup()
  setup_bg_highlights()
end

--- Open the original Trouble-backed DiffReview picker.
function M.open_trouble()
  setup_bg_highlights()
  M._main_win = nil
  local view = require("trouble").open("diff_review")
  if view then
    view.first_render:next(function()
      view:fold_level({ level = 1 })
      -- Show hint + branch in the winbar
      local branch = vim.trim(vim.fn.systemlist({ "git", "rev-parse", "--abbrev-ref", "HEAD" })[1] or "")
      local winbar = " %#Comment#<Tab>%* toggle | %#Comment#S%* stage | %#Comment#U%* unstage | %#Comment#j%* discard | %#Comment#cc%* commit | %#Comment#<CR>%* jump | %#Comment#q%* close | %#Comment#?%* help"
      if branch ~= "" then
        winbar = winbar .. "%=" .. "%#Keyword# " .. branch .. " %*"
      end
      vim.wo[view.win.win].winbar = winbar
      M.auto_preview(view)
      -- Clean up diff buffers when Trouble closes
      view.win:on("WinClosed", function()
        M._cleanup_diff_buffers()
      end)
      view.win:on("BufWipeout", function()
        M._cleanup_diff_buffers()
      end)
      -- Also watch for view.closed flag
      view.win:on("WinLeave", function()
        vim.defer_fn(function()
          if view.closed then
            M._cleanup_diff_buffers()
          end
        end, 100)
      end)
    end)
  end
end

--- Close and wipe all diff buffers
function M._cleanup_diff_buffers()
  M._restore_line_numbers(M._main_win)
  M._saved_wo = nil
  M._diff_bufs = M._diff_bufs or {}
  for key, buf in pairs(M._diff_bufs) do
    if vim.api.nvim_buf_is_valid(buf) then
      vim.api.nvim_buf_delete(buf, { force = true })
    end
  end
  M._diff_bufs = {}
  M._buf_hunks = {}
  M._buf_filename = {}
  M._buf_saved_cursor = {}
  M._main_win = nil
end

---@param cb fun(items: trouble.Item[])
---@param _ctx trouble.Source.ctx
function M.get(cb, _ctx)
  M._ts_context_cache = {} -- clear treesitter context cache on refresh
  M._untracked = {} -- map of absolute path -> repo-relative path for untracked files
  local cwd = git_root()
  if not cwd then
    cb({})
    return
  end

  -- Get both unstaged and staged hunks
  local unstaged = get_hunks(cwd, false)
  local staged = get_hunks(cwd, true)

  local all_hunks = {}
  vim.list_extend(all_hunks, unstaged)
  vim.list_extend(all_hunks, staged)

  -- Get untracked files
  local untracked_files = {}
  local untracked_output = vim.fn.systemlist({ "git", "-C", cwd, "ls-files", "--others", "--exclude-standard" })
  if vim.v.shell_error == 0 then
    for _, f in ipairs(untracked_output) do
      if f ~= "" then
        untracked_files[#untracked_files + 1] = f
      end
    end
  end

  -- Get all changed files (staged + unstaged) to catch files with no hunks
  -- (e.g., empty new files, binary files)
  local tracked_files_with_hunks = {}
  for _, h in ipairs(all_hunks) do
    tracked_files_with_hunks[h.file] = true
  end
  local staged_name_status = vim.fn.systemlist({ "git", "-C", cwd, "diff", "--cached", "--name-status" })
  if vim.v.shell_error == 0 then
    for _, line in ipairs(staged_name_status) do
      local status, file = parse_name_status_line(line)
      if file and not tracked_files_with_hunks[file] then
        -- File has staged changes but no hunks (empty new file, binary, etc.)
        all_hunks[#all_hunks + 1] = {
          file = file,
          pos = 1,
          context = nil,
          diff = nil,
          staged = true,
          added = 0,
          removed = 0,
          status = status,
        }
      end
    end
  end
  local unstaged_name_status = vim.fn.systemlist({ "git", "-C", cwd, "diff", "--name-status" })
  if vim.v.shell_error == 0 then
    for _, line in ipairs(unstaged_name_status) do
      local status, file = parse_name_status_line(line)
      if file and not tracked_files_with_hunks[file] then
        all_hunks[#all_hunks + 1] = {
          file = file,
          pos = 1,
          context = nil,
          diff = nil,
          staged = false,
          added = 0,
          removed = 0,
          status = status,
        }
      end
    end
  end

  -- Compute per-file aggregate stats and staging state
  local file_stats = {} ---@type table<string, { added: number, removed: number, total: number, staged: number }>
  for _, hunk in ipairs(all_hunks) do
    local f = hunk.file
    if not file_stats[f] then
      file_stats[f] = { added = 0, removed = 0, total = 0, staged = 0 }
    end
    local fs = file_stats[f]
    fs.added = fs.added + hunk.added
    fs.removed = fs.removed + hunk.removed
    fs.total = fs.total + 1
    if hunk.staged then
      fs.staged = fs.staged + 1
    end
  end

  local items = {}
  for _, hunk in ipairs(all_hunks) do
    local filename = vim.fn.fnamemodify(cwd .. "/" .. hunk.file, ":p")
    -- Use treesitter scope context if available, fall back to git's @@ context
    local context_text = hunk.context or ""
    if not (_ctx and _ctx.skip_ts_context) then
      local cache_key = filename .. ":" .. hunk.pos
      if not M._ts_context_cache[cache_key] then
        local ts_ctx = M.compute_hunk_context(filename, hunk.pos)
        M._ts_context_cache[cache_key] = ts_ctx or false -- false = no context found
      end
      local cached = M._ts_context_cache[cache_key]
      if cached and cached ~= false then
        context_text = cached
      end
    end
    -- Parse range numbers from @@ header
    local full_header = hunk.status or "@@"
    if hunk.diff then
      full_header = hunk.diff:match("\n(@@[^@]+@@)") or hunk.diff:match("^(@@[^@]+@@)") or "@@"
    end
    local old_range = full_header:match("%-(%d+,?%d*)") or ""
    local new_range = full_header:match("%+(%d+,?%d*)") or ""
    local range_text = "-" .. old_range .. " +" .. new_range

    local fs = file_stats[hunk.file]
    local file_check
    if fs.staged == fs.total then
      file_check = "[x]"
    elseif fs.staged > 0 then
      file_check = "[-]"
    else
      file_check = "[ ]"
    end

    items[#items + 1] = Item.new({
      source = "diff_review",
      filename = filename,
      pos = { hunk.pos, 0 },
      item = {
        category = "Tracked Changes",
        check = hunk.staged and "[x]" or "[ ]",
        file_check = file_check,
        hunk_header = range_text,
        old_range = "-" .. old_range,
        new_range = "+" .. new_range,
        context_text = context_text,
        staged = hunk.staged,
        diff = hunk.diff,
        added = hunk.added,
        removed = hunk.removed,
        added_pad = hunk.added,
        removed_pad = hunk.removed,
        file_added = fs.added,
        file_removed = fs.removed,
      },
    })
  end

  -- Add untracked files
  for _, f in ipairs(untracked_files) do
    local filename = vim.fn.fnamemodify(cwd .. "/" .. f, ":p")
    M._untracked[filename] = f -- remember repo-relative path for the synthetic diff
    items[#items + 1] = Item.new({
      source = "diff_review",
      filename = filename,
      pos = { 1, 0 },
      item = {
        category = "Untracked Files",
        check = "[ ]",
        file_check = "[ ]",
        hunk_header = "new file",
        context_text = "",
        staged = false,
        diff = nil,
        added = 0,
        removed = 0,
        stats = "new",
      },
    })
  end

  -- Build per-file combined diffs + staged status for file-level preview.
  -- order_file_hunks keeps hunks in line order so staging folds a hunk in
  -- place instead of moving it to the end of the file.
  local file_hunks = {} ---@type table<string, table[]>
  for _, hunk in ipairs(all_hunks) do
    local f = hunk.file
    file_hunks[f] = file_hunks[f] or {}
    file_hunks[f][#file_hunks[f] + 1] = hunk
  end
  M._file_diffs = {}
  M._file_hunk_staged = {}
  for f, hunks in pairs(file_hunks) do
    local diffs, flags = order_file_hunks(hunks)
    local filename = vim.fn.fnamemodify(cwd .. "/" .. f, ":p")
    M._file_diffs[filename] = table.concat(diffs, "\n")
    M._file_hunk_staged[filename] = flags
  end

  Item.add_text(items, { mode = "after" })
  cb(items)

  -- Pre-render all diff buffers so file switching is instant
  if not (_ctx and _ctx.skip_pre_render) then
    vim.schedule(function()
      for filename, diff_text in pairs(M._file_diffs) do
        if diff_text and diff_text ~= "" then
          local buf = M.open_diff_buffer(filename)
          M._refresh_diff_buffer(buf, filename)
        end
      end
    end)
  end
end

-- Cache of diff preview items keyed by filename
M._diff_items = {}

-- Namespace for Snacks diff rendering
M._ns = vim.api.nvim_create_namespace("diff_review_preview")

-- Map Snacks diff highlights to bg-only versions so treesitter fg is preserved
-- and the background covers the full line (via the existing add_eol overlays)
local hl_replacements = {
  SnacksDiffAdd = "DiffReviewAddBg",
  SnacksDiffDelete = "DiffReviewDeleteBg",
  SnacksDiffAddLineNr = "DiffReviewAddLineNr",
  SnacksDiffDeleteLineNr = "DiffReviewDeleteLineNr",
  SnacksDiffContext = "DiffReviewContextBg",
  SnacksDiffContextLineNr = "DiffReviewContextLineNr",
}

--- Recursively replace Snacks diff highlights with bg-only versions
--- in a highlight spec (string, table of strings, or nested)
local function replace_hl(h)
  if type(h) == "string" then
    return hl_replacements[h] or h
  elseif type(h) == "table" then
    local new = {}
    for i, v in ipairs(h) do
      new[i] = replace_hl(v)
    end
    return new
  end
  return h
end

--- Walk a line's highlight entries and replace diff hl groups with bg-only versions
local function rewrite_line_hls(line)
  for _, entry in ipairs(line) do
    -- Text entry: { text, hl_group, ... }
    if type(entry[1]) == "string" and entry[2] ~= nil then
      entry[2] = replace_hl(entry[2])
    end
    -- Extmark entry with virt_text: { col=N, virt_text={ {text, hl}, ... } }
    if entry.virt_text then
      for _, vt in ipairs(entry.virt_text) do
        if vt[2] then
          vt[2] = replace_hl(vt[2])
        end
      end
    end
  end
end

local function build_fancy_diff_rows(diff_text, hunk_staged, filename)
  local snacks_diff = require("snacks.picker.util.diff")

  local diff = snacks_diff.get_diff(diff_text)
  local opts = { hunk_header = false }
  local ctx_base = setmetatable({ diff = diff, opts = opts }, { __index = function(_, k)
    if k == "extend" then
      return function(self2, t)
        return setmetatable(t, { __index = self2 })
      end
    end
  end })

  local ret = {} ---@type snacks.picker.Highlight[][]
  local hunk_idx = 0

  for _, block in ipairs(diff.blocks) do
    local block_ctx = ctx_base:extend({ block = block })
    for _, hunk in ipairs(block.hunks) do
      hunk_idx = hunk_idx + 1
      -- Checkbox + @@ separator with treesitter context
      local range_line = hunk.diff[1] or "@@"
      local range_only = range_line:match("^(@@[^@]+@@)") or range_line
      -- Get the new-file start line for treesitter context lookup
      local hunk_line = tonumber(range_line:match("%+(%d+)")) or 1
      local ts_context = nil
      if filename then
        local cache_key = filename .. ":" .. hunk_line
        if M._ts_context_cache[cache_key] == nil then
          M._ts_context_cache[cache_key] = M.compute_hunk_context(filename, hunk_line) or false
        end
        local cached = M._ts_context_cache[cache_key]
        if cached and cached ~= false then
          ts_context = cached
        end
      end
      -- Parse old/new ranges: @@ -222,6 +226,34 @@
      local old_range = range_line:match("%-(%d+,?%d*)") or ""
      local new_range = range_line:match("%+(%d+,?%d*)") or ""
      -- Count added/removed lines in this hunk
      local h_added, h_removed = 0, 0
      local counting = false
      for _, dl in ipairs(hunk.diff) do
        if counting then
          local p = dl:sub(1, 1)
          if p == "+" then h_added = h_added + 1
          elseif p == "-" then h_removed = h_removed + 1 end
        elseif dl:match("^@@") then
          counting = true
        end
      end
      -- Format: @@ MyClass.method +4 -0
      local header_parts = {
        { "@@ ", "DiffReviewHunkHeader" },
      }
      if ts_context then
        header_parts[#header_parts + 1] = { ts_context, "DiffReviewHunkContext" }
        header_parts[#header_parts + 1] = { " ", "DiffReviewHunkHeader" }
      end
      header_parts[#header_parts + 1] = { ("+%d"):format(h_added), "DiffReviewAddRange" }
      header_parts[#header_parts + 1] = { " ", "DiffReviewHunkHeader" }
      header_parts[#header_parts + 1] = { ("-%d"):format(h_removed), "DiffReviewDeleteRange" }
      ret[#ret + 1] = header_parts

      local hunk_ctx = block_ctx:extend({ hunk = hunk })
      local block_file = block.file
      local match = vim.filetype.match
      vim.filetype.match = function(args)
        if type(args) == "table" and args.buf == nil and args.filename == block_file then
          return match(vim.tbl_extend("force", args, {
            filename = filename or block_file,
            buf = ensure_file_buffer(filename or block_file) or 0,
          }))
        end
        return match(args)
      end
      local ok, hunk_lines = pcall(snacks_diff.format_hunk, hunk_ctx)
      vim.filetype.match = match
      if not ok then
        error(hunk_lines)
      end
      vim.list_extend(ret, hunk_lines)
    end
  end

  -- Replace Snacks diff highlights with bg-only versions in-place
  for _, line in ipairs(ret) do
    rewrite_line_hls(line)
  end

  return ret
end

--- Render a fancy diff into a buffer using Snacks' diff renderer,
--- but skip file name headers and hunk context headers for a cleaner look.
--- Replaces Snacks' diff highlight groups with bg-only versions so that
--- treesitter syntax highlighting is preserved and the full line has a
--- colored background.
--- @param buf number
--- @param diff_text string
--- @param hunk_staged? boolean[] staged status per hunk (in order)
local function render_fancy_diff(buf, diff_text, hunk_staged, filename)
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, {})
  vim.bo[buf].modifiable = false
  local ft = filename and detect_filetype(filename) or ""
  if ft ~= "" and vim.bo[buf].filetype ~= ft then
    vim.bo[buf].filetype = ft
  end

  local H = Snacks.picker.highlight
  H.render(buf, M._ns, build_fancy_diff_rows(diff_text, hunk_staged, filename))
end

-- Per-buffer hunk metadata: maps buffer line ranges to raw diff patches
M._buf_hunks = {}
-- Per-buffer filename mapping
M._buf_filename = {}
-- Per-buffer saved cursor position (for restoring after jumping to file)
M._buf_saved_cursor = {}

--- Find which hunk the cursor is in within a diff buffer.
--- Returns the hunk's complete diff patch (with file header) or nil.
---@param buf number
---@return string? diff_patch
---@return number? hunk_start_line
local function get_hunk_at_cursor(buf)
  local hunks = M._buf_hunks[buf]
  if not hunks then return end
  local cursor = vim.api.nvim_win_get_cursor(0)[1]
  for _, h in ipairs(hunks) do
    if cursor >= h.start_line and cursor <= h.end_line then
      return h.diff, h.start_line
    end
  end
end

--- Create (or reuse) a real diff buffer with keymaps.
--- Call _refresh_diff_buffer after setting the buffer on a window.
---@param filename string
---@return number buf
function M.open_diff_buffer(filename)
  local key = "diff:" .. filename
  M._diff_bufs = M._diff_bufs or {}
  local buf = M._diff_bufs[key]

  if not buf or not vim.api.nvim_buf_is_valid(buf) then
    buf = vim.api.nvim_create_buf(true, false)
    vim.bo[buf].bufhidden = "hide"
    vim.bo[buf].buftype = "nofile"
    vim.bo[buf].swapfile = false
    local short = vim.fn.fnamemodify(filename, ":t")
    -- Basenames collide across directories (E95); fall back to a unique name.
    local name = "diff://" .. short
    if not pcall(vim.api.nvim_buf_set_name, buf, name) then
      pcall(vim.api.nvim_buf_set_name, buf, name .. "#" .. buf)
    end
    M._diff_bufs[key] = buf
    M._buf_filename[buf] = filename

    -- Save cursor + invalidate caches on leaving the diff buffer
    vim.api.nvim_create_autocmd("BufLeave", {
      buffer = buf,
      callback = function()
        if not vim.api.nvim_buf_is_valid(buf) then return end
        -- Defer cache invalidation: only invalidate if we switched to a real
        -- file buffer (not Trouble or another diff buffer). This avoids
        -- re-running git on every s/t movement in the Trouble list.
        vim.schedule(function()
          if not vim.api.nvim_buf_is_valid(buf) then return end
          local cur_win_buf = vim.api.nvim_get_current_buf()
          local ft = vim.bo[cur_win_buf].filetype
          local bt = vim.bo[cur_win_buf].buftype
          if ft ~= "trouble" and bt ~= "nofile" then
            M._buf_last_rendered[buf] = nil
            if M._file_diffs then M._file_diffs[filename] = nil end
          end
        end)
        local cur = vim.api.nvim_win_get_cursor(0)
        local hunks = M._buf_hunks[buf]
        if hunks then
          for si, sh in ipairs(hunks) do
            if cur[1] >= sh.start_line and cur[1] <= sh.end_line then
              local header = sh.diff and sh.diff:match("(@@[^@]+@@)") or ""
              M._buf_saved_cursor[buf] = {
                hunk_index = si,
                offset = cur[1] - sh.start_line,
                col = cur[2],
                header = header,
              }
              return
            end
          end
        end
        -- Not in any hunk, save raw position as fallback
        M._buf_saved_cursor[buf] = {
          hunk_index = 1,
          offset = 0,
          col = cur[2],
          header = "",
          raw_line = cur[1],
        }
      end,
    })



    -- Refresh and restore cursor when entering the diff buffer directly
    -- (e.g., via window-switch keybind after editing the real file)
    vim.api.nvim_create_autocmd("BufEnter", {
      buffer = buf,
      callback = function()
        if not vim.api.nvim_buf_is_valid(buf) or not M._buf_filename[buf] then return end
        -- Re-hide the number column: re-entering the diff buffer (e.g. a
        -- window switch back from the real file) restores it otherwise.
        M._hide_line_numbers(vim.api.nvim_get_current_win())
        vim.schedule(function()
          if not vim.api.nvim_buf_is_valid(buf) then return end
          -- Re-fetch diff if cache was invalidated
          if not M._file_diffs or M._file_diffs[filename] == nil then
            M._update_file_diff_cache(filename)
          end
          M._refresh_diff_buffer(buf, filename)
          -- Restore saved cursor
          local saved = M._buf_saved_cursor[buf]
          local new_hunks = M._buf_hunks[buf]
          if saved and new_hunks then
            local target_hunk = nil
            if saved.header and saved.header ~= "" then
              for _, h in ipairs(new_hunks) do
                local hh = h.diff and h.diff:match("(@@[^@]+@@)") or ""
                if hh == saved.header then
                  target_hunk = h
                  break
                end
              end
            end
            if not target_hunk and saved.hunk_index and saved.hunk_index <= #new_hunks then
              target_hunk = new_hunks[saved.hunk_index]
            end
            if target_hunk then
              local line = target_hunk.start_line + (saved.offset or 0)
              local max = vim.api.nvim_buf_line_count(buf)
              line = math.min(math.max(1, line), max)
              pcall(vim.api.nvim_win_set_cursor, 0, { line, saved.col or 0 })
              pcall(vim.cmd, "normal! zv")
            elseif saved.raw_line then
              local max = vim.api.nvim_buf_line_count(buf)
              pcall(vim.api.nvim_win_set_cursor, 0, { math.min(saved.raw_line, max), saved.col or 0 })
            end
            M._buf_saved_cursor[buf] = nil
          end
        end)
      end,
    })

    local kopts = { buffer = buf, silent = true }

    -- Close both diff buffer and trouble
    vim.keymap.set("n", "q", function()
      require("trouble").close("diff_review")
      M._cleanup_diff_buffers()
    end, vim.tbl_extend("force", kopts, { desc = "Close DiffReview", nowait = true }))


    -- Jump to next/prev hunk header
    vim.keymap.set("n", "]c", function()
      local cursor = vim.api.nvim_win_get_cursor(0)
      local lines = vim.api.nvim_buf_get_lines(buf, cursor[1], -1, false)
      for i, line in ipairs(lines) do
        if line:match("^@@") then
          vim.api.nvim_win_set_cursor(0, { cursor[1] + i, 0 })
          return
        end
      end
    end, vim.tbl_extend("force", kopts, { desc = "Next hunk" }))

    vim.keymap.set("n", "[c", function()
      local cursor = vim.api.nvim_win_get_cursor(0)
      local lines = vim.api.nvim_buf_get_lines(buf, 0, cursor[1] - 1, false)
      for i = #lines, 1, -1 do
        if lines[i]:match("^@@") then
          vim.api.nvim_win_set_cursor(0, { i, 0 })
          return
        end
      end
    end, vim.tbl_extend("force", kopts, { desc = "Prev hunk" }))

    -- Jump to the corresponding line in the actual file
    vim.keymap.set("n", "<CR>", function()
      local target_file = M._buf_filename[buf]
      if not target_file then return end

      -- Leaving the diff preview for the real file: restore its number column.
      M._restore_line_numbers(vim.api.nvim_get_current_win())

      -- Save cursor as hunk-relative position so we can restore after re-render.
      -- Store: { hunk_index, offset_within_hunk, col, hunk_header }
      local save_cursor = vim.api.nvim_win_get_cursor(0)
      local save_hunks = M._buf_hunks[buf]
      if save_hunks then
        for si, sh in ipairs(save_hunks) do
          if save_cursor[1] >= sh.start_line and save_cursor[1] <= sh.end_line then
            local header = sh.diff and sh.diff:match("(@@[^@]+@@)") or ""
            M._buf_saved_cursor[buf] = {
              hunk_index = si,
              offset = save_cursor[1] - sh.start_line,
              col = save_cursor[2],
              header = header,
            }
            break
          end
        end
      end
      if not M._buf_saved_cursor[buf] or type(M._buf_saved_cursor[buf]) ~= "table" or not M._buf_saved_cursor[buf].hunk_index then
        M._buf_saved_cursor[buf] = { hunk_index = 1, offset = 0, col = 0, header = "" }
      end

      local hunks = M._buf_hunks[buf]
      local cursor = save_cursor[1]
      local raw_col = save_cursor[2] or 0
      -- Adjust column for the Snacks diff gutter.
      -- The gutter (line numbers + prefix) is rendered as virtual text
      -- overlays, but the buffer text has matching spaces as padding.
      -- The padding width = all leading spaces on a code line.
      -- To distinguish gutter padding from code indentation, check a line
      -- that has actual code (non-space after the gutter). The gutter width
      -- is the same for all lines in a hunk, so find it from any code line.
      -- Compute gutter width: the Snacks renderer pads buffer lines with spaces
      -- for the line number + prefix gutter. The gutter width can be computed from
      -- any code line by finding where the code content starts in the buffer text
      -- vs where it starts in the raw diff line (after stripping the +/-/space prefix).
      local gutter_width = 0
      if hunks then
        for _, h in ipairs(hunks) do
          if cursor >= h.start_line and cursor <= h.end_line then
            -- Parse diff lines for this hunk
            local diff_lines_list = {}
            local found_hdr = false
            for dl in h.diff:gmatch("[^\n]+") do
              if found_hdr then
                diff_lines_list[#diff_lines_list + 1] = dl
              elseif dl:match("^@@") then
                found_hdr = true
              end
            end
            -- Find a non-empty code line to measure gutter
            for l = h.start_line + 1, h.end_line do
              local buf_line = vim.api.nvim_buf_get_lines(buf, l - 1, l, false)[1] or ""
              local dl_idx = l - h.start_line
              if dl_idx >= 1 and dl_idx <= #diff_lines_list then
                local diff_line = diff_lines_list[dl_idx]
                local code = diff_line:sub(2) -- strip +/-/space prefix
                -- Find first non-space char in code
                local code_indent = code:find("%S")
                if code_indent then
                  -- Find same char in buffer line
                  local buf_indent = buf_line:find("%S")
                  if buf_indent then
                    -- Gutter = buf_indent - code_indent
                    gutter_width = buf_indent - code_indent
                    break
                  end
                end
              end
            end
            break
          end
        end
      end
      local cursor_col = math.max(0, raw_col - gutter_width)

      -- No hunks (e.g. "No changes" / empty new file): just open the file
      if not hunks or #hunks == 0 then
        vim.cmd.edit(target_file)
        vim.cmd("normal! zz")
        return
      end

      -- Find which hunk we're in and compute file line number
      for _, h in ipairs(hunks) do
        if cursor >= h.start_line and cursor <= h.end_line then
          -- Parse the @@ header to get the new-file start line
          local new_start = h.diff:match("%+(%d+)")
          new_start = tonumber(new_start) or 1

          -- Walk diff lines to find file line at cursor position
          -- Cursor offset within the hunk (0 = @@ line, 1 = first code line)
          local offset_in_hunk = cursor - h.start_line
          if offset_in_hunk == 0 then
            -- On the @@ line itself, jump to hunk start
            vim.cmd.edit(target_file)
            pcall(vim.api.nvim_win_set_cursor, 0, { new_start, cursor_col })
            vim.cmd("normal! zz")
            return
          end

          -- Walk through diff code lines counting file lines
          local diff_lines = {}
          local found_header = false
          for diff_line in h.diff:gmatch("[^\n]+") do
            if found_header then
              diff_lines[#diff_lines + 1] = diff_line
            elseif diff_line:match("^@@") then
              found_header = true
            end
          end

          local file_line = new_start
          local last_valid_line = new_start
          for i = 1, math.min(offset_in_hunk, #diff_lines) do
            local prefix = diff_lines[i]:sub(1, 1)
            if prefix == " " or prefix == "+" then
              last_valid_line = file_line
              if i < offset_in_hunk then
                file_line = file_line + 1
              end
            elseif prefix == "-" then
              -- Deleted line: doesn't exist in new file
              -- last_valid_line stays as-is
            end
          end

          vim.cmd.edit(target_file)
          local max_line = vim.api.nvim_buf_line_count(0)
          local target_line = math.min(last_valid_line, max_line)
          pcall(vim.api.nvim_win_set_cursor, 0, { target_line, cursor_col })
          vim.cmd("normal! zz")
          return
        end
      end
      -- Cursor not in any hunk: just open the file
      vim.cmd.edit(target_file)
      vim.cmd("normal! zz")
    end, vim.tbl_extend("force", kopts, { desc = "Jump to file", nowait = true }))

    -- Stage the hunk under cursor, then jump to the next hunk
    vim.keymap.set("n", "S", function()
      local patch, hunk_start = get_hunk_at_cursor(buf)
      if not patch then
        vim.notify("No hunk under cursor", vim.log.levels.WARN)
        return
      end
      -- Find the index of the current hunk so we can jump to the next one
      local cur_hunk_idx = nil
      local hunks = M._buf_hunks[buf]
      if hunks and hunk_start then
        for i, h in ipairs(hunks) do
          if h.start_line == hunk_start then
            cur_hunk_idx = i
            break
          end
        end
      end
      if M.stage_patch(patch) then
        vim.notify("Hunk staged", vim.log.levels.INFO)
        local win = vim.api.nvim_get_current_win()
        local filename = M._buf_filename[buf]
        -- Move to the next hunk. The staged hunk keeps its position (it only
        -- folds), so "next" is the following index in the rebuilt hunk map.
        local function goto_next()
          if not vim.api.nvim_win_is_valid(win) or vim.api.nvim_win_get_buf(win) ~= buf then
            return
          end
          local new_hunks = M._buf_hunks[buf]
          if new_hunks and cur_hunk_idx then
            local target = new_hunks[cur_hunk_idx + 1] or new_hunks[cur_hunk_idx]
            if target then
              local max = vim.api.nvim_buf_line_count(buf)
              pcall(vim.api.nvim_win_set_cursor, win, { math.min(target.start_line, max), 0 })
            end
          end
        end
        -- Re-render this file's diff buffer so the staged hunk folds in place.
        if filename then
          M.refresh_open_diff_buffer(filename)
        end
        goto_next()
        require("trouble").refresh("diff_review")
        -- The async list refresh re-renders the buffer (resetting the
        -- cursor), so re-assert the next-hunk position once it settles.
        vim.defer_fn(goto_next, 60)
      end
    end, vim.tbl_extend("force", kopts, { desc = "Stage hunk", nowait = true }))

    -- Unstage the hunk under cursor
    vim.keymap.set("n", "U", function()
      local patch, _ = get_hunk_at_cursor(buf)
      if not patch then
        vim.notify("No hunk under cursor", vim.log.levels.WARN)
        return
      end
      if M.unstage_patch(patch) then
        vim.notify("Hunk unstaged", vim.log.levels.INFO)
        local win = vim.api.nvim_get_current_win()
        local filename = M._buf_filename[buf]
        -- Find the current hunk so we can stay on it after the re-render
        -- (it expands in place; unstaging must not jump to the buffer top).
        local cur_idx, cur_offset = nil, 0
        local cursor_line = vim.api.nvim_win_get_cursor(0)[1]
        local hunks = M._buf_hunks[buf]
        if hunks then
          for i, h in ipairs(hunks) do
            if cursor_line >= h.start_line and cursor_line <= h.end_line then
              cur_idx = i
              cur_offset = cursor_line - h.start_line
              break
            end
          end
        end
        local function stay()
          if not vim.api.nvim_win_is_valid(win) or vim.api.nvim_win_get_buf(win) ~= buf then
            return
          end
          local new_hunks = M._buf_hunks[buf]
          if new_hunks and cur_idx and new_hunks[cur_idx] then
            local max = vim.api.nvim_buf_line_count(buf)
            local line = math.min(new_hunks[cur_idx].start_line + cur_offset, max)
            pcall(vim.api.nvim_win_set_cursor, win, { line, 0 })
          end
        end
        -- Re-render this file's diff buffer so the hunk expands in place.
        if filename then
          M.refresh_open_diff_buffer(filename)
        end
        stay()
        require("trouble").refresh("diff_review")
        -- The async list refresh re-renders the buffer (resetting the
        -- cursor), so re-assert the current-hunk position once it settles.
        vim.defer_fn(stay, 60)
      end
    end, vim.tbl_extend("force", kopts, { desc = "Unstage hunk", nowait = true }))

    -- Toggle fold (collapse/expand) the hunk under cursor
    vim.keymap.set("n", "<Tab>", function()
      local hunks = M._buf_hunks[buf]
      if not hunks then
        vim.notify("No hunk map for buffer", vim.log.levels.WARN)
        return
      end
      local cursor = vim.api.nvim_win_get_cursor(0)[1]
      local found = false
      for i, h in ipairs(hunks) do
        if cursor >= h.start_line and cursor <= h.end_line then
          found = true
          h.folded = not h.folded
          vim.notify("Hunk " .. i .. " folded=" .. tostring(h.folded) .. " range=" .. h.start_line .. "-" .. h.end_line, vim.log.levels.INFO)
          M._render_with_folds(buf)
          pcall(vim.api.nvim_win_set_cursor, 0, { h.start_line, 0 })
          return
        end
      end
      if not found then
        local ranges = {}
        for i, h in ipairs(hunks) do
          ranges[#ranges + 1] = i .. ":[" .. h.start_line .. "-" .. h.end_line .. "]"
        end
        vim.notify("Cursor " .. cursor .. " not in any hunk: " .. table.concat(ranges, ", "), vim.log.levels.WARN)
      end
    end, vim.tbl_extend("force", kopts, { desc = "Toggle hunk fold", nowait = true }))
  end

  return buf
end

--- Compute hunk map from diff text. Each hunk's rendered lines are:
--- 1 line for @@ separator + N code lines (from format_hunk).
--- N = number of lines in hunk.diff AFTER the @@ header, EXCLUDING
--- the file header lines (diff --git, index, ---, +++).
---@param diff_text string
---@return table[]
function M._compute_hunk_map(diff_text)
  local raw_hunks = parse_diff(diff_text, false)
  local rendered_line = 0
  local hunk_map = {}
  for _, h in ipairs(raw_hunks) do
    -- Count code lines the same way Snacks does: lines after @@,
    -- stripping trailing empty/whitespace lines (Snacks parse_hunk does this)
    local code_lines_list = {}
    local found_hunk_header = false
    for diff_line in h.diff:gmatch("[^\n]+") do
      if found_hunk_header then
        code_lines_list[#code_lines_list + 1] = diff_line
      elseif diff_line:match("^@@") then
        found_hunk_header = true
      end
    end
    -- Strip trailing empty lines (matching Snacks parse_hunk behavior)
    while #code_lines_list > 0 and code_lines_list[#code_lines_list]:match("^%s*$") do
      table.remove(code_lines_list)
    end
    local code_lines = #code_lines_list
    -- Rendered: 1 line (@@ separator) + code_lines
    -- end_line is the LAST line of this hunk (exclusive of next hunk's @@)
    local start_line = rendered_line + 1
    local end_line = start_line + code_lines
    hunk_map[#hunk_map + 1] = {
      start_line = start_line,
      end_line = end_line,
      diff = h.diff,
      folded = false,
    }
    rendered_line = end_line
  end
  return hunk_map
end

--- Re-render buffer respecting fold state (placeholder — full fold
--- support would need tracking which hunks to hide/show)
function M._render_with_folds(buf)
  local hunks = M._buf_hunks[buf]
  if not hunks then return end

  -- Find the window showing this buffer
  local win = nil
  for _, w in ipairs(vim.api.nvim_list_wins()) do
    if vim.api.nvim_win_is_valid(w) and vim.api.nvim_win_get_buf(w) == buf then
      win = w
      break
    end
  end
  if not win then return end

  local line_count = vim.api.nvim_buf_line_count(buf)
  -- Ensure fold settings are on the correct window
  vim.wo[win].foldmethod = "manual"
  vim.wo[win].foldenable = true
  vim.api.nvim_win_call(win, function()
    -- Save view to prevent jumping
    local view = vim.fn.winsaveview()
    pcall(vim.cmd, "normal! zE") -- delete all folds
    for _, h in ipairs(hunks) do
      if h.folded then
        local fold_start = h.start_line + 1
        local fold_end = math.min(h.end_line, line_count)
        if fold_end >= fold_start and fold_start <= line_count then
          pcall(vim.cmd, fold_start .. "," .. fold_end .. "fold")
        end
      end
    end
    vim.fn.winrestview(view)
  end)
end

--- Re-fetch diff data and re-render a diff buffer after staging/unstaging.
--- Staged hunks get auto-folded.
---@param buf number
---@param filename string
-- Track what was last rendered per buffer to avoid re-rendering same data
M._buf_last_rendered = {}

function M._refresh_diff_buffer(buf, filename)
  -- Use cached diff data from M.get() instead of re-running git
  local diff_text = M._file_diffs and M._file_diffs[filename]
  local staged_flags = M._file_hunk_staged and M._file_hunk_staged[filename]

  if diff_text and diff_text ~= "" then
    -- Skip re-render if already rendered with the same data, but still
    -- (re)apply folds: the initial pre-render happens off-screen, where
    -- _render_with_folds is a no-op (no window shows the buffer yet), so the
    -- staged-hunk folds must be applied once the buffer becomes visible.
    if M._buf_last_rendered[buf] == diff_text and M._buf_hunks[buf] then
      M._render_with_folds(buf)
      return
    end
    M._buf_last_rendered[buf] = diff_text

    render_fancy_diff(buf, diff_text, staged_flags, filename)
    local hunk_map = M._compute_hunk_map(diff_text)
    -- Auto-fold staged hunks
    if staged_flags then
      for i, h in ipairs(hunk_map) do
        if staged_flags[i] then
          h.folded = true
        end
      end
    end
    M._buf_hunks[buf] = hunk_map
    -- Highlight @@ header lines with subtle gray background
    vim.api.nvim_buf_clear_namespace(buf, M._hunk_header_ns, 0, -1)
    for _, h in ipairs(hunk_map) do
      pcall(vim.api.nvim_buf_set_extmark, buf, M._hunk_header_ns, h.start_line - 1, 0, {
        line_hl_group = "SnacksDiffHunkHeader",
        priority = M._hunk_header_priority,
      })
    end
    M._render_with_folds(buf)
  else
    vim.bo[buf].modifiable = true
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, { "No changes" })
    vim.bo[buf].modifiable = false
    M._buf_hunks[buf] = {}
    vim.api.nvim_buf_clear_namespace(buf, M._hunk_header_ns, 0, -1)
    vim.api.nvim_buf_clear_namespace(buf, M._active_hunk_header_ns, 0, -1)
  end
end

---@param buf number
---@param item_diff string?
---@return table?
function M._highlight_active_hunk(buf, item_diff)
  vim.api.nvim_buf_clear_namespace(buf, M._active_hunk_header_ns, 0, -1)
  if not item_diff then return nil end

  local hunks = M._buf_hunks[buf]
  if not hunks then return nil end

  for _, hunk in ipairs(hunks) do
    if hunk.diff == item_diff then
      pcall(vim.api.nvim_buf_set_extmark, buf, M._active_hunk_header_ns, hunk.start_line - 1, 0, {
        line_hl_group = "DiffReviewActiveHunkHeader",
        priority = M._active_hunk_header_priority,
      })
      return hunk
    end
  end
end

--- Re-fetch the diff for a single file and update the cache.
--- Called before _refresh_diff_buffer to pick up file edits.
---@param filename string
function M._update_file_diff_cache(filename)
  M._file_diffs = M._file_diffs or {}
  M._file_hunk_staged = M._file_hunk_staged or {}
  -- Untracked files: build the diff from disk, never from git. Cache `false`
  -- (not nil) for empty/binary so the preview guard treats it as "checked"
  -- and doesn't re-run this on every cursor move.
  local relpath = M._untracked and M._untracked[filename]
  if relpath then
    local diff_text = build_untracked_diff(filename, relpath)
    M._file_diffs[filename] = diff_text or false
    M._file_hunk_staged[filename] = diff_text and { false } or nil
    return
  end
  local cwd = git_root()
  if not cwd then return end
  local diff_text, flags = file_diff_and_flags(cwd, filename)
  M._file_diffs[filename] = diff_text or false
  M._file_hunk_staged[filename] = flags
end

--- Refresh an open diff buffer for the given filename (if one exists).
--- Called from Trouble S/U actions to sync the diff buffer.
---@param filename string
function M.refresh_open_diff_buffer(filename)
  local key = "diff:" .. filename
  M._diff_bufs = M._diff_bufs or {}
  local buf = M._diff_bufs[key]
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end

  -- Re-fetch diff data for this file only (cache is stale after staging)
  M._update_file_diff_cache(filename)
  M._buf_last_rendered[buf] = nil  -- force re-render
  M._refresh_diff_buffer(buf, filename)
end

--- Capture the main window on first call (before we replace its buffer
--- with a diff scratch buffer, which makes Trouble's main detection fail
--- because buftype="nofile" is excluded).
---@param view trouble.View
---@return number? win
function M.get_main_win(view)
  if not M._main_win or not vim.api.nvim_win_is_valid(M._main_win) then
    local main = view:main()
    if main then
      M._main_win = main.win
    end
  end
  return M._main_win
end

-- Saved number/relativenumber of the main window, captured before we first
-- hide them for the diff preview so they can be restored on jump/close.
M._saved_wo = nil

--- Hide the buffer's line-number column in the diff preview window. The Snacks
--- renderer draws its own old/new line-number columns, so Neovim's number
--- column is a redundant third set. Saves the prior state once for restoring.
---@param win number
function M._hide_line_numbers(win)
  if M._saved_wo == nil then
    M._saved_wo = {
      number = vim.wo[win].number,
      relativenumber = vim.wo[win].relativenumber,
    }
  end
  vim.wo[win].number = false
  vim.wo[win].relativenumber = false
end

--- Restore the line-number columns hidden by _hide_line_numbers (e.g. when
--- jumping to a real file, where the number column is wanted again).
---@param win? number
function M._restore_line_numbers(win)
  if M._saved_wo and win and vim.api.nvim_win_is_valid(win) then
    vim.wo[win].number = M._saved_wo.number
    vim.wo[win].relativenumber = M._saved_wo.relativenumber
  end
end

--- List the file-level group nodes currently rendered in the Trouble list,
--- in display order: { { filename, row, node }, ... }. Used to drive
--- file-to-file cursor movement and folding after a stage.
---@param view trouble.View
---@return table[]
function M.file_nodes(view)
  local locations = view.renderer and view.renderer._locations or {}
  local ret = {}
  for row, loc in pairs(locations) do
    local node = loc.node
    if node and node.group and node.group.fields
        and node.group.fields[1] == "filename"
        and node.item and node.item.filename then
      ret[#ret + 1] = { filename = node.item.filename, row = row, node = node }
    end
  end
  table.sort(ret, function(a, b) return a.row < b.row end)
  return ret
end

--- The file-level group nodes covered by `node`: the node itself when it is a
--- file group, or every file group beneath it when it is a category header
--- ("Tracked Changes" / "Untracked Files"). Lets S/U/j act on a whole group.
---@param node trouble.Node
---@return trouble.Node[] nodes, boolean is_group whether `node` was a category
function M.file_group_nodes(node)
  if not node then return {}, false end
  if node.group and node.group.fields and node.group.fields[1] == "filename" then
    return { node }, false
  end
  local nodes = {}
  local function walk(n)
    if n.group and n.group.fields and n.group.fields[1] == "filename" then
      nodes[#nodes + 1] = n
    else
      for _, child in ipairs(n.children or {}) do
        walk(child)
      end
    end
  end
  walk(node)
  return nodes, true
end

--- Find the rendered file-group node (and its row) for a filename.
---@param view trouble.View
---@param filename? string
---@return trouble.Node?, number?
function M.find_file_node(view, filename)
  if not filename then return nil end
  for _, n in ipairs(M.file_nodes(view)) do
    if n.filename == filename then
      return n.node, n.row
    end
  end
end

--- The filename of the file rendered just after `filename` (or nil if last).
---@param view trouble.View
---@param filename? string
---@return string?
function M.next_file(view, filename)
  local nodes = M.file_nodes(view)
  for i, n in ipairs(nodes) do
    if n.filename == filename then
      return nodes[i + 1] and nodes[i + 1].filename or nil
    end
  end
end

--- Pre-fold the node a file will occupy *after* it moves to `category`, so the
--- next refresh renders it collapsed in a single pass — no second render, no
--- flicker. Trouble keys fold state by `node.id`, which encodes the group path
--- (`…#<category>#<file>`); staging/unstaging only swaps that one segment, so
--- rewrite it to the destination category and seed the fold there.
---
--- Directional on purpose: staging targets "Tracked Changes", unstaging targets
--- "Untracked Files". We seed only the destination, never the current id, so a
--- file that *stays* in its category (e.g. unstaging a modified—not new—file)
--- keeps whatever fold state the user chose. Enforces "untracked files are
--- never expanded" across the stage/unstage round-trip.
---@param view trouble.View
---@param node? trouble.Node
---@param category "Tracked Changes"|"Untracked Files"
function M.prefold_into(view, node, category)
  if not (node and node.id and view.renderer) then return end
  local id = node.id
    :gsub("#Untracked Files#", "#" .. category .. "#")
    :gsub("#Tracked Changes#", "#" .. category .. "#")
  view.renderer._folded[id] = true
end

--- Small centred confirmation popup. Runs `on_yes` only if the user presses
--- `y`; `n` / `q` / <Esc> dismiss it.
---@param lines string[] message lines
---@param on_yes function
local function confirm(lines, on_yes)
  local body = vim.list_extend({}, lines)
  body[#body + 1] = ""
  body[#body + 1] = "  [y] yes    [n] no"
  local width = 32
  for _, line in ipairs(body) do
    width = math.max(width, #line + 4)
  end
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, body)
  vim.bo[buf].modifiable = false
  vim.bo[buf].bufhidden = "wipe"
  local win = vim.api.nvim_open_win(buf, true, {
    relative = "editor",
    width = width,
    height = #body,
    col = math.floor((vim.o.columns - width) / 2),
    row = math.floor((vim.o.lines - #body) / 2),
    style = "minimal",
    border = "rounded",
    title = " Confirm ",
    title_pos = "center",
  })
  local function close()
    if vim.api.nvim_win_is_valid(win) then pcall(vim.api.nvim_win_close, win, true) end
  end
  vim.keymap.set("n", "y", function()
    close()
    on_yes()
  end, { buffer = buf, nowait = true, silent = true })
  for _, key in ipairs({ "n", "q", "<Esc>" }) do
    vim.keymap.set("n", key, close, { buffer = buf, nowait = true, silent = true })
  end
end

--- Discard the change under the cursor, after a y/n confirmation. A tracked
--- hunk is reverse-applied (`--index` too when staged); a file is restored to
--- HEAD (tracked) or deleted (untracked); a category header
--- ("Tracked Changes" / "Untracked Files") discards every file in the group.
---@param view trouble.View
---@param ctx table Trouble action context
function M.discard(view, ctx)
  local cwd = opts.reuse_sections and M._status.cwd or nil
  local root_err = nil
  if not cwd then
    cwd, root_err = git_root()
  end
  if not cwd then
    notify_error(root_err or "Unable to find git root")
    return
  end
  M._status.cwd = cwd

  local function after(files)
    view:refresh()
    for _, file in ipairs(files) do
      M.refresh_open_diff_buffer(file)
    end
    vim.defer_fn(function() M.auto_preview(view) end, 50)
  end

  -- A single tracked hunk under the cursor: reverse-apply just that hunk.
  if ctx.item and ctx.item.item and ctx.item.item.diff then
    local it, filename = ctx.item.item, ctx.item.filename
    confirm({ "Discard this hunk?", "  " .. vim.fn.fnamemodify(filename, ":.") }, function()
      local args = { "apply", "--reverse", "--whitespace=nowarn" }
      if it.staged then
        args[#args + 1] = "--index"
      end
      args[#args + 1] = "-"
      local result = run_git_at_root(cwd, args, it.diff .. "\n")
      if not result.ok then
        notify_error("Discard failed: " .. (result.output ~= "" and result.output or ("git exited " .. result.code)))
        return
      end
      after({ filename })
    end)
    return
  end

  -- Otherwise discard whole files: a single file node, an untracked leaf, or
  -- every file under a category header.
  local entries = {} ---@type { file: string, untracked: boolean }[]
  if ctx.node then
    for _, node in ipairs(M.file_group_nodes(ctx.node)) do
      local file = node.item and node.item.filename
      if file then
        entries[#entries + 1] = {
          file = file,
          untracked = node.item.item and node.item.item.category == "Untracked Files" or false,
        }
      end
    end
  elseif ctx.item then
    local file = ctx.item.filename
    if file then
      entries[#entries + 1] = {
        file = file,
        untracked = ctx.item.item and ctx.item.item.category == "Untracked Files" or false,
      }
    end
  end
  if #entries == 0 then return end

  local all_untracked = true
  for _, entry in ipairs(entries) do
    all_untracked = all_untracked and entry.untracked
  end

  local message
  if #entries == 1 then
    message = {
      entries[1].untracked and "Delete untracked file?" or "Discard ALL changes to file?",
      "  " .. vim.fn.fnamemodify(entries[1].file, ":."),
    }
  else
    message = {
      all_untracked and ("Delete %d untracked files?"):format(#entries)
        or ("Discard ALL changes in %d files?"):format(#entries),
    }
  end

  confirm(message, function()
    local files = {}
    local failures = {}
    for _, entry in ipairs(entries) do
      files[#files + 1] = entry.file
      if entry.untracked then
        local delete_code = vim.fn.delete(entry.file)
        if delete_code ~= 0 then
          failures[#failures + 1] = {
            file = entry.file,
            message = ("delete() failed with code %d"):format(delete_code),
          }
        end
      else
        local relpath, rel_err = repo_relative(entry.file, cwd)
        if not relpath then
          failures[#failures + 1] = { file = entry.file, message = rel_err }
        else
          local checkout_result = run_git_at_root(cwd, { "checkout", "HEAD", "--", relpath })
          if not checkout_result.ok then
            -- Staged-new file (not in HEAD): unstage, then remove it.
            local restore_result = run_git_at_root(cwd, { "restore", "--staged", "--", relpath })
            if restore_result.ok then
              local delete_code = vim.fn.delete(entry.file)
              if delete_code ~= 0 then
                failures[#failures + 1] = {
                  file = entry.file,
                  message = ("delete() failed with code %d after unstaging"):format(delete_code),
                }
              end
            else
              failures[#failures + 1] = {
                file = entry.file,
                output = restore_result.output ~= "" and restore_result.output or checkout_result.output,
                code = restore_result.code,
              }
            end
          end
        end
      end
    end
    if #failures > 0 then
      M.notify_git_failures("Discard failed", failures)
    end
    after(files)
  end)
end

local status_section_order = {
  { name = "unstaged", title = "Unstaged changes", default_folded = false },
  { name = "untracked", title = "Untracked files", default_folded = false },
  { name = "staged", title = "Staged changes", default_folded = false },
}
local status_file_indent = 0
local status_hunk_indent = 0

local status_section_by_name = {}
for _, section in ipairs(status_section_order) do
  status_section_by_name[section.name] = section
end

local function status_folded(key, default)
  M._status = M._status or {}
  M._status.folds = M._status.folds or {}
  local folded = M._status.folds[key]
  if folded == nil then return default end
  return folded
end

local function set_status_folded(key, folded)
  M._status = M._status or {}
  M._status.folds = M._status.folds or {}
  M._status.folds[key] = folded
end

local function status_git_line(cwd, args)
  local command = { "git", "-C", cwd }
  vim.list_extend(command, args)
  local result = vim.fn.systemlist(command)
  if vim.v.shell_error ~= 0 then return nil end
  return vim.trim(result[1] or "")
end

local function status_head_row(name, oid, ref, ref_hl, subject)
  return {
    { ("%-8s"):format(name .. ":"), "DiffReviewStatusLabel" },
    { ("%-7s"):format(oid or ""), "DiffReviewStatusObjectId" },
    { " " },
    { ref or "", ref_hl },
    { " " .. (subject or "") },
  }
end

local function status_head_lines(cwd)
  local lines = {}
  local head_oid = status_git_line(cwd, { "rev-parse", "--short", "HEAD" }) or "0000000"
  local branch = status_git_line(cwd, { "rev-parse", "--abbrev-ref", "HEAD" }) or "(detached)"
  local subject = status_git_line(cwd, { "log", "-1", "--format=%s" }) or "(no commits)"
  lines[#lines + 1] = status_head_row("Head", head_oid, branch, "DiffReviewStatusBranch", subject)

  local upstream = status_git_line(cwd, { "rev-parse", "--abbrev-ref", "@{upstream}" })
  if upstream then
    local upstream_oid = status_git_line(cwd, { "rev-parse", "--short", "@{upstream}" }) or ""
    local upstream_subject = status_git_line(cwd, { "log", "-1", "--format=%s", "@{upstream}" }) or ""
    lines[#lines + 1] = status_head_row("Merge", upstream_oid, upstream, "DiffReviewStatusRemote", upstream_subject)
  end

  local push_ref = status_git_line(cwd, { "rev-parse", "--abbrev-ref", "@{push}" })
  if push_ref then
    local push_oid = status_git_line(cwd, { "rev-parse", "--short", "@{push}" }) or ""
    local push_subject = status_git_line(cwd, { "log", "-1", "--format=%s", "@{push}" }) or ""
    lines[#lines + 1] = status_head_row("Push", push_oid, push_ref, "DiffReviewStatusRemote", push_subject)
  end

  return lines
end

local function status_hunk_key(section_name, filename, diff)
  local hash = diff and vim.fn.sha256(diff) or "file"
  return ("hunk:%s:%s:%s"):format(section_name, filename, hash)
end

local function status_file_key(section_name, filename)
  return ("file:%s:%s"):format(section_name, filename)
end

local function status_section_key(section_name)
  return "section:" .. section_name
end

local function status_add_highlight(line, start_col, end_col, hl_group)
  M._status.highlights[#M._status.highlights + 1] = {
    line = line,
    start_col = start_col,
    end_col = end_col,
    hl_group = hl_group,
  }
end

local function status_add_extmark(line, col, opts)
  M._status.extmarks[#M._status.extmarks + 1] = {
    line = line,
    col = col,
    opts = opts,
  }
end

local function status_add_line(text, entry, line_hl_group)
  M._status.lines[#M._status.lines + 1] = text
  local line = #M._status.lines
  if entry then M._status.entries[line] = entry end
  if line_hl_group then
    M._status.line_highlights[#M._status.line_highlights + 1] = {
      line = line,
      hl_group = line_hl_group,
    }
  end
  return line
end

local function status_add_segment_line(segments, entry)
  local parts = {}
  local col = 0
  local segment_highlights = {}
  for _, segment in ipairs(segments) do
    local text = segment[1] or ""
    parts[#parts + 1] = text
    if segment[2] and text ~= "" then
      segment_highlights[#segment_highlights + 1] = {
        start_col = col,
        end_col = col + #text,
        hl_group = segment[2],
      }
    end
    col = col + #text
  end
  local line = status_add_line(table.concat(parts), entry)
  for _, highlight in ipairs(segment_highlights) do
    status_add_highlight(line, highlight.start_col, highlight.end_col, highlight.hl_group)
  end
end

local function status_add_hint_line()
  status_add_segment_line({
    { "Hint: ", "DiffReviewStatusHint" },
    { "<tab>", "DiffReviewStatusHintKey" },
    { " toggle | ", "DiffReviewStatusHint" },
    { "S", "DiffReviewStatusHintKey" },
    { " stage | ", "DiffReviewStatusHint" },
    { "U", "DiffReviewStatusHintKey" },
    { " unstage | ", "DiffReviewStatusHint" },
    { "j", "DiffReviewStatusHintKey" },
    { " discard | ", "DiffReviewStatusHint" },
    { "c", "DiffReviewStatusHintKey" },
    { " commit | ", "DiffReviewStatusHint" },
    { "P", "DiffReviewStatusHintKey" },
    { " push | ", "DiffReviewStatusHint" },
    { "<CR>", "DiffReviewStatusHintKey" },
    { " jump | ", "DiffReviewStatusHint" },
    { "q", "DiffReviewStatusHintKey" },
    { " close | ", "DiffReviewStatusHint" },
    { "?", "DiffReviewStatusHintKey" },
    { " help", "DiffReviewStatusHint" },
  })
end

local function replace_text_range(text, start_col, replacement)
  local required = start_col + #replacement
  if #text < required then
    text = text .. string.rep(" ", required - #text)
  end
  return text:sub(1, start_col) .. replacement .. text:sub(required + 1)
end

local function status_add_fancy_row(row, entry, indent)
  indent = indent or 0
  local text_parts = {}
  if indent > 0 then
    text_parts[#text_parts + 1] = string.rep(" ", indent)
  end

  local col = indent
  local row_highlights = {}
  local row_extmarks = {}
  local gutter_overlays = {}
  local diff_line = nil
  for _, chunk in ipairs(row) do
    if chunk.meta and chunk.meta.diff then
      diff_line = chunk.meta.diff
    end
    if type(chunk[1]) == "string" then
      local text = chunk[1]
      if text ~= "" then
        text_parts[#text_parts + 1] = text
        if chunk[2] then
          row_highlights[#row_highlights + 1] = {
            start_col = col,
            end_col = col + #text,
            hl_group = chunk[2],
          }
        end
        col = col + #text
      end
    elseif chunk.virt_text then
      local overlay_text = chunk.virt_text[1] and chunk.virt_text[1][1]
      if chunk.virt_text_pos == "overlay" and overlay_text and #overlay_text <= 8 and (chunk.col or 0) <= 8 then
        gutter_overlays[#gutter_overlays + 1] = {
          col = (chunk.col or 0) + indent,
          text = overlay_text,
          hl_group = chunk.virt_text[1][2],
        }
      else
        local opts = {}
        for key, value in pairs(chunk) do
          if key ~= "col" then
            opts[key] = value
          end
        end
        row_extmarks[#row_extmarks + 1] = {
          col = (chunk.col or 0) + indent,
          opts = opts,
        }
      end
    end
  end

  local line_text = table.concat(text_parts)
  table.sort(gutter_overlays, function(left, right)
    return left.col < right.col
  end)
  for _, overlay in ipairs(gutter_overlays) do
    line_text = replace_text_range(line_text, overlay.col, overlay.text)
    if overlay.hl_group then
      row_highlights[#row_highlights + 1] = {
        start_col = overlay.col,
        end_col = overlay.col + #overlay.text,
        hl_group = replace_hl(overlay.hl_group),
      }
    end
  end

  local line_entry = entry
  if diff_line and entry then
    line_entry = vim.tbl_extend("force", entry, { diff_line = diff_line })
  end
  local line = status_add_line(line_text, line_entry)
  for _, highlight in ipairs(row_highlights) do
    status_add_highlight(line, highlight.start_col, highlight.end_col, highlight.hl_group)
  end
  for _, extmark in ipairs(row_extmarks) do
    status_add_extmark(line, extmark.col, extmark.opts)
  end
end

local function status_section_for_item(item)
  local data = item.item or {}
  if data.category == "Untracked Files" then return "untracked" end
  if data.staged then return "staged" end
  return "unstaged"
end

local function status_collect_sections()
  local collected_items = {}
  M.get(function(items)
    collected_items = items or {}
  end, { skip_pre_render = true, skip_ts_context = true })

  local sections = {}
  for _, section_config in ipairs(status_section_order) do
    sections[section_config.name] = {
      name = section_config.name,
      title = section_config.title,
      default_folded = section_config.default_folded,
      files = {},
      files_by_name = {},
    }
  end

  for _, item in ipairs(collected_items) do
    local filename = item.filename
    local data = item.item or {}
    if filename then
      local section_name = status_section_for_item(item)
      local section = sections[section_name]
      local file = section.files_by_name[filename]
      if not file then
        file = {
          filename = filename,
          relpath = vim.fn.fnamemodify(filename, ":."),
          section_name = section_name,
          added = 0,
          removed = 0,
          hunks = {},
          untracked = section_name == "untracked",
          status = data.stats or data.hunk_header or "",
        }
        section.files_by_name[filename] = file
        section.files[#section.files + 1] = file
      end

      file.added = file.added + (data.added or 0)
      file.removed = file.removed + (data.removed or 0)
      if data.diff then
        file.hunks[#file.hunks + 1] = {
          filename = filename,
          section_name = section_name,
          pos = item.pos and item.pos[1] or 1,
          diff = data.diff,
          staged = data.staged == true,
          context_text = data.context_text or "",
          added = data.added or 0,
          removed = data.removed or 0,
        }
      end
    end
  end

  local ordered = {}
  for _, section_config in ipairs(status_section_order) do
    local section = sections[section_config.name]
    table.sort(section.files, function(left_file, right_file)
      return left_file.relpath < right_file.relpath
    end)
    for _, file in ipairs(section.files) do
      table.sort(file.hunks, function(left_hunk, right_hunk)
        return (left_hunk.pos or 0) < (right_hunk.pos or 0)
      end)
    end
    if #section.files > 0 then ordered[#ordered + 1] = section end
  end

  return ordered
end

local function status_diff_hunks_for_file(file)
  if #file.hunks > 0 then return file.hunks end
  if not file.untracked then return {} end

  local relpath = M._untracked and M._untracked[file.filename]
  local diff_text = relpath and build_untracked_diff(file.filename, relpath) or nil
  if not diff_text then return {} end

  local hunks = {}
  for _, parsed_hunk in ipairs(parse_diff(diff_text, false)) do
    hunks[#hunks + 1] = {
      filename = file.filename,
      section_name = file.section_name,
      pos = parsed_hunk.pos,
      diff = parsed_hunk.diff,
      staged = false,
      context_text = parsed_hunk.context or "",
      added = parsed_hunk.added or 0,
      removed = parsed_hunk.removed or 0,
    }
  end
  return hunks
end

local function status_render_hunk(file, hunk)
  local hunk_key = status_hunk_key(file.section_name, file.filename, hunk.diff)
  local hunk_folded = status_folded(hunk_key, false)
  local entry = { id = hunk_key, kind = "hunk", file = file, hunk = hunk }
  M._status.fancy_rows = M._status.fancy_rows or {}
  local rows_key = ("%s:%s"):format(hunk_key, hunk.staged and "staged" or "unstaged")
  local rows = M._status.fancy_rows[rows_key]
  if not rows then
    local ok, built_rows = pcall(build_fancy_diff_rows, hunk.diff, { hunk.staged }, file.filename)
    if ok then
      rows = built_rows
      M._status.fancy_rows[rows_key] = rows
    end
  end
  if not rows then
    local context = hunk.context_text ~= "" and (hunk.context_text .. " ") or ""
    local header = ("%s@@ %s+%d -%d"):format(string.rep(" ", status_hunk_indent), context, hunk.added or 0, hunk.removed or 0)
    status_add_line(header, entry, hunk_folded and "DiffReviewActiveHunkHeader" or "DiffReviewHunkHeader")
    return
  end

  local rendered_rows = hunk_folded and { rows[1] } or rows
  for _, row in ipairs(rendered_rows) do
    if row then
      status_add_fancy_row(row, entry, status_hunk_indent)
    end
  end
end

local function status_render_file(file)
  local file_key = status_file_key(file.section_name, file.filename)
  local file_folded = status_folded(file_key, true)
  local stats = file.untracked and "new" or ("+%d -%d"):format(file.added, file.removed)
  local line = ("%s%s %s"):format(string.rep(" ", status_file_indent), file.relpath, stats)
  local entry = { id = file_key, kind = "file", file = file }
  local line_number = status_add_line(line, entry)
  local path_start = status_file_indent
  local stats_start = #line - #stats
  status_add_highlight(line_number, path_start, stats_start - 1, "DiffReviewStatusPath")
  status_add_highlight(line_number, stats_start, #line, file.untracked and "Comment" or "DiffReviewAddRange")

  if file_folded then return end

  local hunks = status_diff_hunks_for_file(file)
  if #hunks == 0 then
    status_add_line(string.rep(" ", status_hunk_indent) .. "No textual diff", entry, "Comment")
    return
  end
  for _, hunk in ipairs(hunks) do
    status_render_hunk(file, hunk)
  end
end

local function status_render_section(section)
  local section_key = status_section_key(section.name)
  local section_folded = status_folded(section_key, section.default_folded)
  local line = ("%s (%d)"):format(section.title, #section.files)
  local entry = { id = section_key, kind = "section", section = section }
  status_add_line(line, entry, "DiffReviewStatusHeader")
  if section_folded then return end
  for _, file in ipairs(section.files) do
    status_render_file(file)
  end
end

local function status_entry_under_cursor()
  local status = M._status
  if not status then return nil end
  return status.entries[vim.api.nvim_win_get_cursor(0)[1]]
end

local function status_files_for_entry(entry)
  if not entry then return {} end
  if entry.kind == "hunk" then return { entry.file.filename } end
  if entry.kind == "file" then return { entry.file.filename } end
  if entry.kind == "section" then
    local files = {}
    for _, file in ipairs(entry.section.files or {}) do
      files[#files + 1] = file.filename
    end
    return files
  end
  return {}
end

local function status_restore_cursor(buf, target_id, fallback_line)
  local target_line = nil
  if target_id then
    for line, entry in pairs(M._status.entries or {}) do
      if entry.id == target_id then
        target_line = line
        break
      end
    end
  end
  target_line = target_line or math.min(fallback_line or 1, vim.api.nvim_buf_line_count(buf))
  pcall(vim.api.nvim_win_set_cursor, 0, { math.max(target_line, 1), 0 })
end

function M.render_status(buf, target_id, fallback_line, opts)
  opts = opts or {}
  setup_bg_highlights()
  M._status = M._status or {}
  M._status.buf = buf
  M._status.lines = {}
  M._status.entries = {}
  M._status.highlights = {}
  M._status.line_highlights = {}
  M._status.extmarks = {}

  local cwd, root_err = git_root()
  if not cwd then
    notify_error(root_err or "Unable to find git root")
    return
  end

  status_add_hint_line()
  status_add_line("")
  local head_lines = opts.reuse_sections and M._status.head_lines or nil
  if not head_lines then
    head_lines = status_head_lines(cwd)
    M._status.head_lines = head_lines
  end
  for _, head_line in ipairs(head_lines) do
    status_add_segment_line(head_line)
  end
  status_add_line("")

  local sections = opts.reuse_sections and M._status.sections or nil
  if not sections then
    sections = status_collect_sections()
    M._status.sections = sections
    M._status.fancy_rows = {}
  end
  if #sections == 0 then
    status_add_line("No changes", nil, "Comment")
  else
    for index, section in ipairs(sections) do
      if index > 1 then
        status_add_line("")
      end
      status_render_section(section)
    end
  end

  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_clear_namespace(buf, M._status_ns, 0, -1)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, M._status.lines)
  vim.bo[buf].modifiable = false

  for _, line_hl in ipairs(M._status.line_highlights) do
    pcall(vim.api.nvim_buf_set_extmark, buf, M._status_ns, line_hl.line - 1, 0, {
      line_hl_group = line_hl.hl_group,
      priority = 80,
    })
  end
  for _, highlight in ipairs(M._status.highlights) do
    pcall(vim.api.nvim_buf_set_extmark, buf, M._status_ns, highlight.line - 1, highlight.start_col, {
      end_col = highlight.end_col,
      hl_group = highlight.hl_group,
      priority = 90,
    })
  end
  for _, extmark in ipairs(M._status.extmarks) do
    local opts = vim.tbl_extend("force", { priority = 95 }, extmark.opts)
    pcall(vim.api.nvim_buf_set_extmark, buf, M._status_ns, extmark.line - 1, extmark.col, opts)
  end

  status_restore_cursor(buf, target_id, fallback_line)
end

local function render_status_or_notify(buf, target_id, fallback_line, opts)
  local ok, err = xpcall(function()
    M.render_status(buf, target_id, fallback_line, opts)
  end, debug.traceback)
  if not ok then
    notify_error("DiffReview render failed: " .. tostring(err))
  end
end

local function refresh_status_after_action(buf, target_id)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  render_status_or_notify(buf, target_id, vim.api.nvim_win_get_cursor(0)[1])
end

local function status_stage(entry)
  if not entry then return end
  if entry.kind == "hunk" then
    if entry.hunk.staged then return end
    if M.stage_patch(entry.hunk.diff) then
      vim.notify("Hunk staged", vim.log.levels.INFO)
      refresh_status_after_action(M._status.buf, entry.id)
    end
    return
  end

  if entry.kind == "section" and entry.section.name == "staged" then return end
  if entry.kind == "file" and entry.file.section_name == "staged" then return end
  local result = M.stage_files(status_files_for_entry(entry))
  if #result.successes > 0 then
    vim.notify(("Staged %d file(s)"):format(#result.successes), vim.log.levels.INFO)
    refresh_status_after_action(M._status.buf, entry.id)
  end
end

local function status_unstage(entry)
  if not entry then return end
  if entry.kind == "hunk" then
    if not entry.hunk.staged then return end
    if M.unstage_patch(entry.hunk.diff) then
      vim.notify("Hunk unstaged", vim.log.levels.INFO)
      refresh_status_after_action(M._status.buf, entry.id)
    end
    return
  end

  if entry.kind == "section" and entry.section.name ~= "staged" then return end
  if entry.kind == "file" and entry.file.section_name ~= "staged" then return end
  local result = M.unstage_files(status_files_for_entry(entry))
  if #result.successes > 0 then
    vim.notify(("Unstaged %d file(s)"):format(#result.successes), vim.log.levels.INFO)
    refresh_status_after_action(M._status.buf, entry.id)
  end
end

local function status_discard_entries(entries, target_id)
  local cwd, root_err = git_root()
  if not cwd then
    notify_error(root_err or "Unable to find git root")
    return
  end

  local failures = {}
  for _, entry in ipairs(entries) do
    if entry.kind == "hunk" then
      local args = { "apply", "--reverse", "--whitespace=nowarn" }
      if entry.hunk.staged then args[#args + 1] = "--index" end
      args[#args + 1] = "-"
      local result = run_git_at_root(cwd, args, entry.hunk.diff .. "\n")
      if not result.ok then
        failures[#failures + 1] = { file = entry.file.filename, output = result.output, code = result.code }
      end
    elseif entry.file.untracked then
      local delete_code = vim.fn.delete(entry.file.filename)
      if delete_code ~= 0 then
        failures[#failures + 1] = {
          file = entry.file.filename,
          message = ("delete() failed with code %d"):format(delete_code),
        }
      end
    else
      local relpath, rel_err = repo_relative(entry.file.filename, cwd)
      if not relpath then
        failures[#failures + 1] = { file = entry.file.filename, message = rel_err }
      else
        local checkout_result = run_git_at_root(cwd, { "checkout", "HEAD", "--", relpath })
        if not checkout_result.ok then
          local restore_result = run_git_at_root(cwd, { "restore", "--staged", "--", relpath })
          if restore_result.ok then
            local delete_code = vim.fn.delete(entry.file.filename)
            if delete_code ~= 0 then
              failures[#failures + 1] = {
                file = entry.file.filename,
                message = ("delete() failed with code %d after unstaging"):format(delete_code),
              }
            end
          else
            failures[#failures + 1] = {
              file = entry.file.filename,
              output = restore_result.output ~= "" and restore_result.output or checkout_result.output,
              code = restore_result.code,
            }
          end
        end
      end
    end
  end

  if #failures > 0 then M.notify_git_failures("Discard failed", failures) end
  refresh_status_after_action(M._status.buf, target_id)
end

local function status_discard(entry)
  if not entry then return end
  local entries = {}
  if entry.kind == "hunk" or entry.kind == "file" then
    entries[#entries + 1] = entry
  elseif entry.kind == "section" then
    for _, file in ipairs(entry.section.files or {}) do
      entries[#entries + 1] = { kind = "file", file = file }
    end
  end
  if #entries == 0 then return end

  local message
  if #entries == 1 then
    local first_entry = entries[1]
    local prompt = first_entry.kind == "hunk" and "Discard this hunk?"
      or (first_entry.file.untracked and "Delete untracked file?" or "Discard ALL changes to file?")
    message = { prompt, "  " .. first_entry.file.relpath }
  else
    message = { ("Discard changes in %d file(s)?"):format(#entries) }
  end
  confirm(message, function()
    status_discard_entries(entries, entry.id)
  end)
end

local function closest_current_line_for_deleted_diff_line(diff_text, old_target_line)
  local old_line
  local new_line
  local in_hunk = false
  for diff_line in diff_text:gmatch("[^\n]+") do
    if diff_line:match("^@@") then
      old_line = tonumber(diff_line:match("%-(%d+)"))
      new_line = tonumber(diff_line:match("%+(%d+)"))
      in_hunk = old_line ~= nil and new_line ~= nil
    elseif in_hunk then
      local prefix = diff_line:sub(1, 1)
      if prefix == "-" then
        if old_line == old_target_line then
          return new_line
        end
        old_line = old_line + 1
      elseif prefix == "+" then
        new_line = new_line + 1
      else
        if old_line == old_target_line then
          return new_line
        end
        old_line = old_line + 1
        new_line = new_line + 1
      end
    end
  end
end

local function status_jump(entry)
  if not (entry and entry.file and entry.file.filename) then return end
  vim.cmd.edit(vim.fn.fnameescape(entry.file.filename))
  local target_line
  if entry.diff_line and entry.diff_line.line then
    if entry.diff_line.side == "left" and entry.hunk and entry.hunk.diff then
      target_line = closest_current_line_for_deleted_diff_line(entry.hunk.diff, entry.diff_line.line)
    else
      target_line = entry.diff_line.line
    end
  end
  if entry.kind == "hunk" and entry.hunk.diff then
    target_line = target_line or tonumber(entry.hunk.diff:match("@@ %-%d+,?%d* %+(%d+)"))
  end
  if target_line then
    local max_line = vim.api.nvim_buf_line_count(0)
    target_line = math.min(math.max(target_line, 1), max_line)
    pcall(vim.api.nvim_win_set_cursor, 0, { target_line, 0 })
    vim.cmd("normal! zz")
  end
end

local function status_toggle(entry)
  if not entry then return end
  local default = false
  if entry.kind == "file" then
    default = true
  elseif entry.kind == "section" then
    local section_config = status_section_by_name[entry.section.name]
    default = section_config and section_config.default_folded or false
  end
  set_status_folded(entry.id, not status_folded(entry.id, default))
  render_status_or_notify(M._status.buf, entry.id, vim.api.nvim_win_get_cursor(0)[1], { reuse_sections = true })
end

local function status_push(buf)
  local cwd, root_err = git_root()
  if not cwd then
    notify_error(root_err or "Unable to find git root")
    return
  end

  vim.notify("Pushing changes...", vim.log.levels.INFO, { title = "DiffReview" })
  local output = {}
  local job_id = vim.fn.jobstart({ "git", "-C", cwd, "push" }, {
    stdout_buffered = true,
    stderr_buffered = true,
    on_stdout = function(_, data)
      if data then
        vim.list_extend(output, data)
      end
    end,
    on_stderr = function(_, data)
      if data then
        vim.list_extend(output, data)
      end
    end,
    on_exit = function(_, code)
      vim.schedule(function()
        local compact = {}
        for _, line in ipairs(output) do
          if line ~= "" then
            compact[#compact + 1] = line
          end
        end
        if code == 0 then
          vim.notify("Push complete", vim.log.levels.INFO, { title = "DiffReview" })
        else
          notify_error("Push failed: " .. (#compact > 0 and table.concat(compact, "\n") or ("git exited " .. code)))
        end
        if vim.api.nvim_buf_is_valid(buf) then
          render_status_or_notify(buf)
        end
      end)
    end,
  })
  if job_id <= 0 then
    notify_error("Push failed: unable to start git push")
  end
end

local function setup_status_keymaps(buf)
  local opts = { buffer = buf, silent = true, nowait = true }
  vim.keymap.set("n", "q", function()
    if vim.api.nvim_buf_is_valid(buf) then
      pcall(vim.api.nvim_buf_delete, buf, { force = true })
    end
    M._status = nil
  end, vim.tbl_extend("force", opts, { desc = "Close DiffReview" }))

  vim.keymap.set("n", "r", function()
    refresh_status_after_action(buf, (status_entry_under_cursor() or {}).id)
  end, vim.tbl_extend("force", opts, { desc = "Refresh DiffReview" }))

  vim.keymap.set("n", "<Tab>", function()
    status_toggle(status_entry_under_cursor())
  end, vim.tbl_extend("force", opts, { desc = "Toggle fold" }))

  vim.keymap.set("n", "S", function()
    status_stage(status_entry_under_cursor())
  end, vim.tbl_extend("force", opts, { desc = "Stage hunk/file" }))

  vim.keymap.set("n", "U", function()
    status_unstage(status_entry_under_cursor())
  end, vim.tbl_extend("force", opts, { desc = "Unstage hunk/file" }))

  vim.keymap.set("n", "j", function()
    status_discard(status_entry_under_cursor())
  end, vim.tbl_extend("force", opts, { desc = "Discard hunk/file" }))

  vim.keymap.set("n", "<CR>", function()
    status_jump(status_entry_under_cursor())
  end, vim.tbl_extend("force", opts, { desc = "Jump to file" }))

  local function commit()
    require("trouble.sources.diff_review_commit").commit({
      win = vim.api.nvim_get_current_win(),
      on_done = function()
        if vim.api.nvim_buf_is_valid(buf) then render_status_or_notify(buf) end
      end,
    })
  end
  vim.keymap.set("n", "c", commit, vim.tbl_extend("force", opts, { desc = "Commit" }))
  vim.keymap.set("n", "cc", commit, vim.tbl_extend("force", opts, { desc = "Commit" }))

  vim.keymap.set("n", "P", function()
    status_push(buf)
  end, vim.tbl_extend("force", opts, { desc = "Push" }))

  vim.keymap.set("n", "?", function()
    vim.notify("<Tab> toggle | S stage | U unstage | j discard | c commit | P push | <CR> jump | r refresh | q close", vim.log.levels.INFO, {
      title = "DiffReview",
    })
  end, vim.tbl_extend("force", opts, { desc = "DiffReview help" }))
end

--- Open a standalone, Neogit-style DiffReview status buffer.
function M.open()
  setup_bg_highlights()
  local buf = M._status and M._status.buf
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then
    buf = vim.api.nvim_create_buf(true, false)
    vim.bo[buf].bufhidden = "hide"
    vim.bo[buf].buftype = "nofile"
    vim.bo[buf].swapfile = false
    vim.bo[buf].filetype = "DiffReviewStatus"
    pcall(vim.api.nvim_buf_set_name, buf, "DiffReviewStatus")
    M._status = {
      buf = buf,
      folds = {},
      lines = {},
      entries = {},
      highlights = {},
      line_highlights = {},
      extmarks = {},
      sections = nil,
      fancy_rows = {},
    }
    setup_status_keymaps(buf)
  end

  local win = vim.api.nvim_get_current_win()
  local ok, err = pcall(vim.api.nvim_win_set_buf, win, buf)
  if not ok then
    notify_error("DiffReview open failed: " .. tostring(err))
    return
  end
  vim.wo[win].number = false
  vim.wo[win].relativenumber = false
  vim.wo[win].foldcolumn = "0"
  render_status_or_notify(buf)
end

--- Show the appropriate buffer in the main window when cursor moves.
--- On hunk rows: show the real file at the hunk position.
--- On file group headers: show the fancy diff buffer.
--- Bypasses Trouble's preview system entirely to avoid overlay issues.
---@param view trouble.View
function M.auto_preview(view)
  -- The commit flow borrows the main window for its console / message buffer;
  -- don't clobber it on cursor moves while that's running.
  if M.suspend_preview then return end
  local loc = view:at()
  local win = M.get_main_win(view)
  if not win or not vim.api.nvim_win_is_valid(win) then return end

  if loc and loc.item then
    local item = loc.item
    local filename = item.filename
    if not filename then return end

    local diff_buf = M.open_diff_buffer(filename)
    vim.api.nvim_win_set_buf(win, diff_buf)
    M._hide_line_numbers(win)
    -- Re-fetch if cache was invalidated (e.g., after editing the real file)
    if not M._file_diffs or M._file_diffs[filename] == nil then
      M._update_file_diff_cache(filename)
    end
    M._refresh_diff_buffer(diff_buf, filename)

    local item_diff = item.item and item.item.diff
    local active_hunk = M._highlight_active_hunk(diff_buf, item_diff)
    if active_hunk then
      pcall(vim.api.nvim_win_set_cursor, win, { active_hunk.start_line, 0 })
      vim.api.nvim_win_call(win, function()
        vim.cmd("normal! zz")
      end)
    end
    return
  end

  -- File group header: show the fancy diff buffer
  if loc and loc.node and loc.node.item then
    local filename = loc.node.item.filename
    if not filename then return end

    local buf = M.open_diff_buffer(filename)
    vim.api.nvim_win_set_buf(win, buf)
    M._hide_line_numbers(win)
    if not M._file_diffs or M._file_diffs[filename] == nil then
      M._update_file_diff_cache(filename)
    end
    M._refresh_diff_buffer(buf, filename)
    M._highlight_active_hunk(buf, nil)
    -- Restore cursor to the same hunk + offset within hunk
    local saved = M._buf_saved_cursor[buf]
    local new_hunks = M._buf_hunks[buf]
    if saved and saved.hunk_index and new_hunks then
      -- Try to find the same hunk by index, or by matching the @@ header
      local target_hunk = nil
      -- First try: match by @@ header text (survives hunk reordering)
      if saved.header and saved.header ~= "" then
        for _, h in ipairs(new_hunks) do
          local h_header = h.diff and h.diff:match("(@@[^@]+@@)") or ""
          if h_header == saved.header then
            target_hunk = h
            break
          end
        end
      end
      -- Fallback: use the same index if still valid
      if not target_hunk and saved.hunk_index <= #new_hunks then
        target_hunk = new_hunks[saved.hunk_index]
      end
      if target_hunk then
        local target_line = target_hunk.start_line + (saved.offset or 0)
        local max_line = vim.api.nvim_buf_line_count(buf)
        target_line = math.min(target_line, max_line)
        target_line = math.max(1, target_line)
        pcall(vim.api.nvim_win_set_cursor, win, { target_line, saved.col or 0 })
        vim.api.nvim_win_call(win, function()
          vim.cmd("normal! zv") -- open fold
        end)
      else
        vim.api.nvim_win_set_cursor(win, { 1, 0 })
      end
      M._buf_saved_cursor[buf] = nil -- consumed
    else
      vim.api.nvim_win_set_cursor(win, { 1, 0 })
    end
  end
end

return M

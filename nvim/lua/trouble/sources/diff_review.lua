--- Trouble source: diff_review
--- Shows git diff hunks grouped by file with staging checkboxes.
--- Usage: :Trouble diff_review

local Item = require("trouble.item")

local M = {}

-- Background-only highlight groups for diff lines.
-- Pull bg from existing DiffAdd/DiffDelete so they match the gutter colors.
local function setup_bg_highlights()
  local function get_bg(name)
    local hl = vim.api.nvim_get_hl(0, { name = name, link = false })
    return hl.bg
  end
  local add_bg = get_bg("DiffAdd") or "#001200"
  local del_bg = get_bg("DiffDelete") or "#120000"
  vim.api.nvim_set_hl(0, "DiffReviewAddBg", { bg = add_bg })
  vim.api.nvim_set_hl(0, "DiffReviewDeleteBg", { bg = del_bg })
  vim.api.nvim_set_hl(0, "DiffReviewAddLineNr", { fg = "#50fa7b", bg = add_bg, bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewDeleteLineNr", { fg = "#ff5555", bg = del_bg, bold = true })
  vim.api.nvim_set_hl(0, "DiffReviewContextLineNr", { fg = "#555555" })
  vim.api.nvim_set_hl(0, "DiffReviewContextBg", {})
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

M.config = {
  modes = {
    diff_review = {
      desc = "Diff Review",
      source = "diff_review",
      events = { "BufWritePost" },
      groups = {
        { "filename", format = "{file_icon} {basename} {item.stats}" },
      },
      sort = { "filename", "pos" },
      format = "{item.check} {item.hunk_header} {item.context_text}",
      -- Start with groups collapsed
      auto_preview = true,
      focus = true,
    },
  },
}

function M.setup() end

---@param cb fun(items: trouble.Item[])
---@param _ctx trouble.Source.ctx
function M.get(cb, _ctx)
  local cwd = vim.fn.systemlist({ "git", "rev-parse", "--show-toplevel" })[1]
  if vim.v.shell_error ~= 0 or not cwd then
    cb({})
    return
  end
  cwd = vim.trim(cwd)

  -- Get both unstaged and staged hunks
  local unstaged = get_hunks(cwd, false)
  local staged = get_hunks(cwd, true)

  local all_hunks = {}
  vim.list_extend(all_hunks, unstaged)
  vim.list_extend(all_hunks, staged)

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
    local context_text = hunk.context or ""
    local header = "@@"
    if hunk.diff then
      header = hunk.diff:match("\n(@@[^@]+@@)") or hunk.diff:match("^(@@[^@]+@@)") or "@@"
    end

    local fs = file_stats[hunk.file]
    local file_check
    if fs.staged == fs.total then
      file_check = "[x]"
    elseif fs.staged > 0 then
      file_check = "[-]"
    else
      file_check = "[ ]"
    end

    local filename = vim.fn.fnamemodify(cwd .. "/" .. hunk.file, ":p")
    items[#items + 1] = Item.new({
      source = "diff_review",
      filename = filename,
      pos = { hunk.pos, 0 },
      item = {
        check = hunk.staged and "[x]" or "[ ]",
        file_check = file_check,
        hunk_header = header,
        context_text = context_text,
        staged = hunk.staged,
        diff = hunk.diff,
        added = hunk.added,
        removed = hunk.removed,
        stats = "+" .. fs.added .. " -" .. fs.removed,
      },
    })
  end

  -- Build per-file combined diffs for file-level preview
  local file_diffs = {}
  for _, hunk in ipairs(all_hunks) do
    local f = hunk.file
    if not file_diffs[f] then
      file_diffs[f] = {}
    end
    table.insert(file_diffs[f], hunk.diff)
  end
  M._file_diffs = {}
  for f, diffs in pairs(file_diffs) do
    local filename = vim.fn.fnamemodify(cwd .. "/" .. f, ":p")
    M._file_diffs[filename] = table.concat(diffs, "\n")
  end

  Item.add_text(items, { mode = "after" })
  cb(items)
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

--- Render a fancy diff into a buffer using Snacks' diff renderer,
--- but skip file name headers and hunk context headers for a cleaner look.
--- Replaces Snacks' diff highlight groups with bg-only versions so that
--- treesitter syntax highlighting is preserved and the full line has a
--- colored background.
---@param buf number
---@param diff_text string
local function render_fancy_diff(buf, diff_text)
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, {})
  vim.bo[buf].modifiable = false

  local snacks_diff = require("snacks.picker.util.diff")
  local H = Snacks.picker.highlight

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

  for _, block in ipairs(diff.blocks) do
    local block_ctx = ctx_base:extend({ block = block })
    for _, hunk in ipairs(block.hunks) do
      -- Compact @@ separator
      local range_line = hunk.diff[1] or "@@"
      local range_only = range_line:match("^(@@[^@]+@@)") or range_line
      ret[#ret + 1] = { { range_only, "SnacksDiffHunkHeader" } }

      local hunk_ctx = block_ctx:extend({ hunk = hunk })
      local hunk_lines = snacks_diff.format_hunk(hunk_ctx)
      vim.list_extend(ret, hunk_lines)
    end
  end

  -- Replace Snacks diff highlights with bg-only versions in-place
  for _, line in ipairs(ret) do
    rewrite_line_hls(line)
  end

  H.render(buf, M._ns, ret)
end

--- Create (or reuse) a real diff buffer with fancy rendering and keymaps.
--- The buffer persists so navigating back to it is instant.
---@param diff_text string
---@param filename string
---@return number buf
function M.open_diff_buffer(diff_text, filename)
  -- Reuse existing buffer for this file if valid
  local key = "diff:" .. filename
  M._diff_bufs = M._diff_bufs or {}
  local buf = M._diff_bufs[key]

  if not buf or not vim.api.nvim_buf_is_valid(buf) then
    buf = vim.api.nvim_create_buf(true, false)
    vim.bo[buf].bufhidden = "hide"
    vim.bo[buf].buftype = "nofile"
    vim.bo[buf].swapfile = false
    local short = vim.fn.fnamemodify(filename, ":t")
    vim.api.nvim_buf_set_name(buf, "diff://" .. short)
    M._diff_bufs[key] = buf

    -- Buffer-local keymaps for the diff buffer
    local opts = { buffer = buf, silent = true }
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
    end, vim.tbl_extend("force", opts, { desc = "Next hunk" }))

    vim.keymap.set("n", "[c", function()
      local cursor = vim.api.nvim_win_get_cursor(0)
      local lines = vim.api.nvim_buf_get_lines(buf, 0, cursor[1] - 1, false)
      for i = #lines, 1, -1 do
        if lines[i]:match("^@@") then
          vim.api.nvim_win_set_cursor(0, { i, 0 })
          return
        end
      end
    end, vim.tbl_extend("force", opts, { desc = "Prev hunk" }))
  end

  -- Re-render content (may have changed after staging)
  render_fancy_diff(buf, diff_text)
  return buf
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

--- Show the appropriate buffer in the main window when cursor moves.
--- On hunk rows: show the real file at the hunk position.
--- On file group headers: show the fancy diff buffer.
--- Bypasses Trouble's preview system entirely to avoid overlay issues.
---@param view trouble.View
function M.auto_preview(view)
  local loc = view:at()
  local win = M.get_main_win(view)
  if not win or not vim.api.nvim_win_is_valid(win) then return end

  if loc and loc.item then
    -- Hunk row: show the real file at the hunk position
    local item = loc.item
    local path = item.filename
    if path then
      local buf = vim.fn.bufadd(path)
      vim.fn.bufload(buf)
      if vim.api.nvim_win_get_buf(win) ~= buf then
        vim.api.nvim_win_set_buf(win, buf)
      end
      if item.pos and item.pos[1] > 0 then
        local line = math.min(item.pos[1], vim.api.nvim_buf_line_count(buf))
        pcall(vim.api.nvim_win_set_cursor, win, { line, item.pos[2] or 0 })
        vim.api.nvim_win_call(win, function()
          vim.cmd("normal! zz")
        end)
      end
    end
    return
  end

  -- File group header: show the fancy diff buffer
  if loc and loc.node and loc.node.item then
    local filename = loc.node.item.filename
    local diff_text = M._file_diffs and M._file_diffs[filename]
    if not diff_text or diff_text == "" then return end

    local buf = M.open_diff_buffer(diff_text, filename)
    if vim.api.nvim_win_get_buf(win) ~= buf then
      vim.api.nvim_win_set_buf(win, buf)
    end
    vim.api.nvim_win_set_cursor(win, { 1, 0 })
  end
end

return M

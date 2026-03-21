--- Trouble source: diff_review
--- Shows git diff hunks grouped by file with staging checkboxes.
--- Usage: :Trouble diff_review

local Item = require("trouble.item")

local M = {}

-- next

-- Background-only highlight groups for diff lines.
-- Pull bg from existing DiffAdd/DiffDelete so they match the gutter colors.
local function setup_bg_highlights()
  local function get_bg(name)
    local hl = vim.api.nvim_get_hl(0, { name = name, link = false })
    return hl.bg
  end
  local add_bg = get_bg("DiffAdd") or "#002200"
  local del_bg = get_bg("DiffDelete") or "#220000"
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

--- Open the DiffReview picker. Called from the :DiffReview command.
function M.open()
  M._main_win = nil
  local view = require("trouble").open("diff_review")
  if view then
    view.first_render:next(function()
      view:fold_level({ level = 1 })
      M.auto_preview(view)
      view.win:on("WinEnter", function()
        if not view.closed then
          M.auto_preview(view)
        end
      end)
      -- Clean up diff buffers when Trouble closes
      view.win:on("WinClosed", function()
        M._cleanup_diff_buffers()
      end)
    end)
  end
end

--- Close and wipe all diff buffers
function M._cleanup_diff_buffers()
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
      local status, file = line:match("^(%S+)%s+(.+)$")
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
      local status, file = line:match("^(%S+)%s+(.+)$")
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
    local context_text = hunk.context or ""
    local header = hunk.status or "@@"
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
        category = "Tracked Changes",
        check = hunk.staged and "[x]" or "[ ]",
        file_check = file_check,
        hunk_header = header,
        context_text = context_text,
        staged = hunk.staged,
        diff = hunk.diff,
        added = hunk.added,
        removed = hunk.removed,
        stats = (fs.added > 0 or fs.removed > 0)
          and ("+" .. fs.added .. " -" .. fs.removed)
          or (hunk.status == "A" and "new" or hunk.status == "D" and "deleted" or ""),
      },
    })
  end

  -- Add untracked files
  for _, f in ipairs(untracked_files) do
    local filename = vim.fn.fnamemodify(cwd .. "/" .. f, ":p")
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

  -- Build per-file combined diffs + staged status for file-level preview
  local file_diffs = {}
  local file_staged = {}
  for _, hunk in ipairs(all_hunks) do
    local f = hunk.file
    if not file_diffs[f] then
      file_diffs[f] = {}
      file_staged[f] = {}
    end
    table.insert(file_diffs[f], hunk.diff)
    table.insert(file_staged[f], hunk.staged)
  end
  M._file_diffs = {}
  M._file_hunk_staged = {}
  for f, diffs in pairs(file_diffs) do
    local filename = vim.fn.fnamemodify(cwd .. "/" .. f, ":p")
    M._file_diffs[filename] = table.concat(diffs, "\n")
    M._file_hunk_staged[filename] = file_staged[f]
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
--- @param buf number
--- @param diff_text string
--- @param hunk_staged? boolean[] staged status per hunk (in order)
local function render_fancy_diff(buf, diff_text, hunk_staged)
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
  local hunk_idx = 0

  for _, block in ipairs(diff.blocks) do
    local block_ctx = ctx_base:extend({ block = block })
    for _, hunk in ipairs(block.hunks) do
      hunk_idx = hunk_idx + 1
      -- Checkbox + @@ separator
      local range_line = hunk.diff[1] or "@@"
      local range_only = range_line:match("^(@@[^@]+@@)") or range_line
      local is_staged = hunk_staged and hunk_staged[hunk_idx] or false
      local check = is_staged and "[x] " or "[ ] "
      local check_hl = is_staged and "DiagnosticOk" or "Comment"
      ret[#ret + 1] = {
        { check, check_hl },
        { range_only, "SnacksDiffHunkHeader" },
      }

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
    vim.api.nvim_buf_set_name(buf, "diff://" .. short)
    M._diff_bufs[key] = buf
    M._buf_filename[buf] = filename


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

      -- Save cursor so we can restore when returning to this diff buffer
      M._buf_saved_cursor[buf] = vim.api.nvim_win_get_cursor(0)

      local hunks = M._buf_hunks[buf]
      local cursor = M._buf_saved_cursor[buf][1]

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
            pcall(vim.api.nvim_win_set_cursor, 0, { new_start, 0 })
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
          pcall(vim.api.nvim_win_set_cursor, 0, { target_line, 0 })
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
      -- Run from git root
      local cwd = vim.trim(vim.fn.systemlist({ "git", "rev-parse", "--show-toplevel" })[1] or "")
      local result = vim.fn.system(
        { "git", "-C", cwd, "apply", "--cached", "--whitespace=nowarn", "-" },
        patch .. "\n"
      )
      if vim.v.shell_error ~= 0 then
        vim.notify("Stage failed: " .. result, vim.log.levels.ERROR)
      else
        vim.notify("Hunk staged", vim.log.levels.INFO)
        -- Collapse the staged hunk, update checkbox, and jump to next
        if hunks and cur_hunk_idx then
          hunks[cur_hunk_idx].folded = true
          -- Update checkbox from [ ] to [x] on the @@ line
          local hunk_line = hunks[cur_hunk_idx].start_line - 1 -- 0-indexed
          local line_text = vim.api.nvim_buf_get_lines(buf, hunk_line, hunk_line + 1, false)[1] or ""
          local new_text = line_text:gsub("^%[ %]", "[x]", 1)
          if new_text ~= line_text then
            vim.bo[buf].modifiable = true
            vim.api.nvim_buf_set_lines(buf, hunk_line, hunk_line + 1, false, { new_text })
            vim.bo[buf].modifiable = false
          end
          M._render_with_folds(buf)
          -- Jump to next hunk, or stay on current @@ if last
          local next_idx = cur_hunk_idx + 1
          if next_idx <= #hunks then
            pcall(vim.api.nvim_win_set_cursor, 0, { hunks[next_idx].start_line, 0 })
          else
            pcall(vim.api.nvim_win_set_cursor, 0, { hunks[cur_hunk_idx].start_line, 0 })
          end
        end
        require("trouble").refresh("diff_review")
      end
    end, vim.tbl_extend("force", kopts, { desc = "Stage hunk", nowait = true }))

    -- Unstage the hunk under cursor
    vim.keymap.set("n", "U", function()
      local patch, _ = get_hunk_at_cursor(buf)
      if not patch then
        vim.notify("No hunk under cursor", vim.log.levels.WARN)
        return
      end
      local cwd = vim.trim(vim.fn.systemlist({ "git", "rev-parse", "--show-toplevel" })[1] or "")
      local result = vim.fn.system(
        { "git", "-C", cwd, "apply", "--cached", "--reverse", "--whitespace=nowarn", "-" },
        patch .. "\n"
      )
      if vim.v.shell_error ~= 0 then
        vim.notify("Unstage failed: " .. result, vim.log.levels.ERROR)
      else
        vim.notify("Hunk unstaged", vim.log.levels.INFO)
        -- Update checkbox from [x] to [ ] and expand
        local hunks = M._buf_hunks[buf]
        local cursor_line = vim.api.nvim_win_get_cursor(0)[1]
        if hunks then
          for _, h in ipairs(hunks) do
            if cursor_line >= h.start_line and cursor_line <= h.end_line then
              h.folded = false
              local hunk_line = h.start_line - 1
              local line_text = vim.api.nvim_buf_get_lines(buf, hunk_line, hunk_line + 1, false)[1] or ""
              local new_text = line_text:gsub("^%[x%]", "[ ]", 1)
              if new_text ~= line_text then
                vim.bo[buf].modifiable = true
                vim.api.nvim_buf_set_lines(buf, hunk_line, hunk_line + 1, false, { new_text })
                vim.bo[buf].modifiable = false
              end
              M._render_with_folds(buf)
              break
            end
          end
        end
        require("trouble").refresh("diff_review")
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
function M._refresh_diff_buffer(buf, filename)
  local cwd = vim.trim(vim.fn.systemlist({ "git", "rev-parse", "--show-toplevel" })[1] or "")
  if cwd == "" then return end

  -- Fetch both unstaged and staged hunks for this file
  local unstaged = get_hunks(cwd, false)
  local staged = get_hunks(cwd, true)
  local all_hunks = {}
  local norm_filename = vim.fs.normalize(filename)
  for _, h in ipairs(unstaged) do
    if vim.fs.normalize(vim.fn.fnamemodify(cwd .. "/" .. h.file, ":p")) == norm_filename then
      all_hunks[#all_hunks + 1] = { diff = h.diff, staged = false, pos = h.pos }
    end
  end
  for _, h in ipairs(staged) do
    if vim.fs.normalize(vim.fn.fnamemodify(cwd .. "/" .. h.file, ":p")) == norm_filename then
      all_hunks[#all_hunks + 1] = { diff = h.diff, staged = true, pos = h.pos }
    end
  end
  -- Sort by position so hunks appear in file order
  table.sort(all_hunks, function(a, b) return a.pos < b.pos end)

  local file_diffs = {}
  local staged_flags = {}
  for _, h in ipairs(all_hunks) do
    file_diffs[#file_diffs + 1] = h.diff
    staged_flags[#staged_flags + 1] = h.staged
  end

  if #file_diffs > 0 then
    local new_diff = table.concat(file_diffs, "\n")
    render_fancy_diff(buf, new_diff, staged_flags)
    local hunk_map = M._compute_hunk_map(new_diff)
    -- Auto-fold staged hunks
    for i, h in ipairs(hunk_map) do
      if staged_flags[i] then
        h.folded = true
      end
    end
    M._buf_hunks[buf] = hunk_map
    M._render_with_folds(buf)
  else
    vim.bo[buf].modifiable = true
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, { "No changes" })
    vim.bo[buf].modifiable = false
    M._buf_hunks[buf] = {}
  end
end

--- Refresh an open diff buffer for the given filename (if one exists).
--- Called from Trouble S/U actions to sync the diff buffer.
---@param filename string
function M.refresh_open_diff_buffer(filename)
  local key = "diff:" .. filename
  M._diff_bufs = M._diff_bufs or {}
  local buf = M._diff_bufs[key]
  if buf and vim.api.nvim_buf_is_valid(buf) then
    M._refresh_diff_buffer(buf, filename)
  end
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
    -- Hunk row: show the diff buffer and jump to the matching hunk
    local item = loc.item
    local filename = item.filename
    if not filename then return end
    if not (M._file_diffs and M._file_diffs[filename]) then return end

    local diff_buf = M.open_diff_buffer(filename)
    local need_refresh = vim.api.nvim_win_get_buf(win) ~= diff_buf
    vim.api.nvim_win_set_buf(win, diff_buf)
    if need_refresh then
      M._refresh_diff_buffer(diff_buf, filename)
    end

    -- Find the matching hunk in the diff buffer by comparing diff text
    local hunks = M._buf_hunks[diff_buf]
    local item_diff = item.item and item.item.diff
    -- Clear previous hover highlight
    local hover_ns = vim.api.nvim_create_namespace("diff_review_hover")
    vim.api.nvim_buf_clear_namespace(diff_buf, hover_ns, 0, -1)
    if hunks and item_diff then
      for _, h in ipairs(hunks) do
        if h.diff == item_diff then
          -- Highlight the @@ header line (visible even when folded)
          pcall(vim.api.nvim_buf_set_extmark, diff_buf, hover_ns, h.start_line - 1, 0, {
            line_hl_group = "CursorLine",
          })
          pcall(vim.api.nvim_win_set_cursor, win, { h.start_line, 0 })
          vim.api.nvim_win_call(win, function()
            vim.cmd("normal! zz")
          end)
          break
        end
      end
    end
    return
  end

  -- File group header: show the fancy diff buffer
  if loc and loc.node and loc.node.item then
    local filename = loc.node.item.filename
    if not (M._file_diffs and M._file_diffs[filename]) then return end

    local buf = M.open_diff_buffer(filename)
    vim.api.nvim_win_set_buf(win, buf)
    -- Always refresh to get latest staged state
    M._refresh_diff_buffer(buf, filename)
    -- Restore saved cursor position, or start at line 1
    local saved = M._buf_saved_cursor[buf]
    if saved then
      local max_line = vim.api.nvim_buf_line_count(buf)
      pcall(vim.api.nvim_win_set_cursor, win, { math.min(saved[1], max_line), saved[2] or 0 })
    else
      vim.api.nvim_win_set_cursor(win, { 1, 0 })
    end
  end
end

return M

-- next

--- Diff Review picker: two-level tree (file → hunks) for reviewing git diffs
--- in real source files with full LSP support.
---
--- Usage: :DiffReview

local M = {}

local Diff = require("snacks.picker.source.diff")
local format = require("snacks.picker.format")

--- Compute added/removed line counts from a diff patch string
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

--- Build git diff args (mirrors snacks.picker.source.git.git())
---@param staged? boolean
---@return string[]
local function git_diff_args(staged)
  local args = {
    "-c", "core.quotepath=false",
    "--no-pager",
    "diff",
    "--no-color",
    "--no-ext-diff",
  }
  if staged then
    args[#args + 1] = "--cached"
  end
  return args
end

--- Finder: parse git diff output into a file→hunk tree.
--- Runs both `git diff` (unstaged) and `git diff --cached` (staged) so that
--- staged hunks show as checked and unstaged as unchecked.
---@param _opts table
---@param ctx snacks.picker.finder.ctx
function M.finder(_opts, ctx)
  local cwd = ctx:git_root()
  ctx.picker:set_cwd(cwd)

  -- Expand state stored on picker instance (ctx.meta is recreated each find())
  if not ctx.picker._diff_review then
    ctx.picker._diff_review = { expanded = {}, initialized = false }
  end
  local state = ctx.picker._diff_review
  local expanded = state.expanded

  -- Collect flat hunk items from both unstaged and staged diffs
  local unstaged_finder = Diff.diff(ctx:opts({
    cmd = "git",
    args = git_diff_args(false),
    cwd = cwd,
  }), ctx)

  local staged_finder = Diff.diff(ctx:opts({
    cmd = "git",
    args = git_diff_args(true),
    cwd = cwd,
  }), ctx)

  return function(cb)
    local flat_items = {}
    unstaged_finder(function(item)
      item.staged = false
      flat_items[#flat_items + 1] = item
    end)
    staged_finder(function(item)
      item.staged = true
      flat_items[#flat_items + 1] = item
    end)

    -- Group hunks by file, preserving first-seen order
    local file_order = {} ---@type string[]
    local file_groups = {} ---@type table<string, table[]>
    for _, item in ipairs(flat_items) do
      local f = item.file
      if not file_groups[f] then
        file_groups[f] = {}
        file_order[#file_order + 1] = f
      end
      file_groups[f][#file_groups[f] + 1] = item
    end

    -- Sort hunks within each file by line position
    for _, hunks in pairs(file_groups) do
      table.sort(hunks, function(a, b)
        if a.pos[1] ~= b.pos[1] then
          return a.pos[1] < b.pos[1]
        end
        -- Unstaged before staged at same position
        return not a.staged and b.staged
      end)
    end

    -- On first run, default all files to collapsed
    if not state.initialized then
      for _, f in ipairs(file_order) do
        if expanded[f] == nil then
          expanded[f] = false
        end
      end
      state.initialized = true
    end

    -- Build tree items
    for fi, filepath in ipairs(file_order) do
      local hunks = file_groups[filepath]
      local is_last_file = fi == #file_order
      local first_hunk = hunks[1]
      local is_expanded = expanded[filepath] == true

      -- Aggregate stats and staging counts
      local total_added, total_removed = 0, 0
      local staged_count = 0
      local unstaged_diffs = {}
      for _, hunk in ipairs(hunks) do
        local a, r = count_stats(hunk.diff)
        total_added = total_added + a
        total_removed = total_removed + r
        if hunk.staged then
          staged_count = staged_count + 1
        else
          unstaged_diffs[#unstaged_diffs + 1] = hunk.diff
        end
      end

      -- FILE item (copy pos to avoid sharing reference with first hunk —
      -- Snacks preview uses reference equality on pos to skip redundant updates)
      local file_item = {
        text = filepath,
        file = filepath,
        cwd = cwd,
        pos = { first_hunk.pos[1], first_hunk.pos[2] },
        is_file_node = true,
        is_hunk_node = false,
        expanded = is_expanded,
        -- diff for file-level staging: only the unstaged hunks
        diff = #unstaged_diffs > 0 and table.concat(unstaged_diffs, "\n") or nil,
        staged = staged_count == #hunks,
        block = first_hunk.block,
        hunk_count = #hunks,
        staged_count = staged_count,
        stats = { added = total_added, removed = total_removed },
        parent = nil,
        last = is_last_file and not is_expanded,
      }
      cb(file_item)

      -- HUNK items (only if expanded)
      if is_expanded then
        for hi, hunk in ipairs(hunks) do
          local is_last_hunk = hi == #hunks
          local context = nil
          local hunk_header = hunk.diff:match("^@@[^@]+@@%s*(.-)%s*\n")
          if hunk_header and hunk_header ~= "" then
            context = hunk_header
          end

          cb({
            text = filepath .. ":" .. hunk.pos[1],
            file = filepath,
            cwd = cwd,
            pos = { hunk.pos[1], hunk.pos[2] },
            is_file_node = false,
            is_hunk_node = true,
            diff = hunk.diff,
            staged = hunk.staged,
            block = hunk.block,
            context = context,
            parent = file_item,
            last = is_last_hunk,
          })
        end

        file_item.last = false
      end
    end
  end
end

--- Formatter: render file and hunk items with checkboxes
---@param item snacks.picker.Item
---@param picker snacks.Picker
---@return snacks.picker.Highlight[]
function M.format(item, picker)
  local ret = {} ---@type snacks.picker.Highlight[]

  if item.is_file_node then
    -- Expand/collapse icon
    local expand_icon = item.expanded and "▾ " or "▸ "
    ret[#ret + 1] = { expand_icon, "SnacksPickerTree" }

    -- Checkbox: all staged = checked, some = partial, none = unchecked
    local check
    if item.staged then
      check = "[x]"
    elseif item.staged_count and item.staged_count > 0 then
      check = "[-]"
    else
      check = "[ ]"
    end
    local check_hl = item.staged and "DiagnosticOk" or "Comment"
    ret[#ret + 1] = { check, check_hl }
    ret[#ret + 1] = { " " }

    -- Git status indicator (A/M/D/R)
    local block = item.block
    local status_char = "M"
    if block then
      if block.new then
        status_char = "A"
      elseif block.delete then
        status_char = "D"
      elseif block.rename then
        status_char = "R"
      end
    end
    local status_hls = {
      A = "SnacksPickerGitStatusAdded",
      M = "SnacksPickerGitStatusModified",
      D = "SnacksPickerGitStatusDeleted",
      R = "SnacksPickerGitStatusRenamed",
    }
    ret[#ret + 1] = { status_char, status_hls[status_char] or "SnacksPickerGitStatus" }
    ret[#ret + 1] = { " " }

    -- Filename with icon (reuse Snacks formatter)
    vim.list_extend(ret, format.filename(item, picker))

    -- Stats: +N -M
    local stats = item.stats
    if stats then
      if stats.added > 0 then
        ret[#ret + 1] = { "+" .. stats.added, "SnacksPickerGitStatusAdded" }
        ret[#ret + 1] = { " " }
      end
      if stats.removed > 0 then
        ret[#ret + 1] = { "-" .. stats.removed, "SnacksPickerGitStatusDeleted" }
        ret[#ret + 1] = { " " }
      end
    end

  elseif item.is_hunk_node then
    -- Tree indentation (walks item.parent chain)
    vim.list_extend(ret, format.tree(item, picker))

    -- Checkbox
    local check = item.staged and "[x]" or "[ ]"
    local check_hl = item.staged and "DiagnosticOk" or "Comment"
    ret[#ret + 1] = { check, check_hl }
    ret[#ret + 1] = { " " }

    -- Hunk header: extract @@ range @@
    local range = item.diff and item.diff:match("^(@@[^@]+@@)") or "@@"
    ret[#ret + 1] = { range, "DiffChange" }

    -- Function context
    if item.context then
      ret[#ret + 1] = { " " }
      ret[#ret + 1] = { item.context, "Comment" }
    end
  end

  return ret
end

--- Preview: show file in the overlay using Snacks' built-in file previewer,
--- and prepare the real buffer in main so focus_preview lands in the real file.
function M.preview(ctx)
  local item = ctx.item
  if not item or not item.file then return end

  -- Show the file in the preview overlay (syntax highlighted, positioned)
  Snacks.picker.preview.file(ctx)

  -- Prepare the real buffer in main behind the overlay.
  -- When focus_preview is triggered, the overlay goes away and the user
  -- lands in the real file with LSP, mini.diff signs, etc.
  local path = Snacks.picker.util.path(item)
  if not path then return end

  local main = ctx.picker.main
  if not main or not vim.api.nvim_win_is_valid(main) then return end

  -- Only open the file if main isn't already showing it
  local main_buf = vim.api.nvim_win_get_buf(main)
  local main_name = vim.api.nvim_buf_get_name(main_buf)
  local full_path = vim.fn.fnamemodify(path, ":p")
  if vim.fn.fnamemodify(main_name, ":p") ~= full_path then
    vim.api.nvim_win_call(main, function()
      vim.cmd.edit(full_path)
    end)
    main_buf = vim.api.nvim_win_get_buf(main)
  end

  -- Position cursor in main at the hunk
  if item.pos and item.pos[1] > 0 then
    local line = math.min(item.pos[1], vim.api.nvim_buf_line_count(main_buf))
    vim.api.nvim_win_set_cursor(main, { line, item.pos[2] or 0 })
  end
end

--- Action: jump to the real file by hiding the picker layout.
--- The preview overlay covers main, so focus_preview just puts you in the
--- overlay scratch buffer. Instead, we hide the entire picker layout to
--- reveal the real main window with the real buffer (LSP, signs, editing).
function M.confirm(picker)
  local item = picker:current()
  if not item then return end

  -- File nodes: toggle expand instead of jumping
  if item.is_file_node then
    M.toggle_expand(picker)
    return
  end

  local main = picker.main
  if not main or not vim.api.nvim_win_is_valid(main) then return end

  local path = Snacks.picker.util.path(item)
  if not path then return end

  -- Close the picker, then open the real file in main
  picker:close()

  vim.api.nvim_set_current_win(main)
  vim.cmd.edit(vim.fn.fnamemodify(path, ":p"))

  -- Position cursor at the hunk
  if item.pos and item.pos[1] > 0 then
    local buf = vim.api.nvim_win_get_buf(main)
    local line = math.min(item.pos[1], vim.api.nvim_buf_line_count(buf))
    vim.api.nvim_win_set_cursor(main, { line, item.pos[2] or 0 })
    vim.cmd("normal! zz")
  end
end

--- Action: toggle expand/collapse on file nodes
function M.toggle_expand(picker)
  local item = picker:current()
  if not item then return end

  local state = picker._diff_review
  if not state then return end

  local file = item.file
  if item.is_file_node then
    state.expanded[file] = not state.expanded[file]
  elseif item.is_hunk_node then
    state.expanded[file] = false
  end

  picker:find({
    refresh = true,
    on_done = function()
      reveal(picker, file)
    end,
  })
end

--- Find an item by file path and optional line position, then scroll to it.
--- Uses the same pattern as snacks explorer's reveal().
---@param picker snacks.Picker
---@param file string
---@param pos_line? number
local function reveal(picker, file, pos_line)
  for item, idx in picker:iter() do
    if item.file == file then
      if not pos_line or (item.pos and item.pos[1] == pos_line) then
        picker.list:view(idx)
        return
      end
    end
  end
  -- Fallback: at least find the file
  if pos_line then
    for item, idx in picker:iter() do
      if item.file == file then
        picker.list:view(idx)
        return
      end
    end
  end
end

--- Refresh the picker, then restore cursor to the same item by identity.
---@param picker snacks.Picker
---@param file string
---@param pos_line? number
local function refresh_and_reveal(picker, file, pos_line)
  picker:find({
    refresh = true,
    on_done = function()
      reveal(picker, file, pos_line)
    end,
  })
end

--- Action: stage file or hunk
function M.stage(picker)
  local items = picker:selected({ fallback = true })
  if #items == 0 then return end

  -- Remember cursor identity for reveal after refresh
  local current = picker:current()
  local reveal_file = current and current.file
  local reveal_line = current and current.pos and current.pos[1]

  local done = 0
  for _, item in ipairs(items) do
    local cmd_opts = { cwd = item.cwd }
    local cmd

    if item.is_file_node then
      cmd = { "git", "add", item.file }
    elseif item.is_hunk_node and item.diff then
      cmd_opts.input = item.diff
      cmd = { "git", "apply", "--cached" }
    else
      Snacks.notify.error("Can't stage this item", { title = "Diff Review" })
      return
    end

    Snacks.picker.util.cmd(cmd, function()
      done = done + 1
      if done == #items then
        refresh_and_reveal(picker, reveal_file, reveal_line)
      end
    end, cmd_opts)
  end
end

--- Action: unstage file or hunk
function M.unstage(picker)
  local items = picker:selected({ fallback = true })
  if #items == 0 then return end

  local current = picker:current()
  local reveal_file = current and current.file
  local reveal_line = current and current.pos and current.pos[1]

  local done = 0
  for _, item in ipairs(items) do
    local cmd_opts = { cwd = item.cwd }
    local cmd

    if item.is_file_node then
      cmd = { "git", "restore", "--staged", item.file }
    elseif item.is_hunk_node and item.diff then
      cmd_opts.input = item.diff
      cmd = { "git", "apply", "--cached", "--reverse" }
    else
      Snacks.notify.error("Can't unstage this item", { title = "Diff Review" })
      return
    end

    Snacks.picker.util.cmd(cmd, function()
      done = done + 1
      if done == #items then
        refresh_and_reveal(picker, reveal_file, reveal_line)
      end
    end, cmd_opts)
  end
end

--- Open the diff review picker
function M.open(opts)
  opts = opts or {}
  Snacks.picker.pick(vim.tbl_deep_extend("force", {
    source = "diff_review",
    title = "Diff Review",
    finder = M.finder,
    format = M.format,
    preview = M.preview,
    auto_close = false,
    focus = "list",
    matcher = { keep_parents = true },
    sort = { fields = { "idx" } },
    actions = {
      toggle_expand = M.toggle_expand,
      confirm = M.confirm,
      show_picker = M.show_picker,
      stage = M.stage,
      unstage = M.unstage,
    },
    win = {
      input = {
        keys = {
          ["<CR>"] = { "confirm", mode = { "n", "i" } },
        },
      },
      list = {
        keys = {
          ["<CR>"] = "confirm",
          ["<Tab>"] = "toggle_expand",
          ["S"] = "stage",
          ["U"] = "unstage",
          ["u"] = "focus_input",
        },
      },
    },
  }, opts))
end

return M

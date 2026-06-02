--- Commit flow for DiffReview, modelled on Neogit.
---
--- `cc` runs `git commit` as a job with a headless "fake editor" wired to
--- GIT_EDITOR, and reuses the diff-preview window above the Trouble pane:
---   1. Git runs the pre-commit hook first; its output streams into that window
---      (a console buffer).
---   2. When git invokes the editor, the headless instance RPCs back to *this*
---      (parent) nvim, which swaps the same window's buffer to the commit
---      message buffer. `<C-c><C-c>` writes it and lets the headless editor exit
---      0 (git commits); `<C-q>` exits non-zero (git aborts).
---   3. On success/abort the window is handed back to the preview (and the list
---      refreshes). On a real failure the window shows a buffer with git's error
---      output until dismissed with `q`.
---
--- The bridge is the same one Neogit uses: git's editor step can't be driven
--- from the running instance, so a child nvim relays it over RPC (`sockconnect`
--- + `rpcrequest`/`rpcnotify` over the `$NVIM` pipe).

local M = {}

-- Active commit session.
-- { win, list_win, prev_buf, prev_winbar, console, on_done, aborted }
M._active = nil

-- ─── fake editor command ────────────────────────────────────────────────────

--- Config root holding this module, so the `--clean` headless editor can
--- `require` it (it has no user runtimepath otherwise).
local function runtimepath_root()
  local source = debug.getinfo(1, "S").source:sub(2):gsub("\\", "/")
  return (source:gsub("/lua/diff_review/commit%.lua$", ""))
end

--- The GIT_EDITOR command: a headless nvim that runs M.client(). Mirrors
--- Neogit's construction (shellescape per token) so git's shell can parse it.
local function remote_editor_cmd()
  local nvim = vim.fn.shellescape(vim.v.progpath)
  local rtp_cmd = vim.fn.shellescape(("set runtimepath^=%s"):format(vim.fn.fnameescape(runtimepath_root())))
  local lua_cmd = vim.fn.shellescape("lua require('diff_review.commit').client()")
  return table.concat({
    nvim, "--headless", "--clean", "--noplugin", "-n", "-R",
    "-c", rtp_cmd, "-c", lua_cmd,
  }, " ")
end

-- ─── headless client: bridges git's editor step back to the parent ──────────

--- Runs inside the headless GIT_EDITOR instance. Asks the parent nvim to open
--- the message buffer, then keeps running (event loop) until the parent sends
--- `qall` (commit) or `cq` (abort). On any failure, exits non-zero so git
--- aborts instead of hanging on a child that never opened a buffer.
function M.client()
  local ok, err = pcall(function()
    local target = vim.fn.argv()[1]
    assert(target and target ~= "", "no editor target")
    target = vim.fn.fnamemodify(target, ":p")

    local parent = vim.env.NVIM
    assert(parent and parent ~= "", "parent nvim ($NVIM) not set")

    local client_addr = vim.fn.serverstart()
    local code = ("lua require('diff_review.commit').editor(%q, %q)"):format(target, client_addr)
    local connected, chan = pcall(vim.fn.sockconnect, "pipe", parent, { rpc = true })
    assert(connected and chan ~= 0, "cannot reach parent nvim at " .. tostring(parent))
    vim.rpcrequest(chan, "nvim_command", code)
    pcall(vim.fn.chanclose, chan)
  end)
  if not ok then
    pcall(vim.api.nvim_err_writeln, "diff_review.commit (client): " .. tostring(err))
    vim.cmd("cq")
  end
end

-- ─── helpers ────────────────────────────────────────────────────────────────

local function winbar(win, text)
  if win and vim.api.nvim_win_is_valid(win) then
    pcall(function() vim.wo[win].winbar = text end)
  end
end

--- Append job output (array of lines, possibly with a trailing partial) to the
--- console buffer and keep its window scrolled to the bottom.
local function append(buf, data)
  if not vim.api.nvim_buf_is_valid(buf) then return end
  local lines = {}
  for _, line in ipairs(data) do
    lines[#lines + 1] = (line:gsub("\r$", ""))
  end
  if #lines > 1 and lines[#lines] == "" then
    lines[#lines] = nil
  end
  if #lines == 0 then return end
  vim.bo[buf].modifiable = true
  local count = vim.api.nvim_buf_line_count(buf)
  local empty = count == 1 and (vim.api.nvim_buf_get_lines(buf, 0, 1, false)[1] or "") == ""
  vim.api.nvim_buf_set_lines(buf, empty and 0 or count, empty and 1 or count, false, lines)
  vim.bo[buf].modifiable = false
  for _, win in ipairs(vim.fn.win_findbuf(buf)) do
    pcall(vim.api.nvim_win_set_cursor, win, { vim.api.nvim_buf_line_count(buf), 0 })
  end
end

-- ─── message editor (runs in the parent) ────────────────────────────────────

--- Swap the commit message buffer into the borrowed window and bind
--- submit/abort. Invoked over RPC from the headless client; `client_addr` is
--- the headless server to signal.
---@param target string COMMIT_EDITMSG path
---@param client_addr string headless client's server address
function M.editor(target, client_addr)
  local st = M._active
  local function signal(abort)
    local ok, chan = pcall(vim.fn.sockconnect, "pipe", client_addr, { rpc = true })
    if ok and chan ~= 0 then
      pcall(vim.rpcnotify, chan, "nvim_command", abort and "cq" or "qall")
      pcall(vim.fn.chanclose, chan)
    end
  end

  if not (st and st.win and vim.api.nvim_win_is_valid(st.win)) then
    signal(true) -- no window to host the editor; abort rather than hang git
    return
  end

  local buf = vim.fn.bufadd(target)
  vim.fn.bufload(buf)
  vim.bo[buf].buftype = ""
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = "gitcommit"
  vim.bo[buf].bufhidden = "wipe"
  vim.bo[buf].modifiable = true

  vim.api.nvim_win_set_buf(st.win, buf)
  winbar(st.win, " %#Comment#<C-c><C-c>%* commit   %#Comment#<C-q>%* abort ")

  local finished = false
  local function finish(abort)
    if finished then return end
    finished = true
    if M._active then M._active.aborted = abort end
    if not abort and vim.api.nvim_buf_is_valid(buf) then
      vim.api.nvim_buf_call(buf, function() vim.cmd("silent! write") end)
    end
    signal(abort)
    -- The window keeps the message buffer until git exits and M._finish hands
    -- it back to the preview.
  end

  local map = { buffer = buf, nowait = true, silent = true }
  vim.keymap.set({ "n", "i" }, "<C-c><C-c>", function() finish(false) end, map)
  vim.keymap.set({ "n", "i" }, "<C-q>", function() finish(true) end, map)
  vim.keymap.set("n", "q", function() finish(true) end, map)
  vim.api.nvim_create_autocmd("BufWipeout", { buffer = buf, once = true, callback = function() finish(true) end })

  vim.api.nvim_set_current_win(st.win)
  -- Enter in normal mode (no startinsert): keeps <C-q>/<C-c><C-c> reliable.
  pcall(vim.api.nvim_win_set_cursor, st.win, { 1, 0 })
end

-- ─── running git commit + result ────────────────────────────────────────────

--- Hand the borrowed window back to the DiffReview preview and refresh.
local function restore_preview(st)
  if st.prev_buf and vim.api.nvim_buf_is_valid(st.prev_buf)
      and st.win and vim.api.nvim_win_is_valid(st.win) then
    pcall(vim.api.nvim_win_set_buf, st.win, st.prev_buf)
  end
  if st.on_done then pcall(st.on_done) end
end

--- Report the result of the commit: success/abort hand the window back to the
--- preview; a real failure shows git's output in the window until `q`.
function M._finish(code)
  local st = M._active
  if not st then return end
  M._active = nil

  local output = (st.console and vim.api.nvim_buf_is_valid(st.console))
      and vim.api.nvim_buf_get_lines(st.console, 0, -1, false) or {}

  require("diff_review").suspend_preview = false
  winbar(st.win, st.prev_winbar or "")

  if code == 0 or st.aborted then
    vim.notify(code == 0 and "Commit complete" or "Commit aborted", vim.log.levels.INFO)
    if st.console and vim.api.nvim_buf_is_valid(st.console) then
      pcall(vim.api.nvim_buf_delete, st.console, { force = true })
    end
    restore_preview(st)
    return
  end

  -- Real failure: show git's output in the window; q hands it back.
  local errbuf = st.console
  if not (errbuf and vim.api.nvim_buf_is_valid(errbuf)) then
    errbuf = vim.api.nvim_create_buf(false, true)
    vim.bo[errbuf].bufhidden = "wipe"
    vim.bo[errbuf].filetype = "git"
    vim.api.nvim_buf_set_lines(errbuf, 0, -1, false, output)
  end
  append(errbuf, { "", ("✗ git commit exited %d   (q to dismiss)"):format(code) })
  if st.win and vim.api.nvim_win_is_valid(st.win) then
    pcall(vim.api.nvim_win_set_buf, st.win, errbuf)
    winbar(st.win, " %#ErrorMsg# Commit failed %* — %#Comment#q%* dismiss ")
    pcall(vim.api.nvim_set_current_win, st.win)
  end
  for _, key in ipairs({ "q", "<Esc>" }) do
    vim.keymap.set("n", key, function()
      winbar(st.win, st.prev_winbar or "")
      if vim.api.nvim_buf_is_valid(errbuf) then pcall(vim.api.nvim_buf_delete, errbuf, { force = true }) end
      restore_preview(st)
    end, { buffer = errbuf, nowait = true, silent = true })
  end
end

--- Run `git commit` with the fake editor, reusing `opts.win` (the diff-preview
--- window). `opts.list_win` is the Trouble window; `opts.on_done` refreshes the
--- list and restores the preview after the window is handed back.
---@param opts { win: number, list_win?: number, on_done?: function }
function M.commit(opts)
  opts = opts or {}
  local win = opts.win
  if not (win and vim.api.nvim_win_is_valid(win)) then
    vim.notify("No diff window to host the commit", vim.log.levels.ERROR)
    return
  end
  if M._active then M._finish(1) end

  local root = vim.fn.systemlist({ "git", "rev-parse", "--show-toplevel" })[1]
  if vim.v.shell_error ~= 0 or not root or root == "" then
    vim.notify("Not a git repository", vim.log.levels.ERROR)
    return
  end
  root = vim.trim(root)

  local console = vim.api.nvim_create_buf(false, true)
  vim.bo[console].bufhidden = "hide"
  vim.bo[console].filetype = "git"

  M._active = {
    win = win,
    list_win = opts.list_win,
    prev_buf = vim.api.nvim_win_get_buf(win),
    prev_winbar = vim.wo[win].winbar,
    console = console,
    on_done = opts.on_done,
    aborted = false,
  }

  require("diff_review").suspend_preview = true
  vim.api.nvim_win_set_buf(win, console)
  vim.wo[win].number = false
  vim.wo[win].relativenumber = false
  vim.wo[win].wrap = true
  winbar(win, " Committing… ")

  local server = vim.v.servername
  if server == nil or server == "" then server = vim.fn.serverstart() end

  local editor = remote_editor_cmd()
  local job = vim.fn.jobstart({ "git", "commit" }, {
    cwd = root,
    env = { GIT_EDITOR = editor, GIT_SEQUENCE_EDITOR = editor, NVIM = server },
    on_stdout = function(_, data) append(console, data) end,
    on_stderr = function(_, data) append(console, data) end,
    on_exit = function(_, code) M._finish(code) end,
  })

  if job <= 0 then
    local message = "Failed to start `git commit` (jobstart returned " .. job .. ")."
    append(console, { message })
    vim.notify(message, vim.log.levels.ERROR, { title = "Diff Review" })
    M._finish(1)
  end
end

return M

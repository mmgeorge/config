--- Commit flow for the DiffReview Trouble pane, modelled on Neogit.
---
--- `cc` runs `git commit` as a job with a headless "fake editor" wired to
--- GIT_EDITOR. Everything happens in a SINGLE floating window:
---   1. Git runs the pre-commit hook first; its output streams into the window
---      (the console buffer).
---   2. When git invokes the editor, the headless instance RPCs back to *this*
---      (parent) nvim, which swaps the same window's buffer to the commit
---      message buffer. `<C-c><C-c>` writes it and lets the headless editor exit
---      0 (git commits); `<C-q>` exits non-zero (git aborts).
---   3. On success the window closes (and the DiffReview list refreshes). On
---      failure the window closes and a fresh buffer with git's error output
---      opens in its place.
---
--- The bridge is the same one Neogit uses: git's editor step can't be driven
--- from the running instance, so a child nvim relays it over RPC (`sockconnect`
--- + `rpcrequest`/`rpcnotify` over the `$NVIM` pipe).

local M = {}

-- Active commit session: { win, console, on_done, aborted }
M._active = nil

-- ─── fake editor command ────────────────────────────────────────────────────

--- Config root holding this module, so the `--clean` headless editor can
--- `require` it (it has no user runtimepath otherwise).
local function runtimepath_root()
  local source = debug.getinfo(1, "S").source:sub(2):gsub("\\", "/")
  return (source:gsub("/lua/trouble/sources/diff_review_commit%.lua$", ""))
end

--- The GIT_EDITOR command: a headless nvim that runs M.client(). Mirrors
--- Neogit's construction (shellescape per token) so git's shell can parse it.
local function remote_editor_cmd()
  local nvim = vim.fn.shellescape(vim.v.progpath)
  local rtp_cmd = vim.fn.shellescape(("set runtimepath^=%s"):format(vim.fn.fnameescape(runtimepath_root())))
  local lua_cmd = vim.fn.shellescape("lua require('trouble.sources.diff_review_commit').client()")
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
    local code = ("lua require('trouble.sources.diff_review_commit').editor(%q, %q)"):format(target, client_addr)
    local connected, chan = pcall(vim.fn.sockconnect, "pipe", parent, { rpc = true })
    assert(connected and chan ~= 0, "cannot reach parent nvim at " .. tostring(parent))
    vim.rpcrequest(chan, "nvim_command", code)
    pcall(vim.fn.chanclose, chan)
  end)
  if not ok then
    pcall(vim.api.nvim_err_writeln, "diff_review_commit (client): " .. tostring(err))
    vim.cmd("cq")
  end
end

-- ─── floating window ────────────────────────────────────────────────────────

--- Open `buf` in a centred floating window. `o`: width/height fractions of the
--- editor, optional `enter`, `title`.
---@return number win
local function float(buf, o)
  local cols, lines = vim.o.columns, vim.o.lines
  local width = math.max(20, math.floor(cols * (o.width or 0.7)))
  local height = math.max(3, math.floor(lines * (o.height or 0.55)))
  local win = vim.api.nvim_open_win(buf, o.enter == true, {
    relative = "editor",
    width = width,
    height = height,
    col = math.floor((cols - width) / 2),
    row = math.floor((lines - height) / 2),
    style = "minimal",
    border = "rounded",
    title = o.title,
    title_pos = "center",
  })
  vim.wo[win].number = false
  vim.wo[win].relativenumber = false
  vim.wo[win].signcolumn = "no"
  vim.wo[win].wrap = true
  return win
end

local function title(win, text)
  if win and vim.api.nvim_win_is_valid(win) then
    pcall(vim.api.nvim_win_set_config, win, { title = text, title_pos = "center" })
  end
end

--- Bind q / <C-q> / <Esc> in `buf` to close `win` (used for the read-only
--- console and error views).
local function bind_close(buf, win)
  for _, key in ipairs({ "q", "<C-q>", "<Esc>" }) do
    vim.keymap.set("n", key, function()
      if vim.api.nvim_win_is_valid(win) then pcall(vim.api.nvim_win_close, win, true) end
    end, { buffer = buf, nowait = true, silent = true })
  end
end

--- Append job output (array of lines, possibly with a trailing partial) to the
--- console buffer and keep it scrolled to the bottom.
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

--- Swap the commit message buffer into the existing console window and bind
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
    signal(true) -- no window to take over; abort rather than hang git
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
  title(st.win, " Commit  —  <C-c><C-c> commit   <C-q> abort ")

  local finished = false
  local function finish(abort)
    if finished then return end
    finished = true
    if M._active then M._active.aborted = abort end
    if not abort and vim.api.nvim_buf_is_valid(buf) then
      vim.api.nvim_buf_call(buf, function() vim.cmd("silent! write") end)
    end
    signal(abort)
    -- The window stays on the message buffer until git exits and M._finish
    -- tears it down (closing the window wipes this buffer).
  end

  local map = { buffer = buf, nowait = true, silent = true }
  vim.keymap.set({ "n", "i" }, "<C-c><C-c>", function() finish(false) end, map)
  vim.keymap.set({ "n", "i" }, "<C-q>", function() finish(true) end, map)
  vim.keymap.set("n", "q", function() finish(true) end, map)
  -- Closing the buffer any other way must still release git.
  vim.api.nvim_create_autocmd("BufWipeout", { buffer = buf, once = true, callback = function() finish(true) end })

  vim.api.nvim_set_current_win(st.win)
  -- Enter in normal mode (do not startinsert): keeps <C-q>/<C-c><C-c> reliable.
  vim.api.nvim_win_set_cursor(st.win, { 1, 0 })
end

-- ─── console popup + running git commit ─────────────────────────────────────

--- Tear down the commit window/console, then report the result: success closes
--- quietly (and refreshes), an explicit abort closes quietly, a real failure
--- opens a fresh buffer with git's output.
function M._finish(code)
  local st = M._active
  if not st then return end
  M._active = nil

  local output = (st.console and vim.api.nvim_buf_is_valid(st.console))
      and vim.api.nvim_buf_get_lines(st.console, 0, -1, false) or {}

  if st.win and vim.api.nvim_win_is_valid(st.win) then pcall(vim.api.nvim_win_close, st.win, true) end
  if st.console and vim.api.nvim_buf_is_valid(st.console) then
    pcall(vim.api.nvim_buf_delete, st.console, { force = true })
  end

  if code == 0 then
    vim.notify("Commit complete", vim.log.levels.INFO)
    if st.on_done then pcall(st.on_done) end
  elseif st.aborted then
    vim.notify("Commit aborted", vim.log.levels.INFO)
  else
    -- Real failure (e.g. hook rejected, empty message): show git's output.
    local errbuf = vim.api.nvim_create_buf(false, true)
    vim.bo[errbuf].bufhidden = "wipe"
    vim.bo[errbuf].filetype = "git"
    output[#output + 1] = ""
    output[#output + 1] = ("✗ git commit exited %d   (q to close)"):format(code)
    vim.api.nvim_buf_set_lines(errbuf, 0, -1, false, output)
    vim.bo[errbuf].modifiable = false
    local ewin = float(errbuf, { title = " Commit failed ", height = 0.5, enter = true })
    bind_close(errbuf, ewin)
  end
end

--- Run `git commit` with the fake editor in a single floating window.
--- `on_done` runs after a successful commit (e.g. to refresh the list).
---@param on_done? function
function M.commit(on_done)
  -- Only one session at a time.
  if M._active then M._finish(1) end

  local root = vim.fn.systemlist({ "git", "rev-parse", "--show-toplevel" })[1]
  if vim.v.shell_error ~= 0 or not root or root == "" then
    vim.notify("Not a git repository", vim.log.levels.ERROR)
    return
  end
  root = vim.trim(root)

  local console = vim.api.nvim_create_buf(false, true)
  vim.bo[console].bufhidden = "hide" -- survive the swap to the message buffer
  vim.bo[console].filetype = "git"
  local win = float(console, { title = " Committing… " })
  bind_close(console, win)

  M._active = { win = win, console = console, on_done = on_done, aborted = false }

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
    append(console, { "Failed to start `git commit` (jobstart returned " .. job .. ")." })
  end
end

return M

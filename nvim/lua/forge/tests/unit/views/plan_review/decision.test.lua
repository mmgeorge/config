-- Unit tests for `forge.views.plan_review.decision` (decision.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

-- The popup opens over the review, and its callback hides the review and focuses the Harness
-- transcript, which binds Enter in Normal mode. Typed keys must travel the real input loop, so
-- the case drives a child Neovim. The child's loop clock jumps past the popup's settle interval
-- once it opens, so the typed comment is not discarded.
T["enter_submits_once_and_leaves_insert_mode"] = function()
  local child = MiniTest.new_child_neovim()
  child.start({ "-u", "NONE" })
  MiniTest.finally(function() pcall(child.stop) end)
  child.lua([[
    vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
    _G.submitted, _G.transcript_enter = {}, 0
    _G.observed = {}
    vim.on_key(function(_, typed)
      if typed == "" then return end
      _G.observed[#_G.observed + 1] = { key = vim.fn.keytrans(typed), mode = vim.api.nvim_get_mode().mode, filetype = vim.bo.filetype }
    end)
    local transcript_win = vim.api.nvim_get_current_win()
    local transcript_buf = vim.api.nvim_get_current_buf()
    vim.api.nvim_buf_set_lines(transcript_buf, 0, -1, true, { "transcript row" })
    vim.bo[transcript_buf].modifiable = false
    vim.bo[transcript_buf].filetype = "ForgeHarness"
    vim.keymap.set("n", "<CR>", function() _G.transcript_enter = _G.transcript_enter + 1 end, { buffer = transcript_buf })
    local transcript_tab = vim.api.nvim_get_current_tabpage()
    -- The review owns a tab and holds an unsaved comment, so its buffer is modified.
    vim.cmd("tabnew")
    local review_tab, review_win = vim.api.nvim_get_current_tabpage(), vim.api.nvim_get_current_win()
    local review_buf = vim.api.nvim_get_current_buf()
    vim.bo[review_buf].bufhidden = "hide"
    vim.api.nvim_buf_set_lines(review_buf, 0, -1, true, { "pub fn requested_change();", "Unsaved comment" })
    require("forge.views.plan_review.decision").open({ title = "Request changes", parent_win = review_win }, function(comment)
      _G.submitted[#_G.submitted + 1] = comment
      -- `hide_review` closes the review's tab and returns to the transcript window.
      vim.api.nvim_set_current_tabpage(review_tab)
      vim.cmd("tabclose")
      vim.api.nvim_set_current_tabpage(transcript_tab)
      vim.api.nvim_set_current_win(transcript_win)
    end)
  ]])
  wait_until(function() return child.api.nvim_get_mode().mode == "i" end, "the decision popup did not enter Insert mode")
  child.lua([[
    local now = vim.uv.now
    vim.uv.now = function() return now() + 60000 end
  ]])
  child.type_keys("Tighten the boundary", "<CR>")
  wait_until(function() return #child.lua_get("_G.submitted") == 1 end, "Enter did not submit the decision")
  eq(child.lua_get("_G.submitted"), { "Tighten the boundary" })
  eq(child.lua_get("_G.transcript_enter"), 0)
  -- The Enter is read once, in the popup's Insert mode; no later key reaches the transcript.
  local observed = child.lua_get("_G.observed")
  eq(observed[#observed], { key = "<CR>", mode = "i", filetype = "ForgePlanDecision" })
  -- Insert mode must not outlive the popup: the transcript is not editable, and a lingering
  -- Insert mode defers every presentation that waits for the user to stop typing.
  eq(child.api.nvim_get_mode(), { mode = "n", blocking = false })
end

-- Keys that arrive in one burst are already in the typeahead when the review's `oY` mapping opens
-- the popup, and the popup starts empty. The child receives the whole burst in one input call,
-- as a terminal delivers keys typed together. A key-hint plugin such as mini.clue turns `o` into
-- a trigger that reads the next key itself and feeds the completed sequence back, which the
-- case reproduces with a plain mapping so that it needs no such plugin.
T["a repeated oY typed in one burst leaves the popup empty"] = function()
  local child = MiniTest.new_child_neovim()
  child.start({ "-u", "NONE" })
  MiniTest.finally(function() pcall(child.stop) end)
  child.lua([[
    vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
    _G.opened, _G.submitted = 0, {}
    local review_win = vim.api.nvim_get_current_win()
    vim.api.nvim_buf_set_lines(0, 0, -1, true, { "pub fn requested_change();" })
    vim.bo.modifiable = false
    vim.keymap.set("n", "oY", function()
      _G.opened = _G.opened + 1
      _G.popup = require("forge.views.plan_review.decision").open({ title = "Approval comment (optional)", parent_win = review_win },
        function(comment) _G.submitted[#_G.submitted + 1] = comment end)
    end, { buffer = 0 })
    local review_buf = vim.api.nvim_get_current_buf()
    local function trigger()
      local key = vim.fn.getcharstr()
      pcall(vim.keymap.del, "n", "o", { buffer = review_buf })
      vim.api.nvim_feedkeys("o" .. key, "mit", false)
      vim.schedule(function()
        if vim.api.nvim_buf_is_valid(review_buf) then vim.keymap.set("n", "o", trigger, { buffer = review_buf, nowait = true }) end
      end)
    end
    vim.keymap.set("n", "o", trigger, { buffer = review_buf, nowait = true })
  ]])
  child.api.nvim_input("oYoY")
  wait_until(function() return child.lua_get("_G.opened") == 1 and child.api.nvim_get_mode().mode == "i"
    and child.lua_get("vim.fn.getchar(1)") == 0 end, "the decision popup did not open in Insert mode with the burst consumed")
  eq({
    text = child.lua_get("vim.api.nvim_buf_get_lines(vim.api.nvim_win_get_buf(_G.popup), 0, -1, false)"),
    opened = child.lua_get("_G.opened"),
    submitted = child.lua_get("_G.submitted"),
  }, { text = { "" }, opened = 1, submitted = {} })
end

-- The popup takes focus as soon as its key is pressed, so keys typed right behind that key, such
-- as a repeated `oY`, would land in it as comment text or submit it. It discards typed keys until
-- the keyboard has been quiet for the settle interval, each discarded key restarting it.
T["keys typed right after opening are discarded until typing stops"] = function()
  local clock = { now = 1000 }
  stub(vim.uv, "now", function() return clock.now end)
  local submitted = {}
  local parent = vim.api.nvim_get_current_win()
  local window = require("forge.views.plan_review.decision").open({ title = "Approval comment (optional)", parent_win = parent },
    function(comment) submitted[#submitted + 1] = comment end)
  local popup_buffer = vim.api.nvim_win_get_buf(window)
  local function type_keys(keys) vim.api.nvim_feedkeys(vim.keycode(keys), "xt", false) end
  local function text() return vim.api.nvim_buf_is_valid(popup_buffer) and table.concat(vim.api.nvim_buf_get_lines(popup_buffer, 0, -1, false), "\n") end
  type_keys("oY")
  clock.now = clock.now + 300
  type_keys("<CR>")
  eq({ text = text(), submitted = submitted, window = vim.api.nvim_get_current_win() }, { text = "", submitted = {}, window = window })
  clock.now = clock.now + 351
  -- A headless case leaves the popup's startinsert pending between fed keys, so the comment is
  -- typed from Normal mode either way.
  type_keys("<C-Bslash><C-n>aShip it<CR>")
  eq(submitted, { "Ship it" })
end

return T

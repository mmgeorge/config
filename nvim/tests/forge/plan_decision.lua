local function mapping_callback(buffer, mode, key)
  for _, mapping in ipairs(vim.api.nvim_buf_get_keymap(buffer, mode)) do
    if mapping.lhs == key then return mapping.callback end
  end
end

local function open_decision(title)
  local parent_win = vim.api.nvim_get_current_win()
  local received = {}
  local window = require("forge.views.plan_review.decision").open({
    title = title,
    parent_win = parent_win,
  }, function(value)
    received[#received + 1] = { value = value }
  end)
  return parent_win, window, vim.api.nvim_win_get_buf(window), received
end

local ok, failure = xpcall(function()
  local decision_keys = {
    ["<CR>"] = "submit",
    ["<C-s>"] = "submit",
    ["<Esc>"] = "cancel",
    ["<C-c>"] = "cancel",
  }

  for key, action in pairs(decision_keys) do
    for _, mode in ipairs({ "n", "i" }) do
      local parent_win, decision_win, decision_buf, received = open_decision("Decision keys")
      local normal_callback = mapping_callback(decision_buf, "n", key)
      local insert_callback = mapping_callback(decision_buf, "i", key)
      assert(normal_callback, "missing normal mode mapping for " .. key)
      assert(insert_callback, "missing insert mode mapping for " .. key)
      vim.api.nvim_buf_set_lines(decision_buf, 0, -1, false, { "review comment" })
      local decision_callback = mode == "n" and normal_callback or insert_callback
      decision_callback()
      assert(vim.api.nvim_get_current_win() == parent_win, key .. " did not restore parent focus")
      assert(#received == 1, key .. " did not complete exactly once")
      assert(received[1].value == (action == "submit" and "review comment" or nil),
        key .. " returned the wrong decision value")
      decision_callback()
      assert(#received == 1, key .. " callback completed more than once")
      assert(not vim.api.nvim_win_is_valid(decision_win), key .. " left its popup open")
    end
  end

  local parent_win, _, decision_buf, received = open_decision("Empty comment")
  mapping_callback(decision_buf, "n", "<CR>")()
  assert(#received == 1 and received[1].value == "", "empty comment was not accepted")
  assert(vim.api.nvim_get_current_win() == parent_win, "empty submission did not restore focus")

  local long_comment = string.rep("long review comment ", 2048) .. "\nsecond line\n" .. string.rep("tail", 4096)
  parent_win, _, decision_buf, received = open_decision("Long comment")
  local comment_line_list = vim.split(long_comment, "\n", { plain = true })
  vim.api.nvim_buf_set_lines(decision_buf, 0, -1, false, comment_line_list)
  mapping_callback(decision_buf, "n", "<C-s>")()
  assert(received[1].value == long_comment, "long multiline comment was not retained exactly")
  assert(vim.api.nvim_get_current_win() == parent_win, "long submission did not restore focus")

  parent_win = vim.api.nvim_get_current_win()
  local callback_count = 0
  local duplicate_callback
  local decision_win = require("forge.views.plan_review.decision").open({
    title = "Duplicate callback",
    parent_win = parent_win,
  }, function(value)
    callback_count = callback_count + 1
    assert(value == "once")
  end)
  local duplicate_buf = vim.api.nvim_win_get_buf(decision_win)
  vim.api.nvim_buf_set_lines(duplicate_buf, 0, -1, false, { "once" })
  duplicate_callback = mapping_callback(duplicate_buf, "n", "<CR>")
  duplicate_callback()
  duplicate_callback()
  assert(callback_count == 1, "duplicate callback submitted more than once")
  assert(vim.api.nvim_get_current_win() == parent_win, "duplicate callback changed restored focus")

  local surviving_win = vim.api.nvim_get_current_win()
  vim.cmd("vsplit")
  parent_win, decision_win, _, received = open_decision("Parent cancellation")
  vim.api.nvim_win_close(parent_win, true)
  assert(#received == 1 and received[1].value == nil, "closing parent did not cancel the popup")
  assert(not vim.api.nvim_win_is_valid(decision_win), "parent cancellation left its popup open")
  assert(vim.api.nvim_win_is_valid(surviving_win), "parent cancellation closed the remaining split")
end, debug.traceback)

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  print("plan_decision: passed")
  vim.cmd("qa!")
end

vim.loader.enable(false)
vim.opt.rtp:prepend(vim.fn.stdpath("data") .. "/lazy/mini.clue")
require("mini.clue").setup({ triggers = { { mode = "n", keys = "o" }, { mode = "n", keys = "g" } }, window = { delay = 200 } })
local config = require("forge.infra.config")
config.setup({})
vim.o.timeout, vim.o.timeoutlen = true, 300
local replica = require("forge.buffer")
local session = replica.open("prefix-fixture")
assert(replica.apply_snapshot(session, { document = session.document, revision = 0,
  block = { { id = "header", text = { "Unstaged changes:" }, metadata = { decoration = {}, target = {}, editable_region = {}, fold = {} } } } }).kind == "Applied")
vim.api.nvim_set_current_buf(session.buffer)
local observed = {}
local handler = {}
for _, action in ipairs({ "open", "push", "pull", "pr", "review", "close" }) do
  handler[action] = function() observed[#observed + 1] = action end
end
local owner = require("forge.document_commands").attach(session, { view = "status", handler = handler })
local steps = {}
local function step(delay, callback) steps[#steps + 1] = { delay = delay, callback = callback } end
local function sequence(keys, expected)
  step(20, function()
    observed = {}
    local index = 0
    local timer = assert(vim.uv.new_timer())
    timer:start(50, 50, function()
      index = index + 1
      vim.api.nvim_input(keys:sub(index, index))
      if index == #keys then timer:stop() timer:close() end
    end)
  end)
  step(#keys * 50 + 100, function()
    assert(vim.deep_equal(observed, { expected }), keys .. " dispatched " .. vim.inspect(observed))
  end)
end

step(20, function()
  assert(vim.fn.maparg("o", "n", false, true).desc == 'Query keys after "o"')
end)
step(20, function() vim.api.nvim_input("o") end)
step(350, function()
  local popup_text = ""
  for _, window in ipairs(vim.api.nvim_list_wins()) do
    if vim.api.nvim_win_get_config(window).relative ~= "" then
      popup_text = popup_text .. table.concat(vim.api.nvim_buf_get_lines(vim.api.nvim_win_get_buf(window), 0, -1, false), "\n")
    end
  end
  assert(popup_text:find("Start PR review", 1, true), "missing command popup: " .. popup_text)
  vim.api.nvim_input("p")
end)
step(350, function()
  local popup_text = ""
  for _, window in ipairs(vim.api.nvim_list_wins()) do
    if vim.api.nvim_win_get_config(window).relative ~= "" then
      popup_text = popup_text .. table.concat(vim.api.nvim_buf_get_lines(vim.api.nvim_win_get_buf(window), 0, -1, false), "\n")
    end
  end
  assert(popup_text:find("Pull", 1, true) and popup_text:find("Push", 1, true), "missing push/pull submenu: " .. popup_text)
  vim.api.nvim_input("<Esc>")
end)
sequence("opp", "push")
sequence("opP", "pull")
sequence("ogp", "pr")
sequence("or", "review")
step(20, function()
  owner.close()
  local rebound_handler = vim.tbl_extend("force", handler, { push = function() observed[#observed + 1] = "rebound" end })
  owner = require("forge.document_commands").attach(session, { view = "status", handler = rebound_handler })
  assert(vim.fn.maparg("o", "n", false, true).desc == 'Query keys after "o"')
end)
sequence("opp", "rebound")
step(20, function()
  owner.close()
  owner = require("forge.document_commands").attach(session, { view = "status", handler = handler,
    keymaps = { open = "g", push = "gpp" } })
end)
step(20, function()
  assert(vim.fn.maparg("g", "n", false, true).desc == 'Query keys after "g"')
end)
sequence("gpp", "push")
step(20, function()
  owner.close()
  config.setup({ keymaps = { plan_review = { open = "g", schema = "gpp" } } })
  local commands = require("forge.shared.view_command_set")
  local set = commands.new()
  commands.register(set, "open", handler.open)
  commands.register(set, "schema", handler.push)
  require("forge.shared.keymaps").setup_view_keymaps(session.buffer, "plan_review", set)
end)
step(20, function()
  assert(vim.fn.maparg("g", "n", false, true).desc == 'Query keys after "g"')
end)
sequence("gpp", "push")
local next_step = 0
local function advance()
  next_step = next_step + 1
  local item = steps[next_step]
  if not item then print("command_clue OK") vim.cmd("qa!") return end
  vim.defer_fn(function()
    local ok, failure = xpcall(item.callback, debug.traceback)
    if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") return end
    advance()
  end, item.delay)
end
advance()

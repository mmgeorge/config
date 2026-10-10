vim.loader.enable(false)
local test_path = debug.getinfo(1, "S").source:sub(2)
local runtime = vim.fs.dirname(vim.fs.dirname(vim.fs.dirname(test_path)))

if vim.g.forge_fold_marker_child then
  local replica = require("forge.buffer")
  local owner = replica.open("fold-markers")
  vim.api.nvim_set_current_buf(owner.buffer)
  local function block(id, text, marker, indent, last)
    local metadata = { target = {}, decoration = {}, editable_region = {},
      layout = { indent = indent or 2, marker = marker and { text = marker, capture = "Normal" } or nil } }
    if vim.startswith(text, "● ") or vim.startswith(text, "◇ ") or vim.startswith(text, "▸ ") then
      metadata.conceal = { { range = { start = { row = 0, column = 0 },
        ["end"] = { row = 0, column = #"▸ " } }, replacement = "", line = false, priority = 100 } }
    end
    if last then metadata.fold = { { id = id, start = { row = 0, column = 0 },
      ["end"] = { block = last, position = { row = 1, column = 0 } }, closed = false } } end
    return { id = id, text = { text }, metadata = metadata }
  end
  local exchange = block("exchange", "▸ Executing plan", "▸", 2, "tool-result")
  exchange.metadata.fold[#exchange.metadata.fold + 1] = {
    id = "exchange-inner", start = { row = 0, column = 0 },
    ["end"] = { block = "tool", position = { row = 1, column = 0 } }, closed = false,
  }
  local function deferred(id, text, marker, indent)
    local item = block(id, text, marker, indent)
    item.metadata.node = { id = id, kind = "tool_group", lifecycle = "settled", generation = 1,
      content_revision = 0, loaded_rows = 0, loaded_bytes = 0, more = false,
      order = 0, display = "heading", default_display = "heading" }
    return item
  end
  local status = block("status", "Resolving", nil)
  status.metadata.status = { row = 0, animated = true, hint = "working" }
  assert(replica.apply_snapshot(owner, { document = owner.document, revision = 0, block = {
    block("prompt", "● /execute last", "●"),
    block("event", "◇ Plan accepted", "◇"),
    exchange,
    block("tool", "• 2s cargo build"),
    block("tool-result", "Build complete"),
    block("file", "Modified game.rs", "▸", 4, "change"),
    block("hunk", "@@ +1 -1 restart", nil, 4, "change"),
    block("change", "+ reset_round();"),
    deferred("lazy-exchange", "▸ Deferred exchange", "▸", 2),
    deferred("lazy-group", "Deferred tools", "▸", 4),
    deferred("lazy-tool", "Deferred command", "•", 6),
    status,
  } }).kind == "Applied")
  local options = { margin = 0, fold_markers = true, columns = { signcolumn = "yes:1", statuscolumn = "%s" },
    conceal = { level = 3, cursor = "nvic" } }
  require("forge.input").open(owner, 0, options)
  local opened = vim.api.nvim_get_current_win()
  vim.cmd("vsplit")
  local closed = vim.api.nvim_get_current_win()
  require("forge.input").open(owner, closed, options)
  require("forge.nodes").set_open(owner, closed, "exchange", false)
  require("forge.nodes").set_open(owner, closed, "file", false)
  vim.o.showtabline, vim.o.laststatus = 0, 0
  local hint = require("forge.views.harness.status_hint")
  local commands = require("forge.shared.view_command_set").new()
  hint.render(owner, commands, 55)
  _G.marker_fixture = { owner = owner, opened = opened, closed = closed, commands = commands }
  return
end

local job = vim.fn.jobstart({ vim.v.progpath, "--embed", "--headless", "-u", "NONE" }, { rpc = true })
assert(job > 0)
local function run(source, arguments) return vim.rpcrequest(job, "nvim_exec_lua", source, arguments or {}) end
local ok, failure = xpcall(function()
  vim.rpcrequest(job, "nvim_ui_attach", 120, 24, { rgb = true })
  run("local runtime, path = ...; vim.opt.rtp:prepend(runtime); vim.g.forge_fold_marker_child = true; dofile(path)",
    { runtime, test_path })
  local function capture()
    return run([[
      vim.cmd("redraw!")
      local result = {}
      for _, name in ipairs({ "opened", "closed" }) do
        local window = marker_fixture[name]
        local position = vim.api.nvim_win_get_position(window)
        local rows = {}
        for row = position[1] + 1, position[1] + 14 do
          local cells = {}
          for column = position[2] + 1, position[2] + vim.api.nvim_win_get_width(window) do
            cells[#cells + 1] = vim.fn.screenstring(row, column)
          end
          rows[#rows + 1] = table.concat(cells)
        end
        result[name] = table.concat(rows, "\n")
      end
      return result
    ]])
  end
  local screen = capture()
  for _, name in ipairs({ "opened", "closed" }) do
    local prompt = screen[name]:match("[^\n]*/execute last[^\n]*")
    local event = screen[name]:match("[^\n]*Plan accepted[^\n]*")
    assert(vim.trim(prompt) == "● /execute last", "duplicate prompt marker: " .. prompt)
    assert(vim.trim(event) == "◇ Plan accepted", "duplicate event marker: " .. event)
    assert(screen[name]:match("[⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏]+ +Resolving"), "missing rendered spinner: " .. screen[name])
  end
  run([[
    marker_fixture.owner.status_notice = { text = "Waiting for your approval", animated = false, waiting = true }
    require("forge.views.harness.status_hint").render(marker_fixture.owner, marker_fixture.commands, 55)
  ]])
  local waiting = capture()
  for _, name in ipairs({ "opened", "closed" }) do
    local line = waiting[name]:match("[^\n]*Waiting for your approval[^\n]*")
    assert(line and vim.trim(line) == "◷ Waiting for your approval", "missing or duplicate waiting icon: " .. waiting[name])
  end
  run([[
    marker_fixture.owner.status_notice = nil
    require("forge.views.harness.status_hint").render(marker_fixture.owner, marker_fixture.commands, 55)
  ]])
  assert(screen.opened:find("◇ Plan accepted", 1, true), screen.opened)
  assert(screen.opened:find("▸ Executing plan", 1, true), screen.opened)
  assert(screen.closed:find("▸ Executing plan", 1, true), screen.closed)
  assert(screen.opened:find("▸ Modified game.rs", 1, true), screen.opened)
  assert(screen.closed:find("▸ Modified game.rs", 1, true), screen.closed)
  assert(not screen.opened:find("• 2s cargo build", 1, true), screen.opened)
  for _, name in ipairs({ "opened", "closed" }) do
    assert(screen[name]:find("▸ Deferred exchange", 1, true), screen[name])
    assert(screen[name]:find("▸ Deferred tools", 1, true), screen[name])
    assert(screen[name]:find("• Deferred command", 1, true), screen[name])
  end
  assert(not screen.opened:find("▾▾", 1, true), "coincident fold starts displayed multiple arrows")
  assert(not screen.opened:find("▾ @@", 1, true) and not screen.opened:find("▸ @@", 1, true), screen.opened)
  run([[require("forge.nodes").set_open(marker_fixture.owner, marker_fixture.opened, "exchange", true)]])
  assert(capture().opened:find("▾ Executing plan", 1, true), "opening did not change the arrow")
  run([[require("forge.nodes").set_open(marker_fixture.owner, marker_fixture.opened, "file", true)]])
  assert(capture().opened:find("▾ Executing plan", 1, true), "opening file changed the exchange arrow")
  run([[require("forge.nodes").set_open(marker_fixture.owner, marker_fixture.opened, "exchange-inner", false)]])
  local nested = capture().opened
  assert(nested:find("▸ Executing plan", 1, true), "closed inner fold did not change the row arrow")
  assert(not nested:find("▾▸", 1, true) and not nested:find("▸▸", 1, true),
    "nested fold state displayed multiple arrows")
  assert(run("return vim.api.nvim_buf_get_lines(marker_fixture.owner.buffer,0,-1,false)")[3] == "▸ Executing plan",
    "markers changed source heading text")
end, debug.traceback)
vim.fn.jobstop(job)
assert(ok, failure)
print("harness_fold_markers: shared projected arrows, prompts, events, tools, and hunks passed")
vim.cmd("qa!")


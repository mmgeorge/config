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
  assert(replica.apply_snapshot(owner, { document = owner.document, revision = 0, block = {
    block("prompt", "● /execute last", "●"),
    block("event", "◇ Plan accepted", "◇"),
    block("exchange", "▸ Executing plan", "▸", 2, "tool"),
    block("tool", "• 2s cargo build"),
    block("file", "Modified game.rs", "▸", 4, "change"),
    block("hunk", "@@ +1 -1 restart", nil, 4, "change"),
    block("change", "+ reset_round();"),
  } }).kind == "Applied")
  local options = { margin = 0, fold_markers = true, columns = { signcolumn = "yes:1", statuscolumn = "%s" },
    conceal = { level = 3, cursor = "nvic" } }
  require("forge.input").open(owner, 0, options)
  local opened = vim.api.nvim_get_current_win()
  vim.cmd("vsplit")
  local closed = vim.api.nvim_get_current_win()
  require("forge.input").open(owner, closed, options)
  require("forge.folds").set_open(owner, closed, "exchange", false)
  require("forge.folds").set_open(owner, closed, "file", false)
  vim.o.showtabline, vim.o.laststatus = 0, 0
  _G.marker_fixture = { owner = owner, opened = opened, closed = closed }
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
        for row = position[1] + 1, position[1] + 9 do
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
  assert(screen.opened:find("● /execute last", 1, true), screen.opened)
  assert(screen.opened:find("◇ Plan accepted", 1, true), screen.opened)
  assert(screen.opened:find("▾ Executing plan", 1, true), screen.opened)
  assert(screen.closed:find("▸ Executing plan", 1, true), screen.closed)
  assert(screen.opened:find("▾ Modified game.rs", 1, true), screen.opened)
  assert(screen.closed:find("▸ Modified game.rs", 1, true), screen.closed)
  assert(screen.opened:find("• 2s cargo build", 1, true), screen.opened)
  assert(not screen.opened:find("▾ @@", 1, true) and not screen.opened:find("▸ @@", 1, true), screen.opened)
  run([[vim.api.nvim_set_current_win(marker_fixture.opened); vim.api.nvim_win_set_cursor(0, {3,0}); vim.cmd("normal! zc")]])
  assert(capture().opened:find("▸ Executing plan", 1, true), "native close did not change the arrow")
  run([[vim.cmd("normal! zo")]])
  assert(capture().opened:find("▾ Executing plan", 1, true), "native open did not change the arrow")
  assert(run("return vim.api.nvim_buf_get_lines(marker_fixture.owner.buffer,0,-1,false)")[3] == "▸ Executing plan",
    "window-local markers changed shared buffer text")
end, debug.traceback)
vim.fn.jobstop(job)
assert(ok, failure)
print("harness_fold_markers: per-window arrows, prompts, events, tools, and hunks passed")
vim.cmd("qa!")


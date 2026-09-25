vim.loader.enable(false)
local replica = require("forge.buffer")
local input = require("forge.input")

local passed, failure = xpcall(function()
  for _, indent in ipairs({ 2, 4, 6 }) do
    local owner = replica.open("content-layout-" .. indent)
    vim.api.nvim_set_current_buf(owner.buffer)
    local heading = { target = {}, decoration = {}, editable_region = {},
      layout = { indent = indent, marker = { text = "▸", capture = "Normal" } },
      conceal = { { range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = #"▸ " } },
        replacement = "", line = false, priority = 100 } },
      fold = { { id = "activity", start = { row = 0, column = 0 },
        ["end"] = { block = "commentary", position = { row = 1, column = 0 } }, closed = false } },
    }
    local commentary = { target = {}, decoration = {}, editable_region = {},
      layout = { indent = indent, marker = { text = "↳", capture = "Normal" } },
    }
    assert(replica.apply_snapshot(owner, { document = owner.document, revision = 0, block = {
      { id = "summary", text = { "▸ Thinking" }, metadata = heading },
      { id = "commentary", text = { "Inspecting the implementation." }, metadata = commentary },
    } }).kind == "Applied")
    local view = input.open(owner, 0, { margin = 0,
      columns = { signcolumn = "yes:1", statuscolumn = "%s" },
      conceal = { level = 3, cursor = "nvic" } })
    assert(vim.fn.foldclosed(1) == -1, "running activity started folded")
    local closed = vim.deepcopy(heading)
    closed.fold[1].closed = true
    assert(replica.apply_patch(owner, { document = owner.document, base = 0, next = 1,
      base_rows = 2, next_rows = 2, base_blocks = 2, next_blocks = 2,
      block_edit = {}, removed_block = {}, text_edit = {},
      metadata_edit = { { block = "summary", row_count = 1, metadata = closed } },
    }).kind == "Applied")
    assert(vim.fn.foldclosed(1) == 1, "completed activity did not fold")
    local prefix = indent == 2 and "" or string.rep(" ", indent - 4) .. "▸ "
    assert(vim.fn.foldtextresult(1) == prefix .. "Thinking", "fold added another marker or indentation")
    vim.cmd("normal! zR")
    assert(vim.api.nvim_buf_get_lines(owner.buffer, 1, 2, false)[1] == "Inspecting the implementation.",
      "fold transition changed commentary source")
    input.close(view)
    replica.close(owner)
  end
end, debug.traceback)
if not passed then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
io.write("harness_content_layout: fold transitions preserve resolved indent and marker\n")
vim.cmd("qa!")

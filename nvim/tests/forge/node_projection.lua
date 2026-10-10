vim.loader.enable(false)
local buffer = require("forge.buffer")
local nodes = require("forge.nodes")
local session = buffer.open("projected-nodes", { preserve_view = true })
local function block(id, text, fold)
  return { id = id, text = text, metadata = { target = {}, decoration = {}, editable_region = {}, fold = fold or {} } }
end
local snapshot = { document = session.document, revision = 0, block = {
  block("file", { "File" }, { { id = "file", start = {row=0,column=0}, ["end"]={block="tail",position={row=1,column=0}},closed=false } }),
  block("hunk", { "@@ hunk @@" }, { { id = "hunk",start={row=0,column=0},["end"]={block="body",position={row=2,column=0}},closed=true } }),
  block("body", { "first", "second" }), block("tail", { "tail" }),
} }
local function text() return vim.api.nvim_buf_get_lines(session.buffer,0,-1,true) end
local applied = buffer.apply_snapshot(session,snapshot)
assert(applied.kind == "Applied", vim.inspect(applied))
assert(vim.deep_equal(text(), {"File","@@ hunk @@","tail"}), vim.inspect(text()))
local window=vim.api.nvim_get_current_win()
vim.api.nvim_win_set_buf(window,session.buffer)
nodes.attach(session,window)
assert(not vim.wo[window].foldenable)
vim.api.nvim_win_set_cursor(window,{2,3})
assert(nodes.toggle_heading(session,window))
assert(vim.deep_equal(text(), {"File","@@ hunk @@","first","second","tail"}),vim.inspect(text()))
assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window),{2,3}))
vim.api.nvim_win_set_cursor(window,{4,2})
assert(nodes.collapse_parent(session,window))
assert(vim.deep_equal(text(), {"File","@@ hunk @@","tail"}),vim.inspect(text()))
assert(vim.api.nvim_win_get_cursor(window)[1] == 2, vim.inspect(vim.api.nvim_win_get_cursor(window)))
assert(buffer.set_expansion(session,"hunk",true))
assert(buffer.set_expansion(session,"file",false))
assert(vim.deep_equal(text(),{"File"}))
assert(buffer.set_expansion(session,"file",true))
assert(vim.deep_equal(text(),{"File","@@ hunk @@","first","second","tail"}))
buffer.close(session)
print("node_projection: passed")

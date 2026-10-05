vim.loader.enable(false)
local editable = require("forge.editable")
local buffer = vim.api.nvim_create_buf(false, true)
local state = editable.new("local-presentation")
vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { "Heading", "Draft", "Footer" })
editable.register(state, "comment", 0)
editable.attach(state, buffer, {
  comment = { start = { row = 1, column = 0 }, finish = { row = 1, column = 5 } },
}, {})
vim.api.nvim_buf_set_text(buffer, 1, 5, 1, 5, { " λ" })
local capture = editable.capture_draft(state)
local entry = state.region.comment
assert(capture[1].text == "Draft λ")

editable.applying(state, true)
vim.api.nvim_buf_set_lines(buffer, 0, 0, false, { "Source" })
editable.reanchor(state, {
  comment = { start = { row = 2, column = 0 }, finish = { row = 2, column = #"Draft λ" } },
})
editable.applying(state, false)
assert(state.region.comment == entry, "presentation remapping must retain the draft owner")
assert(vim.deep_equal(editable.capture_draft(state), capture), "presentation must not change the explicit capture")
vim.api.nvim_buf_set_text(buffer, 2, #"Draft λ", 2, #"Draft λ", { " new" })
assert(editable.capture_draft(state)[1].text == "Draft λ new", "typing must use the remapped physical body")
assert(capture[1].text == "Draft λ", "later typing must not mutate an earlier capture")

editable.applying(state, true)
editable.reanchor(state, {})
editable.applying(state, false)
assert(state.region.comment.pending, "compact comments must retain their pending bodies")
assert(editable.capture_draft(state)[1].text == "Draft λ new")
editable.detach(state)
vim.api.nvim_buf_delete(buffer, { force = true })
print("editable_presentation: local row remapping and collapse retain exact draft captures")

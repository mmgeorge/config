vim.opt.runtimepath:append("nvim")
local review = require("forge.review_document")
local editable = require("forge.editable")
local closed = 0
review._set_runner_for_test(function(method, params, callback)
  if method == "review.open_pr" then callback({ document = "uncached-review" })
  elseif method == "review.header" or method == "review.load" or method == "review.view" then callback({})
  elseif method == "review.materialize" then
    callback({ snapshot = { document = params.document, revision = 0, block = {
      { id = "region:body", text = { "initial" }, metadata = {
        target = {}, decoration = {}, editable_region = {
          { id = "body", revision = 0, range = { start = { row = 0, column = 0 },
            ["end"] = { row = 0, column = 7 } } },
        },
      } },
    } } })
  elseif method == "review.close" then closed = closed + 1 callback({})
  else error("unexpected request " .. method) end
end)
local origin = vim.api.nvim_get_current_buf()
local state = review.open({ directory = vim.fn.getcwd(), target = { number = 7 }, on_error = error })
assert(vim.wait(1000, function() return state.shown and not state.rendering end))
assert(state.cache_key)
local retained_buffer = state.replica.buffer
vim.bo[retained_buffer].modifiable = true
vim.api.nvim_buf_set_text(retained_buffer, 0, 0, 0, 7, { "retained", "λ\r", "" })
local before = editable.capture_draft(state.replica.editable)
assert(before[1].text == "retained\nλ\r\n")
review.close(state)
assert(state.active and state.hidden and not state.closing)
assert(vim.api.nvim_get_current_buf() == origin)
assert(vim.api.nvim_buf_is_loaded(retained_buffer) and closed == 0)
assert(vim.deep_equal(editable.capture_draft(state.replica.editable), before))
local reopened = review.open({ directory = vim.fn.getcwd(), target = { number = 7 }, on_error = error })
assert(reopened == state and reopened.replica.buffer == retained_buffer)
assert(vim.deep_equal(editable.capture_draft(state.replica.editable), before))
assert(closed == 0)
vim.api.nvim_buf_delete(retained_buffer, { force = true })
assert(vim.wait(1000, function() return closed == 1 end))
review._set_runner_for_test(nil)
print("review_uncached_close: dirty close retains exact draft text without saving or collecting")

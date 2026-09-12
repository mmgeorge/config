vim.loader.enable(false)
local client = require("forge.client")
local original_request, original_accepting = client.request_for, client.host_accepting
local original_generation = client.host_generation
local generation = 1
client.host_generation = function() return generation end
client.host_accepting = function() return true end
local requests = {}
client.request_for = function(_, method, params, callback)
  assert(method == "harness.document")
  requests[#requests + 1] = { params = params, callback = callback }
end
local path = vim.fn.tempname() .. ".md"
vim.fn.writefile({ "# Native plan", "", "Task" }, path)
vim.cmd("edit " .. vim.fn.fnameescape(path))
local native_buffer = vim.api.nvim_get_current_buf()
vim.wo.number = false
vim.wo.relativenumber = true
vim.wo.signcolumn = "yes"
vim.wo.statuscolumn = "%l "
vim.wo.conceallevel = 2
vim.wo.concealcursor = "nc"
vim.wo.breakindent = true
vim.wo.breakindentopt = "shift:0"
local owner, attached
local success, failure = xpcall(function()
  owner = require("forge.views.plan_review.document").attach({ session_id = "session", buffer = native_buffer,
    window = vim.api.nvim_get_current_win(), plan = { id = "plan", review_digest = "canonical" },
  }, function(value, error_message) assert(not error_message, error_message) attached = value end)
  local open = requests[1].params
  requests[1].callback({ path = path, version = 1, saved_source_digest = "saved",
    snapshot = { document = open.document, revision = 0, block = { { id = "plan:source",
      text = { "# Native plan", "", "Task" }, metadata = { decoration = {}, editable_region = {}, target = {
        { id = "plan:source:1", range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = 13 } } },
      } } } } } })
  assert(attached == owner and owner.saved_source_digest == "saved")
  assert(vim.wo.number and not vim.wo.relativenumber and vim.wo.signcolumn == "yes"
    and vim.wo.statuscolumn == vim.go.statuscolumn, "plan review did not restore absolute source columns")
  assert(vim.wo.conceallevel == 3 and vim.wo.concealcursor == "", "plan review did not apply Markdown concealment")
  assert(vim.wo.breakindent and vim.wo.breakindentopt == "shift:0", "plan review lost source continuation indentation")
  local alternate = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_win_set_buf(0, alternate)
  vim.wo.breakindent = false
  vim.wo.breakindentopt = "shift:2"
  vim.api.nvim_win_set_buf(0, native_buffer)
  assert(vim.wo.breakindent and vim.wo.breakindentopt == "shift:0", "plan review lost indentation on re-entry")
  vim.api.nvim_win_set_buf(0, alternate)
  assert(not vim.wo.breakindent and vim.wo.breakindentopt == "shift:2", "plan review leaked indentation on release")
  vim.api.nvim_win_set_buf(0, native_buffer)
  assert(vim.bo[native_buffer].modifiable and vim.fs.normalize(vim.api.nvim_buf_get_name(native_buffer)) == vim.fs.normalize(path))
  assert(vim.deep_equal(vim.fn.readfile(path), { "# Native plan", "", "Task" }), "projection overwrote its physical source")
  local selected
  owner.action("open", function(value) selected = value end)
  local action = requests[#requests]
  assert(action.params.input.target == "plan:source:1")
  assert(owner.is_current(action.params.input))
  vim.api.nvim_buf_set_text(native_buffer, 0, 0, 0, 0, { "new " })
  assert(not owner.is_current(action.params.input), "follow-up effect retained authority after newer physical typing")
  action.callback({ json_path = "/title" })
  assert(selected == nil, "late source action replaced newer physical typing")
  assert(owner.close() == false, "unacknowledged physical typing was discarded by close")
  generation = 2
  assert(owner.close())
  assert(vim.api.nvim_buf_is_valid(native_buffer) and vim.api.nvim_buf_get_lines(native_buffer, 0, 1, false)[1] == "new # Native plan")
end, debug.traceback)
if owner then owner.close() end
client.request_for, client.host_accepting = original_request, original_accepting
client.host_generation = original_generation
vim.fn.delete(path)
assert(success, failure)
print("plan_review_document: passed")

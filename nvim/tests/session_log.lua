vim.opt.runtimepath:prepend("nvim")
local directory = vim.g.session_log_test_directory or vim.fn.tempname()
vim.fn.mkdir(directory, "p")
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind)
  if kind == "log" then return directory end
  return original_stdpath(kind)
end
local delivered
vim.notify = function(message, level, options)
  delivered = { message = message, level = level, options = options }
  return "forwarded"
end
local logger = require("session_log")
local path = assert(logger.setup())
assert(logger.setup() == path)
local message = string.rep("complete diagnostic ", 4096)
local options = { title = "Diagnostic test" }
assert(vim.notify(message, vim.log.levels.ERROR, options) == "forwarded")
assert(delivered.message == message and delivered.options == options)
vim.v.errmsg = "synthetic editor failure"
assert(vim.wait(2000, function()
  for _, line in ipairs(vim.fn.readfile(path)) do
    local record = vim.json.decode(line)
    if record.event == "editor.error" then return record.fields.message == vim.v.errmsg end
  end
end, 20))
local notification_found = false
for _, line in ipairs(vim.fn.readfile(path)) do
  local record = vim.json.decode(line)
  if record.event == "notification.error" then
    assert(record.fields.message == message, "error content was truncated")
    notification_found = true
  end
end
assert(notification_found)
assert(#vim.fn.glob(directory .. "/sessions/*", false, true) == 1)
local original_writefile = vim.fn.writefile
vim.fn.writefile = function() error("simulated diagnostic write failure") end
assert(vim.notify("failure remains visible", vim.log.levels.ERROR) == "forwarded")
assert(delivered.message == "failure remains visible", "logging failure swallowed the notification")
vim.fn.writefile = original_writefile
vim.wait(20, function() return false end)
print("session_log: passed " .. path)
vim.cmd("qa!")

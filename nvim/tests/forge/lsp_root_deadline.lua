local startup = require("forge.lsp.startup")
local directory = vim.fn.tempname()
for _, name in ipairs({ "one", "two", "three" }) do
  vim.fn.mkdir(directory .. "/" .. name, "p")
  vim.fn.writefile({ "return {}" }, directory .. "/" .. name .. "/main.lua")
end
local original = { configs = vim.lsp.get_configs, start = vim.lsp.start, clients = vim.lsp.get_clients,
  client = vim.lsp.get_client_by_id, attached = vim.lsp.buf_is_attached, now = vim.uv.now, notify = vim.notify }
local callbacks, clients, notices = {}, {}, {}
local elapsed, launches, respond = 0, 0, false
vim.uv.now = function() return original.now() + elapsed end
vim.notify = function(message) notices[#notices + 1] = message end
vim.lsp.get_configs = function()
  return { { name = "root-deadline", filetypes = { "lua" }, root_dir = function(_, receive)
    callbacks[#callbacks + 1] = receive
    if respond then vim.schedule(function() receive(directory) end) end
  end } }
end
vim.lsp.get_clients = function() return clients end
vim.lsp.get_client_by_id = function(id) return clients[id] end
vim.lsp.buf_is_attached = function() return false end
vim.lsp.start = function(config)
  launches = launches + 1
  assert(launches == 1, "concurrent roots started duplicate servers")
  clients[1] = { id = 1, name = config.name, root_dir = directory, config = config, initialized = false,
    is_stopped = function() return false end }
  return 1
end
local success, failure = xpcall(function()
  local state = startup.start(directory)
  assert(vim.wait(500, function() return #callbacks == 3 end, 5), "root discovery serialized unanswered callbacks")
  elapsed = 2001
  assert(vim.wait(500, function() return not state.discovering end, 5), "root discovery did not stop after two seconds")
  for _, job in pairs(state.jobs) do assert(job.done and job.rejected and not job.failure) end
  assert(#notices == 0, "a skipped root was reported as a server failure")
  for _, receive in ipairs(callbacks) do receive(directory) end
  vim.wait(50, function() return launches > 0 end, 5)
  assert(launches == 0, "late root callback launched a server")
  startup.close()

  elapsed, respond, callbacks = 0, true, {}
  state = startup.start(directory)
  assert(vim.wait(500, function() return not state.discovering end, 5), "valid roots did not finish discovery")
  assert(launches == 1)
  elapsed = 2500
  vim.wait(100, function() return #notices > 0 end, 5)
  assert(#notices == 0, "root timeout was applied to server initialization")
  elapsed = 60001
  assert(vim.wait(500, function() return #notices > 0 end, 5), "server initialization lost its longer deadline")
  assert(#notices == 1 and notices[1]:find("initialization exceeded 60 seconds", 1, true))
end, debug.traceback)
startup.close()
vim.lsp.get_configs, vim.lsp.start, vim.lsp.get_clients = original.configs, original.start, original.clients
vim.lsp.get_client_by_id, vim.lsp.buf_is_attached = original.client, original.attached
vim.uv.now, vim.notify = original.now, original.notify
vim.fn.delete(directory, "rf")
assert(success, failure)
print("lsp_root_deadline: passed")

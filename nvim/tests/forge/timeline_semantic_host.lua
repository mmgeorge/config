vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
vim.loader.enable(false)
local origin = vim.fn.getcwd()
local root = vim.fn.tempname()
local workspace, data = root .. "/repository", root .. "/data"
assert(vim.fn.mkdir(workspace, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
local executable = vim.fn.fnamemodify(require("forge.builder").binary_path(), ":p")
assert(vim.fn.executable(executable) == 1, "build the Forge host before this integration test")
local stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return (kind == "data" or kind == "config") and data or stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  callback({ ok = true, path = executable })
  return function() end
end }
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local session_id, owner, controller
local pending_request = 0
local notices = {}
local function wait(predicate, description)
  assert(vim.wait(15000, function() return #notices > 0 or predicate() end, 10), description)
  assert(#notices == 0, table.concat(notices, "\n"))
end
local function request(method, params)
  local done, result, failure
  client.request({ method = method, session_id = session_id, params = params or {} }, function(value, problem)
    done, result, failure = true, value, problem
  end)
  wait(function() return done end, method .. " did not settle")
  assert(not failure, failure)
  return result
end
local function git(arguments)
  local command = { "git", "-C", workspace }
  vim.list_extend(command, arguments)
  local result = vim.system(command, { text = true, timeout = 10000 }):wait()
  assert(result.code == 0, result.stderr)
end
local function source(public, private)
  assert(vim.fn.writefile({ "pub fn visible() {", "    " .. public .. "();", "}",
    "fn hidden() { " .. private .. "(); }" }, workspace .. "/source.rs") == 0)
end
local function rows()
  return table.concat(vim.api.nvim_buf_get_lines(owner.transcript.buffer, 0, -1, false), "\n")
end
local function settled()
  wait(function() return owner.ready and not owner.pending and not owner.syncing and not owner.applying
    and not owner.diff_inflight and not owner.highlighting and pending_request == 0 end, "timeline did not settle")
  assert(owner.transcript.status == "Applied", owner.transcript.failure)
end
local function invoke(key)
  local mapping = vim.fn.maparg(key, "n", false, true)
  assert(mapping.buffer == 1 and mapping.callback, "missing timeline mapping " .. key)
  mapping.callback()
  settled()
end
local function select(kind, text)
  for row, line in ipairs(vim.api.nvim_buf_get_lines(owner.transcript.buffer, 0, -1, false)) do
    local location = require("forge.buffer").locate(owner.transcript, row - 1, 0)
    local node = location and owner.transcript.sequence.node[location.block].entry.metadata.node
    if node and node.kind == kind and (not text or line:find(text, 1, true)) then
      vim.api.nvim_win_set_cursor(0, { row, 0 })
      return
    end
  end
  error("missing timeline " .. kind .. " heading " .. tostring(text))
end
local success, failure = xpcall(function()
  git({ "init", "-q" })
  git({ "config", "user.name", "Timeline fixture" })
  git({ "config", "user.email", "timeline@example.invalid" })
  git({ "config", "core.autocrlf", "false" })
  source("previous", "old_private")
  git({ "add", "source.rs" })
  git({ "commit", "-qm", "fixture" })
  vim.fn.chdir(workspace)
  require("forge").setup({ harness = { backend = "mock", backends = { mock = { command = { "writing-blocking" } } } } })
  local initialized, initialization_failure
  client.session_initialize(function(value, problem) initialized, initialization_failure = value, problem end)
  wait(function() return initialized or initialization_failure end, "Harness initialization did not settle")
  assert(not initialization_failure, initialization_failure)
  session_id = initialized.session.id
  local completed, prompt_failure
  client.request({ method = "session.prompt.submit", session_id = session_id, params = { text = "Capture timeline changes" } },
    function(_, problem) completed, prompt_failure = true, problem end)
  wait(function() return vim.fn.filereadable(workspace .. "/mock-provider-change.txt") == 1 end, "mock provider did not start")
  source("updated", "new_private")
  request("session.turn.cancel")
  wait(function() return completed end, "captured exchange did not finalize")
  assert(prompt_failure and prompt_failure:find("cancel", 1, true), prompt_failure)
  local history = request("session.exchange.list")
  assert(#history == 1 and history[1].checkpoint_before and history[1].checkpoint_after)
  source("later_worktree", "later_private")
  local transcript = vim.api.nvim_create_buf(false, true)
  local composer = vim.api.nvim_create_buf(false, true)
  local window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(window, transcript)
  owner = require("forge.views.harness.presentation").open({ session_id = session_id,
    transcript_buffer = transcript, composer_buffer = composer, transcript_window = window, read_only = true,
    is_alive = function() return true end, notice = function(message) notices[#notices + 1] = message end,
    on_update = function() if controller then controller.refresh_winbar() end end,
    request = function(params, callback)
      pending_request = pending_request + 1
      client.request({ method = "session.document", session_id = session_id, params = params }, function(...)
        pending_request = pending_request - 1
        callback(...)
      end)
    end,
  }, function(value, problem) assert(value and not problem, problem) end)
  settled()
  local state = require("forge.session").harness
  state.presentation, state.transcript_win = owner, window
  controller = require("forge.views.harness.controller")
  controller.attach_transcript(transcript)
  controller.refresh_winbar()
  assert(owner.diff_view.presentation == "full" and owner.diff_view.visibility == "all")
  select("changes", "Workspace changes")
  invoke("<Tab>")
  select("file", "source.rs")
  invoke("<Tab>")
  assert(rows():find("updated();", 1, true) and rows():find("new_private();", 1, true))
  assert(not rows():find("later_worktree", 1, true), "timeline read mutable worktree bytes")
  invoke("ox")
  assert(owner.diff_view.presentation == "semantic")
  assert(rows():find("1 change", 1, true) and not rows():find("updated();", 1, true))
  invoke("ov")
  assert(owner.diff_view.visibility == "public" and not rows():find("hidden", 1, true))
  select("group", "pub fn visible()")
  invoke("<Tab>")
  assert(rows():find("updated();", 1, true) and not rows():find("later_worktree", 1, true))
  invoke("ox")
  assert(owner.diff_view.presentation == "full" and owner.diff_view.visibility == "public")
  assert(rows():find("updated();", 1, true) and not rows():find("hidden", 1, true))
  invoke("ov")
  assert(owner.diff_view.visibility == "all" and rows():find("new_private();", 1, true))
  local snapshot = request("session.document", { operation = "snapshot", document = owner.document })
  assert(snapshot.document == owner.document and snapshot.revision == owner.transcript.revision)
  assert(snapshot.diff_view.presentation == "full" and snapshot.diff_view.visibility == "all")
end, debug.traceback)
if owner then owner.close() end
client.host_stop()
assert(vim.wait(10000, function() return client._client.process == nil end, 10), "timeline host was not collected")
vim.fn.chdir(origin)
vim.fn.stdpath = stdpath
assert(vim.fs.normalize(root) ~= vim.fs.normalize(origin) and workspace == root .. "/repository")
vim.fn.delete(root, "rf")
assert(success, failure)
print("timeline_semantic_host: captured sources, native controls, semantic summaries, unfolding and recovery passed")
vim.cmd("qa!")

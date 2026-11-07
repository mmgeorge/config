vim.loader.enable(false)
local root = vim.fn.getcwd()
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
local executable = data .. "/forge" .. (vim.fn.has("win32") == 1 and ".exe" or "")
-- Launch the shared pinned executable lease rather than a freshly scanned per-run copy.
executable = dofile(vim.fs.dirname(debug.getinfo(1, "S").source:sub(2)) .. "/support/host_executable.lua")()
local original_stdpath, original_notify = vim.fn.stdpath, vim.notify
vim.fn.stdpath = function(kind) return kind == "data" and data or original_stdpath(kind) end
local errors = {}
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then errors[#errors + 1] = tostring(message) end
end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = executable }) end)
  return function() end
end }
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local picker = require("forge.views.picker")
local approval = dofile(vim.fs.dirname(debug.getinfo(1, "S").source:sub(2)) .. "/support/approval.lua")
local responder = approval.responder()
local function await(predicate, message)
  -- First planning configures checks, whose definition edit waits for an approval answer.
  assert(vim.wait(10000, function()
    approval.answer_pending(responder)
    return #errors > 0 or predicate()
  end, 10), message)
  assert(#errors == 0, table.concat(errors, "\n"))
end
local function invoke(key)
  local mapping = vim.fn.maparg(key, "n", false, true)
  assert(mapping.callback, "missing binding " .. key)
  mapping.callback()
end
local function submit(text)
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { text })
  controller.submit()
end
local success, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false, harness = { provider = "mock" } })
  require("forge.views.harness").open()
  await(function() return state.presentation and state.presentation.ready end, "Harness did not open")
  submit("/plan build the feature")
  await(function() return not require("forge.views.harness.activity").busy(state) and state.active_plan and state.active_plan.model_revision == 1 end, "initial plan missing")
  local source = vim.deepcopy(state.active_plan)
  submit("Why did you choose this design?")
  -- The plan exchange can still be finalizing when the question is submitted, so poll durable
  -- history instead of treating an idle presentation as the discussion's completion.
  local history, discussion, listing
  local function find_discussion()
    if not listing then
      listing = true
      client.request({ method = "session.exchange.list", params = {}, session_id = assert(require("forge.session").harness.session).id },
        function(value, failure) assert(not failure, failure) history, listing = value, false end)
    end
    discussion = history and vim.iter(history):find(function(exchange)
      return exchange.prompt == "Why did you choose this design?" and exchange.state == "complete"
    end)
    return discussion ~= nil and not require("forge.views.harness.activity").busy(state)
  end
  local settled = vim.wait(10000, function() return #errors > 0 or find_discussion() end, 20)
  assert(#errors == 0, table.concat(errors, "\n"))
  assert(settled, "discussion exchange missing: " .. vim.inspect(vim.tbl_map(function(exchange)
    return { prompt = exchange.prompt:sub(1, 60), state = exchange.state, plan_id = exchange.plan_id }
  end, history or {})))
  assert(require("forge.views.harness.task").current(state).kind == "plan", "discussion lost its planning task")
  assert(state.active_plan.id == source.id and state.active_plan.model_revision == source.model_revision,
    "discussion changed the submitted plan")
  -- Exchanges no longer record plan ownership; the task assertions above carry that association.
  assert(discussion.origin.kind == "user" and discussion.check_setup == nil,
    "discussion was not recorded as an ordinary user exchange: " .. vim.inspect(discussion.origin))
  local revision_path = vim.fs.joinpath(vim.fs.dirname(source.working_path), "revisions", "submitted-0001.json")
  local original = vim.fn.readfile(revision_path, "b")
  local response
  client.request({ session_id = state.session.id, method = "session.plan.changes.request", params = { plan_id = source.id, comment = "Refine the description" } }, function(result, request_error)
    assert(not request_error, request_error)
    response = result
  end)
  await(function() return response and state.active_plan.model_revision == 2 end, "second revision missing")
  -- Session status renders in the status split above the composer, not in the transcript.
  await(function()
    local status_buffer = state.presentation.status_view.buffer
    return table.concat(vim.api.nvim_buf_get_lines(status_buffer, 0, -1, false), "\n")
      :find("Waiting for plan review", 1, true) ~= nil
  end, "status split did not show the revised plan review")
  vim.api.nvim_set_current_win(state.transcript_win)
  invoke("or")
  await(function() return not state.aborting_plan and not state.state_sync_pending
    and not state.active_plan and not state.session.active_task end, "abort did not clear the planning task")
  assert(not state.active_elicitation, "abort retained input status")
  await(function()
    for _, rendered in ipairs({ state.transcript_buf, state.presentation.status_view.buffer }) do
      if table.concat(vim.api.nvim_buf_get_lines(rendered, 0, -1, false), "\n"):find("Waiting for plan review", 1, true) then
        return false
      end
    end
    return true
  end, "abort retained the rendered review status")
  submit("/plan")
  await(function() return picker.is_open() end, "replan picker missing")
  local choice = picker._state_for_test().spec.page_list[1].option_list[1]
  assert(choice.value.id == source.id and choice.detail:find("cancelled", 1, true))
  invoke("<CR>")
  local actions = picker._state_for_test().spec
  local fork = vim.iter(actions.page_list[1].option_list):find(function(option) return option.label == "Fork plan" end)
  assert(fork)
  picker.close(false)
  actions.on_confirm({ option = fork })
  await(function() return not require("forge.views.harness.activity").busy(state) and state.active_plan and state.active_plan.id ~= source.id
    and state.active_plan.model_revision == 1 end, "replan did not create a new planning exchange")
  assert(require("forge.views.harness.task").current(state).kind == "plan")
  assert(vim.deep_equal(vim.fn.readfile(revision_path, "b"), original), "replan modified the source revision")
  controller.abort_plan()
  await(function() return not state.aborting_plan and not state.active_plan end, "replanned plan did not abort")
  vim.api.nvim_set_current_win(state.transcript_win)
  await(function()
    if state.presentation then invoke("q") end
    return state.presentation == nil
  end, "Harness did not exit")
end, debug.traceback)
picker.close(false)
client.host_stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(root)
vim.fn.stdpath, vim.notify = original_stdpath, original_notify
vim.fn.delete(workspace, "rf")
vim.fn.delete(data, "rf")
assert(success and collected, failure or "Harness host was not collected")
print("harness_replan_host: passed")

vim.loader.enable(false)

local opened
package.loaded["forge.client"] = {
  request = function(request, callback)
    assert(request.session_id == "session" and request.method == "session.provider.capabilities.get")
    callback(vim.json.decode([[{
      "provider":"fixture","label":"Fixture","revision":2,"effective_revision":7,
      "provider_version":"1.2.3","notice":[],"task_steering":[],"capability":[
        {"id":"prompt","label":"Prompts","required":true,"implementation":"provider","support":"supported","application":"immediate","reason":"Ordinary input"},
        {"id":"native_fork","label":"Fork","required":false,"implementation":"provider","support":"unsupported","application":"settled","reason":"Unavailable"},
        {"id":"native_steer","label":"Steer","required":false,"implementation":"provider","support":{"limited":{"restrictions":["settled_only",{"task_kinds":["goal"]},"permission_callbacks_only","checkpoint_required","unstructured_diffs","enterprise_terminal_policy","inherited_skills"]}},"application":"settled","reason":"Negotiated restriction"}
      ]
    }]]))
  end,
}
package.loaded["forge.infra.notifications"] = { error = function(message) error(message) end }
package.loaded["forge.views.picker"] = { open = function(options) opened = options end }

local ok, failure = xpcall(function()
  require("forge.views.harness.capability_picker").open({}, "session")
  local rows = opened.page_list[1].option_list
  assert(rows[1].id == "revision" and rows[1].detail == "Schema 2 · Effective 7")
  assert(rows[2].id == "version" and rows[2].detail == "1.2.3")
  assert(rows[3].detail == "supported · provider · immediate · Ordinary input")
  assert(rows[4].detail == "unsupported · provider · settled · Unavailable")
  for _, label in ipairs({ "settled invocation required", "tasks: goal", "permission callbacks only", "checkpoint required",
    "native edits may omit structured diffs", "enterprise terminal policy may bypass approval callbacks", "inherited native skills remain available" }) do
    assert(rows[5].detail:find(label, 1, true), "missing serialized restriction: " .. label)
  end
  for _, row in ipairs(rows) do assert(row.id ~= "availability", "removed availability state remained visible") end
end, debug.traceback)
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") else vim.cmd("qa!") end

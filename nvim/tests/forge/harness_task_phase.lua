vim.loader.enable(false)
local task = require("forge.views.harness.task")

for _, workflow in ipairs({
  { kind = "plan", label = "plan", phases = { "draft", "review" } },
  { kind = "execute", label = "exec", phases = { "implement", "check", "resolve", "verify" } },
  { kind = "goal", label = "goal", phases = { "work" } },
}) do
  for _, phase in ipairs(workflow.phases) do
    for _, status in ipairs({ "running", "waiting", "paused", "blocked", "failed" }) do
      local current = { kind = workflow.kind, phase = phase, status = status }
      local expected = workflow.label .. " · " .. phase
      if status ~= "running" and status ~= "waiting" then expected = expected .. " · " .. status end
      assert(task.phase(current) == phase and task.status(current) == status)
      assert(task.label(current) == expected, vim.inspect(current))
    end
  end
end

local native = { kind = "execute", phase = "check", status = "paused",
  execution = { record = { phase = "implement", state = "active" } } }
assert(task.label(native) == "exec · check · paused", "Lua inferred lifecycle from an artifact snapshot")
print("harness_task_phase: passed")

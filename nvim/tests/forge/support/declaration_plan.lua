-- Drive a `/plan` request that revises existing declaration files through the real plan tools.
--
-- The unscripted mock planner only adds `src/change.rs`, so a workspace that already contains
-- that file never reaches review through it. Plan tools also need the plan's ID and shared
-- version, which exist only after Harness admits the `/plan` request. This helper therefore
-- scripts a first Plan turn that ends without submitting, holds it until the plan exists, and
-- queues the revision for the continuation turn that Harness issues afterwards. The
-- continuation edits the overview, replaces each source file's declarations, rewrites each
-- configuration file, and calls `plan_submit`.
local support_directory = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p:h")
local mock = dofile(support_directory .. "/mock_provider.lua")
local M = {}

---@class ForgeDeclarationPlanSource
---@field path string Workspace-relative source file whose declarations the plan replaces.
---@field declaration string[] Proposed declaration lines, including behavior sections.
---@class ForgeDeclarationPlanConfiguration
---@field path string Workspace-relative configuration file.
---@field baseline string Complete current file contents.
---@field proposed string Complete proposed file contents.

---@class ForgeDeclarationPlanOptions
---@field request string Text submitted after `/plan`.
---@field overview table plan_overview_edit fields other than plan_id and expected_version.
---@field source ForgeDeclarationPlanSource[]
---@field configuration? ForgeDeclarationPlanConfiguration[]
---@field timeout? integer Milliseconds to wait for each host response; defaults to 10000.

local function request(session_id, method, params, timeout)
  local completed, result, failure = false, nil, nil
  require("forge.client").request({ session_id = session_id, method = method, params = params },
    function(response, diagnostic) result, failure, completed = response, diagnostic, true end)
  assert(vim.wait(timeout or 10000, function() return completed end, 10), method .. " did not complete")
  assert(not failure, method .. ": " .. tostring(failure))
  return result
end

local function script(session_id, steps, timeout)
  request(session_id, "session.provider.mock_response",
    { operation = "script", script = { steps = steps, terminal = "completed" } }, timeout)
end

---Read the shared version from the first fenced declaration design that names `plan_id`.
---@param text string
---@param plan_id string
---@return integer?
local function declaration_version(text, plan_id)
  for block in text:gmatch("```json\n(.-)\n```") do
    local decoded_ok, document = pcall(vim.json.decode, block)
    if decoded_ok and type(document) == "table" and document.plan_id == plan_id then
      return tonumber(document.version)
    end
  end
end

local function lines(text)
  return vim.split((text:gsub("\n$", "")), "\n", { plain = true })
end

---Submit the plan request and queue the revising continuation. The caller awaits review.
---@param options ForgeDeclarationPlanOptions
---@return string plan_id
function M.submit(options)
  local state = require("forge.session").harness
  local session_id = assert(state.session, "Harness has no session").id
  script(session_id, { { kind = "message", content = "response", text = "Inspecting the declarations." } }, options.timeout)
  local hold = mock.hold(session_id, "plan")
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "/plan " .. options.request })
  require("forge.views.harness.controller").submit()
  local held = hold:wait()
  local task = request(session_id, "session.task.get", vim.empty_dict(), options.timeout)
  local plan = assert(type(task) == "table" and task.plan and task.plan.artifact, "held Plan turn has no plan artifact")
  local plan_id = assert(plan.id, "held Plan turn has no plan ID")
  local version = declaration_version(held.prompt, plan_id)
    or (task.entry and task.entry.instruction and declaration_version(task.entry.instruction, plan_id))
  assert(version, "held Plan turn carries no declaration design for " .. plan_id)
  local overview = vim.tbl_extend("force", vim.deepcopy(options.overview), { plan_id = plan_id, expected_version = version })
  local steps = { { kind = "tool_call", name = "plan_overview_edit", arguments = overview } }
  -- A Delete captures each source baseline without restating its current declarations, and
  -- the following Add proposes the replacement under that baseline.
  local removal = { "*** Begin Patch" }
  for _, source in ipairs(options.source) do removal[#removal + 1] = "*** Delete File: " .. source.path end
  for _, configuration in ipairs(options.configuration or {}) do
    removal[#removal + 1] = "*** Update File: " .. configuration.path
    removal[#removal + 1] = "@@"
    for _, line in ipairs(lines(configuration.baseline)) do removal[#removal + 1] = "-" .. line end
    for _, line in ipairs(lines(configuration.proposed)) do removal[#removal + 1] = "+" .. line end
  end
  removal[#removal + 1] = "*** End Patch"
  local addition = { "*** Begin Patch" }
  for _, source in ipairs(options.source) do
    addition[#addition + 1] = "*** Add File: " .. source.path
    for _, line in ipairs(source.declaration) do addition[#addition + 1] = "+" .. line end
  end
  addition[#addition + 1] = "*** End Patch"
  steps[#steps + 1] = { kind = "tool_call", name = "plan_file_edit",
    arguments = { plan_id = plan_id, expected_version = version + 1, patch = table.concat(removal, "\n") } }
  steps[#steps + 1] = { kind = "tool_call", name = "plan_file_edit",
    arguments = { plan_id = plan_id, expected_version = version + 2, patch = table.concat(addition, "\n") } }
  steps[#steps + 1] = { kind = "tool_call", name = "plan_submit",
    arguments = { plan_id = plan_id, expected_version = version + 3 } }
  script(session_id, steps, options.timeout)
  hold:release()
  return plan_id
end

return M

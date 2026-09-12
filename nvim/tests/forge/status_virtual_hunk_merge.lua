local fixture = dofile(vim.fn.getcwd() .. "/nvim/tests/forge/support/status_fixture.lua")
vim.loader.enable(false)

local forge = require("forge")
local status = require("forge.status")

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function metadata(target)
  return {
    target = {},
    decoration = {}, editable_region = {}, visible_decoration = {}, fold = {}, gutter = {},
  }
end

local function merged_hunk_body(document)
  local text = {
          "@@ -57,26 +67,26 @@",
          "  fn compact_neighbor(context: &gpu::Context) -> Self {",
          "    let bind_group = context.create_bind_group(shaders::model::render::BindGroup1Descriptor {",
          "      color_texture: color.clone(),",
          "      normal_texture: normal.clone(),",
          "      roughness_metallic_texture: metallic_roughness.clone(),",
          "    });",
          "    let particle_bind_group =",
          "      context.create_bind_group(shaders::model::particle_render::BindGroup1Descriptor {",
          "        color_texture: color,",
          "      });",
          "    // TODO: Initial size is not correct",
          "    Self {",
          "      particle_bind_group,",
          "    }",
        }
  return fixture.body(document, text)
end

local function line_count(lines, needle)
  local count = 0
  for _, line in ipairs(lines) do
    if line:find(needle, 1, true) then count = count + 1 end
  end
  return count
end

local requested = {}
status._set_runner_for_test(function(method, params, callback)
  assert_true(method == "status", "Status hunk fixture must use the native route")
  requested[#requested + 1] = params.operation
  if params.operation == "open" then
    callback(fixture.snapshot(params.document, { path = "src/compact_neighbor.rs" }))
  elseif params.operation == "demand" then
    callback(merged_hunk_body(params.input.document))
  elseif params.operation == "close_view" then
    callback(vim.NIL)
  elseif params.operation == "close" then
    callback({ closed = true })
  else
    error("unexpected native Status operation: " .. params.operation)
  end
end)

local ok, failure = xpcall(function()
  forge.setup({ about_auto_generate = false })
  local state = forge.open()
  assert_true(vim.wait(2000, function() return state.replica.status == "Applied" end, 10),
    "native Status did not apply the merged hunk snapshot")
  vim.api.nvim_win_set_cursor(0, { 4, 0 })
  vim.cmd("normal! za")
  status.demand(state)
  assert(vim.wait(2000, function() return state.replica.file[1].body ~= nil end))
  local lines = vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, false)
  assert_true(line_count(lines, "@@ -57,26 +67,26 @@") == 1,
    "native Status split one virtual hunk into multiple headers")
  assert_true(line_count(lines, "let particle_bind_group =") == 1,
    "native Status duplicated a neighboring virtual hunk")
  assert_true(line_count(lines, "// TODO: Initial size is not correct") == 1,
    "native Status omitted or duplicated bridge context between merged hunks")
  assert_true(line_count(lines, "    Self {") == 1,
    "native Status duplicated semantic context in the merged hunk")
  assert_true(requested[1] == "open", "native Status did not request its document")
  status.close(state)
  assert_true(vim.wait(1000, function() return not state.active end, 10), "native Status did not close the merged hunk document")
end, debug.traceback)

status._set_runner_for_test(nil)
if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
end
print("status_virtual_hunk_merge: native merged-hunk projection passed")
vim.cmd("qa!")

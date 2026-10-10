vim.loader.enable(false)
require("render-markdown").setup(require("plugins.markdown")[1].opts())
local system, active, maximum, calls = vim.system, 0, 0, 0
vim.system = function(command, options, callback)
  if not options.stdin then return system(command, options, callback) end
  assert(callback, "math conversion waited synchronously")
  active, calls = active + 1, calls + 1
  maximum = math.max(maximum, active)
  local expression = options.stdin
  vim.defer_fn(function()
    active = active - 1
    callback({ code = 0, stdout = "converted " .. expression, stderr = "" })
  end, 50)
  return { wait = function() error("math conversion blocked the main loop") end }
end
local buffer = vim.api.nvim_create_buf(false, true)
vim.api.nvim_set_current_buf(buffer)
vim.bo[buffer].filetype = "ForgeHarness"
vim.b[buffer].forge_native_document = true
local source = { "# Math", "" }
for index = 1, 8 do vim.list_extend(source, { "$$", "x_" .. index, "$$", "" }) end
source[2] = "Inline $x_9$ remains rendered."
vim.api.nvim_buf_set_lines(buffer, 0, -1, false, source)
local tick = 0
local timer = vim.uv.new_timer()
timer:start(0, 1, vim.schedule_wrap(function() tick = tick + 1 end))
require("forge.render.harness.markdown").render(buffer, vim.api.nvim_get_current_win(), {
  { first0 = 0, after0 = #source },
})
assert(vim.wait(5000, function() return calls == 9 and active == 0 end, 1), "conversions did not drain")
assert(maximum == 4 and tick >= 4, "conversion did not retain bounded asynchronous execution: " .. maximum .. "/" .. tick)
assert(vim.deep_equal(vim.api.nvim_buf_get_lines(buffer, 0, -1, false), source))
timer:stop()
timer:close()
vim.api.nvim_buf_delete(buffer, { force = true })
assert(vim.wait(300, function() return active == 0 end, 1))
print("markdown_math_async OK: 9 conversions, maximum 4 active, " .. tick .. " main-loop ticks")
vim.cmd("qa!")

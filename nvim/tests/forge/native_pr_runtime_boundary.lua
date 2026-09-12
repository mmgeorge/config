vim.loader.enable(false)

package.loaded["forge"] = nil
package.loaded["forge.views.status.state"] = nil
package.preload["forge.views.status.state"] = function()
  error("legacy status controller loaded through native public setup")
end

local forge = require("forge")
local ok, failure = xpcall(function()
  forge.setup({ about_auto_generate = false })
  assert(package.loaded["forge.views.status.state"] == nil, "native public setup loaded the legacy status controller")
  assert(package.loaded["forge.views.pr.pr_overview"] == nil, "native public setup loaded the legacy PR overview")
  assert(package.loaded["forge.views.pr.pr_edit"] == nil, "native public setup loaded legacy PR editing")
  assert(package.loaded["forge.views.pr.review"] == nil, "native public setup loaded legacy review rendering")
  assert(package.loaded["forge.review"] == nil, "native public setup loaded the legacy review binding")
end, debug.traceback)

package.preload["forge.views.status.state"] = nil
if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  print("native_pr_runtime_boundary: native setup does not load legacy PR/review rendering")
  vim.cmd("qa!")
end

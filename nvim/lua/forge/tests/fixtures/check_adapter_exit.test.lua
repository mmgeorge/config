local MiniTest = require("mini.test")
local T = MiniTest.new_set()

T["exit_without_reporting"] = function()
  os.exit(9)
end

return T

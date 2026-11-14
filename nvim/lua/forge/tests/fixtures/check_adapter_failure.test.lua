local MiniTest = require("mini.test")
local T = MiniTest.new_set()

T["intentional_failure"] = function()
  MiniTest.expect.equality("actual", "expected")
end

return T

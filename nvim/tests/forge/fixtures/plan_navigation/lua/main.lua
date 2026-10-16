local Planned = require("model")
local External = require("dependency")

local function run()
  local value = Planned.new()
  return value.count + External.read()
end

return { run = run }

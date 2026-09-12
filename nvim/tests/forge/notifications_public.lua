vim.opt.rtp:append("D:/config/nvim")

local recorded
local native_state = { active = true }
local original = package.loaded["forge.notifications"]
package.loaded["forge.notifications"] = {
  open = function(options)
    recorded = options
    return native_state
  end,
}
package.loaded["github.notifications"] = nil

local notifications = require("github.notifications")
local options = { hostname = "github.example", workspace = "D:/work", window = 12 }
assert(notifications.open(options) == native_state, "public notifications entry point did not return native state")
assert(recorded == options, "public notifications entry point did not preserve adapter options")

package.loaded["github.notifications"] = nil
package.loaded["forge.notifications"] = original
print("public notifications entry point passed")
vim.cmd("qa!")

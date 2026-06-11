-- mcphub.nvim glue: turns the tools of active MCP servers into AITool
-- entries for generate_with_tools. mcphub owns server lifecycles, schemas,
-- and the tools/call protocol; this module only adapts its Lua API:
--   require("mcphub").get_hub_instance()  -> hub (nil until setup completed)
--   hub:get_servers()[].capabilities.tools[]  -> { name, description, inputSchema }
--   hub:call_tool(server, tool, args, { parse_response = true, callback })
--     -> callback(result, err) with result = { text, images, error? }
-- Reference consumer: mcphub's own codecompanion extension
-- (lua/mcphub/extensions/codecompanion/core.lua), which also wraps call_tool
-- in plenary.async.run.

---@class AIMcpModule
local M = {}

---@param name string
---@return string
local function safe_name(name)
  return (tostring(name):gsub("[^%w_-]", "_"))
end

---@param hub table
---@param server_name string
---@param tool_name string
---@param args table
---@param done fun(output: string)
local function call_tool(hub, server_name, tool_name, args, done)
  local function invoke()
    hub:call_tool(server_name, tool_name, args or vim.empty_dict(), {
      caller = { type = "ai" },
      parse_response = true,
      callback = function(result, err)
        vim.schedule(function()
          if err or not result then
            done("Error: " .. (err and tostring(err) or "no response from MCP tool"))
          elseif result.error then
            done("Error: " .. tostring(result.error))
          elseif type(result.text) == "string" and result.text ~= "" then
            done(result.text)
          else
            done("(tool returned no text)")
          end
        end)
      end,
    })
  end

  -- mcphub executes tool calls in a plenary.async context (see its
  -- codecompanion extension); mirror that when plenary is available.
  local ok_async, async = pcall(require, "plenary.async")
  if ok_async then
    async.run(invoke)
  else
    invoke()
  end
end

--- Collect the tools of the named mcphub servers as AITool entries. Tool
--- names are namespaced "<server>__<tool>" (sanitized to the characters
--- providers accept in function names).
---@param server_names string[]
---@return AITool[]? tools
---@return string? error
function M.tools(server_names)
  local ok, mcphub = pcall(require, "mcphub")
  if not ok then
    return nil, "mcphub.nvim is not installed"
  end
  local hub = mcphub.get_hub_instance()
  if not hub then
    return nil, "mcphub is not ready (no hub instance; is setup complete?)"
  end

  ---@type table<string, boolean>
  local wanted = {}
  for _, name in ipairs(server_names) do
    wanted[name] = false
  end

  ---@type AITool[]
  local tools = {}
  for _, server in ipairs(hub:get_servers() or {}) do
    if wanted[server.name] ~= nil then
      wanted[server.name] = true
      local server_name = server.name
      for _, tool in ipairs(server.capabilities and server.capabilities.tools or {}) do
        local tool_name = tool.name
        tools[#tools + 1] = {
          name = safe_name(server_name) .. "__" .. safe_name(tool_name),
          description = tool.description or "",
          parameters = tool.inputSchema or { type = "object", properties = vim.empty_dict() },
          run = function(args, done)
            call_tool(hub, server_name, tool_name, args, done)
          end,
        }
      end
    end
  end

  for name, found in pairs(wanted) do
    if not found then
      return nil, ("mcp server %q is not active in mcphub"):format(name)
    end
  end
  return tools
end

return M

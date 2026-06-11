-- Reusable tool registry for require("ai").generate_with_tools. A consumer
-- references a registry tool by its key, mixes in one-off inline tools, or
-- both:
--
--   require("ai").generate_with_tools({
--     model = "gpt-mini",
--     prompt = "...",
--     tools = {
--       "git_log",                            -- registry reference
--       { name = "one_off", ... , run = ... } -- inline definition
--     },
--   }, cb)
--
-- Registry entries omit `name`; the key is the name. Keep entries small,
-- read-only, and fast - they run inside interactive flows.

---@class AIToolDefinition an AITool without the name (the registry key is the name)
---@field description string
---@field parameters table JSON Schema for the arguments object
---@field run fun(args: table, done: fun(output: string)) async; must call done exactly once

---@class AIToolsModule
local M = {}

---@param command string[]
---@param done fun(output: string)
local function system_text(command, done)
  local ok, process = pcall(vim.system, command, { text = true }, function(result)
    vim.schedule(function()
      if result.code ~= 0 then
        local stderr = vim.trim(result.stderr or "")
        done("Error: " .. (stderr ~= "" and stderr or ("command exited with code " .. tostring(result.code))))
        return
      end
      local stdout = vim.trim(result.stdout or "")
      done(stdout ~= "" and stdout or "(no output)")
    end)
  end)
  if not ok then
    vim.schedule(function()
      done("Error: " .. tostring(process))
    end)
  end
end

---@type table<string, AIToolDefinition>
M.registry = {
  git_log = {
    description = "Recent commit subjects from the current repository (git log --oneline).",
    parameters = {
      type = "object",
      properties = {
        count = { type = "integer", description = "Number of commits to show (default 10, max 50)" },
      },
    },
    run = function(args, done)
      local count = math.min(tonumber(args.count) or 10, 50)
      system_text({ "git", "log", "-n", tostring(count), "--oneline", "--no-color" }, done)
    end,
  },
  git_diff_stat = {
    description = "Summary of currently changed files (git diff --stat), staged or unstaged.",
    parameters = {
      type = "object",
      properties = {
        staged = { type = "boolean", description = "Show the staged diff instead of the unstaged one" },
      },
    },
    run = function(args, done)
      local command = { "git", "diff", "--no-color", "--no-ext-diff", "--stat" }
      if args.staged then command[#command + 1] = "--staged" end
      system_text(command, done)
    end,
  },
}

--- Resolve a registry key to a callable AITool (the key becomes the name).
---@param name string
---@return AITool? tool nil when no registry entry exists
function M.get(name)
  local definition = M.registry[name]
  if not definition then return nil end
  return {
    name = name,
    description = definition.description,
    parameters = definition.parameters,
    run = definition.run,
  }
end

return M

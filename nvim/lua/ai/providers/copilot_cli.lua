-- GitHub Copilot CLI provider.
--
-- Runs one non-interactive prompt with every tool source excluded. Prompt
-- content travels over stdin so large diffs do not hit command-line limits.

---@class AICopilotCLIProvider: AIProvider
local M = {
  name = "copilot-cli",
}

local excluded_tool_patterns = {
  "builtin:*",
  "mcp:*",
  "custom:*",
}

---@type table<string, AIPreset>
M.presets = {
  ["copilot-cli"] = {
    model = "gpt-5.6-luna",
    thinking = {
      minimal = "minimal",
      low = "low",
      medium = "medium",
      high = "high",
      max = "max",
    },
  },
}

---@param spec AIModelSpec
---@return string[]
local function command(spec)
  local args = {
    "copilot",
    "--silent",
    "--no-color",
    "--stream", "off",
    "--model", spec.model,
    "--allow-all-tools",
    "--excluded-tools",
  }
  vim.list_extend(args, excluded_tool_patterns)
  vim.list_extend(args, {
    "--disable-builtin-mcps",
    "--no-custom-instructions",
    "--no-ask-user",
    "--no-auto-update",
    "--no-bash-env",
    "--disable-remote-sessions",
  })
  if spec.thinking then
    vim.list_extend(args, { "--effort", tostring(spec.thinking) })
  end
  return args
end

---@param request AIGenerateRequest
---@return string
local function prompt_text(request)
  local sections = { request.prompt }
  if request.system and vim.trim(request.system) ~= "" then
    sections[#sections + 1] = "Response requirements:\n" .. request.system
  end
  sections[#sections + 1] = "Return only the requested final text. Do not use tools."
  return table.concat(sections, "\n\n")
end

---@param ctx AIRequestContext
---@param spec AIModelSpec
---@param request AIGenerateRequest
---@param cb fun(result: AIGenerateResult)
function M.generate(ctx, spec, request, cb)
  if request.history or (request.tools and #request.tools > 0) then
    cb({ ok = false, error = "copilot-cli: tool calling and multi-round prompts are not supported" })
    return
  end

  ctx.run_process({
    command = command(spec),
    input = prompt_text(request),
    cwd = vim.fn.stdpath("cache"),
  }, function(result)
    if result.code ~= 0 then
      local message = vim.trim(result.output)
      cb({
        ok = false,
        error = "copilot-cli: " .. (message ~= "" and message or ("exited with code " .. tostring(result.code))),
      })
      return
    end

    local content = vim.trim(result.stdout)
    if content == "" then
      cb({ ok = false, error = "copilot-cli: empty response" })
      return
    end
    cb({ ok = true, content = content })
  end)
end

return M

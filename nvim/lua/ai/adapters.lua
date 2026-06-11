local M = {}

--- Adapter selection for AI features. `chat`, `inline`, and `cmd` are
--- CodeCompanion adapter configs (the chat UI still runs on CodeCompanion).
--- `commit` is a model token for the local `ai` prompt library (see
--- AGENTS.md in this directory).
---@return { chat: table|string, inline: table|string, cmd: string, commit: string }
function M.get()
  if os.getenv("GEMINI_API_KEY") then
    return {
      chat = {
        name = "gemini",
        model = "gemini-3-pro-preview",
      },
      inline = {
        name = "gemini",
        model = "gemini-3-flash-preview",
      },
      cmd = "gemini",
      commit = "gemini3-lite,thinking=minimal",
    }
  end

  return {
    chat = "copilot",
    inline = "copilot",
    cmd = "copilot",
    commit = "copilot-nano",
  }
end

return M

local M = {}

--- Adapter selection for AI features. `chat`, `inline`, and `cmd` are
--- CodeCompanion adapter configs (the chat UI still runs on CodeCompanion).
--- `commit` selects an API model token or "harness" for the current Harness
--- backend. `pr_create` and `inline_edit` are local `ai` model tokens.
---@return { chat: table|string, inline: table|string, cmd: string, commit: string, pr_create: string, inline_edit: string }
function M.get()
  if os.getenv("GEMINI_API_KEY") then
    return {
      chat = {
        name = "gemini",
        model = "gemini-3.1-pro-preview",
      },
      inline = {
        name = "gemini",
        model = "gemini-3.8-flash",
      },
      cmd = "gemini",
      commit = "gemini3-lite,thinking=minimal",
      pr_create = "gemini3,thinking=low",
      inline_edit = "gemini3,thinking=low",
    }
  end

  return {
    chat = "copilot",
    inline = "copilot",
    cmd = "copilot",
    commit = "harness",
    pr_create = "copilot-cli,model=gpt-5.6-luna,thinking=low",
    inline_edit = "copilot-mini",
  }
end

return M

local M = {}

---@return table
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
      background = "gemini",
      commit = {
        name = "gemini",
        model = "gemini-3.1-flash-lite-preview",
        reasoning_effort = "none",
      },
    }
  end

  return {
    chat = "copilot",
    inline = "copilot",
    cmd = "copilot",
    background = "copilot",
    commit = {
      name = "copilot",
      model = "gpt-4.1",
    },
  }
end

return M

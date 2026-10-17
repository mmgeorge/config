local M = {}

---@alias ForgeCommitComparison 'head'|'staged'|'latest_commit'
---@class ForgeCommitPromptPreviewOptions
---@field comparison? ForgeCommitComparison
---@field workspace? string

---@class ForgeCommitPromptInput
---@field system string
---@field user string
---@field input_bytes integer
---@field limit_bytes integer

---@class ForgeCommitPromptResponse
---@field state 'ready'|'none'|'retry'
---@field prompt? ForgeCommitPromptInput
---@field diff_pairs integer
---@field source_requests integer

--- Opens exact native commit input in a read-only scratch buffer, without provider access.
--- Defaults to HEAD versus the current workspace. Closing the buffer discards late responses.
---@param options? ForgeCommitPromptPreviewOptions
---@return integer? buffer
function M.open(options)
  options = options or {}
  local comparison = options.comparison or "head"
  if comparison ~= "head" and comparison ~= "staged" and comparison ~= "latest_commit" then
    vim.notify("Expected head, staged, or latest_commit", vim.log.levels.ERROR, { title = "ForgeDebugCommitPromptPreview" })
    return nil
  end
  local workspace = options.workspace or vim.b.forge_workspace or vim.fn.getcwd()
  local buffer = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_name(buffer, ("ForgeDebugCommitPromptPreview://%s/%d"):format(comparison, buffer))
  vim.bo[buffer].bufhidden = "wipe"
  vim.bo[buffer].swapfile = false
  vim.bo[buffer].filetype = "diff"
  vim.bo[buffer].readonly = true
  vim.b[buffer].forge_workspace = workspace
  vim.b[buffer].forge_commit_prompt_comparison = comparison
  require("forge.window_presentation").attach(buffer, {
    number = false, relativenumber = false, signcolumn = "no", foldcolumn = "0", statuscolumn = "",
    wrap = false, linebreak = false, foldenable = false, conceallevel = 0, virtualedit = "",
  })
  local heading = { "Commit prompt preview", "Workspace: " .. workspace, "Comparison: " .. comparison, "" }
  ---@param lines string[]
  local function publish(lines)
    if not vim.api.nvim_buf_is_valid(buffer) then return end
    vim.bo[buffer].readonly = false
    vim.bo[buffer].modifiable = true
    vim.api.nvim_buf_set_lines(buffer, 0, -1, false, lines)
    vim.bo[buffer].modified = false
    vim.bo[buffer].modifiable = false
    vim.bo[buffer].readonly = true
  end
  publish(vim.list_extend(vim.deepcopy(heading), { "Preparing prompt…" }))
  vim.api.nvim_win_set_buf(0, buffer)
  vim.keymap.set("n", "q", function()
    if vim.api.nvim_buf_is_valid(buffer) then vim.api.nvim_buf_delete(buffer, { force = true }) end
  end, { buffer = buffer, nowait = true, desc = "Close commit prompt preview" })
  local attempts = 0
  local request
  local function fail(message)
    publish(vim.list_extend(vim.deepcopy(heading), { "Preview failed: " .. message }))
    vim.notify(message, vim.log.levels.ERROR, { title = "ForgeDebugCommitPromptPreview" })
  end
  request = function()
    if not vim.api.nvim_buf_is_valid(buffer) then return end
    attempts = attempts + 1
    require("forge.client").request_host("repository.generate", {
      operation = "preview", workspace = workspace, comparison = comparison,
    }, function(result, failure)
      if not vim.api.nvim_buf_is_valid(buffer) then return end
      if failure then fail(failure) return end
      ---@cast result ForgeCommitPromptResponse?
      if not result then fail("Missing commit prompt response") return end
      if result.state == "retry" then
        if attempts >= 3 then fail("Repository changed while preparing the prompt. Run the command again.")
        else vim.defer_fn(request, 120) end
        return
      end
      if result.state == "none" then
        publish(vim.list_extend(vim.deepcopy(heading), { "No changes for this comparison." }))
        return
      end
      local prompt = result.prompt
      if result.state ~= "ready" or not prompt or type(prompt.system) ~= "string" or type(prompt.user) ~= "string"
        or type(prompt.input_bytes) ~= "number" or type(prompt.limit_bytes) ~= "number" then
        fail("Invalid commit prompt response")
        return
      end
      vim.b[buffer].forge_commit_prompt_input = prompt
      local lines = vim.list_extend(vim.deepcopy(heading), {
        ("Input: %d / %d UTF-8 bytes"):format(prompt.input_bytes, prompt.limit_bytes), "", "SYSTEM", "",
      })
      vim.list_extend(lines, vim.split(prompt.system, "\n", { plain = true }))
      vim.list_extend(lines, { "", "USER", "" })
      vim.list_extend(lines, vim.split(prompt.user, "\n", { plain = true }))
      publish(lines)
    end)
  end
  request()
  return buffer
end

return M

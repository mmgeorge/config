local M = {}
local configured=false

---@param buffer integer
---@param owner? table
function M.attach(buffer,owner)
  local config=require("forge.infra.config").options.text_correction
  if not config or not vim.api.nvim_buf_is_valid(buffer) then return end
  local explicit=owner~=nil
  owner=owner or {kind=vim.bo[buffer].filetype=="markdown" and "markdown" or vim.bo[buffer].filetype=="gitcommit" and "commit"
    or vim.bo[buffer].filetype=="ForgeHarnessInput" and "prompt"
    or (vim.bo[buffer].filetype=="text" or vim.bo[buffer].filetype=="mail") and "prose" or "source"}
  owner.explicit=explicit
  owner.root=owner.root or vim.b[buffer].forge_workspace or vim.fs.root(vim.api.nvim_buf_get_name(buffer),{ ".git" }) or vim.fn.getcwd()
  if config.enabled then require("forge.text.input").attach(buffer,owner) end
  require("forge.text.harper").attach(buffer,owner)
end

---@param buffer integer
function M.invalidate(buffer)
  if package.loaded["forge.text.input"] then require("forge.text.input").invalidate(buffer) end
  if package.loaded["forge.text.harper"] then require("forge.text.harper").invalidate(buffer) end
end

---@param buffer integer
function M.detach(buffer)
  if package.loaded["forge.text.input"] then require("forge.text.input").detach(buffer) end
  if package.loaded["forge.text.harper"] then require("forge.text.harper").detach(buffer) end
end

function M.setup()
  require("forge.text.harper").setup()
  if package.loaded["forge.text.input"] then require("forge.text.input").invalidate_all() end
  if not configured then
    configured=true
    local group=vim.api.nvim_create_augroup("ForgeTextCorrection",{clear=true})
    vim.api.nvim_create_autocmd({"FileType","BufEnter","BufReadPost"},{group=group,callback=function(event)
      local filetype=vim.bo[event.buf].filetype
      if filetype~="" and (vim.bo[event.buf].buftype=="" or filetype=="ForgeHarnessInput") then M.attach(event.buf) end
    end})
    vim.api.nvim_create_autocmd("BufWritePre",{group=group,callback=function(event) M.invalidate(event.buf) end})
    vim.api.nvim_create_autocmd("BufWritePost",{group=group,callback=function(event)
      if require("forge.text.wordlist").invalidate(event.file) then
        if package.loaded["forge.text.input"] then require("forge.text.input").invalidate_all() end
        require("forge.text.harper").refresh_dictionaries()
      end
    end})
  end
  if not require("forge.infra.config").options.text_correction.enabled then
    if package.loaded["forge.text.input"] then require("forge.text.input").detach_all() end
  end
  for _, buffer in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_loaded(buffer) and vim.bo[buffer].filetype~="" and vim.bo[buffer].buftype=="" then M.attach(buffer) end
  end
end

return M

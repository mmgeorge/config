---@param context table
---@return boolean
local function parser_available(context)
  if context.incmd then return true end
  if vim.bo.filetype == 'ForgeHarnessInput' then return false end
  local available = context.save[parser_available]
  if available == nil then
    local success, parser = pcall(vim.treesitter.get_parser)
    available = success and parser ~= nil and parser ~= false
    context.save[parser_available] = available
  end
  return available
end

return {
  {
    'altermo/ultimate-autopair.nvim',
    event = { 'InsertEnter', 'CmdlineEnter' },
    branch = 'v0.6',     --recommended as each new version will have breaking changes
    opts = {
      extensions = {
        filetype = {
          p = 120,
          nft = { 'TelescopePrompt', 'ForgeHarnessInput' },
          tree = parser_available,
          ---@param context table
          ---@return string[]?
          ft = function(context)
            if not parser_available(context) then return {} end
          end,
        },
      },
      internal_pairs = { -- *ultimate-autopair-pairs-default-pairs*
        { '[', ']', fly = true,     dosuround = true, newline = true, space = true },
        { '(', ')', fly = true,     dosuround = true, newline = true, space = true },
        { '{', '}', fly = true,     dosuround = true, newline = true, space = true },
        { '"', '"', suround = true, multiline = false },
        -- {"'","'",suround=true,cond=function(fn) return not fn.in_lisp() or fn.in_string() end,alpha=true,nft={'tex'},multiline=false},
        {
          '`',
          '`',
          -- cond = function(fn) return not fn.in_lisp() or fn.in_string() end,
          nft = { 'tex' },
          multiline = false
        },
        { '``',   "''",  ft = { 'tex' } },
        { '```',  '```', newline = true,              ft = { 'markdown' } },
        { '<!--', '-->', ft = { 'markdown', 'html' }, space = true },
        { '"""',  '"""', newline = true,              ft = { 'python' } },
        { "'''",  "'''", newline = true,              ft = { 'python' } },
      },
      tabout = {
        enable = false,
        --map = "ozz",
        --cmap = "ozz",
        hopout = true,
        multi = false,
      }
      --Config goes here
    },
  },
}

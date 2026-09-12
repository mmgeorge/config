vim.loader.enable(false)
local plugin_root = vim.fs.joinpath(vim.fn.stdpath('data'), 'lazy', 'ultimate-autopair.nvim')
assert(vim.fn.isdirectory(plugin_root) == 1, 'composer_autopair requires the installed ultimate-autopair plugin')
vim.opt.runtimepath:append(plugin_root)
local autopair = require('ultimate-autopair')
local parser_original = vim.treesitter.get_parser
local parser_calls = 0
local success, failure = xpcall(function()
  autopair.setup(require('plugins.autopair')[1].opts)
  vim.bo.filetype = 'ForgeHarnessInput'
  vim.treesitter.get_parser = function()
    parser_calls = parser_calls + 1
    error('native composer entered the autopair parser path')
  end
  local filetype_extension = require('plugins.autopair')[1].opts.extensions.filetype
  assert(filetype_extension.tree({ incmd = true }), 'command-line pairing was disabled')
  assert(filetype_extension.ft({ incmd = true }) == nil, 'command-line filetype was excluded')
  vim.api.nvim_feedkeys(vim.keycode('iManual draft (<CR>Second line<Esc>'), 'xt', false)
  assert(parser_calls == 0, 'composer guard ran after parser lookup')
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(0, 0, -1, false), {
    'Manual draft (', 'Second line',
  }), 'composer typing lost or generated text')
  for _, filetype in ipairs({ 'gitcommit', 'text', '' }) do
    for _, failure_kind in ipairs({ 'nil', 'false', 'error' }) do
      vim.cmd('enew!')
      vim.bo.filetype = filetype
      vim.v.errmsg = ''
      vim.treesitter.get_parser = function()
        if failure_kind == 'error' then error('parser unavailable') end
        if failure_kind == 'false' then return false end
        return nil
      end
      vim.api.nvim_feedkeys(vim.keycode('iManual accepted commit (<CR>Second line<Esc>'), 'xt', false)
      assert(vim.deep_equal(vim.api.nvim_buf_get_lines(0, 0, -1, false), {
        'Manual accepted commit (', 'Second line',
      }), 'parserless typing changed for ' .. filetype .. '/' .. failure_kind)
      assert(vim.fn.mode() == 'n', 'Escape failed for ' .. filetype .. '/' .. failure_kind)
      assert(vim.v.errmsg == '', 'parserless input reported ' .. vim.v.errmsg)
    end
  end
  vim.treesitter.get_parser = parser_original
  vim.cmd('enew!')
  vim.bo.filetype = 'lua'
  assert(vim.treesitter.get_parser(), 'ordinary Lua pairing check requires its installed parser')
  vim.api.nvim_feedkeys(vim.keycode('i(<Esc>'), 'xt', false)
  assert(vim.api.nvim_get_current_line() == '()', 'ordinary source pairing was disabled')
end, debug.traceback)
vim.treesitter.get_parser = parser_original
autopair.clear()
if not success then vim.api.nvim_err_writeln(failure) vim.cmd('cquit 1') end
print('composer_autopair OK')
vim.cmd('qa!')

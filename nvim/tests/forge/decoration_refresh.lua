vim.loader.enable(false)
local runtime = vim.fs.dirname(vim.fs.dirname(vim.fs.dirname(debug.getinfo(1, 'S').source:sub(2))))
local host = vim.fn.jobstart({vim.v.progpath, '--embed', '--headless', '-u', 'NONE', '-i', 'NONE'}, {rpc = true})
assert(host > 0)

local function run(source, arguments)
  return vim.rpcrequest(host, 'nvim_exec_lua', source, arguments or {})
end

local success, failure = xpcall(function()
  vim.rpcrequest(host, 'nvim_ui_attach', 100, 30, {rgb = true})
  run([[
    vim.opt.rtp:prepend(...)
    vim.loader.enable(false)
    vim.o.laststatus, vim.o.showtabline, vim.o.showmode, vim.o.ruler = 0, 0, false, false
    vim.api.nvim_set_hl(0, 'Normal', {fg = 0xffffff, bg = 0})
    vim.api.nvim_set_hl(0, 'ForgeHarnessOutput', {fg = 0x696969})
    _G.replica = require('forge.buffer')
    _G.owner = replica.open('decoration-refresh', {source_projected = true})
    vim.api.nvim_set_current_buf(owner.buffer)
    vim.wo.number, vim.wo.relativenumber, vim.wo.signcolumn = false, false, 'no'
    assert(replica.apply_snapshot(owner, {document = owner.document, revision = 0, block = {
      {id = 'head', text = {'• 0.8s Search web'}, metadata = {target = {}, decoration = {}, editable_region = {}}},
      {id = 'body', text = {'  └ {', '      "type": "webSearch",', '      "query": "hotfix"', '    }'},
        metadata = {target = {}, decoration = {}, editable_region = {}}},
    }}).kind == 'Applied')
    vim.api.nvim_win_set_cursor(0, {1, 0})
    _G.windows = {vim.api.nvim_get_current_win()}
    vim.cmd('vsplit')
    windows[#windows + 1] = vim.api.nvim_get_current_win()
    _G.update = function(revision, capture)
      completed = false
      local spans = capture and {{range = {start = {row = 0, column = 0}, ['end'] = {row = 4, column = 0}},
        capture = capture, priority = 100}} or {}
      replica.apply_async(owner, {document = owner.document, base = revision - 1, next = revision,
        base_rows = 5, next_rows = 5, base_blocks = 2, next_blocks = 2,
        text_edit = {}, block_edit = {}, removed_block = {},
        metadata_edit = {{block = 'body', row_count = 4,
          metadata = {target = {}, editable_region = {}, decoration = {}, visible_decoration = spans}}},
      }, function() return true end, function(result)
        assert(result.kind == 'Applied', vim.inspect(result))
        completed = true
      end)
    end
  ]], {runtime})

  local function colors()
    return run([[
      local colors = {}
      for _, window in ipairs(windows) do
        assert(vim.api.nvim_win_get_cursor(window)[1] == 1, 'highlight update moved the cursor')
        local position = vim.fn.screenpos(window, 3, 8)
        local cell = vim.api.nvim__inspect_cell(1, position.row - 1, position.col - 1)
        assert(cell[1] == 't', 'expected the JSON key cell')
        colors[#colors + 1] = cell[2].foreground or 0xffffff
      end
      return colors
    ]])
  end

  vim.wait(100, function() return false end, 10)
  assert(vim.deep_equal(colors(), {0xffffff, 0xffffff}))
  run([[update(1, 'ForgeHarnessOutput')]])
  assert(vim.wait(2000, function() return run('return completed') end, 10))
  vim.wait(100, function() return false end, 10)
  assert(vim.deep_equal(colors(), {0x696969, 0x696969}),
    'metadata update did not redraw the darker output highlight without cursor movement')

  run([[update(2, nil)]])
  assert(vim.wait(2000, function() return run('return completed') end, 10))
  vim.wait(100, function() return false end, 10)
  assert(vim.deep_equal(colors(), {0xffffff, 0xffffff}),
    'removed decoration remained cached until cursor movement')
end, debug.traceback)
vim.fn.jobstop(host)
if not success then vim.api.nvim_err_writeln(failure) vim.cmd('cquit 1') end
print('decoration_refresh OK: metadata updates redraw both splits without cursor movement')
vim.cmd('qa!')

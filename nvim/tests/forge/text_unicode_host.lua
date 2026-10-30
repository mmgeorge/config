local fixture = dofile('nvim/tests/forge/support/text_fixture.lua').native()
local ok, failure = xpcall(function()
  fixture.exec([[
    text_unicode_wire={}
    local client=require('forge.client')
    local request=client.request
    client.request=function(specification,callback,progress)
      if specification.method~='text.correct' then return request(specification,callback,progress) end
      local captured={request=vim.deepcopy(specification)}
      text_unicode_wire[#text_unicode_wire+1]=captured
      return request(specification,function(result,failure,detail)
        captured.result=result
        captured.error=failure
        callback(result,failure,detail)
      end,progress)
    end
  ]])
  local function replace_buffer()
    fixture.type('<Esc>')
    fixture.exec([[vim.api.nvim_buf_set_lines(0,0,-1,false,{''});vim.api.nvim_win_set_cursor(0,{1,0});text_unicode_wire={}]])
  end
  local function targets()
    local captured = fixture.exec('return text_unicode_wire')
    assert(#captured > 0, 'native model received no English correction request')
    local originals = {}
    for _, wire in ipairs(captured) do
      local params = wire.request.params
      local original = params.context:sub(params.target.start + 1, params.target['end'])
      assert(not original:find('[\128-\255]'), 'unsupported target reached native service: ' .. original)
      assert(wire.error == nil or wire.error == vim.NIL, vim.inspect(wire))
      originals[#originals + 1] = original
    end
    return originals
  end

  fixture.type('i東京 café Ω Please recieve ')
  fixture.wait('東京 café Ω Please receive ')
  assert(vim.tbl_contains(targets(), 'recieve'))

  replace_buffer()
  fixture.type('i日本語<CR>Please recieve ')
  fixture.wait('日本語\nPlease receive ')
  assert(vim.tbl_contains(targets(), 'recieve'))

  replace_buffer()
  fixture.type('iRead "D:/資料/my files/recieve.txt" then recieve ')
  fixture.wait('Read "D:/資料/my files/recieve.txt" then receive ')
  local originals = targets()
  assert(not vim.tbl_contains(originals, 'txt'), 'quoted path extension reached native service')
  local misspelling_count = 0
  for _, original in ipairs(originals) do
    if original == 'recieve' then misspelling_count = misspelling_count + 1 end
  end
  assert(misspelling_count == 1, 'quoted path segment reached native service: ' .. vim.inspect(originals))

  replace_buffer()
  fixture.type('iRead "./資料 folder/Ω.txt" then recieve ')
  fixture.wait('Read "./資料 folder/Ω.txt" then receive ')
  assert(not vim.tbl_contains(targets(), 'txt'), 'relative quoted path extension reached native service')

  replace_buffer()
  fixture.type('iPlease "recieve ')
  fixture.wait('Please "receive ')
  fixture.type('the package" ')
  fixture.wait('Please "receive the package" ')
  assert(vim.tbl_contains(targets(), 'recieve'), 'plain quoted prose never reached the native model')
  assert(fixture.exec('return #text_errors') == 0, vim.inspect(fixture.exec('return text_errors')))
end, debug.traceback)
fixture.close()
assert(ok, failure)
print('text Unicode host: native model preserves Unicode and quoted paths while correcting English targets')
vim.cmd('qa!')

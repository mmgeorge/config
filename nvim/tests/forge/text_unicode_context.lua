vim.opt.runtimepath:prepend(vim.fn.getcwd() .. '/nvim')
local fixture = dofile('nvim/tests/forge/support/text_fixture.lua').open()
local ok, failure = xpcall(function()
  local function replace_buffer()
    fixture.type('<Esc>')
    fixture.exec([[vim.api.nvim_buf_set_lines(0,0,-1,false,{''});vim.api.nvim_win_set_cursor(0,{1,0});text_requests={}]])
  end

  fixture.type('i東京 café Ω Please recieve ')
  fixture.wait('東京 café Ω Please receive ')
  local requests = fixture.exec('return vim.tbl_map(function(request)return request.occurrence.context.original end,text_requests)')
  assert(vim.tbl_contains(requests, 'recieve'), vim.inspect(requests))
  for _, target in ipairs(requests) do
    assert(not target:find('[\128-\255]'), 'non-ASCII correction target: ' .. target)
  end

  replace_buffer()
  fixture.type('i日本語<CR>Please recieve ')
  fixture.wait('日本語\nPlease receive ')
  requests = fixture.exec('return vim.tbl_map(function(request)return request.occurrence.context.original end,text_requests)')
  assert(vim.tbl_contains(requests, 'recieve'), vim.inspect(requests))
  for _, target in ipairs(requests) do
    assert(not target:find('[\128-\255]'), 'newline admitted a Unicode target: ' .. target)
  end

  replace_buffer()
  fixture.type('iRead "D:/資料/my files/recieve.txt" then recieve ')
  fixture.wait('Read "D:/資料/my files/recieve.txt" then receive ')
  requests = fixture.exec('return vim.tbl_map(function(request)return request.occurrence.context.original end,text_requests)')
  assert(not vim.tbl_contains(requests, 'txt'), 'quoted path extension was submitted')
  local misspelling_count = 0
  for _, target in ipairs(requests) do
    if target == 'recieve' then misspelling_count = misspelling_count + 1 end
  end
  assert(misspelling_count == 1, 'quoted path segment was submitted: ' .. vim.inspect(requests))

  replace_buffer()
  fixture.type('iRead "./資料 folder/Ω.txt" then recieve ')
  fixture.wait('Read "./資料 folder/Ω.txt" then receive ')
  requests = fixture.exec('return vim.tbl_map(function(request)return request.occurrence.context.original end,text_requests)')
  assert(not vim.tbl_contains(requests, 'txt'), 'relative quoted path extension was submitted')

  replace_buffer()
  fixture.type('iSay "teh ')
  fixture.wait('Say "the ')
  fixture.type('sentence, words" ')
  fixture.wait('Say "the sentence, words" ')
  assert(fixture.exec('return #text_requests') > 0, 'plain quoted prose was protected as a path')
  assert(fixture.exec('return #text_errors') == 0, vim.inspect(fixture.exec('return text_errors')))
end, debug.traceback)
fixture.close()
assert(ok, failure)
print('text Unicode context: exact mixed text, newline target eligibility, quoted paths, and quoted prose passed')
vim.cmd('qa!')

-- Sustained typing and long contexts against the real text host. Each word is typed only after
-- the previous correction applies, so the fixture advances on host replies rather than on a
-- typing cadence measured by the real clock.
local fixture=dofile('nvim/tests/forge/support/text_fixture.lua').native()
local word_count=120
local ok,failure=xpcall(function()
  fixture.type('i')
  fixture.exec('text_measurements={}')
  local function applied_count()
    return fixture.exec([[
      local count=0
      for _,measurement in ipairs(text_measurements) do
        if measurement.event=='text.correction' and measurement.fields.status=='applied' then count=count+1 end
      end
      return count
    ]])
  end
  for word=1,word_count do
    fixture.type('recieve ')
    assert(vim.wait(2000,function()return applied_count()>=word end,1),
      ('correction %d did not apply: %s'):format(word,vim.inspect(fixture.exec('return text_measurements'))))
  end
  local redraw_count=fixture.exec([[
    local count=0
    for _,measurement in ipairs(text_measurements) do
      if measurement.event=='text.correction.redraw' then count=count+1 end
    end
    return count
  ]])
  assert(redraw_count>0,'no applied correction reached redraw')
  -- Sentence-start capitalization applies to the first word only.
  local expected='Receive '..string.rep('receive ',word_count-1)
  assert(fixture.text()==expected,'sustained input left an uncorrected word: '..fixture.text():sub(1,200))
  fixture.type('<Esc>')
  for _,words in ipairs({10,100,1000,10000}) do
    fixture.exec([[
      local words=...
      local prefix=string.rep('previous ',words)
      vim.api.nvim_buf_set_lines(0,0,-1,false,{prefix..'Please recieve a package.'})
      vim.api.nvim_win_set_cursor(0,{1,#prefix+#'Please recieve'})
      text_measurements={}
    ]],{words})
    fixture.type('i ')
    assert(vim.wait(2000,function()return fixture.exec('return #text_measurements>=2')end,1),
      ('a %d-word context did not settle'):format(words))
    local status=fixture.exec([[
      for _,entry in ipairs(text_measurements) do
        if entry.event=='text.correction' then return entry.fields.status end
      end
    ]])
    assert(status=='applied',('a %d-word context ended with %s'):format(words,vim.inspect(status)))
    fixture.type('<Esc>')
  end
  assert(#fixture.exec('return text_errors')==0,vim.inspect(fixture.exec('return text_errors')))
end,debug.traceback)
fixture.close()
assert(ok,failure)
vim.cmd('qa!')

local fixture=dofile('nvim/tests/forge/support/text_fixture.lua').native()
local ok,failure=xpcall(function()
  fixture.type('i')
  fixture.exec([[
    text_workload={count=0,done=false,started=vim.uv.hrtime()}
    local function next_word()
      text_workload.count=text_workload.count+1
      vim.api.nvim_input('recieve ')
      if text_workload.count<120 then vim.defer_fn(next_word,50)
      else vim.defer_fn(function()text_workload.done=true end,100) end
    end
    next_word()
  ]])
  assert(vim.wait(10000,function()return fixture.exec('return text_workload.done')end,10),'workload did not finish')
  local measurements=fixture.exec('return text_measurements')
  local applied,redraw,cold,status={}, {},nil,{}
  for _,measurement in ipairs(measurements) do
    if measurement.event=='text.correction' then
      local name=measurement.fields.status
      status[name]=(status[name] or 0)+1
    end
    if measurement.event=='text.correction' and measurement.fields.status=='applied' then
      applied[#applied+1]=measurement.fields.elapsed_ms
    elseif measurement.event=='text.correction.redraw' then
      redraw[#redraw+1]=measurement.fields.elapsed_us/1000
    elseif measurement.event=='text.initialize' then cold=measurement.fields.elapsed_ms end
  end
  assert(#applied>0 and #redraw>0,'no visible corrections were measured')
  table.sort(applied);table.sort(redraw)
  local function percentile(values,percent)return values[math.ceil(#values*percent)]end
  local report={platform=vim.uv.os_uname(),words=120,words_per_second=20,cold_ms=cold,status=status,
    applied_count=#applied,redraw_count=#redraw,
    applied={p95_ms=percentile(applied,.95),p99_ms=percentile(applied,.99)},
    redraw={p95_ms=percentile(redraw,.95),p99_ms=percentile(redraw,.99)}}
  report.long_context={}
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
    assert(vim.wait(5000,function()return fixture.exec('return #text_measurements>=2')end,5),'long context did not settle')
    local entries=fixture.exec('return text_measurements')
    local sample={words=words}
    for _,entry in ipairs(entries) do
      if entry.event=='text.correction' then sample.applied_ms=entry.fields.elapsed_ms;sample.status=entry.fields.status end
      if entry.event=='text.correction.redraw' then sample.redraw_ms=entry.fields.elapsed_us/1000 end
    end
    assert(sample.status=='applied',vim.inspect(sample))
    report.long_context[#report.long_context+1]=sample
    fixture.type('<Esc>')
  end
  vim.fn.writefile({vim.json.encode(report)},'nvim/rust/forge/target/text-latency.json')
  print(vim.inspect(report))
  assert(#fixture.exec('return text_errors')==0)
end,debug.traceback)
fixture.close()
assert(ok,failure)
vim.cmd('qa!')

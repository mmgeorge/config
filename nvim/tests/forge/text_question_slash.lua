local fixture = dofile("nvim/tests/forge/support/text_fixture.lua").open()

---@param text string
local function begin(text)
  fixture.type("<Esc>")
  fixture.exec([[vim.api.nvim_buf_set_lines(0,0,-1,false,{''});vim.api.nvim_win_set_cursor(0,{1,0})]])
  fixture.type("i" .. text)
  fixture.wait(text)
end

local ok, failure = xpcall(function()
  for _, ending in ipairs({ { " ", " " }, { "<CR>", "\n" } }) do
    begin("foo/")
    fixture.type(ending[1])
    fixture.wait("foo?" .. ending[2])
    local cursor = fixture.exec("return vim.api.nvim_win_get_cursor(0)")
    assert(cursor[1] == (ending[2] == "\n" and 2 or 1))
    assert(cursor[2] == (ending[2] == "\n" and 0 or 5))
    fixture.type("<BS>")
    fixture.wait("foo/")
    fixture.type(ending[1])
    fixture.wait("foo/" .. ending[2])
  end

  begin("Please recieve/")
  fixture.type(" ")
  fixture.wait("Please receive? ")
  fixture.type("<BS>")
  fixture.wait("Please recieve/")
  fixture.type(" ")
  fixture.wait("Please recieve/ ")
  fixture.type("teh ")
  fixture.wait("Please recieve/ the ")

  begin("café/")
  fixture.type(" ")
  fixture.wait("café? ")
  fixture.type("<BS>")
  fixture.wait("café/")

  for _, text in ipairs({
    "1/2", "1/", "foo1/", "foo/bar", "foo/bar/", "@rust/skill", "@rust/",
    "https://example.com/", "/model", "/foo/", "foo//", "foo /",
    "`foo/`", "`foo/", "[link](foo/)", "foo_bar/", "file.lua/", "C:\\foo/",
    "    foo/", "```lua\nfoo/",
  }) do
    begin(text)
    fixture.type(" ")
    fixture.wait(text .. " ")
  end
  begin("foo/")
  fixture.type("<Tab>")
  fixture.wait("foo/\t")

  begin("")
  fixture.exec([[vim.api.nvim_paste('foo/ ',true,-1)]])
  fixture.wait("foo/ ")

  begin("Please recieve/")
  fixture.exec("text_hold=true")
  fixture.type(" ")
  fixture.wait("Please recieve? ")
  fixture.type("<BS>")
  fixture.wait("Please recieve/")
  fixture.exec([[local request=text_requests[#text_requests]
    request.callback({decision='replace',occurrence=request.occurrence.id,
      original='recieve',replacement='receive',profile='fixture',profile_revision='revision'})]])
  fixture.wait("Please recieve/")
  assert(fixture.exec("return #text_errors") == 0)
end, debug.traceback)
fixture.close()
assert(ok, failure)
print("text_question_slash: delayed punctuation, rejection, spelling, and protected tokens passed")
vim.cmd("qa!")

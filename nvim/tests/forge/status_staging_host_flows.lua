local config = vim.fn.getcwd()
dofile(config .. "/nvim/tests/forge/fixtures/status_contention_manual.lua")
local client = require("forge.client")
local state = forge_manual.state

local function await(predicate, phase)
  assert(vim.wait(15000, predicate, 10), phase .. " timed out: " .. vim.inspect(forge_manual.notices))
end

local function key(value)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(value, true, false, true), "xt", false)
end

local function settled()
  return state.replica.status == "Applied" and #(state.replica.inventory.pending or {}) == 0
    and not state.pending and not state.request_active and next(client._client.pending) == nil
end

local function git(arguments)
  local result = vim.system(vim.list_extend({ "git", "-C", forge_manual.fixture }, arguments), { text = true, timeout = 10000 }):wait()
  assert(result.code == 0, result.stderr)
  return result.stdout:gsub("\r", "")
end

local function select_file(path, section)
  for _, file in ipairs(state.replica.inventory.file) do
    if file.path == path and file.section == section then
      local _, row = state.replica.sequence:position("file:" .. file.id)
      vim.api.nvim_win_set_cursor(0, { row + 1, 0 })
      return file, row
    end
  end
  error("missing " .. section .. " file " .. path)
end

local function expand(path, section)
  local file, row = select_file(path, section)
  if vim.fn.foldclosed(row + 1) >= 0 then key("<Tab>") end
  local context = "Source" .. tonumber(path:match("source_(%d+)")) .. ".render"
  await(function()
    local body = state.replica.file[file.id].body
    if not body or not settled() then return false end
    for _, block in pairs(body.block) do
      for _, text in ipairs(block.text) do
        if text:find(context, 1, true) then return true end
      end
    end
    return false
  end, "expand " .. path)
  return file
end

local function audit(phase)
  await(settled, phase)
  local previous, owned, installed = {}, 0, 0
  for _, file in ipairs(state.replica.inventory.file) do
    assert(not previous[file.section] or previous[file.section] < file.path, phase .. ": unstable file order")
    previous[file.section] = file.path
  end
  for id, handles in pairs(state.replica.marks) do
    local block = assert(state.replica.block[id], phase .. ": removed body owns marks")
    local _, start = state.replica.sequence:position(id)
    local expected = {}
    for _, gutter in ipairs(block.metadata.gutter or {}) do
      local row = start + gutter.position.row
      expected[row] = (expected[row] or 0) + 1
    end
    for _, handle in ipairs(handles) do
      local mark = vim.api.nvim_buf_get_extmark_by_id(state.replica.buffer, state.replica.namespace, handle, { details = true })
      if mark[3] and mark[3].virt_text then
        owned = owned + 1
        assert((expected[mark[1]] or 0) > 0, phase .. ": gutter outside source row or duplicated")
        expected[mark[1]] = expected[mark[1]] - 1
      end
    end
    for _, remaining in pairs(expected) do assert(remaining == 0, phase .. ": missing source gutter") end
  end
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(state.replica.buffer, state.replica.namespace, 0, -1, { details = true })) do
    if mark[4].virt_text then installed = installed + 1 end
  end
  assert(installed == owned, phase .. ": orphan gutter marks")
  for _, notice in ipairs(forge_manual.notices) do
    assert(notice.message:find("no Copilot OAuth token found", 1, true), phase .. ": " .. notice.message)
  end
end

local function index_is(expected, phase)
  audit(phase)
  assert(git({ "diff", "--cached", "--name-only" }) == expected, phase .. ": unexpected index paths")
end

local function select_change(file, text)
  local body = assert(state.replica.file[file.id].body)
  for id, block in pairs(body.block) do
    for offset, row in ipairs(block.text) do
      if row:find(text, 1, true) then
        local _, start = state.replica.sequence:position(id)
        vim.api.nvim_win_set_cursor(0, { start + offset, 0 })
        return
      end
    end
  end
  error("missing source change " .. text)
end

local ok, failure = xpcall(function()
  await(function() return state.replica.status == "Applied" end, "open")
  expand("source_01.rs", "unstaged")
  select_file("source_01.rs", "unstaged")
  key("S")
  index_is("source_01.rs\n", "stage expanded file")
  select_file("source_01.rs", "staged")
  key("U")
  index_is("", "unstage expanded file")
  local _, untouched_row = select_file("source_03.rs", "unstaged")
  assert(vim.fn.foldclosed(untouched_row + 1) == untouched_row + 1, "unstaging opened an untouched file")
  local _, closed_row = select_file("source_02.rs", "unstaged")
  assert(vim.fn.foldclosed(closed_row + 1) == closed_row + 1, "unstaging opened the adjacent file")
  expand("source_02.rs", "unstaged")
  select_file("source_01.rs", "unstaged")
  key("S")
  index_is("source_01.rs\n", "stage with adjacent file expanded")
  select_file("source_01.rs", "staged")
  key("U")
  index_is("", "unstage with adjacent file expanded")
  local _, expanded_row = select_file("source_02.rs", "unstaged")
  assert(vim.fn.foldclosed(expanded_row + 1) < 0, "unstaging closed an untouched expanded file")

  local file = expand("source_01.rs", "unstaged")
  select_change(file, "let value_31 = 1031;")
  key("S")
  index_is("source_01.rs\n", "stage hunk")
  assert(git({ "diff", "--cached", "--numstat" }) == "1\t1\tsource_01.rs\n", "hunk staged neighboring changes")
  local expected = git({ "show", "HEAD:source_01.rs" }):gsub("let value_31 = 31;", "let value_31 = 1031;")
  assert(git({ "show", ":source_01.rs" }) == expected, "hunk changed unrelated index bytes")
  file = expand("source_01.rs", "staged")
  select_change(file, "let value_31 = 1031;")
  key("U")
  index_is("", "unstage hunk")

  select_file("source_03.rs", "unstaged")
  if vim.fn.foldclosed(".") < 0 then key("<Tab>") end
  key("V")
  key("<Down><Down>")
  key("S")
  index_is("source_03.rs\nsource_04.rs\nsource_05.rs\n", "stage visual file group")
  select_file("source_03.rs", "staged")
  key("V")
  key("<Down><Down>")
  key("U")
  index_is("", "unstage visual file group")

  select_file("source_08.rs", "unstaged")
  key("<Tab>S")
  index_is("source_08.rs\n", "stage during body demand")
  select_file("source_08.rs", "staged")
  key("U")
  index_is("", "unstage after overlapping demand")

  local _, section = state.replica.sequence:position("section:unstaged")
  vim.api.nvim_win_set_cursor(0, { section + 2, 0 })
  key("S")
  local paths = {}
  for ordinal = 1, 72 do paths[#paths + 1] = ("source_%02d.rs\n"):format(ordinal) end
  index_is(table.concat(paths), "stage whole section")
  assert(git({ "diff", "--name-only" }) == "", "section left unstaged bytes")
  _, section = state.replica.sequence:position("section:staged")
  vim.api.nvim_win_set_cursor(0, { section + 2, 0 })
  key("U")
  index_is("", "unstage whole section")
  _, section = state.replica.sequence:position("section:unstaged")
  assert(vim.fn.foldclosed(section + 2) < 0, "replacement section inherited a closed fold")
  print("host flows: file, hunk, visual group, and section stage/unstage passed")
end, debug.traceback)
require("forge.status").close(state)
client.stop()
assert(vim.wait(6000, function() return client._client.process == nil end, 20), "host did not exit")
vim.fn.chdir(config)
assert(ok, failure)

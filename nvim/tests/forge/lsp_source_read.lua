local source = require("forge.lsp.source")
local directory = vim.fs.joinpath(vim.fn.getcwd(), ".forge-source-read-" .. tostring(vim.uv.hrtime()))
vim.fn.mkdir(directory, "p")
local path = vim.fs.joinpath(directory, "source.rs")
local contents = string.rep("α source\n", 1024 * 1024) .. "last source byte"

---@param filename string
---@return string?, string?
local function read(filename)
  local completed, result, failure = false, nil, nil
  source.read(filename, function(text, message)
    assert(not completed, "source read completed more than once")
    completed, result, failure = true, text, message
  end)
  assert(vim.wait(10000, function() return completed end, 5), "source read did not finish")
  return result, failure
end

local original_read, original_stat = vim.uv.fs_read, vim.uv.fs_fstat
local success, failure = xpcall(function()
  local descriptor = assert(vim.uv.fs_open(path, "w", 384))
  assert(vim.uv.fs_write(descriptor, contents, 0) == #contents)
  assert(vim.uv.fs_close(descriptor))
  local result, message = read(path)
  assert(not message and result == contents, "large source lost content")

  local reads = 0
  vim.uv.fs_fstat = function(file, receive)
    return original_stat(file, function(error, stat)
      if stat then stat.size = 1 end
      receive(error, stat)
    end)
  end
  vim.uv.fs_read = function(file, length, offset, receive)
    reads = reads + 1
    return original_read(file, math.min(length, 32 * 1024), offset, receive)
  end
  result, message = read(path)
  assert(not message and result == contents, "short reads or stale metadata truncated source")
  assert(reads > 256, "source did not continue beyond its initial metadata size")

  vim.uv.fs_read = function(_, _, _, receive)
    vim.schedule(function() receive("EIO: fixture read failure", nil) end)
  end
  result, message = read(path)
  assert(not result and message:find("EIO: fixture read failure", 1, true), "read failure lost its cause")
end, debug.traceback)
vim.uv.fs_read, vim.uv.fs_fstat = original_read, original_stat
if success then
  success, failure = xpcall(function()
    vim.fn.writefile({ "first" }, path)
    local buffer, owned = source.open(path, "first\n", "rust")
    assert(owned)
    source.retain(buffer, owned)
    source.release(buffer)
    assert(source.loaded(path) == buffer, "released source should remain reusable")
    local tick = vim.api.nvim_buf_get_changedtick(buffer)
    local function refresh()
      local done, error = false, nil
      source.refresh(buffer, function(message) done, error = true, message end)
      assert(vim.wait(1000, function() return done end, 5))
      assert(not error, error)
    end
    refresh()
    assert(vim.api.nvim_buf_get_changedtick(buffer) == tick, "unchanged disk source was rewritten")
    vim.fn.writefile({ "second" }, path)
    refresh()
    assert(vim.api.nvim_buf_get_lines(buffer, 0, -1, false)[1] == "second")
    vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { "unsaved" })
    refresh()
    assert(vim.api.nvim_buf_get_lines(buffer, 0, -1, false)[1] == "unsaved", "refresh replaced unsaved source")
    vim.api.nvim_buf_delete(buffer, { force = true })
    source.close()
  end, debug.traceback)
end
vim.fn.delete(directory, "rf")
assert(success, failure)
print("lsp_source_read: passed")

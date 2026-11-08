-- Unit tests for `forge.lsp.source` (source.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local buffer_support = dofile("nvim/lua/forge/tests/support/buffer.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({
  hooks = isolation.hooks({
    post_case = function()
      local source = package.loaded["forge.lsp.source"]
      if source then source.close() end
    end,
  }),
})

T["lsp_source_read"] = function()
  local source = require("forge.lsp.source")
  local directory = temp.directory("source-read")
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
    wait_until(function() return completed end, "source read did not finish", { timeout_ms = 4000 })
    return result, failure
  end

  local original_read, original_stat = vim.uv.fs_read, vim.uv.fs_fstat
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
  vim.uv.fs_read, vim.uv.fs_fstat = original_read, original_stat
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
    wait_until(function() return done end, "source refresh did not finish")
    assert(not error, error)
  end
  refresh()
  assert(vim.api.nvim_buf_get_changedtick(buffer) == tick, "unchanged disk source was rewritten")
  vim.fn.writefile({ "second" }, path)
  refresh()
  eq(buffer_support.line(buffer, 1), "second")
  buffer_support.set(buffer, { "unsaved" })
  refresh()
  eq(buffer_support.line(buffer, 1), "unsaved", { fail_reason = "refresh replaced unsaved source" })
  vim.api.nvim_buf_delete(buffer, { force = true })
  source.close()
end

return T

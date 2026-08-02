package.path = "nvim/lua/?.lua;nvim/lua/?/init.lua;" .. package.path

local ignored_path_store = require("diff_review.views.status.ignored_path_store")

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function assert_equal(actual, expected, message)
  if not vim.deep_equal(actual, expected) then
    error(("%s\nexpected: %s\nactual: %s"):format(message, vim.inspect(expected), vim.inspect(actual)), 2)
  end
end

---@return DiffReviewIgnoredPathIo, fun(): string?
local function memory_io()
  local stored_content = nil ---@type string?
  local backend = {
    read_async = function(_, callback) callback(stored_content, nil) end,
    write_atomic_async = function(_, content, callback)
      stored_content = content
      callback(nil)
    end,
  } ---@type DiffReviewIgnoredPathIo
  return backend, function() return stored_content end
end

local function load(root)
  local result = nil ---@type boolean?
  ignored_path_store.load_async(root, function(ok) result = ok end)
  assert_true(vim.wait(3000, function() return result ~= nil end, 10), "ignored-path load did not complete")
  return result
end

local function assert_real_io_round_trip()
  local root = "D:/real-io-repo"
  local data_dir = vim.fn.tempname()
  ignored_path_store.reset_for_test()
  ignored_path_store.set_data_dir_for_test(data_dir)
  assert_true(load(root), "real ignored-path store failed its empty load")
  ignored_path_store.ignore_paths(root, { "persisted.lua" })
  local store_path = ignored_path_store._store_path(root)
  assert_true(vim.wait(3000, function() return vim.uv.fs_stat(store_path) ~= nil end, 10), "real ignored-path write did not finish")

  ignored_path_store.reset_for_test()
  ignored_path_store.set_data_dir_for_test(data_dir)
  assert_true(load(root), "real ignored-path payload failed to reload")
  assert_true(ignored_path_store.contains(root, "persisted.lua"), "real ignored-path payload lost its marker")
  ignored_path_store.reset_for_test()
  vim.fn.delete(data_dir, "rf")
end

local function assert_persistence_and_transactional_stage()
  local root = "D:/repo"
  local backend, stored_content = memory_io()
  ignored_path_store.reset_for_test()
  ignored_path_store.set_io_for_test(backend)
  assert_true(load(root), "empty ignored-path state failed to load")

  assert_true(ignored_path_store.ignore_paths(root, { "src\\alpha.lua", "src/beta.lua" }), "ignore did not change state")
  assert_true(ignored_path_store.contains(root, "src/alpha.lua"), "ignored path did not normalize separators")
  assert_true(stored_content() ~= nil, "ignore did not persist state")

  ignored_path_store.begin_stage(root, 7, { "src/alpha.lua", "src/beta.lua" })
  local suppressed = ignored_path_store.project(root, {
    {
      name = "unstaged",
      title = "Unstaged changes",
      files = {
        { filename = "D:/repo/src/alpha.lua", relpath = "src/alpha.lua", section_name = "unstaged", hunks = {} },
      },
    },
  })
  assert_true(suppressed[1].name == "unstaged", "pending stage did not suppress the virtual marker")
  ignored_path_store.finish_stage(root, 7, { "src/alpha.lua" })
  assert_true(not ignored_path_store.contains(root, "src/alpha.lua"), "completed stage retained its ignored marker")
  assert_true(ignored_path_store.contains(root, "src/beta.lua"), "partial stage removed a failed target marker")

  ignored_path_store.begin_stage(root, 8, { "src/beta.lua" })
  ignored_path_store.cancel_stage(root, 8)
  assert_true(ignored_path_store.contains(root, "src/beta.lua"), "cancelled stage removed its ignored marker")
  local restored = ignored_path_store.project(root, {
    {
      name = "unstaged",
      title = "Unstaged changes",
      files = {
        { filename = "D:/repo/src/beta.lua", relpath = "src/beta.lua", section_name = "unstaged", hunks = {} },
      },
    },
  })
  assert_true(restored[1].name == "ignored", "cancelled stage did not restore the virtual marker")

  local persisted = stored_content()
  ignored_path_store.reset_for_test()
  ignored_path_store.set_io_for_test({
    read_async = function(_, callback) callback(persisted, nil) end,
    write_atomic_async = backend.write_atomic_async,
  })
  assert_true(load(root), "persisted ignored-path state failed to reload")
  assert_true(ignored_path_store.contains(root, "src/beta.lua"), "ignored marker did not survive restart")
  assert_true(not ignored_path_store.contains(root, "src/alpha.lua"), "completed staged marker returned after restart")
end

local function assert_stale_payload_stays_hidden()
  local root = "D:/repo"
  ignored_path_store.reset_for_test()
  ignored_path_store.set_io_for_test({
    read_async = function(_, callback)
      callback(vim.json.encode({ version = 0, root = "d:/repo", ignored_paths = { "stale.lua" } }), nil)
    end,
    write_atomic_async = function(_, _, callback) callback(nil) end,
  })
  assert_true(not load(root), "stale ignored-path payload reported success")
  assert_true(not ignored_path_store.contains(root, "stale.lua"), "stale ignored-path payload became visible")
end

local function assert_writes_coalesce_to_latest_generation()
  local root = "D:/coalesced-repo"
  local write_content_list = {} ---@type string[]
  local write_callback_list = {} ---@type (fun(error?: string))[]
  ignored_path_store.reset_for_test()
  ignored_path_store.set_io_for_test({
    read_async = function(_, callback) callback(nil, nil) end,
    write_atomic_async = function(_, content, callback)
      write_content_list[#write_content_list + 1] = content
      write_callback_list[#write_callback_list + 1] = callback
    end,
  })
  assert_true(load(root), "coalesced ignored-path state failed to load")
  ignored_path_store.ignore_paths(root, { "first.lua" })
  ignored_path_store.ignore_paths(root, { "second.lua" })
  assert_equal(#write_content_list, 1, "concurrent marker changes started parallel writes")
  write_callback_list[1]("first write failed")
  assert_equal(#write_content_list, 2, "newer marker generation did not retry after an older write failed")
  local latest_payload = vim.json.decode(write_content_list[2])
  assert_equal(latest_payload.ignored_paths, { "first.lua", "second.lua" }, "coalesced write lost the latest markers")
  write_callback_list[2](nil)
end

local function run()
  assert_persistence_and_transactional_stage()
  assert_stale_payload_stays_hidden()
  assert_writes_coalesce_to_latest_generation()
  assert_real_io_round_trip()
  ignored_path_store.reset_for_test()
end

local ok, err = xpcall(run, debug.traceback)
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")

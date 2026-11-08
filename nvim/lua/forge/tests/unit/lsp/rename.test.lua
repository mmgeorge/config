local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local buffer_support = dofile("nvim/lua/forge/tests/support/buffer.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks({
  post_case = function() require("forge.lsp.rename").close() end,
}) })

-- A Rust workspace whose `item.rs` is open in an editor buffer, served by a mock language server
-- that renames `value` and answers `willRenameFiles` with the module references in `lib.rs`. The
-- mock tracks attachment so a case can see the rename detach and reattach the buffer.
local function rename_workspace()
  local workspace = temp.directory("workspace")
  vim.fn.mkdir(workspace .. "/src", "p")
  vim.fn.writefile({ "pub mod item;", "pub fn call() -> u32 { item::value() }" }, workspace .. "/src/lib.rs")
  vim.fn.writefile({ "pub fn value() -> u32 { 7 }" }, workspace .. "/src/item.rs")
  local scene = { workspace = workspace, item = workspace .. "/src/item.rs", lib = workspace .. "/src/lib.rs",
    renamed = workspace .. "/src/renamed.rs", notifications = {} }
  local mock = { id = 8901, name = "rename-fixture", initialized = true, root_dir = workspace,
    config = { root_dir = workspace, filetypes = { "rust" } }, offset_encoding = "utf-16",
    server_capabilities = { workspace = { fileOperations = {
      willRename = { filters = { { pattern = { glob = "**/*.rs" } } } },
      didRename = { filters = { { pattern = { glob = "**/*.rs" } } } },
    } } } }
  function mock:is_stopped() return false end
  function mock:supports_method() return true end
  function mock:cancel_request() end
  function mock:notify(method, params) scene.notifications[#scene.notifications + 1] = { method = method, params = params } end
  function mock:request(method, params, callback)
    vim.schedule(function()
      if method == "textDocument/prepareRename" then callback(nil, { defaultBehavior = true })
      elseif method == "textDocument/rename" then callback(nil, { changes = {
        [vim.uri_from_fname(scene.item)] = { { range = { start = { line = 0, character = 7 }, ["end"] = { line = 0, character = 12 } }, newText = params.newName } },
        [vim.uri_from_fname(scene.lib)] = { { range = { start = { line = 1, character = 29 }, ["end"] = { line = 1, character = 34 } }, newText = params.newName } },
      } })
      elseif method == "workspace/willRenameFiles" then callback(nil, { changes = {
        [vim.uri_from_fname(scene.lib)] = {
          { range = { start = { line = 0, character = 8 }, ["end"] = { line = 0, character = 12 } }, newText = "renamed" },
          { range = { start = { line = 1, character = 23 }, ["end"] = { line = 1, character = 27 } }, newText = "renamed" },
        },
      } })
      else callback({ message = "unsupported fixture request: " .. method }) end
    end)
    return true, 1
  end
  scene.mock = mock
  stub(require("forge.lsp.startup"), "wait", function(_, _, callback) callback() return function() end end)
  stub(vim.lsp, "get_clients", function() return { mock } end)
  stub(vim.lsp, "get_client_by_id", function(id) return id == mock.id and mock or nil end)
  scene.buffer = vim.fn.bufadd(scene.item)
  vim.fn.bufload(scene.buffer)
  vim.bo[scene.buffer].filetype = "rust"
  local attached = { [scene.buffer] = true }
  scene.attached = attached
  stub(vim.lsp, "buf_is_attached", function(bufnr, client_id)
    return (client_id == nil or client_id == mock.id) and attached[bufnr] == true
  end)
  stub(vim.lsp, "buf_attach_client", function(bufnr, client_id)
    if client_id == mock.id then attached[bufnr] = true end
    return true
  end)
  stub(vim.lsp, "buf_detach_client", function(bufnr, client_id)
    if client_id == mock.id then attached[bufnr] = nil end
  end)
  return scene
end

-- Prepares `request` with every path authorized and an empty structural analysis, and returns
-- the prepared operations.
local function prepare(scene, id, request)
  local result, failure, done
  require("forge.lsp.rename").prepare(id, scene.workspace, request,
    function(paths, receive)
      local allowed = {}
      for _, path in ipairs(paths) do allowed[path] = true end
      receive(allowed)
    end,
    function(_, _, receive) receive({ declarations = {}, complete = true }) end,
    function(value, error) result, failure, done = value, error, true end)
  wait_until(function() return done end, "rename preparation did not finish")
  assert(not failure, failure)
  return result.operations
end

local function text_operation(operations, path)
  for _, operation in ipairs(operations) do
    if operation.kind == "text" and vim.fs.normalize(operation.path) == vim.fs.normalize(path) then return operation end
  end
  error("no text operation for " .. path .. ": " .. vim.inspect(operations))
end

T["symbol_rename_prepares_edits_without_writing"] = function()
  local scene = rename_workspace()
  local operations = prepare(scene, "symbol-rename", { kind = "symbol", new_name = "count",
    target = { path = scene.item, line = 1, column = 8 } })
  eq(text_operation(operations, scene.item).after, "pub fn count() -> u32 { 7 }\n")
  eq(text_operation(operations, scene.lib).after, "pub mod item;\npub fn call() -> u32 { item::count() }\n")
  eq(vim.fn.readfile(scene.item), { "pub fn value() -> u32 { 7 }" }, { fail_reason = "preparation wrote the source file" })
  assert(not vim.bo[scene.buffer].modifiable, "a prepared rename left its source buffer editable")
  require("forge.lsp.rename").release("symbol-rename", false)
  assert(vim.bo[scene.buffer].modifiable, "releasing an uncommitted rename did not restore editing")
  eq(buffer_support.line(scene.buffer, 1), "pub fn value() -> u32 { 7 }", { fail_reason = "an uncommitted rename changed the editor" })
end

T["file_rename_applies_reference_edits_and_follows_the_buffer"] = function()
  local scene = rename_workspace()
  local operations = prepare(scene, "file-rename", { kind = "file", old_path = scene.item, new_path = scene.renamed })
  local library = text_operation(operations, scene.lib).after
  assert(library:find("pub mod renamed;", 1, true) and library:find("renamed::value()", 1, true),
    "file rename did not apply the language server's reference edits: " .. library)
  -- The host commits the prepared operations to disk before it releases the hold.
  assert(vim.uv.fs_rename(scene.item, scene.renamed))
  vim.fn.writefile(vim.split(library, "\n", { plain = true, trimempty = true }), scene.lib)
  require("forge.lsp.rename").release("file-rename", true)
  assert(vim.api.nvim_buf_get_name(scene.buffer):gsub("\\", "/"):find("/renamed.rs", 1, true), "buffer path did not follow the file")
  wait_until(function() return scene.attached[scene.buffer] end, "renamed buffer lost its LSP attachment")
  eq(scene.notifications[1] and scene.notifications[1].method, "workspace/didRenameFiles",
    { fail_reason = "committed rename did not notify the language server" })
end

return T

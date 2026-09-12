vim.loader.enable(false)

local grammar_root = "nvim/rust/forge/crates/forge-diff/grammar"
local query_root = "nvim/rust/forge/crates/forge-diff/query"
local grammar_manifest = vim.json.decode(table.concat(vim.fn.readfile(vim.fs.joinpath(grammar_root, "manifest.json")), "\n"))
local query_manifest = vim.json.decode(table.concat(vim.fn.readfile(vim.fs.joinpath(query_root, "manifest.json")), "\n"))

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function file_text(path)
  return table.concat(vim.fn.readfile(path, "b"), "\n")
end

local function file_sha256(path)
  return vim.fn.sha256(file_text(path))
end

local function capture_count(text)
  local count = 0
  for _ in text:gmatch("@[%w%._%-]+") do count = count + 1 end
  return count
end

local started = vim.uv.hrtime()
local grammar_name = {}
local grammar_file_count = 0
local grammar_bytes = 0
for _, grammar in ipairs(grammar_manifest.grammar or {}) do
  assert_true(type(grammar.name) == "string" and not grammar_name[grammar.name], "duplicate grammar name")
  grammar_name[grammar.name] = true
  for _, entry in ipairs(grammar.files or {}) do
    local source_root = vim.fs.joinpath(grammar_root, grammar.name)
    local path = vim.fs.joinpath(source_root, entry.path)
    if vim.uv.fs_stat(path) == nil and grammar.location and grammar.location ~= "" then
      path = vim.fs.joinpath(source_root, grammar.location, entry.path)
    end
    assert_true(vim.uv.fs_stat(path) ~= nil, "missing grammar asset: " .. path)
    assert_true(file_sha256(path) == entry.sha256, "grammar digest differs: " .. path)
    grammar_file_count = grammar_file_count + 1
    grammar_bytes = grammar_bytes + #(file_text(path))
  end
end
assert_true(vim.tbl_count(grammar_name) == 22, "expected exactly 22 configured grammars")

local query_file_count = 0
local query_bytes = 0
local query_captures = 0
for _, entry in ipairs(query_manifest.file or {}) do
  local relative_path = entry.path:gsub("^query/", "")
  local path = vim.fs.joinpath(query_root, relative_path)
  assert_true(vim.uv.fs_stat(path) ~= nil, "missing query asset: " .. path)
  local text = file_text(path)
  assert_true(vim.fn.sha256(text) == entry.sha256, "query digest differs: " .. path)
  local captures = capture_count(text)
  assert_true(captures <= 65536, "query capture admission exceeds native limit: " .. path)
  query_file_count = query_file_count + 1
  query_bytes = query_bytes + #text
  query_captures = query_captures + captures
end

local aliases = {
  "rust", "typescript", "ts", "tsx", "typescriptreact", "lua", "vim", "vimdoc", "help", "json", "query",
  "javascript", "js", "javascriptreact", "css", "html", "wgsl", "wgslx", "glsl", "frag", "vert", "c_sharp",
  "cs", "csharp", "toml", "slang", "shaderslang", "yaml", "nu", "markdown", "markdown_inline", "latex", "tex", "cue",
}
local asset_source = file_text("nvim/rust/forge/crates/forge-diff/src/syntax/assets.rs")
for _, alias in ipairs(aliases) do
  assert_true(asset_source:find('"' .. alias .. '"', 1, true) ~= nil, "native alias is absent: " .. alias)
end

local elapsed_us = math.floor((vim.uv.hrtime() - started) / 1000)
print(vim.json.encode({
  fixture = "syntax_resource_acceptance",
  grammars = vim.tbl_count(grammar_name),
  aliases = #aliases,
  grammar_files = grammar_file_count,
  grammar_bytes = grammar_bytes,
  query_files = query_file_count,
  query_bytes = query_bytes,
  query_capture_occurrences = query_captures,
  elapsed_us = elapsed_us,
}))
vim.cmd("qa!")

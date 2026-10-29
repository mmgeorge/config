vim.loader.enable(false)
local fixture = vim.fn.tempname()
assert(vim.fn.mkdir(fixture, "p") == 1)
local client = require("forge.client")
local function git(arguments)
  local result = vim.system(vim.list_extend({ "git", "-C", fixture }, arguments), { text = true, timeout = 10000 }):wait()
  assert(result.code == 0, result.stderr)
end
---@param comparison string
---@param operation? string
---@param generation? ForgeTextGenerationConfig
---@return table? response
---@return string? failure
local function request(comparison, operation, generation)
  local response, failure, completed
  client.request({ method = "repository.generation", params = { operation = operation or "generate", workspace = fixture, comparison = comparison,
    model = operation ~= "preview" and not generation and { provider = "fixture", model = "fixture" } or nil,
    generation = generation } }, function(result, error)
      response, failure, completed = result, error, true
    end)
  assert(vim.wait(10000, function() return completed end, 10), "generation host did not respond")
  return response, failure
end
local ok, failure = xpcall(function()
  git({ "init", "--quiet" })
  git({ "config", "user.name", "Forge Fixture" })
  git({ "config", "user.email", "forge@example.invalid" })
  vim.fn.writefile({ "before" }, fixture .. "/tracked.txt")
  git({ "add", "." })
  git({ "commit", "--quiet", "-m", "initial" })
  vim.fn.writefile({ "after" }, fixture .. "/tracked.txt")
  vim.fn.writefile({ "new" }, fixture .. "/untracked.txt")
  local preview, preview_error = request("head", "preview")
  assert(not preview_error and preview.state == "ready" and preview.prompt, vim.inspect({ preview, preview_error }))
  assert(preview.prompt.user:find("tracked.txt", 1, true) and preview.prompt.user:find("untracked.txt", 1, true))
  assert(preview.prompt.input_bytes == #preview.prompt.system + #preview.prompt.user)
  assert(preview.prompt.limit_bytes == 500000 and preview.prompt.input_bytes <= preview.prompt.limit_bytes)
  assert(not client._client.harness_ready, "prompt preview initialized a conversation")
  local result, error = request("head")
  assert(not result and error == "unsupported detached generation provider fixture", vim.inspect({ result, error }))
  result, error = request("staged")
  assert(not error and result.state == "none", vim.inspect({ result, error }))
  preview, preview_error = request("staged", "preview")
  assert(not preview_error and preview.state == "none" and not preview.prompt, vim.inspect({ preview, preview_error }))
  git({ "add", "tracked.txt" })
  preview, preview_error = request("staged", "preview")
  assert(not preview_error and preview.prompt.user:find("tracked.txt", 1, true))
  assert(not preview.prompt.user:find("untracked.txt", 1, true))
  preview, preview_error = request("latest_commit", "preview")
  assert(not preview_error and preview.prompt.user:find("+ before", 1, true))
  assert(not preview.prompt.user:find("+ after", 1, true))
  result, error = request("staged")
  assert(not result and error == "unsupported detached generation provider fixture", vim.inspect({ result, error }))
  local generation = {
    backend = { kind = "mock", command = { "mock" } }, model = "fixture-model", data_root = fixture .. "/generation-data",
  }
  local rejected, rejection = request("head", "preview", generation)
  assert(not rejected and rejection:find("prompt preview does not accept provider configuration", 1, true), rejection)
  for _, comparison in ipairs({ "staged", "head", "latest_commit" }) do
    result, error = request(comparison, "generate", generation)
    assert(not error and result and result.state == "ready", vim.inspect({ comparison, result, error }))
    assert(result.message == "feat: describe repository changes", vim.inspect(result))
    assert(not result.prompt, "generation response exposed preview data")
    assert(result.diff_pairs == (comparison == "head" and 2 or 1), vim.inspect(result))
  end
  assert(not client._client.harness_ready, "standalone generation initialized a conversation")
  result, error = request("head", "fingerprint")
  assert(not result and error:find("unknown detached generation operation", 1, true), "obsolete fingerprint request remains supported")
  local specification = dofile("nvim/lua/plugins/forge.lua")[1]
  specification.config(nil, { about_auto_generate = false })
  local github = require("forge.integrations.gh")
  github.prs_for_branch_async = function(_, _, _, callback) callback({ ok = true, prs = {} }) end
  github.current_pr_async = function(_, callback) callback({ ok = true }) end
  local status = require("forge.status")
  local state = status.open({ workspace = fixture })
  assert(vim.wait(10000, function() return state.ready end, 10), "status did not open")
  assert(vim.b.forge_workspace == fixture)
  vim.cmd("ForgeDebugCommitPromptPreview")
  local buffer = vim.api.nvim_get_current_buf()
  assert(vim.wait(10000, function() return vim.b[buffer].forge_commit_prompt_input ~= nil end, 10), "preview command did not render")
  local rendered = table.concat(vim.api.nvim_buf_get_lines(buffer, 0, -1, false), "\n")
  assert(rendered:find("SYSTEM\n", 1, true) and rendered:find("USER\n", 1, true))
  assert(rendered:find("untracked.txt", 1, true) and not vim.bo[buffer].modifiable and vim.bo[buffer].readonly)
  vim.api.nvim_buf_delete(buffer, { force = true })
  status.close(state)
  local chain = { "#[allow(dead_code)]", "fn run() {", "    let result = values", "        .iter()",
    "        .filter(|value| value.active())", "        .map(|value| transform(value))", "        .collect();", "}" }
  vim.fn.writefile({ "#[allow(dead_code)]", "fn run() {", "    previous();", "}",
    "fn removed() {", "    hidden_removed_body();", "    another_hidden_removed_body();", "}" }, fixture .. "/chain.rs")
  git({ "add", "chain.rs" })
  git({ "commit", "--quiet", "-m", "chain baseline", "--only", "--", "chain.rs" })
  vim.fn.writefile(chain, fixture .. "/chain.rs")
  preview, preview_error = request("head", "preview")
  local ordered = {}
  for position = 3, 7 do ordered[#ordered + 1] = "+ " .. chain[position] end
  assert(not preview_error and preview.prompt.user:find(table.concat(ordered, "\n"), 1, true), "native prompt split a method chain")
  assert(preview.prompt.user:find("- fn removed() {\n", 1, true))
  assert(not preview.prompt.user:find("hidden_removed_body", 1, true))
  vim.fn.writefile({ "#[test]", "fn synchronous_case() {", "    hidden_sync_body();", "}",
    '#[tokio::test(flavor = "current_thread")]', "async fn asynchronous_case() { hidden_async_body(); }",
    "fn helper() {", "    visible_helper_body();", "}" }, fixture .. "/case_test.rs")
  preview, preview_error = request("head", "preview")
  assert(not preview_error, preview_error)
  assert(preview.prompt.user:find("+ #[test]\n+ fn synchronous_case() {\n  ... 2 lines (+2 -0)", 1, true))
  assert(preview.prompt.user:find("+ async fn asynchronous_case() {\n", 1, true))
  assert(not preview.prompt.user:find("hidden_sync_body", 1, true) and not preview.prompt.user:find("hidden_async_body", 1, true))
  assert(preview.prompt.user:find("+ fn helper() {\n", 1, true))
  assert(not preview.prompt.user:find("visible_helper_body", 1, true))
  assert(not preview.prompt.user:find("lines omitted", 1, true))
  git({ "add", "case_test.rs" })
  git({ "commit", "--quiet", "-m", "test baseline", "--only", "--", "case_test.rs" })
  local cases = vim.fn.readfile(fixture .. "/case_test.rs")
  for position, line in ipairs(cases) do
    cases[position] = line:gsub("visible_helper_body", "changed_helper_body")
      :gsub("hidden_sync_body", "changed_sync_body"):gsub("hidden_async_body", "changed_async_body")
  end
  vim.fn.writefile(cases, fixture .. "/case_test.rs")
  preview, preview_error = request("head", "preview")
  assert(not preview_error and preview.prompt.user:find("-     visible_helper_body();\n+     changed_helper_body();", 1, true))
  assert(preview.prompt.user:find("-     hidden_sync_body();\n+     changed_sync_body();", 1, true))
  assert(preview.prompt.user:find("+ async fn asynchronous_case() { changed_async_body(); }", 1, true))
  local inventory_only = { "LICENSE", "LICENSE.md", "Cargo.lock", "package-lock.json",
    "pnpm-lock.yaml", "Package.resolved", "pylock.production.toml" }
  for _, path in ipairs(inventory_only) do
    vim.fn.writefile({ "inventory_hidden_marker" }, fixture .. "/" .. path)
  end
  preview, preview_error = request("head", "preview")
  assert(not preview_error, preview_error)
  for _, path in ipairs(inventory_only) do
    assert(preview.prompt.user:find("  " .. path .. " (+1 -0", 1, true), path .. "\n" .. preview.prompt.user)
    assert(not preview.prompt.user:find("\n" .. path .. " (", 1, true), path)
  end
  assert(not preview.prompt.user:find("inventory_hidden_marker", 1, true))
  vim.fn.writefile({ "fn target(value: i32) -> i32 {", "    value", "}" }, fixture .. "/paired.rs")
  git({ "add", "paired.rs" })
  git({ "commit", "--quiet", "-m", "signature baseline", "--only", "--", "paired.rs" })
  vim.fn.writefile({ "fn helper() {", "    first();", "    second();", "}",
    "fn target(", "    value: i64,", ") -> i64 {", "    value", "}" }, fixture .. "/paired.rs")
  preview, preview_error = request("head", "preview")
  assert(not preview_error, preview_error)
  local signature_pair = "- fn target(value: i32) -> i32 {\n+ fn target(\n+     value: i64,\n+ ) -> i64 {\n"
  assert(preview.prompt.user:find(signature_pair, 1, true), "old and new signatures were separated")
  local grammar = { "const rules = {" }
  for position = 1, 30 do
    grammar[#grammar + 1] = "    rule_" .. position .. ": $ => seq($.identifier),"
    grammar[#grammar + 1] = ""
    grammar[#grammar + 1] = " \t"
  end
  table.remove(grammar)
  table.remove(grammar)
  grammar[#grammar + 1] = "};"
  vim.fn.writefile(grammar, fixture .. "/grammar.js")
  local entries = {}
  for position = 1, 21 do entries[position] = "entry_" .. position end
  vim.fn.writefile(entries, fixture .. "/rows.unknown")
  preview, preview_error = request("head", "preview")
  assert(not preview_error, preview_error)
  assert(preview.prompt.user:find("+     rule_30: $ =>\n", 1, true))
  assert(preview.prompt.user:find("+ entry_21\n", 1, true))
  assert(not preview.prompt.user:find("\n  ... 1 lines (", 1, true))
  local grammar_excerpt = assert(preview.prompt.user:match("\ngrammar%.js [^\n]+\n\n(.-)\n\n"))
  assert(not grammar_excerpt:find("  ... ", 1, true), grammar_excerpt)
  local array_path = fixture .. "/array.h"
  local array = vim.fn.readfile("nvim/rust/forge/crates/forge-diff/tests/fixtures/c_headers/array.h")
  vim.fn.writefile(array, array_path)
  preview, preview_error = request("head", "preview")
  assert(not preview_error, preview_error)
  assert(preview.prompt.user:find("+ static inline void _array__grow(", 1, true), "recovered C header lost signatures")
  assert(not preview.prompt.user:find("ts_free(self->contents)", 1, true), "added C function retained its body")
  assert(not preview.prompt.user:find("+ #include", 1, true), "recovered C header retained includes")
  git({ "add", "array.h" })
  git({ "commit", "--quiet", "-m", "C header baseline", "--only", "--", "array.h" })
  for position, line in ipairs(array) do
    array[position] = line:gsub("self%->capacity %* 2", "self->capacity * 3")
  end
  vim.fn.writefile(array, array_path)
  preview, preview_error = request("head", "preview")
  assert(not preview_error, preview_error)
  assert(preview.prompt.user:find("  static inline void _array__grow(", 1, true), "modified C function lost its signature")
  assert(preview.prompt.user:find("+     uint32_t new_capacity = self->capacity * 3;", 1, true), "modified C function lost its body change")
  local renamed_source = { "fn renamed_file_marker() {" }
  for _ = 1, 30 do renamed_source[#renamed_source + 1] = "    original_rename_body();" end
  renamed_source[#renamed_source + 1] = "}"
  vim.fn.writefile(renamed_source, fixture .. "/rename_source.rs")
  git({ "add", "rename_source.rs" })
  git({ "commit", "--quiet", "-m", "rename baseline", "--only", "--", "rename_source.rs" })
  git({ "mv", "rename_source.rs", "renamed.rs" })
  renamed_source[2] = "    changed_rename_body();"
  vim.fn.writefile(renamed_source, fixture .. "/renamed.rs")
  git({ "add", "renamed.rs" })
  for _, comparison in ipairs({ "head", "staged", "latest_commit" }) do
    if comparison == "latest_commit" then
      git({ "commit", "--quiet", "-m", "rename", "--only", "--", "rename_source.rs", "renamed.rs" })
    end
    vim.b.forge_workspace = fixture
    vim.cmd("ForgeDebugCommitPromptPreview " .. comparison)
    local renamed_buffer = vim.api.nvim_get_current_buf()
    assert(vim.wait(10000, function() return vim.b[renamed_buffer].forge_commit_prompt_input ~= nil end, 10))
    local renamed_prompt = vim.b[renamed_buffer].forge_commit_prompt_input.user
    assert(renamed_prompt:find("  renamed.rs (Renamed from rename_source.rs)\n", 1, true), renamed_prompt)
    assert(not renamed_prompt:find("renamed_file_marker", 1, true) and not renamed_prompt:find("changed_rename_body", 1, true))
    local renamed_lines = vim.api.nvim_buf_get_lines(renamed_buffer, 0, -1, false)
    assert(vim.tbl_contains(renamed_lines, "  renamed.rs (Renamed from rename_source.rs)"))
    vim.api.nvim_buf_delete(renamed_buffer, { force = true })
  end
  local large = {}
  for position = 1, 6000 do large[position] = string.rep('\\"', 50) end
  vim.fn.writefile(large, fixture .. "/large.md")
  preview, preview_error = request("head", "preview")
  assert(not preview_error and preview.prompt.input_bytes > 400000 and preview.prompt.input_bytes <= 500000,
    vim.inspect({ preview_error, preview and preview.state }))
  assert(#preview.prompt.user + #preview.prompt.system == preview.prompt.input_bytes)
  assert(not client._client.harness_ready, "preview command initialized a conversation")
end, debug.traceback)
client.host_stop()
assert(vim.wait(6000, function() return client._client.process == nil end, 20), "host did not stop")
vim.fn.delete(fixture, "rf")
assert(ok, failure)
print("native generation and prompt preview: scopes, command, byte cap, multipart transport, and provider isolation OK")

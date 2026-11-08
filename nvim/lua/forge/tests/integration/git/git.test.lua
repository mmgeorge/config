local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local git_repository = dofile("nvim/lua/forge/tests/support/git_repository.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

-- Builds the baseline repository with CRLF and clean-filter attributes and returns the checks that
-- stage and unstage through `forge.git.index_mutation` against it.
local function index_mutation_fixture()
  local index_mutation = require("forge.git.index_mutation")
  local git_provider = require("forge.git.git_provider")
  local status_snapshot = require("forge.git.status_snapshot")

  local repository = git_repository.create()
  local test_root = repository.path

  ---@param path string
  ---@param content string
  local function write_bytes(path, content)
    local file_handle, open_error = io.open(path, "wb")
    assert(file_handle ~= nil, "open failed for " .. path .. ": " .. tostring(open_error))
    local write_ok, write_error = file_handle:write(content)
    file_handle:close()
    assert(write_ok ~= nil, "write failed for " .. path .. ": " .. tostring(write_error))
  end

  ---@param path string
  ---@return string
  local function read_bytes(path)
    local file_handle, open_error = io.open(path, "rb")
    assert(file_handle ~= nil, "open failed for " .. path .. ": " .. tostring(open_error))
    local content = file_handle:read("*a")
    file_handle:close()
    assert(content ~= nil, "read failed for " .. path)
    return content
  end

  local run_git_text = repository.run

  ---@param diff_text string
  ---@param hunk_number integer
  ---@return string
  local function select_hunk_patch(diff_text, hunk_number)
    local line_list = vim.split(diff_text, "\n", { plain = true })
    local hunk_start_list = {}
    for line_index, line in ipairs(line_list) do
      if line:sub(1, 3) == "@@ " then hunk_start_list[#hunk_start_list + 1] = line_index end
    end
    local selected_start = hunk_start_list[hunk_number]
    assert(selected_start ~= nil, "missing hunk " .. hunk_number .. " in:\n" .. diff_text)
    local selected_end = (hunk_start_list[hunk_number + 1] or (#line_list + 1)) - 1
    while selected_end >= selected_start and line_list[selected_end] == "" do
      selected_end = selected_end - 1
    end

    local patch_line_list = {}
    for line_index = 1, selected_start - 1 do
      patch_line_list[#patch_line_list + 1] = line_list[line_index]
    end
    for line_index = selected_start, selected_end do
      patch_line_list[#patch_line_list + 1] = line_list[line_index]
    end
    return table.concat(patch_line_list, "\n")
  end

  ---@param diff_text string
  ---@return string[]
  local function changed_body_line_list(diff_text)
    local result = {}
    for _, line in ipairs(vim.split(diff_text, "\n", { plain = true })) do
      local prefix = line:sub(1, 1)
      local header_prefix = line:sub(1, 3)
      if (prefix == "+" or prefix == "-") and header_prefix ~= "+++" and header_prefix ~= "---" then
        result[#result + 1] = line
      end
    end
    return result
  end

  ---@param direction ForgeIndexMutationDirection
  ---@param target ForgeIndexMutationTarget
  ---@return ForgeIndexMutationResult
  local function execute_target(direction, target)
    local mutation_result
    index_mutation.execute_async(test_root, {
      direction = direction,
      target_list = { target },
    }, function(result)
      mutation_result = result
    end)
    wait_until(function() return mutation_result ~= nil end, "index mutation did not complete", { timeout_ms = 4000 })
    assert(mutation_result.ok, "index mutation failed: " .. tostring(mutation_result.error))
    eq(mutation_result.count, 1, { fail_reason = "completed mutation count" })
    eq(mutation_result.hunk_count, target.kind == "hunk" and 1 or 0, { fail_reason = "completed hunk count" })
    eq(mutation_result.file_count, target.kind == "hunk" and 0 or 1, { fail_reason = "completed file count" })
    return mutation_result
  end

  local function initialize_repository()
    run_git_text({ "config", "core.autocrlf", "false" })
    run_git_text({ "config", "core.safecrlf", "true" })
    run_git_text({ "config", "core.eol", "lf" })
    run_git_text({ "config", "filter.forge-clean.clean", "git stripspace" })
    run_git_text({ "config", "filter.forge-clean.required", "true" })

    write_bytes(test_root .. "/.gitattributes", table.concat({
      "*crlf*.txt text eol=crlf",
      "*filtered*.txt text eol=lf filter=forge-clean",
      "",
    }, "\n"))
    write_bytes(test_root .. "/basic.txt", table.concat({
      "one",
      "two",
      "three",
      "four",
      "five",
      "six",
      "seven",
      "eight",
      "nine",
      "ten",
      "",
    }, "\n"))
    write_bytes(test_root .. "/crlf.txt", "alpha\r\nbeta\r\ngamma\r\n")
    write_bytes(test_root .. "/filtered.txt", "alpha   \n")
    run_git_text({ "add", "--", ".gitattributes", "basic.txt", "crlf.txt", "filtered.txt" })
    run_git_text({ "commit", "-m", "baseline" })

    eq(run_git_text({ "show", ":crlf.txt" }), "alpha\nbeta\ngamma\n", { fail_reason = "CRLF baseline index blob" })
    eq(run_git_text({ "show", ":filtered.txt" }), "alpha\n", { fail_reason = "clean-filter baseline index blob" })
    eq(read_bytes(test_root .. "/crlf.txt"), "alpha\r\nbeta\r\ngamma\r\n", { fail_reason = "CRLF baseline worktree" })
    eq(read_bytes(test_root .. "/filtered.txt"), "alpha   \n", { fail_reason = "clean-filter baseline worktree" })
    eq(run_git_text({ "status", "--porcelain" }), "", { fail_reason = "baseline repository status" })
  end

  local function assert_basic_hunk_stage_and_reverse_unstage()
    write_bytes(test_root .. "/basic.txt", table.concat({
      "one",
      "TWO",
      "three",
      "four",
      "five",
      "six",
      "seven",
      "eight",
      "NINE",
      "ten",
      "",
    }, "\n"))
    local full_diff = run_git_text({ "diff", "--no-color", "--no-ext-diff", "--unified=0", "--", "basic.txt" })
    local first_hunk_patch = select_hunk_patch(full_diff, 1)
    assert(select_hunk_patch(full_diff, 2):find("NINE", 1, true) ~= nil, "second hunk fixture missing")

    execute_target("stage", { kind = "hunk", path = test_root .. "/basic.txt", diff = first_hunk_patch })
    local staged_diff = run_git_text({ "diff", "--cached", "--no-color", "--unified=0", "--", "basic.txt" })
    local unstaged_diff = run_git_text({ "diff", "--no-color", "--unified=0", "--", "basic.txt" })
    eq(changed_body_line_list(staged_diff), { "-two", "+TWO" }, { fail_reason = "staged first hunk" })
    eq(changed_body_line_list(unstaged_diff), { "-nine", "+NINE" }, { fail_reason = "remaining unstaged hunk" })

    execute_target("unstage", { kind = "hunk", path = test_root .. "/basic.txt", diff = first_hunk_patch })
    eq(run_git_text({ "diff", "--cached", "--", "basic.txt" }), "", { fail_reason = "reverse unstage clears cached diff" })
    local restored_unstaged_diff = run_git_text({ "diff", "--no-color", "--unified=0", "--", "basic.txt" })
    eq(changed_body_line_list(restored_unstaged_diff), { "-two", "+TWO", "-nine", "+NINE" }, { fail_reason = "reverse unstage restores both worktree hunks" })
  end

  local function assert_crlf_policy_preserves_semantic_hunk()
    local worktree_content = "alpha\r\nBETA\r\ngamma\r\n"
    write_bytes(test_root .. "/crlf.txt", worktree_content)
    local full_diff = run_git_text({ "diff", "--no-color", "--no-ext-diff", "--unified=0", "--", "crlf.txt" }, { text = false })
    local patch = select_hunk_patch(full_diff, 1)
    assert(patch:find("\r", 1, true) == nil, "CRLF diff patch was not normalized")
    eq(changed_body_line_list(patch), { "-beta", "+BETA" }, { fail_reason = "normalized CRLF patch" })

    execute_target("stage", { kind = "hunk", path = test_root .. "/crlf.txt", diff = patch })
    local staged_diff = run_git_text({ "diff", "--cached", "--no-color", "--unified=0", "--", "crlf.txt" })
    eq(changed_body_line_list(staged_diff), { "-beta", "+BETA" }, { fail_reason = "CRLF cached diff" })
    eq(run_git_text({ "show", ":crlf.txt" }), "alpha\nBETA\ngamma\n", { fail_reason = "CRLF staged index blob" })
    eq(read_bytes(test_root .. "/crlf.txt"), worktree_content, { fail_reason = "CRLF worktree after stage" })
    eq(run_git_text({ "diff", "--", "crlf.txt" }), "", { fail_reason = "CRLF path after stage" })

    execute_target("unstage", { kind = "hunk", path = test_root .. "/crlf.txt", diff = patch })
    eq(run_git_text({ "diff", "--cached", "--", "crlf.txt" }), "", { fail_reason = "CRLF reverse unstage clears cached diff" })
    local unstaged_diff = run_git_text({ "diff", "--no-color", "--unified=0", "--", "crlf.txt" })
    eq(changed_body_line_list(unstaged_diff), { "-beta", "+BETA" }, { fail_reason = "CRLF diff after reverse unstage" })
    eq(run_git_text({ "show", ":crlf.txt" }), "alpha\nbeta\ngamma\n", { fail_reason = "CRLF index blob after reverse unstage" })
    eq(read_bytes(test_root .. "/crlf.txt"), worktree_content, { fail_reason = "CRLF worktree after reverse unstage" })
  end

  local function assert_clean_filter_preserves_canonical_hunk()
    local worktree_content = "BETA   \n"
    write_bytes(test_root .. "/filtered.txt", worktree_content)
    local full_diff = run_git_text({ "diff", "--no-color", "--no-ext-diff", "--unified=0", "--", "filtered.txt" })
    local patch = select_hunk_patch(full_diff, 1)
    eq(changed_body_line_list(patch), { "-alpha", "+BETA" }, { fail_reason = "clean-filter patch" })

    execute_target("stage", { kind = "hunk", path = test_root .. "/filtered.txt", diff = patch })
    local staged_diff = run_git_text({ "diff", "--cached", "--no-color", "--unified=0", "--", "filtered.txt" })
    eq(changed_body_line_list(staged_diff), { "-alpha", "+BETA" }, { fail_reason = "clean-filter cached diff" })
    eq(run_git_text({ "show", ":filtered.txt" }), "BETA\n", { fail_reason = "clean-filter staged index blob" })
    eq(read_bytes(test_root .. "/filtered.txt"), worktree_content, { fail_reason = "clean-filter worktree after stage" })
    eq(run_git_text({ "diff", "--", "filtered.txt" }), "", { fail_reason = "clean-filter path after stage" })

    execute_target("unstage", { kind = "hunk", path = test_root .. "/filtered.txt", diff = patch })
    eq(run_git_text({ "diff", "--cached", "--", "filtered.txt" }), "", { fail_reason = "clean-filter reverse unstage clears cached diff" })
    local unstaged_diff = run_git_text({ "diff", "--no-color", "--unified=0", "--", "filtered.txt" })
    eq(changed_body_line_list(unstaged_diff), { "-alpha", "+BETA" }, { fail_reason = "clean-filter diff after reverse unstage" })
    eq(run_git_text({ "show", ":filtered.txt" }), "alpha\n", { fail_reason = "clean-filter index blob after reverse unstage" })
    eq(read_bytes(test_root .. "/filtered.txt"), worktree_content, { fail_reason = "clean-filter worktree after reverse unstage" })
  end

  ---@param relpath string
  ---@param raw_content string
  ---@param expected_synthetic_line_list string[]
  ---@param expected_staged_line_list string[]
  ---@param expected_index_content string
  local function assert_untracked_whole_file_canonicalization(
    relpath,
    raw_content,
    expected_synthetic_line_list,
    expected_staged_line_list,
    expected_index_content
  )
    local path = test_root .. "/" .. relpath
    write_bytes(path, raw_content)
    eq(run_git_text({ "status", "--porcelain", "--", relpath }), "?? " .. relpath .. "\n", { fail_reason = relpath .. " starts untracked" })

    local synthetic_patch = status_snapshot._build_untracked_diff_from_bytes(relpath, raw_content)
    assert(type(synthetic_patch) == "string", "missing synthetic untracked patch for " .. relpath)
    ---@cast synthetic_patch string
    local synthetic_line_list = changed_body_line_list(synthetic_patch)
    eq(synthetic_line_list, expected_synthetic_line_list, { fail_reason = relpath .. " raw synthetic patch" })

    execute_target("stage", { kind = "untracked_file", path = path })
    eq(run_git_text({ "status", "--porcelain", "--", relpath }), "A  " .. relpath .. "\n", { fail_reason = relpath .. " staged status" })
    eq(read_bytes(path), raw_content, { fail_reason = relpath .. " raw worktree after whole-file stage" })
    eq(run_git_text({ "show", ":" .. relpath }), expected_index_content, { fail_reason = relpath .. " canonical index blob" })

    local staged_diff = run_git_text({ "diff", "--cached", "--no-color", "--unified=0", "--", relpath })
    local staged_line_list = changed_body_line_list(staged_diff)
    eq(staged_line_list, expected_staged_line_list, { fail_reason = relpath .. " staged canonical diff" })
    assert(
      table.concat(synthetic_line_list, "\n") ~= table.concat(staged_line_list, "\n"),
      relpath .. " canonicalization should differ from the raw synthetic patch"
    )

    execute_target("unstage", { kind = "added_file", path = path })
    eq(run_git_text({ "diff", "--cached", "--", relpath }), "", { fail_reason = relpath .. " cached diff after unstage" })
    eq(run_git_text({ "ls-files", "--stage", "--", relpath }), "", { fail_reason = relpath .. " index entry after unstage" })
    eq(run_git_text({ "status", "--porcelain", "--", relpath }), "?? " .. relpath .. "\n", { fail_reason = relpath .. " returns untracked" })
    eq(read_bytes(path), raw_content, { fail_reason = relpath .. " raw worktree after unstage" })
  end

  local function assert_untracked_whole_file_stage_and_unstage()
    assert_untracked_whole_file_canonicalization(
      "untracked-crlf.txt",
      "first\r\nsecond\r\n",
      { "+first\r", "+second\r" },
      { "+first", "+second" },
      "first\nsecond\n"
    )
    assert_untracked_whole_file_canonicalization(
      "untracked-filtered.txt",
      "first   \nsecond\t\n",
      { "+first   ", "+second\t" },
      { "+first", "+second" },
      "first\nsecond\n"
    )
  end

  MiniTest.finally(function() pcall(git_provider.reset_provider) end)
  git_provider.reset_provider()
  initialize_repository()
  return {
    basic_hunk = assert_basic_hunk_stage_and_reverse_unstage,
    crlf_hunk = assert_crlf_policy_preserves_semantic_hunk,
    clean_filter_hunk = assert_clean_filter_preserves_canonical_hunk,
    untracked_file = assert_untracked_whole_file_stage_and_unstage,
  }
end

T["index_mutation_basic_hunk"] = function() index_mutation_fixture().basic_hunk() end
T["index_mutation_crlf_hunk"] = function() index_mutation_fixture().crlf_hunk() end
T["index_mutation_clean_filter_hunk"] = function() index_mutation_fixture().clean_filter_hunk() end
T["index_mutation_untracked_file"] = function() index_mutation_fixture().untracked_file() end

return T

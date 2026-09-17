vim.opt.runtimepath:append("nvim")
require("github.repo_cache").ensure_metadata = function() end
require("github.issue_index").ensure_repo = function() end
local review = require("forge.review_document")
local loaded, complete_load = false, nil
local function metadata(fold)
  return { target = {}, decoration = {}, editable_region = {}, fold = fold or {} }
end
local function comments_fold()
  return { id = "review:section:Conversation", start = { row = 0, column = 0 },
    ["end"] = { block = "comments", position = { row = loaded and 5 or 2, column = 0 } }, closed = true }
end
local function comment_fold()
  return { id = "review:item:comment:1", start = { row = 1, column = 0 },
    ["end"] = { block = "comments", position = { row = 5, column = 0 } }, closed = true }
end
review._set_runner_for_test(function(method, params, callback)
  if method == "review.open" then callback({ document = "comment-folds" })
  elseif method == "github.actor" then callback({ login = "viewer" })
  elseif method == "review.header" then callback({ ready = true })
  elseif method == "review.view" then callback(vim.NIL)
  elseif method == "review.load" then complete_load = callback
  elseif method == "review.materialize" then
    local folds = { comments_fold() }
    if loaded then folds[2] = comment_fold() end
    local rows = loaded and { "Comments (1):", "author", "first line", "second line", "last line" }
      or { "Comments:", "Loading..." }
    local snapshot = { document = params.document, revision = loaded and 1 or 0, block = {
      { id = "title", text = { "PR title" }, metadata = metadata() },
      { id = "comments", text = rows, metadata = metadata(folds) },
    } }
    callback({ snapshot = snapshot, patch = loaded and {
      document = params.document, base = 0, next = 1, base_rows = 3, next_rows = 6,
      base_blocks = 2, next_blocks = 2, removed_block = {}, block_edit = {},
      text_edit = { { start_row = 1, removed_rows = 2, text = rows } },
      metadata_edit = { { block = "comments", row_count = 5, metadata = metadata(folds) } },
    } or vim.NIL })
  elseif method == "review.close" then callback({ closed = true })
  else error("Unexpected request: " .. method) end
end)
local options = { directory = vim.fn.getcwd(), repository = { hostname = "github.com", owner = "owner", name = "repo" },
  number = 1, on_error = error }
local state = review.open(options)
assert(vim.wait(1000, function() return state.shown and complete_load end))
assert(vim.fn.foldclosed(2) == 2, "Comments placeholder must start folded")
loaded = true
complete_load({ diagnostic = {} })
assert(vim.wait(1000, function() return state.replica.revision == 1 end))
assert(vim.fn.foldclosed(2) == 2 and vim.fn.foldclosedend(2) == 6, "Loaded Comments must remain folded")
vim.cmd("2foldopen")
assert(vim.fn.foldclosed(3) == 3 and vim.fn.foldclosedend(3) == 6, "Individual comments must start folded")
review.close(state)
local reopened = review.open(options)
assert(reopened == state and vim.fn.foldclosed(2) == -1, "Cached reopen lost user's section fold choice")
assert(vim.fn.foldclosed(3) == 3, "Cached reopen expanded comment bodies")
review._set_runner_for_test(nil)
print("review_comment_folds: loading, arrival, nested comment, and cache fold state passed")

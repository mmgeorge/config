--- Executes one ordered stage or unstage action at a known repository root.
local M = {}

local git_backend = require("diff_review.git.git_backend")
local paths = require("diff_review.infra.paths")

---@alias DiffReviewIndexMutationDirection "stage"|"unstage"
---@alias DiffReviewIndexMutationTargetKind "hunk"|"tracked_file"|"untracked_file"|"added_file"

---@class DiffReviewIndexMutationTarget
---@field kind DiffReviewIndexMutationTargetKind
---@field path string
---@field original_path? string
---@field diff? string

---@class DiffReviewIndexMutationSpec
---@field direction DiffReviewIndexMutationDirection
---@field target_list DiffReviewIndexMutationTarget[]

---@class DiffReviewIndexMutationFailure
---@field target DiffReviewIndexMutationTarget
---@field message? string
---@field output? string
---@field code? integer
---@field args? string[]

---@class DiffReviewIndexMutationResult: DiffReviewMutationResult
---@field completed_target_list DiffReviewIndexMutationTarget[]
---@field hunk_count integer
---@field file_count integer
---@field failure? DiffReviewIndexMutationFailure

---@param root string
---@param direction DiffReviewIndexMutationDirection
---@param target DiffReviewIndexMutationTarget
---@return string[]? args
---@return string? input
---@return string? error
local function command_for_target(root, direction, target)
  if target.kind == "hunk" then
    if not target.diff or target.diff == "" then return nil, nil, "Missing hunk patch" end
    local args = { "apply", "--cached" }
    if direction == "unstage" then args[#args + 1] = "--reverse" end
    vim.list_extend(args, { "--whitespace=nowarn", "--unidiff-zero", "-" })
    return args, target.diff .. "\n", nil
  end

  local relpath, path_error = paths.repo_relative(target.path, root)
  if not relpath then return nil, nil, path_error end
  local pathspec_list = { relpath }
  if target.original_path then
    local original_relpath, original_path_error = paths.repo_relative(target.original_path, root)
    if not original_relpath then return nil, nil, original_path_error end
    if original_relpath ~= relpath then pathspec_list[#pathspec_list + 1] = original_relpath end
  end
  if direction == "stage" then
    local args = target.kind == "tracked_file" and { "add", "-u", "--" } or { "add", "--" }
    vim.list_extend(args, pathspec_list)
    return args, nil, nil
  end
  if target.kind == "added_file" then
    local args = { "rm", "--cached", "--ignore-unmatch", "--" }
    vim.list_extend(args, pathspec_list)
    return args, nil, nil
  end
  local args = { "restore", "--staged", "--" }
  vim.list_extend(args, pathspec_list)
  return args, nil, nil
end

--- Execute ordered index targets and stop at the first Git failure.
---@param root string
---@param spec DiffReviewIndexMutationSpec
---@param callback fun(result: DiffReviewIndexMutationResult)
function M.execute_async(root, spec, callback)
  local completed_target_list = {}
  local hunk_count = 0
  local file_count = 0
  local mutation_finished = false

  ---@param failure? DiffReviewIndexMutationFailure
  local function finish(failure)
    if mutation_finished then return end
    mutation_finished = true
    callback({
      ok = failure == nil,
      error = failure and (failure.message or failure.output) or nil,
      failure = failure,
      completed_target_list = completed_target_list,
      hunk_count = hunk_count,
      file_count = file_count,
      count = #completed_target_list,
    })
  end

  ---@param target_index integer
  local function execute_target(target_index)
    local target = spec.target_list[target_index]
    if not target then
      finish()
      return
    end

    local args, input, command_error = command_for_target(root, spec.direction, target)
    if not args then
      finish({ target = target, message = command_error or "Unable to build Git mutation" })
      return
    end
    local target_finished = false
    git_backend.run_git_at_root_async(root, args, input, function(result)
      if mutation_finished or target_finished then return end
      target_finished = true
      if not result.ok then
        finish({
          target = target,
          output = result.output,
          code = result.code,
          args = result.args,
        })
        return
      end
      completed_target_list[#completed_target_list + 1] = target
      if target.kind == "hunk" then
        hunk_count = hunk_count + 1
      else
        file_count = file_count + 1
      end
      execute_target(target_index + 1)
    end)
  end

  if not root or root == "" then
    finish({
      target = spec.target_list[1] or { kind = "tracked_file", path = "" },
      message = "Missing repository root",
    })
    return
  end
  execute_target(1)
end

return M

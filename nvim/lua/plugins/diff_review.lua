return {
  {
    "diff-review-local",
    dir = vim.fn.stdpath("config"),
    dependencies = { "folke/snacks.nvim" },
    cmd = {
      "GitStatus", "GitBranchDiff", "GitBranchDiffFile", "GitFileRevision", "GitDiffCompactPreview",
      "Harness", "HarnessNew", "Interactions", "Permissions",
    },
    opts = {
      perf_logging = true,
      status_cursor_prewarm = true,
      walkthrough_inventory = "sem",
      -- status_cursor_prewarm_max_hunks = 2
    },
    config = function(_, opts)
      vim.g.diff_review_gitstatus_debug = 1
      vim.g.diff_review_gitstatus_perf = 1
      local diff_review = require("diff_review")
      diff_review.setup(opts)
      vim.api.nvim_create_user_command("GitStatus", function()
        diff_review.open()
      end, { desc = "Review git changes" })
      vim.api.nvim_create_user_command("Harness", function()
        diff_review.open_harness()
      end, { desc = "Open the DiffReview AI Harness" })
      vim.api.nvim_create_user_command("HarnessNew", function()
        diff_review.new_harness_session()
      end, { desc = "Create a fresh Harness session" })
      vim.api.nvim_create_user_command("Interactions", function()
        diff_review.open_interactions()
      end, { desc = "Review and roll back Harness interactions" })
      vim.api.nvim_create_user_command("Permissions", function()
        require("diff_review.views.permissions").open()
      end, { desc = "Edit Harness permissions" })
      local function complete_branches(arglead)
        local refs = vim.fn.systemlist({ "git", "for-each-ref", "--format=%(refname:short)", "refs/heads", "refs/remotes" })
        if vim.v.shell_error ~= 0 then return {} end
        return vim.tbl_filter(function(ref)
          return ref:find(arglead, 1, true) == 1
        end, refs)
      end

      vim.api.nvim_create_user_command("GitBranchDiff", function(command)
        diff_review.open_branch_diff(command.args)
      end, {
        nargs = 1,
        complete = complete_branches,
        desc = "Diff the working tree against a branch or revision",
      })
      vim.api.nvim_create_user_command("GitBranchDiffFile", function(command)
        local file, branch = command.fargs[1], command.fargs[2]
        if not (file and branch) then
          vim.notify("Usage: GitBranchDiffFile <file> <branch>", vim.log.levels.WARN, { title = "GitBranchDiff" })
          return
        end
        diff_review.open_branch_diff(branch, { file = file })
      end, {
        nargs = "+",
        complete = function(arglead, cmdline)
          local args = vim.split(vim.trim((cmdline:gsub("^%S+%s*", ""))), "%s+", { trimempty = true })
          local completing_file = #args == 0 or (#args == 1 and arglead ~= "")
          if completing_file then
            return vim.fn.getcompletion(arglead, "file")
          end
          return complete_branches(arglead)
        end,
        desc = "Diff one file in the working tree against a branch or revision",
      })
      vim.api.nvim_create_user_command("GitFileRevision", function(command)
        local file, rev = command.fargs[1], command.fargs[2]
        if not (file and rev) then
          vim.notify("Usage: GitFileRevision <file> <commit>", vim.log.levels.WARN, { title = "GitFileRevision" })
          return
        end
        diff_review.open_file_revision(file, rev)
      end, {
        nargs = "+",
        complete = function(arglead, cmdline)
          local args = vim.split(vim.trim((cmdline:gsub("^%S+%s*", ""))), "%s+", { trimempty = true })
          local completing_file = #args == 0 or (#args == 1 and arglead ~= "")
          if completing_file then
            return vim.fn.getcompletion(arglead, "file")
          end
          return complete_branches(arglead)
        end,
        desc = "Open a file read-only as it exists at a git revision",
      })
      vim.api.nvim_create_user_command("GitDiffCompactPreview", function(command)
        diff_review.open_compact_preview({ staged = command.bang })
      end, { bang = true, desc = "Preview compacted git diff; use bang for staged diff" })
    end,
  },
}

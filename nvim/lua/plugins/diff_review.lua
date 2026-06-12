return {
  {
    "diff-review-local",
    dir = vim.fn.stdpath("config"),
    dependencies = { "folke/snacks.nvim" },
    cmd = { "GitStatus", "GitBranchDiff", "GitBranchDiffFile", "GitDiffCompactPreview" },
    config = function(_, opts)
      local diff_review = require("diff_review")
      diff_review.setup(opts)
      vim.api.nvim_create_user_command("GitStatus", function()
        diff_review.open()
      end, { desc = "Review git changes" })
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
      vim.api.nvim_create_user_command("GitDiffCompactPreview", function(command)
        diff_review.open_compact_preview({ staged = command.bang })
      end, { bang = true, desc = "Preview compacted git diff; use bang for staged diff" })
    end,
  },
}

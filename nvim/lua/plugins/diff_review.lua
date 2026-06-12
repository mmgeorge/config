return {
  {
    "diff-review-local",
    dir = vim.fn.stdpath("config"),
    dependencies = { "folke/snacks.nvim" },
    cmd = { "GitStatus", "GitDiffCompactPreview" },
    config = function(_, opts)
      local diff_review = require("diff_review")
      diff_review.setup(opts)
      vim.api.nvim_create_user_command("GitStatus", function()
        diff_review.open()
      end, { desc = "Review git changes" })
      vim.api.nvim_create_user_command("GitDiffCompactPreview", function(command)
        diff_review.open_compact_preview({ staged = command.bang })
      end, { bang = true, desc = "Preview compacted git diff; use bang for staged diff" })
    end,
  },
}

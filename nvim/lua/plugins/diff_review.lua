return {
  {
    "diff-review-local",
    dir = vim.fn.stdpath("config"),
    dependencies = { "folke/snacks.nvim" },
    cmd = { "DiffReview" },
    opts = {
      about_auto_generate = false,
    },
    config = function(_, opts)
      local diff_review = require("diff_review")
      diff_review.setup(opts)
      vim.api.nvim_create_user_command("DiffReview", function()
        diff_review.open()
      end, { desc = "Review git changes" })
    end,
  },
}

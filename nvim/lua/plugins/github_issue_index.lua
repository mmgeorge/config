local crate_dir = vim.fs.joinpath(vim.fn.stdpath("config"), "rust", "github-issue-index")

return {
  {
    "github-issue-index",
    dir = crate_dir,
    lazy = true,
    build = "cargo build --release",
    init = function()
      vim.api.nvim_create_autocmd("VimEnter", {
        once = true,
        desc = "Auto-build the GitHub issue indexer sidecar",
        callback = function()
          if #vim.api.nvim_list_uis() == 0 then return end
          require("github.issue_index_builder").ensure(function(result)
            if result.ok then return end
            vim.notify(tostring(result.message), vim.log.levels.ERROR, { title = "GitHub Issues" })
          end)
        end,
      })
    end,
  },
}

return {
  {
    "pwntester/octo.nvim",
    cmd = "Octo",
    init = function()
      vim.api.nvim_create_user_command("GithubOpenPR", function()
        require("github.open_pr").open()
      end, {
        desc = "Create a draft GitHub PR for the current branch",
      })

      vim.api.nvim_create_user_command("GithubIssuse", function()
        require("github.pickers").issues()
      end, {
        desc = "Search GitHub issues involving me",
      })

      vim.api.nvim_create_user_command("GithubIssue", function()
        require("github.pickers").issues()
      end, {
        desc = "Search GitHub issues involving me",
      })

      vim.api.nvim_create_user_command("GithubIssueOpen", function(opts)
        require("github.issue_view").open_command(opts.args)
      end, {
        nargs = "+",
        desc = "Open a GitHub issue buffer",
      })

      vim.api.nvim_create_user_command("GithubPR", function()
        require("github.pickers").prs()
      end, {
        desc = "Search my open GitHub PRs",
      })

      vim.api.nvim_create_user_command("GithubReview", function()
        require("github.pickers").reviews()
      end, {
        desc = "Search GitHub PR review requests",
      })

      vim.api.nvim_create_user_command("GithubNotifications", function()
        require("github.notifications").open()
      end, {
        desc = "Open GitHub notifications",
      })
    end,
    opts = {
      -- or "fzf-lua" or "snacks" or "default"
      picker = "snacks",
      -- bare Octo command opens picker of commands
      enable_builtin = true,
      -- github_hostname = "devtopia.esri.com",
      -- gh_cmd = "gh",
      gh_env = {
        GH_HOST = "devtopia.esri.com",
        -- DBUS_SESSION_BUS_ADDRESS = vim.env["DBUS_SESSION_BUS_ADDRESS"]
      },
      github_hostname = "devtopia.esri.com",
      default_remote = { "origin", "upstream" },
      ssh_aliases = {
        ["devtopia.esri.com"] = "devtopia.esri.com"
      },
      reviews = {
        auto_show_threads = false
      },
      -- resolved_icon = "R",
    },
    keys = {
      {
        "<leader>oi",
        "<CMD>Octo issue list<CR>",
        desc = "List GitHub Issues",
      },
      {
        "<leader>op",
        "<CMD>Octo pr list<CR>",
        desc = "List GitHub PullRequests",
      },
      {
        "<leader>od",
        "<CMD>Octo discussion list<CR>",
        desc = "List GitHub Discussions",
      },
      {
        "<leader>on",
        "<CMD>Octo notification list<CR>",
        desc = "List GitHub Notifications",
      },
      {
        "<leader>os",
        function()
          require("octo.utils").create_base_search_command { include_current_repo = true }
        end,
        desc = "Search GitHub",
      },
    },
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
      -- OR "ibhagwan/fzf-lua",
      -- OR "folke/snacks.nvim",
      "nvim-tree/nvim-web-devicons",
    },
  }
}

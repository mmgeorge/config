local key = require("../keys").key

return {
  {
    'akinsho/git-conflict.nvim',
    config = function()
      require "git-conflict".setup({
        -- disable buffer local mapping created by this plugin
        default_mappings = false,
      })
    end,
  },
  {
    "NeogitOrg/neogit",
    -- commit = "0cae7abc30cb91d661f28257c331fcb5b5198e31",
    dependencies = {
      "nvim-lua/plenary.nvim",  -- required
      "sindrets/diffview.nvim", -- optional - Diff integration
      "folke/snacks.nvim",
    },
    keys = {
      {
        key("og"),
        "<cmd>Neogit<cr>",
        desc = "Neogit",
      }
    },
    config = function()
      vim.api.nvim_create_autocmd({ 'BufEnter' }, {
        pattern = "COMMIT_EDITMSG",
        callback = function(args)
          if vim.b[args.buf].ai_commit_generated then return end

          -- Only generate the commit message if the buffer is empty
          if vim.api.nvim_buf_get_lines(0, 0, -1, false)[1] == nil or
              vim.api.nvim_buf_get_lines(0, 0, -1, false)[1] == "" then
            local ok, err = pcall(function()
              vim.notify("GEN MESSAGE")
              require("codecompanion").prompt("commit")
              vim.b[args.buf].ai_commit_generated = true
              -- For some reason, codecompanion inline assistant map gets added?
              vim.cmd("mapclear <buffer>")
            end)
            if not ok then
              -- Might not have an LLM setup
              vim.notify("Could not generate commit message" .. tostring(err), vim.log.levels.ERROR)
            end
          end
        end,
      })

      local neogit = require("neogit")
      neogit.setup {
        disable_hint = false,
        disable_context_highlighting = true,
        process_spinner = true,
        disable_signs = true,
        -- Changes what mode the Commit Editor starts in. `true` will leave nvim in normal mode,
        -- `false` will change nvim to insert mode, and `"auto"` will change nvim to insert mode
        -- IF the commit message is empty, otherwise leaving it in
        disable_insert_on_commit = true,
        -- disable_insert_on_commit = "auto",
        -- When enabled, will watch the `.git/` directory for changes and refresh the status
        -- buffer in response to filesystem
        filewatcher = {
          interval = 1000,
          enabled = true,
        },
        -- "ascii"   is the graph the git CLI generates
        -- "unicode" is the graph like https://github.com/rbong/vim-flog
        graph_style = "unicode",
        -- Used to generate URL's for branch popup action "pull request".
        -- git_services = {
        --   ["github.com"] = "https://github.com/${owner}/${repository}/compare/${branch_name}?expand=1",
        -- },
        initial_branch_name = "matt9222/",
        -- Persist the values of switches/options within and across sessions
        remember_settings = true,
        -- Scope persisted settings on a per-project basis
        use_per_project_settings = true,
        -- Table of settings to never persist. Uses format "Filetype--cli-value"
        ignored_settings = {
          "NeogitPushPopup--force-with-lease",
          "NeogitPushPopup--force",
          "NeogitPullPopup--rebase",
          "NeogitCommitPopup--allow-empty",
          "NeogitRevertPopup--no-edit",
        },
        -- Configure highlight group features
        -- highlight = {
        --   italic = true,
        --   bold = true,
        --   underline = true
        -- },
        -- Neogit refreshes its internal state after specific events, which can be
        -- expensive depending on the repository size. Disabling `auto_refresh`
        auto_refresh = true,
        -- Value used for `--sort` option for `git branch` command
        -- By default, branches will be sorted by commit date descending
        -- Flag description: https://git-scm.com/docs/git-branch#Documentation/git-branch.txt---sortltkeygt
        -- Sorting keys: https://git-scm.com/docs/git-for-each-ref#_options
        sort_branches = "-committerdate",
        -- Change the default way of opening neogit
        kind = "tab",
        -- Disable line numbers and relative line numbers
        disable_line_numbers = true,
        -- The time after which an output console is shown for slow running commands
        console_timeout = 2000,
        -- Automatically show console if a command takes more than console_timeout milliseconds
        auto_show_console = true,
        auto_close_console = true,
        status = {
          recent_commit_count = 10,
        },
        commit_editor = {
          -- kind = "tab",
          kind = "auto",
          show_staged_diff = false,
          spell_check = true
        },
        commit_select_view = {
          kind = "tab",
        },
        commit_view = {
          kind = "vsplit",
          -- kind = "tab",
          -- Can be set to true or false, otherwise we try to find the binary
          verify_commit = os.execute("which gpg") == 0,
        },
        log_view = {
          kind = "tab",
        },
        rebase_editor = {
          kind = "auto",
        },
        reflog_view = {
          kind = "tab",
        },
        merge_editor = {
          kind = "auto",
        },
        tag_editor = {
          kind = "auto",
        },
        preview_buffer = {
          kind = "split",
        },
        popup = {
          kind = "split",
        },
        -- signs = {
        --   -- { CLOSED, OPENED }
        --   hunk = { "##", "##" },
        --   item = { ">", "v" },
        --   section = { ">", "v" },
        -- },
        -- Each Integration is auto-detected through plugin presence, however, it can be disabled by setting to `false`
        integrations = {
          snacks = true,
          diffview = false,
        },
        use_default_keymaps = false,
        mappings = {
          commit_editor = {
            -- ["q"] = "Close",
            ["<c-c><c-c>"] = "Submit",
            ["<c-c><c-k>"] = "Abort",
            ["<m-p>"] = "PrevMessage",
            ["<m-n>"] = "NextMessage",
            ["<m-r>"] = "ResetMessage",
          },
          commit_editor_I = {
            ["<c-c><c-c>"] = "Submit",
            ["<c-c><c-k>"] = "Abort",
          },
          rebase_editor = {
            -- ["p"] = "Pick",
            ["R"] = "Reword",
            ["E"] = "Edit",
            ["S"] = "Squash",
            ["F"] = "Fixup",
            ["X"] = "Execute",
            ["D"] = "Drop",
            ["B"] = "Break",
            ["q"] = "Close",
            ["<cr>"] = "OpenCommit",
            ["gk"] = "MoveUp",
            ["gj"] = "MoveDown",
            ["<c-c><c-c>"] = "Submit",
            ["<c-c><c-k>"] = "Abort",
            -- ["[c"] = "OpenOrScrollUp",
            -- ["]c"] = "OpenOrScrollDown",
          },
          rebase_editor_I = {
            ["<c-c><c-c>"] = "Submit",
            ["<c-c><c-k>"] = "Abort",
          },
          finder = {
            ["<cr>"] = "Select",
            ["<c-c>"] = "Close",
            ["<esc>"] = "Close",
            ["<c-n>"] = "Next",
            ["<c-p>"] = "Previous",
            ["<down>"] = "Next",
            ["<up>"] = "Previous",
            ["<tab>"] = "InsertCompletion",
            ["<space>"] = "MultiselectToggleNext",
            ["<s-space>"] = "MultiselectTogglePrevious",
            ["<c-j>"] = "NOP",
            ["<ScrollWheelDown>"] = "ScrollWheelDown",
            ["<ScrollWheelUp>"] = "ScrollWheelUp",
            ["<ScrollWheelLeft>"] = "NOP",
            ["<ScrollWheelRight>"] = "NOP",
            ["<LeftMouse>"] = "MouseClick",
            ["<2-LeftMouse>"] = "NOP",
          },
          -- Setting any of these to `false` will disable the mapping.
          popup = {
            ["?"] = "HelpPopup",
            ["oa"] = "CherryPickPopup",
            ["od"] = "DiffPopup",
            ["oR"] = "RemotePopup",
            ["op"] = "PushPopup",
            ["oX"] = "ResetPopup",
            ["oz"] = "StashPopup",
            ["oi"] = "IgnorePopup",
            -- ["t"] = "TagPopup",
            ["ob"] = "BranchPopup",
            ["oB"] = "BisectPopup",
            ["ow"] = "WorktreePopup",
            ["c"] = "CommitPopup",
            ["of"] = "FetchPopup",
            ["ol"] = "LogPopup",
            ["om"] = "MergePopup",
            ["oP"] = "PullPopup",
            ["or"] = "RebasePopup",
            ["ov"] = "RevertPopup",
          },
          status = {
            -- ["j"] = "MoveDown",
            -- ["k"] = "MoveUp",
            -- ["o"] = "OpenTree",
            ["<Esc>"] = "Close",
            ["q"] = "Close",
            ["I"] = "InitRepo",
            ["1"] = "Depth1",
            ["2"] = "Depth2",
            ["3"] = "Depth3",
            ["4"] = "Depth4",
            ["Q"] = "Command",
            ["<tab>"] = "Toggle",
            ["j"] = "Discard",
            -- ["s"] = "Stage",
            -- ["S"] = "StageUnstaged",
            ["S"] = "Stage",
            ["s"] = "MoveUp",
            ["t"] = "MoveDown",
            ["<c-s>"] = "StageAll",
            -- ["u"] = "Unstage",
            ["U"] = "Unstage",
            ["K"] = "Untrack",
            -- ["U"] = "UnstageStaged",
            ["<C-u>"] = "UnstageStaged",
            ["y"] = "ShowRefs",
            ["$"] = "CommandHistory",
            ["Y"] = "YankSelected",
            ["<c-r>"] = "RefreshBuffer",
            ["<cr>"] = "GoToFile",
            ["<s-cr>"] = "PeekFile",
            -- ["<c-v>"] = "VSplitOpen",
            -- ["<c-x>"] = "SplitOpen",
            -- ["<c-t>"] = "TabOpen",
            -- ["{"] = "GoToPreviousHunkHeader",
            -- ["}"] = "GoToNextHunkHeader",
            -- ["[c"] = "OpenOrScrollUp",
            -- ["]c"] = "OpenOrScrollDown",
            -- ["<c-k>"] = "PeekUp",
            -- ["<c-j>"] = "PeekDown",
            -- ["<c-n>"] = "NextSection",
            -- ["<c-p>"] = "PreviousSection",
          },
        },
        sections = {
          -- Reverting/Cherry Picking
          sequencer = {
            folded = false,
            hidden = false,
          },
          untracked = {
            folded = false,
            hidden = false,
          },
          staged = {
            folded = false,
            hidden = false,
          },
          stashes = {
            folded = true,
            hidden = false,
          },
          unpulled_upstream = {
            folded = true,
            hidden = false,
          },
          unmerged_upstream = {
            folded = false,
            hidden = false,
          },
          unpulled_pushRemote = {
            folded = true,
            hidden = false,
          },
          unmerged_pushRemote = {
            folded = false,
            hidden = false,
          },
          recent = {
            folded = false,
            hidden = false,
          },
          rebase = {
            folded = true,
            hidden = true,
          },
        },
      }
    end
  },

  {
    'echasnovski/mini.diff',
    version = '*',
    opts = {
      view = {
        signs = { add = '┃', change = '┃', delete = '▁' }
      }
    }
  },
  -- {
  --   "lewis6991/gitsigns.nvim",
  --   lazy = false, -- messes up keybindings
  --   config = function()
  --     require('gitsigns').setup()
  --   end
  -- }
}

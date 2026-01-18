function adapters()
  if os.getenv("GEMINI_API_KEY") then
    return {
      chat = {
        name = "gemini",
        model = "gemini-3-pro-preview",
        -- reasoning_effort = "low",
      },
      inline = {
        name = "gemini",
        model = "gemini-3-flash-preview",
        -- reasoning_effort = "minimal",
      },
      cmd = "gemini",
      background = "gemini"
    }
  end

  return {
    chat = "copilot",
    inline = "copilot",
    cmd = "copilot",
    background = "copilot"
  }
end


return {
  {
    "azorng/goose.nvim",
    config = function()
      require("goose").setup({})
    end,
    dependencies = {
      "nvim-lua/plenary.nvim",
      "MeanderingProgrammer/render-markdown.nvim",
    },
  },
  {
    "olimorris/codecompanion.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-treesitter/nvim-treesitter",
      { "echasnovski/mini.diff", opts = {} },
      -- "j-hui/fidget.nvim",
    },
    keys = {
      {
        "<leader>cc",
        mode = { "n", "x" },
        ":CodeCompanion",
        desc = "Codecompanion Inline",
      },
      {
        "<leader>ai",
        mode = { "n", "x" },
        ":CodeCompanion #buffer add the following impls for the selected code: ",
        desc = "AI Implement",
      },
      {
        "<leader>ad",
        mode = { "n", "x" },
        ":CodeCompanion #buffer generate an enum dispatch method, which calls the following method on each underlying variant: ",
        desc = "AI Enum Dispatch",
      },
      {
        "<leader>ae",
        mode = { "n", "x" },
        ":CodeCompanion /extract<cr>",
        desc = "AI Extract Function",
      },
      {
        "<leader>ad",
        mode = { "n", "x" },
        ":CodeCompanion /doc<cr>",
        desc = "AI Document",
      },
      {
        "<leader>co",
        mode = { "n" },
        function()
          require("codecompanion").toggle()
        end,
        desc = "Codecompanion Open",
      },
      {
        "<Esc>",
        mode = { "n" },
        ":CodeCompanionChat Toggle<cr>",
        desc = "Codecompanion Open",
        ft = "codecompanion",
        silent = true
      },
      {
        "<leader>ca",
        mode = { "n", "x" },
        ":CodeCompanionActions<cr>",
        desc = "Codecompanion Actions",
      }

    },
    init = function()
      -- require("codecompanion_fidget"):init()
    end,
    opts = {
      interactions = {
        inline = {
          adapter = adapters().inline,
          keymaps = {
            accept_change = {
              modes = { n = "ga" },
            },
            reject_change = {
              modes = { n = "gr" },
            },
            always_accept = {
              modes = { n = "gdy" },
            },
            stop = {
              callback = "keymaps.stop",
              description = "Stop request",
              index = 4,
              modes = { n = "<C-q>" },
            },
          },
        },
        chat = {
          adapter = adapters().chat,
          tools = {
            opts = {
              default_tools = {
                -- "my_tool",
              },
              auto_submit_errors = true,
              auto_submit_success = true,
            },
          },
          keymaps = {
            send = {
              modes = { n = "<C-s>", i = "<C-s>" },
              opts = {},
            },
            close = {
              modes = { n = "q" },
              opts = {},
            },
          }
        },
        cmd = {
          adapter = adapters().cmd,
        },
        background = {
          adapter = adapters().background,
        }
      },
      display = {
        chat =  {
          intro_message = "Start chatting. Press ? for options",
          separator = "─",
          show_context = true,
          show_header_separator = true,
          show_settings = true,
          show_token_count = true,
          show_tools_processing = true,
          start_in_insert_mode = true,
          auto_scroll = false,
          -- fold_reasoning = false,
          -- show_reasoning = false,
          window = {
            layout = "horizontal", -- float|vertical|horizontal|buffer
            position = "bottom",   -- left|right|top|bottom (nil will default depending on vim.opt.plitright|vim.opt.splitbelow)
            border = "single",
            height = 0.5,
            width = 0.5,
            relative = "editor",
            full_height = false,
            opts = {
              breakindent = true,
              cursorcolumn = false,
              cursorline = false,
              foldcolumn = "0",
              linebreak = true,
              list = false,
              numberwidth = 1,
              signcolumn = "no",
              spell = false,
              wrap = true,
            },
          },
        }
      }
    }
  },
  -- {
  --   "zbirenbaum/copilot.lua",
  --   cmd = "Copilot",
  --   event = "InsertEnter",
  --   config = function()
  --     require("copilot").setup({})
  --   end,
  -- },
}

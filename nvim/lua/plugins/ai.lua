return {
  {
    "olimorris/codecompanion.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-treesitter/nvim-treesitter",
      { "echasnovski/mini.diff", opts = {} },
    },
    keys = {
      {
        "<leader>cc",
        mode = { "n", "x" },
        ":CodeCompanion",
        desc = "Codecompanion Inline",
      },
      {
        "<leader>cm",
        mode = { "n" },
        ":CodeCompanion /commit<cr>",
        desc = "Codecompanion Commit",
      },
      {
        "<leader>co",
        mode = { "n"},
        ":CodeCompanionChat<cr>",
        desc = "Codecompanion Open",
      },
      {
        "<leader>ca",
        mode = { "n", "x" },
        ":CodeCompanionActions<cr>",
        desc = "Codecompanion Actions",
      }

    },
    opts = {
      opts = {
        -- log_level = "TRACE",
      },
      prompt_library = {
        ["Generate documentation"] = {
          strategy = "inline",
          description = "Generate documentation",
          opts = {
            index = 10,
            is_default = true,
            is_slash_cmd = true,
            short_name = "doc",
            auto_submit = true,
            -- placement = "before|false"
            -- stop_context_insertion = true
          },
          prompts = {
--             {
--               role = "system", 
--               content = [[When asked to document code, follow these steps: 
-- 1. Identify the programming language 
-- 2. Lookup the best style practices for adding documentation in that language. For instance, when documenting javascript or typescript, use jsdoc style. 
-- 3. Identify every function, method, or class in the passed code. These are the things that you will add documentation to. 
-- 4. Be as succinct as possible in the documentation that you generate.
--               ]], 
--               opts = {
--                 visible = false,
--               },
--             },
            {
              role = "user",
              content = function(context)
                local code = require("codecompanion.helpers.actions").get_code(context.start_line, context.end_line)
                local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
                local buffer_text = table.concat(lines, '\n') 
                
                return string.format(
                  [[Document the below code. Use the following steps: 
1. Identify the programming language 
2. Lookup the best style practices for adding documentation in that language. For instance, when documenting javascript or typescript, use jsdoc style. 
3. Identify every function, method, or class in the passed code. These are the things that you will add documentation to. 
4. If the function already has documentation, try to incorporate that into the new documentation. It is OK to remove some of it, but the content should still be there. 
5. Be as succinct as possible in the documentation that you generate (2 sentences).
6. Do NOT document specific function parameters or the return value.

Each line of the generated documentation should not be longer than 80 characters.

For additional context, the entire file is: 
```%s
%s
```

This is the code to document:
```%s
%s
```
]], context.filetype, buffer_text, context.filetype, code)
              end,
              opts = {
                contains_code = true,
              },
            },
--             {
--               role = "user",
--               content = function(context)
--                 local code = require("codecompanion.helpers.actions").get_code(context.start_line, context.end_line)
--                 -- local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
--                 -- local buffer_text = table.concat(lines, '\n') 
--                 
--                 return string.format(
--                   [[You are an expert in writing succinct technical documentation geared towards other developers already familiar with the codebase.
-- - Lookup the best practices for documenting the specified language.
-- - Generate concise documentation for the code below below. Each line of the generated documentation should not be longer than 80 characters. 
--
-- ```%s
-- %s
-- ```
-- ]], context.filetype, code)
--               end,
--               opts = {
--                 contains_code = true,
--               },
--             }
          },
        },
        ["Generate a Commit Message"] = {
          strategy = "inline",
          description = "Generate a commit message",
          opts = {
            index = 10,
            is_default = true,
            is_slash_cmd = true,
            short_name = "commit",
            placement = "before|false"
            -- auto_submit = true,
            -- stop_context_insertion = true
          },
          prompts = {
            {
              role = "user",
              content = function()
                local handle = io.popen("git diff --cached")
                local result = handle:read("*a")
                handle:close()


                return string.format(
                  [[You are an expert at following the Conventional Commit specification. Given the git diff listed below, generate a commit message for me. Do not indent the first line. The description under the description should be no more than 2 sentences. Prefer omitting this description when possible. Do not list any breaking changes:

```diff
%s
```
]],
                  vim.fn.system("git diff --no-ext-diff --staged")
                )
--                 return string.format(
--                   [[Given the diff listed below, please generate a short commit message that is only a single line. Pay attention to the gutter in the commit message that indicates what was changed. Only those lines that were changed and have a + or a - in the gutter should be considered for the commit message. The commit message should not be more than 50 characters. 
--
-- The format of the message should be <type>: <description>
-- - valid options for <type> are: fix, feat, build, chore, ci, docs, style, refactor, perf, test, lint, cleanup
-- - <description> should be a very short summary of the changes. 
--
-- Some examples commit messages might be: 
-- - feat: send an email to the customer when a product is shipped 
-- - chore: drop support for Node 6
-- - docs: correct spelling of CHANGELOG 
-- - docs: add doc for the Foo component
--
-- Prefer using abbreviations like "doc" instead of "documentation" when possible to reduce the length of the commit message
--
-- The first letter of <description> should be lowercase.
--
-- The diff is below:
-- ```diff
-- %s
-- ```
-- ]], result)
              end,
              opts = {
                contains_code = true,
              },
            },
          },
        },
      },
      display = {
        chat = {
          auto_scroll = false,
          intro_message = "Welcome to CodeCompanion ?! Press ? for options",
          show_header_separator = true, -- Show header separators in the chat buffer? Set this to false if you're using an external markdown formatting plugin
          -- separator = "�", -- The separator between the different messages in the chat buffer
          show_references = true, -- Show references (from slash commands and variables) in the chat buffer?
          show_settings = true, -- Show LLM settings at the top of the chat buffer?
          show_token_count = true, -- Show the token count for each response?
          start_in_insert_mode = true, -- Open the chat buffer in insert mode?
          -- Options to customize the UI of the chat buffer
          window = {
            layout = "buffer", -- float|vertical|horizontal|buffer
            position = "bottom", -- left|right|top|bottom (nil will default depending on vim.opt.plitright|vim.opt.splitbelow)
            border = "single",
            height = 0.8,
            width = 0.8,
            relative = "editor",
            full_height = false, -- when set to false, vsplit will be used to open the chat buffer vs. botright/topleft vsplit
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
        },
        diff = {
          enabled = true,
          close_chat_at = 240, -- Close an open chat buffer if the total columns of your display are less than...
          -- with mini diff this is inline instead
          layout = "vertical", -- vertical|horizontal split for default provider
          opts = { "internal", "filler", "closeoff", "algorithm:patience", "followwrap", "linematch:120" },
          provider = "mini_diff", -- default|mini_diff
        }
      },
      gemini = function()
        return require("codecompanion.adapters").extend("gemini", {
          schema = {
            env = {
              api_key = "GEMINI_API_KEY"
            } ,
            model = {
              default = "gemini-2.0-flash",
            },
          },
        })
      end,
      strategies = {
        chat = {
          adapter = "copilot",
        },
        inline = {
          adapter = "copilot",
          keymaps = {
            accept_change = {
              modes = { n = "ga" },
              description = "Accept the suggested change",
            },
            reject_change = {
              modes = { n = "gr" },
              description = "Reject the suggested change",
            },
          },
        },
        cmd = {
          adapter = "copilot",
        }
      }
    }
  },
  {
    "zbirenbaum/copilot.lua",
    cmd = "Copilot",
    event = "InsertEnter",
    config = function()
      require("copilot").setup({})
    end,
  },
  -- {
  --   "yetone/avante.nvim",
  --   event = "VeryLazy",
  --   version = false, -- Never set this value to "*"! Never!
  --   config = function ()
  --     require("avante.providers.vertex").parse_api_key = function ()
  --       -- gcloud auth application-default print-access-token
  --       -- setx VERTEX_TOKEN [token] 
  --       return vim.fn.getenv("VERTEX_TOKEN")
  --     end
  --     require"avante".setup({
  --       vertex = {
  --         model = "gemini-2.0-flash-001", 
  --         max_tokens = 1024,
  --         -- location = "us-central1" 
  --       },
  --       -- add any opts here
  --       -- for example
  --       provider = "vertex",
  --       cursor_applying_provider = "vertex", 
  --       -- provider = "copilot",
  --       -- cursor_applying_provider = "copilot", 
  --       ---Specify the special dual_boost mode
  --       ---1. enabled: Whether to enable dual_boost mode. Default to false.
  --       ---2. first_provider: The first provider to generate response. Default to "openai".
  --       ---3. second_provider: The second provider to generate response. Default to "claude".
  --       ---4. prompt: The prompt to generate response based on the two reference outputs.
  --       ---5. timeout: Timeout in milliseconds. Default to 60000.
  --       ---How it works:
  --       --- When dual_boost is enabled, avante will generate two responses from the first_provider and second_provider respectively. Then use the response from the first_provider as provider1_output and the response from the second_provider as provider2_output. Finally, avante will generate a response based on the prompt and the two reference outputs, with the default Provider as normal.
  --       ---Note: This is an experimental feature and may not work as expected.
  --       dual_boost = {
  --         enabled = false,
  --         first_provider = "openai",
  --         second_provider = "claude",
  --         prompt = "Based on the two reference outputs below, generate a response that incorporates elements from both but reflects your own judgment and unique perspective. Do not provide any explanation, just give the response directly. Reference Output 1: [{{provider1_output}}], Reference Output 2: [{{provider2_output}}]",
  --         timeout = 60000, -- Timeout in milliseconds
  --       },
  --       behaviour = {
  --         auto_suggestions = false, -- Experimental stage
  --         auto_set_highlight_group = true,
  --         auto_set_keymaps = true,
  --         auto_apply_diff_after_generation = false,
  --         support_paste_from_clipboard = false,
  --         minimize_diff = true, -- Whether to remove unchanged lines when applying a code block
  --         enable_token_counting = true, -- Whether to enable token counting. Default to true.
  --         enable_cursor_planning_mode = false, -- Whether to enable Cursor Planning Mode. Default to false.
  --         enable_claude_text_editor_tool_mode = false, -- Whether to enable Claude Text Editor Tool Mode.
  --       },
  --       mappings = {
  --         --- @class AvanteConflictMappings
  --         diff = {
  --           ours = "co",
  --           theirs = "ct",
  --           all_theirs = "ca",
  --           both = "cb",
  --           cursor = "cc",
  --           next = "]x",
  --           prev = "[x",
  --         },
  --         suggestion = {
  --           accept = "<M-l>",
  --           next = "<M-]>",
  --           prev = "<M-[>",
  --           dismiss = "<C-]>",
  --         },
  --         jump = {
  --           next = "]]",
  --           prev = "[[",
  --         },
  --         submit = {
  --           normal = "<CR>",
  --           insert = "<C-s>",
  --         },
  --         cancel = {
  --           normal = { "<C-c>", "<Esc>", "q" },
  --           insert = { "<C-c>" },
  --         },
  --         files = {
  --           add_current = "<leader>ac", -- Add current buffer to selected files
  --         },
  --         sidebar = {
  --           apply_all = "A",
  --           apply_cursor = "a",
  --           retry_user_request = "r",
  --           edit_user_request = "e",
  --           switch_windows = "<Tab>",
  --           reverse_switch_windows = "<S-Tab>",
  --           remove_file = "j",
  --           add_file = "a",
  --           close = { "<Esc>", "q" },
  --           close_from_input = nil, -- e.g., { normal = "<Esc>", insert = "<C-d>" }
  --         },
  --       },
  --       hints = { enabled = true },
  --       windows = {
  --         ---@type "right" | "left" | "top" | "bottom"
  --         position = "right", -- the position of the sidebar
  --         wrap = true, -- similar to vim.o.wrap
  --         width = 50, -- default % based on available width
  --         sidebar_header = {
  --           enabled = true, -- true, false to enable/disable the header
  --           align = "center", -- left, center, right for title
  --           rounded = true,
  --         },
  --         input = {
  --           prefix = "> ",
  --           height = 8, -- Height of the input window in vertical layout
  --         },
  --         edit = {
  --           border = "rounded",
  --           start_insert = true, -- Start insert mode when opening the edit window
  --         },
  --         ask = {
  --           floating = true, -- Open the 'AvanteAsk' prompt in a floating window
  --           start_insert = true, -- Start insert mode when opening the ask window
  --           border = "rounded",
  --           ---@type "ours" | "theirs"
  --           focus_on_apply = "ours", -- which diff to focus after applying
  --         },
  --       },
  --       highlights = {
  --         ---@type AvanteConflictHighlights
  --         diff = {
  --           current = "DiffText",
  --           incoming = "DiffAdd",
  --         },
  --       },
  --       --- @class AvanteConflictUserConfig
  --       diff = {
  --         autojump = true,
  --         ---@type string | fun(): any
  --         list_opener = "copen",
  --         --- Override the 'timeoutlen' setting while hovering over a diff (see :help timeoutlen).
  --         --- Helps to avoid entering operator-pending mode with diff mappings starting with `c`.
  --         --- Disable by setting to -1.
  --         override_timeoutlen = 500,
  --       },
  --       suggestion = {
  --         debounce = 600,
  --         throttle = 600,
  --       },
  --     })
  --   end,
  --
  --   -- build = "make",
  --   -- if you want to build from source then do `make BUILD_FROM_SOURCE=true`
  --   -- build = "make BUILD_FROM_SOURCE=true",
  --   build = "powershell -ExecutionPolicy Bypass -File Build.ps1 -BuildFromSource false", -- for windows
  --   dependencies = {
  --     "nvim-treesitter/nvim-treesitter",
  --     "stevearc/dressing.nvim",
  --     "nvim-lua/plenary.nvim",
  --     "MunifTanjim/nui.nvim",
  --     --- The below dependencies are optional,
  --     "echasnovski/mini.pick", -- for file_selector provider mini.pick
  --     "nvim-telescope/telescope.nvim", -- for file_selector provider telescope
  --     "hrsh7th/nvim-cmp", -- autocompletion for avante commands and mentions
  --     "ibhagwan/fzf-lua", -- for file_selector provider fzf
  --     "nvim-tree/nvim-web-devicons", -- or echasnovski/mini.icons
  --     "zbirenbaum/copilot.lua", -- for providers='copilot'
  --     {
  --       -- support for image pasting
  --       "HakonHarnes/img-clip.nvim",
  --       event = "VeryLazy",
  --       opts = {
  --         -- recommended settings
  --         default = {
  --           embed_image_as_base64 = false,
  --           prompt_for_file_name = false,
  --           drag_and_drop = {
  --             insert_mode = true,
  --           },
  --           -- required for Windows users
  --           use_absolute_path = true,
  --         },
  --       },
  --     },
  --     {
  --       -- Make sure to set this up properly if you have lazy=true
  --       'MeanderingProgrammer/render-markdown.nvim',
  --       opts = {
  --         file_types = { "markdown", "Avante" },
  --       },
  --       ft = { "markdown", "Avante" },
  --     },
  --   },
  -- }
}

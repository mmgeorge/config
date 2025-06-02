function filetype(context)
  if context.filetype == "typescriptreact" then
    return "typescriptreact"
  end

  return context.filetype
end

function detect_adapter()
  -- if os.getenv("OPENAI_API_KEY") then
  --   return {
  --     chat = "openai",
  --     inline = "openai",
  --     commit = {
  --       name = "gemini",
  --       -- model = "gemini-2.5-flash-preview-05-20",
  --       -- opts = { can_reason = false }
  --       model = "gemini-2.0-flash"
  --     }
  --     -- commit = {
  --     --   name = "openai",
  --     --   model = "gpt-4.1-mini"
  --     --   -- model = "gpt-4o-mini"
  --     -- }
  --   }
  -- end
  --
  if os.getenv("GEMINI_API_KEY") then
    return {
      chat = "gemini",
      inline = "gemini",
      cmd = "gemini",
      commit = {
        name = "gemini",
        model = "gemini-2.0-flash"
      }
    }
  end

  return {
    chat = "copilot",
    inline = "copilot_4o",
    cmd = "copilot_4o",
    commit = {
      name = "copilot_4o"
    }
  }
end

local adapter = detect_adapter()

return {
  {
    "olimorris/codecompanion.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-treesitter/nvim-treesitter",
      { "echasnovski/mini.diff", opts = {} },
      "j-hui/fidget.nvim",
    },
    keys = {
      {
        "<leader>cc",
        mode = { "n", "x" },
        ":CodeCompanion",
        desc = "Codecompanion Inline",
      },
      -- {
      --   "<leader>cm",
      --   mode = { "n" },
      --   ":CodeCompanion /commit<cr>",
      --   desc = "Codecompanion Commit",
      -- },
      {
        "oc",
        mode = { "n" },
        function()
          require("codecompanion").toggle()
        end,
        -- ":CodeCompanionChat Toggle<cr>",
        desc = "Codecompanion Open",
      },
      {
        "<leader>co",
        mode = { "n" },
        function()
          require("codecompanion").toggle()
        end,
        -- ":CodeCompanionChat Toggle<cr>",
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
      require("codecompanion_fidget"):init()
    end,
    opts = {
      opts = {
        -- log_level = "TRACE",
      },
      adapters = {
        copilot = function()
          return require("codecompanion.adapters").extend("copilot", {
            schema = {
              model = {
                default = "gpt-4.1",
                -- choices = {
                --   ["gemini-2.5-flash-preview-04-17"] = { opts = { can_reason = true } },
                --   "gemini-2.5-pro-preview-03-25",
                --   "gemini-2.0-flash",
                -- }
              },
            },
          })
        end,
        copilot_4o = function()
          return require("codecompanion.adapters").extend("copilot", {
            schema = {
              model = {
                default = "gpt-4o",
                -- choices = {
                --   ["gemini-2.5-flash-preview-04-17"] = { opts = { can_reason = true } },
                --   "gemini-2.5-pro-preview-03-25",
                --   "gemini-2.0-flash",
                -- }
              },
            },
          })
        end,
        gemini = function()
          return require("codecompanion.adapters").extend("gemini", {
            schema = {
              env = {
                api_key = "GEMINI_API_KEY"
              },
              model = {
                default = "gemini-2.5-flash-preview-04-17",
                choices = {
                  ["gemini-2.5-flash-preview-04-17"] = { opts = { can_reason = false } },
                  "gemini-2.5-pro-preview-03-25",
                  "gemini-2.0-flash",
                }
              },
            },
          })
        end,
        openai = function()
          return require("codecompanion.adapters").extend("openai", {
            schema = {
              env = {
                api_key = "OPENAI_API_KEY"
              },
              model = {
                default = "gpt-4.1",
              },
            },
          })
        end,
      },
      strategies = {
        chat = {
          adapter = adapter.chat,
          keymaps = {
            options = {
              modes = {
                n = "?",
              },
              callback = "keymaps.options",
              description = "Options",
              hide = true,
            },
            completion = {
              modes = {
                i = "<C-_>",
              },
              index = 1,
              callback = "keymaps.completion",
              description = "Completion Menu",
            },
            send = {
              modes = {
                n = { "<CR>", "<C-s>" },
                i = "<C-s>",
              },
              index = 2,
              callback = "keymaps.send",
              description = "Send",
            },
            regenerate = {
              modes = {
                n = "gr",
              },
              index = 3,
              callback = "keymaps.regenerate",
              description = "Regenerate the last response",
            },
            close = {
              modes = {
                n = "<C-c>",
                i = "<C-c>",
              },
              index = 4,
              callback = "keymaps.close",
              description = "Close Chat",
            },
            stop = {
              modes = {
                n = "q",
              },
              index = 5,
              callback = "keymaps.stop",
              description = "Stop Request",
            },
            clear = {
              modes = {
                n = "gx",
              },
              index = 6,
              callback = "keymaps.clear",
              description = "Clear Chat",
            },
            codeblock = {
              modes = {
                n = "gc",
              },
              index = 7,
              callback = "keymaps.codeblock",
              description = "Insert Codeblock",
            },
            yank_code = {
              modes = {
                n = "gl",
              },
              index = 8,
              callback = "keymaps.yank_code",
              description = "Yank Code",
            },
            pin = {
              modes = {
                n = "gp",
              },
              index = 9,
              callback = "keymaps.pin_reference",
              description = "Pin Reference",
            },
            watch = {
              modes = {
                n = "gw",
              },
              index = 10,
              callback = "keymaps.toggle_watch",
              description = "Watch Buffer",
            },
            next_chat = {
              modes = {
                n = "}",
              },
              index = 11,
              callback = "keymaps.next_chat",
              description = "Next Chat",
            },
            previous_chat = {
              modes = {
                n = "{",
              },
              index = 12,
              callback = "keymaps.previous_chat",
              description = "Previous Chat",
            },
            next_header = {
              modes = {
                n = "]]",
              },
              index = 13,
              callback = "keymaps.next_header",
              description = "Next Header",
            },
            previous_header = {
              modes = {
                n = "[[",
              },
              index = 14,
              callback = "keymaps.previous_header",
              description = "Previous Header",
            },
            change_adapter = {
              modes = {
                n = "ga",
              },
              index = 15,
              callback = "keymaps.change_adapter",
              description = "Change adapter",
            },
            fold_code = {
              modes = {
                n = "gf",
              },
              index = 15,
              callback = "keymaps.fold_code",
              description = "Fold code",
            },
            debug = {
              modes = {
                n = "gd",
              },
              index = 16,
              callback = "keymaps.debug",
              description = "View debug info",
            },
            system_prompt = {
              modes = {
                n = "gs",
              },
              index = 17,
              callback = "keymaps.toggle_system_prompt",
              description = "Toggle the system prompt",
            },
            auto_tool_mode = {
              modes = {
                n = "gta",
              },
              index = 18,
              callback = "keymaps.auto_tool_mode",
              description = "Toggle automatic tool mode",
            },
          },
        },
        inline = {
          adapter = adapter.inline,
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
          adapter = adapter.cmd,
        }
      },

      display = {
        chat = {
          auto_scroll = true,
          intro_message = "Welcome to CodeCompanion ?! Press ? for options",
          show_header_separator = true, -- Show header separators in the chat buffer? Set this to false if you're using an external markdown formatting plugin
          -- separator = "Ä",Th- see rapar totwben eee thffdientrent messages in the chat buff
          show_references = true,       -- Show references (from slash commands and variables) in the chat buffer?
          show_settings = false,        -- Show LLM settings at the top of the chat buffer?
          show_token_count = true,      -- Show the token count for each response?
          start_in_insert_mode = false, -- Open the chat buffer in insert mode?
          -- Options to customize the UI of the chat buffer
          window = {
            layout = "horizontal", -- float|vertical|horizontal|buffer
            position = "bottom",   -- left|right|top|bottom (nil will default depending on vim.opt.plitright|vim.opt.splitbelow)
            border = "single",
            height = 0.5,
            width = 0.5,
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
          close_chat_at = 240,    -- Close an open chat buffer if the total columns of your display are less than...
          -- with mini diff this is inline instead
          layout = "vertical",    -- vertical|horizontal split for default provider
          opts = { "internal", "filler", "closeoff", "algorithm:patience", "followwrap", "linematch:120" },
          provider = "mini_diff", -- default|mini_diff
        }
      },

      prompt_library = {
        ["Extract Function"] = {
          strategy = "workflow",
          description = "Extract function",
          opts = {
            -- placement = "after",
            index = 10,
            is_default = true,
            is_slash_cmd = true,
            short_name = "extract",
            -- auto_submit = true,
            -- user_prompt = true,

            -- adapter = adapter.commit,
          },
          prompts = {
            {
              {
                role = "system",
                opts = {
                  auto_submit = true
                },
                content = function(context)
                  local ft = filetype(context)

                  if ft == "typescriptreact" then
                    return
                    [[I want you to act as a senior SolidJS frontend developer, skilled in refactoring code according to clean code principles. The user will provide you with a buffer and a code snippet from it. Take the code snippet and extract it into a new function. Use arrow functions. You are likely be extracting some subset of a component into a new one.

Be sure to also out move any code that is only needed in the new function you created. Our goal is to see if we can simplify the code and remove any of the arguments you added in the new function. Pay close attention to any signals that you may now be able to move into the new function.
]]
                  end

                  return "I want you to act as a senior " .. ft ..
                      [[ developer, skilled in refactoring code according to clean code principles. The user will ask you to extract code, providing the snippet and buffer the snippet is from. Take the snippet, and move it to separate function below the current function. Do so in a way that minimizes the number of arguments that we need to pass to the new function.
]]
                end,
              },
              {
                role = "user",
                opts = {
                  contains_code = true,
                  auto_submit = true
                },
                content = function(context)
                  local code = require("codecompanion.helpers.actions").get_code(context.start_line, context.end_line)
                  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
                  -- local buffer_text = table.concat(lines, '\n')
                  local ft = filetype(context)

                  return string.format(
                    [[#buffer{watch} Extract the code snippet below:
```%s
%s
```
]], ft, code)
                end,
              },
              -- {
              --   role = "user",
              --   opts = {
              --     auto_submit = true
              --   },
              --   content = function(context)
              --     local ft = filetype(context)
              --
              --     if ft == "tsx" then
              --       return
              --       "Now move any code that is only needed in the new function you created. Our goal is to see if we can simplify the code and remove any of the arguments you added in the new function. Pay particular attention to any signals that you may now be able to move into the new function."
              --     end
              --
              --     return
              --     "Now move any code that is only needed in the new function you created. Our goal is to see if we can simplify the code and remove any of the arguments you added in the new function."
              --   end
              -- }

            },
            -- Wait until the end to submit this
            {
              {
                role = "user",
                opts = {
                  auto_submit = true
                },
                content = function()
                  return
                  "Can you apply the suggested changes to the buffer with the @editor tool? Replace the entire buffer's contents with the new ones."
                end
              }
            }
          },
        },

        ["Generate documentation"] = {
          strategy = "inline",
          description = "Generate documentation",
          opts = {
            placement = "before",
            index = 10,
            is_default = true,
            is_slash_cmd = true,
            short_name = "doc",
            auto_submit = true,
          },
          prompts = {
            {
              role = "user",
              content = function(context)
                local code = require("codecompanion.helpers.actions").get_code(context.start_line, context.end_line)
                local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
                local buffer_text = table.concat(lines, '\n')
                local ft = filetype(context)

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
]], ft, buffer_text, ft, code)
              end,
              opts = {
                contains_code = true,
              },
            },
          },
        },
        ["Generate a Commit Message"] = {
          strategy = "inline",
          description = "Generate a commit message",
          opts = {
            adapter = adapter.commit,
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
                  [[You are an expert at following the Conventional Commit specification. Given the git diff listed below, generate a commit message for me. Do not indent the first line. The description under the description should be no more than 2 sentences, with each line no more that 80 characters. Prefer omitting this description when possible. Do not list any breaking changes:
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

local function adapters()
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
      background = "gemini",
      commit = {
        name = "gemini",
        model = "gemini-3.1-flash-lite-preview",
        reasoning_effort = "none",
      }
    }
  end

  return {
    chat = "copilot",
    inline = "copilot",
    cmd = "copilot",
    background = "copilot",
    commit = "copilot",
  }
end

local function get_open_buffers_by_filetype(ft)
  local bufs = vim.api.nvim_list_bufs()
  local open_bufs = {}
  for _, buf in ipairs(bufs) do
    if vim.api.nvim_buf_is_loaded(buf)
        and vim.bo[buf].buflisted
        and vim.bo[buf].filetype == ft then
      table.insert(open_bufs, buf)
    end
  end
  return open_bufs
end

local function filetype(context)
  if context.filetype == "typescriptreact" then
    return "typescriptreact"
  end

  return context.filetype
end


return {
  {
    "zbirenbaum/copilot.lua",
    cmd = "Copilot",
    event = "InsertEnter",
    config = function()
      require("copilot").setup({})
    end,
  },
  -- {
  --   "azorng/goose.nvim",
  --   config = function()
  --     require("goose").setup({})
  --   end,
  --   dependencies = {
  --     "nvim-lua/plenary.nvim",
  --     "MeanderingProgrammer/render-markdown.nvim",
  --   },
  -- },
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
      vim.api.nvim_create_autocmd({ "BufEnter" }, {
        pattern = "COMMIT_EDITMSG",
        callback = function(args)
          print("Creating commit message!")
          if vim.b[args.buf].ai_commit_generated then return end

          local lines = vim.api.nvim_buf_get_lines(args.buf, 0, -1, false)
          if lines[1] and lines[1] ~= "" then return end

          vim.b[args.buf].ai_commit_generated = true

          local diff = vim.fn.system("git diff --no-ext-diff --staged")
          if vim.v.shell_error ~= 0 or vim.trim(diff) == "" then return end

          local Background = require("codecompanion.interactions.background")
          local adapter_config = adapters().commit
          local adapter_name = type(adapter_config) == "table" and adapter_config.model or adapter_config
          local background = Background.new({ adapter = adapter_config })

          local spinner = { "⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏" }
          local notif_id = "commit_msg"

          vim.notify("Generating commit message...", "info", {
            id = notif_id,
            title = "commit (" .. adapter_name .. ")",
            opts = function(notif)
              notif.icon = spinner[math.floor(vim.uv.hrtime() / (1e6 * 80)) % #spinner + 1]
            end,
          })

          background:ask({
            {
              role = "system",
              content = [[You are a precise commit message generator following Conventional Commits.
Respond with ONLY the commit message — no code blocks, no explanation.

Format:
<type>: <description>

[body]

Types: feat, fix, docs, style, refactor, perf, test, build, ci, chore, revert

Description rules:
- Imperative mood ("add feature" not "added feature")
- Lowercase first letter, no trailing period
- Max 50 characters

Body (optional):
- Never write a body for chore commits
- Only include a body for significant changes
- Explain why, not what. Wrap at 72 chars. Max three lines.]]
            },
            {
              role = "user",
              content = "Generate a conventional commit message for these staged changes:\n```diff\n" .. diff .. "\n```",
            },
          }, {
            method = "async",
            silent = true,
            on_done = function(result)
              vim.schedule(function()
                if not vim.api.nvim_buf_is_valid(args.buf) then return end

                local content = result and result.output and result.output.content
                if not content then
                  vim.notify("No response from model", "warn", { id = notif_id, title = "commit" })
                  return
                end
                local msg = vim.trim(content)
                if msg == "" then return end

                msg = msg:gsub("^```%w*\n", ""):gsub("\n```$", "")

                local msg_lines = vim.split(msg, "\n", { plain = true })
                vim.api.nvim_buf_set_lines(args.buf, 0, 0, false, msg_lines)

                local winid = vim.fn.bufwinid(args.buf)
                if winid ~= -1 then
                  vim.api.nvim_win_set_cursor(winid, { 1, 0 })
                end

                vim.notify(msg_lines[1], "info", {
                  id = notif_id,
                  title = "commit (" .. adapter_name .. ")",
                  icon = " ",
                })
              end)
            end,
            on_error = function(err)
              vim.schedule(function()
                vim.notify("Failed: " .. tostring(err), "error", {
                  id = notif_id,
                  title = "commit (" .. adapter_name .. ")",
                  icon = " ",
                })
              end)
            end,
          })
        end,
      })
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
            layout = "horizontal",
            position = "bottom",
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
      },
      prompt_library = {
        ["Generate documentation"] = {
          interaction = "inline",
          description = "Generate documentation",
          opts = {
            placement = "before",
            index = 10,
            is_default = true,
            is_slash_cmd = true,
            alias = "doc",
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
        ["Generate component"] = {
          interaction = "chat",
          description = "Generate component",
          opts = {
            index = 100,
            is_default = true,
            is_slash_cmd = true,
            alias = "comp",
            auto_submit = true,
            user_prompt = false,
            stop_context_insertion = true
          },
          prompts = {
            {
              role = "system",
              content = function(context)
                return
                  [[**Instructions**
Act as a senior SolidJS Typescript frontend developer, skilled in writing new code components. The user will provide you with several buffers that show what some of the code in the current code base looks like. Use this code as a reference, and try to match the style and code shown. You will then be prompted to create a new standalone component.

**Guidlines**
- Prefer using arrow functions
- Use interfaces for props. Name in this format: IComponentNameProps
- Make callbacks optional
- When designing generic components, include common functionality that you think might be needed.
- Once done, prompt the user with some additional options they might want to add to the component
]]
              end,
            },
            {
              role = "user",
              content = function(context)
                local ft = context.filetype
                local buffers = get_open_buffers_by_filetype(ft)
                local buffer_texts = {}
                for _, buffer in ipairs(buffers) do
                  local buffer_name = vim.api.nvim_buf_get_name(buffer)
                  local buffer_text = table.concat(vim.api.nvim_buf_get_lines(buffer, 0, -1, false), '\n')
                  table.insert(buffer_texts, string.format(
                    [[
Buffer: %s
```%s
%s
```
]], buffer_name, ft, buffer_text))
                end

                local context = table.concat(buffer_texts, "\n")


                return "**Context**\n" .. context .. "\n\n"
              end,
            },
          },
        },
      }
    }
  },
}

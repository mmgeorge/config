local function adapters()
  return require("ai.adapters").get()
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

-- Inline editing commands (ported from the CodeCompanion inline prompt
-- library) on top of require("ai.inline"): the model generates complete
-- replacement/insertion text that is spliced around the selection.

---@param cmd_args table user command callback args
---@return AIInlineRange
local function inline_range(cmd_args)
  return { start_line = cmd_args.line1, end_line = cmd_args.line2 }
end

---@param buf integer
---@return fun(selected_text: string): string
local function doc_prompt(buf)
  return function(selected_text)
    local ft = vim.bo[buf].filetype
    local buffer_text = table.concat(vim.api.nvim_buf_get_lines(buf, 0, -1, false), "\n")
    return string.format(
      [[Write a documentation comment for the selected code. Follow these rules:
1. Use the idiomatic documentation style for the language (for instance jsdoc for javascript and typescript, /// doc comments for rust).
2. If the code already has documentation, incorporate its content.
3. Be as succinct as possible (at most 2 sentences).
4. Do NOT document specific function parameters or the return value.
5. Keep every line under 80 characters.
6. Respond with ONLY the comment lines that go directly above the selection.

For additional context, the entire file is:
```%s
%s
```

The code to document:
```%s
%s
```]],
      ft, buffer_text, ft, selected_text
    )
  end
end

local function setup_inline_commands()
  vim.api.nvim_create_user_command("AIDoc", function(cmd_args)
    require("ai.inline").inline({
      type = "before",
      range = inline_range(cmd_args),
      prompt = doc_prompt(vim.api.nvim_get_current_buf()),
    })
  end, { range = true, desc = "AI: insert documentation above the selection" })

  vim.api.nvim_create_user_command("AIReplace", function(cmd_args)
    require("ai.inline").inline({
      type = "replace",
      range = inline_range(cmd_args),
      prompt = cmd_args.args,
    })
  end, { range = true, nargs = "+", desc = "AI: rewrite the selection per the instruction" })

  vim.api.nvim_create_user_command("AIBefore", function(cmd_args)
    require("ai.inline").inline({
      type = "before",
      range = inline_range(cmd_args),
      prompt = cmd_args.args,
    })
  end, { range = true, nargs = "+", desc = "AI: insert generated text above the selection" })

  vim.api.nvim_create_user_command("AIAfter", function(cmd_args)
    require("ai.inline").inline({
      type = "after",
      range = inline_range(cmd_args),
      prompt = cmd_args.args,
    })
  end, { range = true, nargs = "+", desc = "AI: insert generated text below the selection" })
end

local function first_file_line(path)
  local ok, lines = pcall(vim.fn.readfile, path, "", 1)
  if not ok or not lines[1] then
    return nil
  end

  local line = vim.trim(lines[1])
  if line == "" then
    return nil
  end

  return line
end

local function absolute_path(path, base)
  if path:sub(1, 1) == "/" then
    return vim.fs.normalize(path)
  end

  return vim.fs.normalize(vim.fs.joinpath(base, path))
end

local function worktree_root_from_git_dir(git_dir)
  if not git_dir or git_dir == "" then
    return nil
  end

  local worktree_git_file = first_file_line(vim.fs.joinpath(git_dir, "gitdir"))
  if worktree_git_file then
    return vim.fs.dirname(absolute_path(worktree_git_file, git_dir))
  end

  if vim.fs.basename(git_dir) == ".git" then
    return vim.fs.dirname(git_dir)
  end

  return nil
end

local function commit_buffer_git_root(buf)
  local name = vim.api.nvim_buf_get_name(buf)
  if name == "" or vim.fs.basename(name) ~= "COMMIT_EDITMSG" then
    return nil
  end

  return worktree_root_from_git_dir(vim.fs.dirname(name))
end

-- Commit-message generation lives in diff_review.ai_commit (context building,
-- pregeneration state, staged/HEAD fingerprint reuse) on top of the `ai`
-- prompt library. This file only wires the global triggers: pregenerate on
-- Neogit refresh, and populate plain COMMIT_EDITMSG buffers that DiffReview
-- does not own.
local pregen_debounce_timer = nil

local function pregenerate_commit()
  vim.system({ "git", "rev-parse", "--show-toplevel" }, {
    text = true,
    stdout = true,
    stderr = true,
  }, function(result)
    vim.schedule(function()
      if result.code ~= 0 then return end
      local root = vim.trim(result.stdout or "")
      if root == "" then return end
      require("diff_review.ai_commit").ensure(root, { ref = "HEAD" })
    end)
  end)
end

local function schedule_pregenerate()
  if pregen_debounce_timer and not pregen_debounce_timer:is_closing() then
    pregen_debounce_timer:stop()
  else
    pregen_debounce_timer = vim.uv.new_timer()
  end
  pregen_debounce_timer:start(500, 0, vim.schedule_wrap(function()
    pregenerate_commit()
  end))
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
      -- {
      --   "<leader>ai",
      --   mode = { "n", "x" },
      --   ":CodeCompanion #buffer add the following impls for the selected code: ",
      --   desc = "AI Implement",
      -- },
      -- {
      --   "<leader>ad",
      --   mode = { "n", "x" },
      --   ":CodeCompanion #buffer generate an enum dispatch method, which calls the following method on each underlying variant: ",
      --   desc = "AI Enum Dispatch",
      -- },
      -- {
      --   "<leader>ae",
      --   mode = { "n", "x" },
      --   ":CodeCompanion /extract<cr>",
      --   desc = "AI Extract Function",
      -- },
      -- {
      --   "<leader>ad",
      --   mode = { "n", "x" },
      --   ":CodeCompanion /doc<cr>",
      --   desc = "AI Document",
      -- },
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
      setup_inline_commands()
      vim.api.nvim_create_autocmd("User", {
        pattern = "NeogitStatusRefreshed",
        callback = function()
          schedule_pregenerate()
        end,
      })
      vim.api.nvim_create_autocmd({ "BufEnter" }, {
        pattern = "COMMIT_EDITMSG",
        callback = function(args)
          if vim.b[args.buf].diff_review_commit_buffer then return end
          if vim.b[args.buf].ai_commit_generated then return end

          local cwd = commit_buffer_git_root(args.buf)
          if not cwd then
            vim.notify("Could not resolve git root for commit message", vim.log.levels.WARN, { title = "commit" })
            return
          end

          require("diff_review.ai_commit").populate_commit_buffer_when_ready(args.buf, cwd, function(message, level)
            vim.notify(message, level, { title = "commit" })
          end)
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

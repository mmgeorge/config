local karma_task = nil
local karma_file = nil
local karma_grep_str = nil

local function url_encode(str)
  if not str then return "" end

  local suffix = str:find(" ") and "%24" or ""
  local safe = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-._~"
  local encoded = str:gsub(".", function(char)
    if char == "-" then
      return "%5Cx2d"
    elseif char == "(" then
      return "%5C("
    elseif char == ")" then
      return "%5C)"
    elseif safe:find(char, 1, true) then
      return char
    else
      return string.format("%%%02X", char:byte())
    end
  end)

  return encoded .. suffix
end

function encode_current_string()
  local lnum, col = unpack(vim.api.nvim_win_get_cursor(0))
  local cursor_col_1based = col + 1 -- Adjust to 1-based indexing for Lua string functions

  local line = vim.api.nvim_get_current_line()
  local pattern = '([\'"`])(.-)([\'"`])'
  local start_col, end_col, inner_match = nil, nil, nil

  local search_start = 1
  while true do
    local m_start, m_end, open_quote, inner, close_quote = line:find(pattern, search_start)
    if not m_start then
      break
    end
    if cursor_col_1based >= m_start and cursor_col_1based <= m_end then
      start_col = m_start + 1 -- skip opening quote
      end_col = m_end - 1     -- skip closing quote
      inner_match = inner
      break
    end
    search_start = m_end + 1
  end

  if inner_match then
    local encoded_content = url_encode(inner_match)
    return encoded_content
  else
    return nil
  end
end

function create_tab_script(include_delay, grep_str)
  local delay_snippet = ''
  if include_delay then
    delay_snippet = [[
    -- Wait for karma to start, then refresh the tab.
    delay 2.5
    ]]
  end

  local url_base = 'http://localhost:9876/debug.html'
  local url = url_base
  if grep_str ~= nil then
    url = url_base .. '?grep=' .. grep_str
  end

  return [[
  osascript -e '
    -- Capture terminal to restore after
    tell application "System Events"
      set currentApp to name of first application process whose frontmost is true
    end tell

    set tabIndex to 0
    set targetURLBase to "]] .. url_base .. [["
    set targetURL to "]] .. url .. [["

    tell application "Google Chrome"
      if not running then
        open location targetURL
      else
        set found to false

        repeat with w in windows
          set tabIndex to 1
          repeat with t in tabs of w
            if URL of t starts with targetURLBase then
              set found to true
              set index of w to 1
              set active tab index of window 1 to tabIndex
              set URL of t to targetURL
              exit repeat
            end if
            set tabIndex to tabIndex + 1
          end repeat
          if found then exit repeat
        end repeat

        if not found then
          make new tab at end of window 1 with properties {URL:targetURL}
        end if
        activate
      end if
    end tell

    -- Immediately return control to the terminal
    tell application "System Events"
      set frontmost of process currentApp to true
    end tell
]] .. delay_snippet .. [[
    tell application "Google Chrome"
      activate
      reload active tab of window 1
    end tell

    -- Return control back to the terminal
    tell application "System Events"
      set frontmost of process currentApp to true
    end tell
  '
]]
end

local components = {
  { "on_complete_dispose", require_view = {}, timeout = 1 },
  "default",
  "open_output"
}

return {
  {
    'stevearc/overseer.nvim',
    dependencies = {
      { "akinsho/toggleterm.nvim" },
      -- { "rcarriga/nvim-notify" },
      { "stevearc/dressing.nvim" },
    },
    keys = {
      {
        "<leader>tp",
        "<cmd>OverseerRun<CR>",
        mode = { "n", "x" },
        desc = "Pick Task",
      },
      {
        "<leader>tq",
        function()
          if karma_task ~= nil then
            karma_task:dispose(true)
          end
        end,
        mode = { "n", "x" },
        desc = "Kill Karma Task",
      },
      {
        "<leader>tt",
        function()
          local overseer = require("overseer")
          local file = vim.fn.expand '%:~:.'
          karma_grep_str = encode_current_string()

          if karma_file == file then
            vim.fn.jobstart(create_tab_script(false, karma_grep_str))
            return
          end

          karma_file = file

          if karma_task ~= nil then
            karma_task:dispose(true)
          end

          karma_task = overseer.new_task({
            cmd = { 'npx', 'karma', 'start' },
            args = { "--root", file },
          })

          karma_task:start()

          vim.fn.jobstart(create_tab_script(true, karma_grep_str))
        end,
        mode = { "n", "x" },
        desc = "Run Karma",
      },
      {
        "<leader>tr",
        function()
          vim.fn.jobstart(create_tab_script(false, karma_grep_str))
        end,
        mode = { "n", "x" },
        desc = "Refresh Karma Tab",
      },
      {
        "<leader>tl",
        "<cmd>OverseerToggle<CR>",
        mode = { "n", "x" },
        desc = "Open Last Task",
      },

      {
        "<leader>to",
        "<cmd>OverseerToggle<CR>",
        mode = { "n", "x" },
        desc = "Open Tasks",
      },
    },
    config = function()
      require("overseer").setup({
        strategy = {
          "toggleterm",
          -- load your default shell before starting the task
          use_shell = false,
          -- overwrite the default toggleterm "direction" parameter
          direction = nil,
          -- overwrite the default toggleterm "highlights" parameter
          highlights = nil,
          -- overwrite the default toggleterm "auto_scroll" parameter
          auto_scroll = nil,
          -- have the toggleterm window close and delete the terminal buffer
          -- automatically after the task exits
          close_on_exit = false,
          -- have the toggleterm window close without deleting the terminal buffer
          -- automatically after the task exits
          -- can be "never, "success", or "always". "success" will close the window
          -- only if the exit code is 0.
          quit_on_exit = "never",
          -- open the toggleterm window when a task starts
          open_on_start = false,
          -- mirrors the toggleterm "hidden" parameter, and keeps the task from
          -- being rendered in the toggleable window
          hidden = false,
          -- command to run when the terminal is created. Combine with `use_shell`
          -- to run a terminal command before starting the task
          on_create = nil,
        },
        -- Default task strategy
        -- strategy = "terminal",
        -- Template modules to load
        templates = { "builtin" },
        -- When true, tries to detect a green color from your colorscheme to use for success highlight
        auto_detect_success_color = true,
        -- Patch nvim-dap to support preLaunchTask and postDebugTask
        dap = true,
        -- Configure the task list
        task_list = {
          -- Default detail level for tasks. Can be 1-3.
          default_detail = 1,
          -- Width dimensions can be integers or a float between 0 and 1 (e.g. 0.4 for 40%)
          -- min_width and max_width can be a single value or a list of mixed integer/float types.
          -- max_width = {100, 0.2} means "the lesser of 100 columns or 20% of total"
          max_width = { 100, 0.2 },
          -- min_width = {40, 0.1} means "the greater of 40 columns or 10% of total"
          min_width = { 40, 0.1 },
          -- optionally define an integer/float for the exact width of the task list
          width = nil,
          max_height = { 20, 0.1 },
          min_height = 8,
          height = nil,
          -- String that separates tasks
          -- separator = "컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴",
          -- Default direction. Can be "left", "right", or "bottom"
          direction = "bottom",
          -- Set keymap to false to remove default behavior
          -- You can add custom keymaps here as well (anything vim.keymap.set accepts)
          bindings = {
            ["?"] = "ShowHelp",
            ["g?"] = "ShowHelp",
            ["<CR>"] = "RunAction",
            ["<C-e>"] = "Edit",
            ["o"] = "Open",
            ["<C-v>"] = "OpenVsplit",
            ["<C-s>"] = "OpenSplit",
            ["<C-f>"] = "OpenFloat",
            ["<C-q>"] = "OpenQuickFix",
            ["p"] = "TogglePreview",
            ["<C-l>"] = "IncreaseDetail",
            ["<C-h>"] = "DecreaseDetail",
            ["L"] = "IncreaseAllDetail",
            ["H"] = "DecreaseAllDetail",
            ["["] = "DecreaseWidth",
            ["]"] = "IncreaseWidth",
            ["{"] = "PrevTask",
            ["}"] = "NextTask",
            ["<C-k>"] = "ScrollOutputUp",
            ["<C-j>"] = "ScrollOutputDown",
            ["q"] = "Close",
          },
        },
        -- See :help overseer-actions
        actions = {},
        -- Configure the floating window used for task templates that require input
        -- and the floating window used for editing tasks
        form = {
          border = "rounded",
          zindex = 40,
          -- Dimensions can be integers or a float between 0 and 1 (e.g. 0.4 for 40%)
          -- min_X and max_X can be a single value or a list of mixed integer/float types.
          min_width = 80,
          max_width = 0.9,
          width = nil,
          min_height = 10,
          max_height = 0.9,
          height = nil,
          -- Set any window options here (e.g. winhighlight)
          win_opts = {
            winblend = 0,
          },
        },
        task_launcher = {
          -- Set keymap to false to remove default behavior
          -- You can add custom keymaps here as well (anything vim.keymap.set accepts)
          bindings = {
            i = {
              ["<C-s>"] = "Submit",
              ["<C-c>"] = "Cancel",
            },
            n = {
              ["<CR>"] = "Submit",
              ["<C-s>"] = "Submit",
              ["q"] = "Cancel",
              ["?"] = "ShowHelp",
            },
          },
        },
        task_editor = {
          -- Set keymap to false to remove default behavior
          -- You can add custom keymaps here as well (anything vim.keymap.set accepts)
          bindings = {
            i = {
              ["<CR>"] = "NextOrSubmit",
              ["<C-s>"] = "Submit",
              ["<Tab>"] = "Next",
              ["<S-Tab>"] = "Prev",
              ["<C-c>"] = "Cancel",
            },
            n = {
              ["<CR>"] = "NextOrSubmit",
              ["<C-s>"] = "Submit",
              ["<Tab>"] = "Next",
              ["<S-Tab>"] = "Prev",
              ["q"] = "Cancel",
              ["?"] = "ShowHelp",
            },
          },
        },
        -- Configure the floating window used for confirmation prompts
        confirm = {
          border = "rounded",
          zindex = 40,
          -- Dimensions can be integers or a float between 0 and 1 (e.g. 0.4 for 40%)
          -- min_X and max_X can be a single value or a list of mixed integer/float types.
          min_width = 20,
          max_width = 0.5,
          width = nil,
          min_height = 6,
          max_height = 0.9,
          height = nil,
          -- Set any window options here (e.g. winhighlight)
          win_opts = {
            winblend = 0,
          },
        },
        -- Configuration for task floating windows
        task_win = {
          -- How much space to leave around the floating window
          padding = 2,
          border = "rounded",
          -- Set any window options here (e.g. winhighlight)
          win_opts = {
            winblend = 0,
          },
        },
        -- Configuration for mapping help floating windows
        help_win = {
          border = "rounded",
          win_opts = {},
        },
        -- Aliases for bundles of components. Redefine the builtins, or create your own.
        component_aliases = {
          -- Most tasks are initialized with the default components
          default = {
            "on_exit_set_status",
            -- "on_complete_notify",
            { "on_complete_dispose", require_view = { "SUCCESS", "FAILURE" } },
          },
          -- Tasks from tasks.json use these components
          default_vscode = {
            "default",
            "on_result_diagnostics",
          },
        },
        bundles = {
          -- When saving a bundle with OverseerSaveBundle or save_task_bundle(), filter the tasks with
          -- these options (passed to list_tasks())
          save_task_opts = {
            bundleable = true,
          },
          -- Autostart tasks when they are loaded from a bundle
          autostart_on_load = true,
        },
        -- A list of components to preload on setup.
        -- Only matters if you want them to show up in the task editor.
        preload_components = {},
        -- Controls when the parameter prompt is shown when running a template
        --   always    Show when template has any params
        --   missing   Show when template has any params not explicitly passed in
        --   allow     Only show when a required param is missing
        --   avoid     Only show when a required param with no default value is missing
        --   never     Never show prompt (error if required param missing)
        default_template_prompt = "allow",
        -- For template providers, how long to wait (in ms) before timing out.
        -- Set to 0 to disable timeouts.
        template_timeout = 3000,
        -- Cache template provider results if the provider takes longer than this to run.
        -- Time is in ms. Set to 0 to disable caching.
        template_cache_threshold = 100,
        -- Configure where the logs go and what level to use
        -- Types are "echo", "notify", and "file"
        log = {
          {
            type = "echo",
            level = vim.log.levels.WARN,
          },
          {
            type = "file",
            filename = "overseer.log",
            level = vim.log.levels.WARN,
          },
        },
      })

      -- require("overseer").register_template({
      --   name = "Tsc FastWatch",
      --   builder = function()
      --     return {
      --       cmd = { "npx", "tsc", "--watch", "--assumeChangesOnlyAffectDirectDependencies --noEmit" },
      --       components = {
      --         {
      --           "on_output_parse",
      --           problem_matcher = "$tsc-watch"
      --         },
      --         "default",
      --         "on_result_notify",
      --         "on_result_diagnostics",
      --         "on_complete_restart",
      --         "on_exit_set_status",
      --       },
      --       env = {
      --         NODE_OPTIONS = "--max-old-space-size=8192"
      --       },
      --     }
      --   end
      -- })
      --
      -- require("overseer").register_template({
      --   name = "Tsc Watch",
      --   builder = function()
      --     return {
      --       cmd = { "npx", "tsc", "--watch", "--noEmit", "-p", "tsconfig.app.json" },
      --       components = {
      --         {
      --           "on_output_parse",
      --           problem_matcher = "$tsc-watch"
      --         },
      --         "default",
      --         "on_result_notify",
      --         "on_result_diagnostics",
      --         "on_complete_restart",
      --         "on_exit_set_status",
      --       },
      --       env = {
      --         NODE_OPTIONS = "--max-old-space-size=8192"
      --       },
      --     }
      --   end
      -- })
      --
      require("overseer").register_template({
        -- Required fields
        name = "Karma start",
        builder = function(params)
          local file = vim.fn.expand '%:~:.'
          -- return {
          --   name = "Start karma",
          --   strategy = {
          --     "orchestrator",
          --     tasks = {
          --       {
          --         name = "Start Karma",
          --         cmd = { 'npx', 'karma', 'start' },
          --         args = { "--root", file },
          --       },
          --       {
          --         name = "Open Windows Chrome",
          --         cmd = {
          --           "powershell.exe",
          --           "-NoProfile",
          --           "-Command",
          --           "Start-Process",
          --           "chrome.exe",
          --           "http://localhost:5137"
          --         },
          --         -- cmd = { "cmd.exe", "/c", "start", "chrome.exe", "http://localhost:9876/debug.html" },
          --       }
          --     }
          --   }
          -- }
          -- This must return an overseer.TaskDefinition
          return {
            -- cmd is the only required field
            cmd = { 'npx', 'karma', 'start' },
            -- additional arguments for the cmd
            args = { "--root", file },
            -- the name of the task (defaults to the cmd of the task)
            -- name = "",
            -- set the working directory for the task
            -- cwd = "/tmp",
            -- additional environment variables
            env = {
            },
            -- the list of components or component aliases to add to the task
            -- components = {"my_custom_component", "default"},
            components = {},
            -- arbitrary table of data for your own personal use
            metadata = {
            },
          }
        end,
        -- Optional fields
        -- desc = "Optional description of task",
        -- Tags can be used in overseer.run_template()
        -- tags = {overseer.TAG.BUILD},
        params = {
          -- See :help overseer-params
        },
        -- Determines sort order when choosing tasks. Lower comes first.
        priority = 50,
        -- Add requirements for this template. If they are not met, the template will not be visible.
        -- All fields are optional.
        condition = {
          -- A string or list of strings
          -- Only matches when current buffer is one of the listed filetypes
          filetype = { "typescript" },
          -- A string or list of strings
          -- Only matches when cwd is inside one of the listed dirs
          -- dir = "/home/user/my_project",
          -- Arbitrary logic for determining if task is available
          -- callback = function(search)
          -- print(vim.inspect(search))
          -- return true
          -- end,
        },
      });
    end,
  }
}

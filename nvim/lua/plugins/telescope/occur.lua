local provides = {}

local utils = require("utils")
local builtin = require("telescope.builtin")
local actions = require("telescope.actions")
local action_state = require "telescope.actions.state"

provides.occur_in_file = function ()
  local action_set = require "telescope.actions.set"
  local initial_window = vim.api.nvim_get_current_win()
  local initial_buffer = vim.api.nvim_get_current_buf()
  local initial_buffer_line_count = vim.api.nvim_buf_line_count(initial_buffer)
  local initial_cursor = vim.api.nvim_win_get_cursor(0);

  local ns_id = vim.api.nvim_create_namespace("buffer text search")

  local win_height = vim.api.nvim_win_get_height(initial_window)
  local picker_height = 20 -- win_height * .4 -- Fix hardcode
  local top_height = win_height - picker_height
  local half_top_height = math.floor(top_height / 2) - 4 -- 4 to compensate. Telescope window not exact
  -- local cursor_offset = picker_height + half_top_height

  -- This is the best we can do. The value always gets clamped at win_height / 2, though
  -- ideally when we have a window below, we'd like it to be larger than this.
  --vim.api.nvim_win_set_option(0, 'scrolloff', 999):
  -- local prompt_height = math.floor(win_height * .3) - 20
  --vim.api.nvim_win_set_option(initial_window, 'scrolloff',
  --                          win_height)
  --vim.api.nvim_win_set_option(initial_window, 'scrolloff', math.ceil(win_height / 2))

  local last_mark
  local did_accept = false

  -- hide cursor
  --vim.api.nvim_command('set mouse=n')
  --vim.api.nvim_set_option('guicursor', '')
  --vim.api.nvim_set_option('guicursor', 'a:noCursor')
  -- to reveal:     vim.api.nvim_command('set mouse=a')

  local function on_occur_change(prompt, map)
    actions.select_default:replace(function ()
      if last_mark then
        vim.api.nvim_buf_del_extmark(initial_buffer, ns_id, last_mark)
      end

      did_accept = true 
      return action_set.select(prompt, "default")
    end)

    actions.close:replace(function(prompt_bufnr)
      local a = vim.api
      local picker = action_state.get_current_picker(prompt_bufnr)
      local original_win_id = picker.original_win_id
      local cursor_valid, original_cursor = pcall(a.nvim_win_get_cursor, original_win_id)

      actions.close_pum(prompt_bufnr)

      require("telescope.pickers").on_close_prompt(prompt_bufnr)
      pcall(a.nvim_set_current_win, original_win_id)
      if cursor_valid and a.nvim_get_mode().mode == "i" and picker._original_mode ~= "i" then
        pcall(a.nvim_win_set_cursor, original_win_id, { original_cursor[1], original_cursor[2] + 1 })
      end

      if last_mark then
        vim.api.nvim_buf_del_extmark(initial_buffer, ns_id, last_mark)
      end

      if not did_accept then
        -- restore original cursor
        vim.api.nvim_win_set_cursor(initial_window, initial_cursor)
      end 
    end
    )

    actions.move_selection_next:replace(function ()
      action_set.shift_selection(prompt, 1)

      local selection = action_state.get_selected_entry(prompt)
      local lnum = selection.lnum - 1

      if last_mark then
        vim.api.nvim_buf_del_extmark(initial_buffer, ns_id, last_mark)
      end
      -- vim.api.nvim_buf_clear_namespace(initial_buffer, -1, ns_id, -1)

      last_mark = vim.api.nvim_buf_set_extmark(
        initial_buffer,
        ns_id,
        lnum,
        0,
        {
          end_row = lnum,
          line_hl_group = 'CursorColumn',
          sign_hl_group = 'CursorColumn',
          number_hl_group = 'CursorColumn',
        }
      )

      -- Clamp to the buffer max line count
      -- local next_cursor_position = math.min(initial_buffer_line_count, lnum + cursor_offset )
      -- vim.api.nvim_win_set_cursor(initial_window, { lnum, 1 })
      vim.api.nvim_buf_call(initial_buffer, function()
        vim.api.nvim_command('normal! ' .. lnum - half_top_height .. 'zt')
      end)
      -- vim.api.nvim_win_call(
        -- initial_window,
        -- function ()
          -- vim.api.nvim_feedkeys('<C-e>', 'n', false, initial_window) 
          -- vim.cmd('winrestview({})')
          -- vim.cmd("normal! 30<C-e>")
          -- vim.cmd('normal! zt10<C-y>')
        -- end
      -- )
    end)

    actions.move_selection_previous:replace(function ()
      action_set.shift_selection(prompt, -1)

      local selection = action_state.get_selected_entry(prompt)
      local lnum = selection.lnum - 1

      -- vim.api.nvim_win_set_cursor(initial_window, { math.max(1, lnum - half_top_height), 1 })

      if last_mark then
        vim.api.nvim_buf_del_extmark(initial_buffer, ns_id, last_mark)
      end
      -- vim.api.nvim_buf_clear_namespace(initial_buffer, -1, ns_id, -1)

      last_mark = vim.api.nvim_buf_set_extmark(
        initial_buffer,
        ns_id,
        lnum,
        0,
        {
          end_row = lnum,
          line_hl_group = 'CursorColumn',
          sign_hl_group = 'CursorColumn',
          number_hl_group = 'CursorColumn',
        }
      )

      vim.api.nvim_buf_call(initial_buffer, function()
        vim.api.nvim_command('normal! ' .. lnum - half_top_height .. 'zt')
      end)

    end)

    return true
  end


  local opts = {
    attach_mappings=on_occur_change,
    -- default_text=utils.get_visual_selection(),
    -- tiebreak = function(entry1, entry2, prompt)
    --    local start_pos1, _ = entry1.ordinal:find(prompt)
    --    if start_pos1 then
    --       local start_pos2, _ = entry2.ordinal:find(prompt)
    --       if start_pos2 then
    --          return start_pos1 < start_pos2
    --       end
    --    end
    --    return false
    -- end,
    -- additional_args = { "--ignore-case", "--pcre2" },
  }

  builtin.current_buffer_fuzzy_find(opts)
end

return provides

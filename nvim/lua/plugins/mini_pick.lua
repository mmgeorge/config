local function replace_backslash_with_slash(input)
  if type(input) == "string" then
    return string.gsub(input, "\\", "/")
  end
  
  input.text =string.gsub(input.text, "\\", "/") 
  return input
end

local function replace_slash_with_backslash(input)
  return string.gsub(input, "/", "\\")
end

return {
  {
    'echasnovski/mini.extra',
    version = '*', 
    config = function ()
      require"mini.extra".setup()
    end
  },
  {
    'echasnovski/mini.pick',
    version = '*',
    keys = {
      {
        "or",
        function ()
          require"mini.pick".registry.explorer({
            cwd = vim.fn.expand('%:p:h')
          }) 
        end,
        mode = { "n", "x" },
        desc = "Explore Files",
      }, 
      {
        "os",
        function ()
          require"mini.pick".registry.xbuffers() 
        end,
        mode = { "n", "x" },
        desc = "Search Buffer",
      }, 
      {
        "ot",
        function ()
          require"mini.pick".registry.grep_live() 
        end,
        mode = { "n", "x" },
        desc = "Grep Live",
      }, 
      {
        "oh",
        function ()
          require"mini.pick".registry.files() 
        end,
        mode = { "n", "x" },
        desc = "Search Files",
      },
      {
        "oo",
        function ()
          require"mini.pick".registry.lsp({
            scope = "document_symbol",
            -- symbol_query = "function" How to get this to work?
          }) 
        end,
        mode = { "n", "x" },
        desc = "Search Functions",
      }, 
      {
        "Sr",
        function ()
          require"mini.pick".registry.spellsuggest({
            n_suggestions = 10
          }) 
        end,
        mode = { "n", "x" },
        desc = "Search Functions",
      }, 
    },
    config = function ()
      local MiniPick = require"mini.pick"
     
      MiniPick.setup({
        -- Delays (in ms; should be at least 1)
        delay = {
          -- Delay between forcing asynchronous behavior
          async = 10,

          -- Delay between computation start and visual feedback about it
          busy = 50,
        },

        -- Keys for performing actions. See `:h MiniPick-actions`.
        mappings = {
          caret_left  = '<Left>',
          caret_right = '<Right>',

          choose            = '<CR>',
          choose_in_split   = '<C-s>',
          choose_in_tabpage = '<C-t>',
          choose_in_vsplit  = '<C-v>',
          choose_marked     = '<M-CR>',

          delete_char       = '<BS>',
          delete_char_right = '<Del>',
          delete_left       = '<C-u>',
          delete_word       = '<C-w>',

          mark     = '<A-n>',
          mark_all = '<C-a>',

          move_down  = '<A-a>',
          move_start = '<C-g>',
          move_up    = '<A-e>',

          paste = '<C-y>',

          refine        = '<C-Space>',
          refine_marked = '<M-Space>',

          scroll_down  = '<C-f>',
          scroll_left  = '<C-h>',
          scroll_right = '<C-l>',
          scroll_up    = '<C-b>',

          stop = '<Esc>',

          toggle_info    = '<S-Tab>',
          toggle_preview = '<Tab>',
        },

        -- General options
        options = {
          -- Whether to show content from bottom to top
          content_from_bottom = false,

          -- Whether to cache matches (more speed and memory on repeated prompts)
          use_cache = false,
        },

        -- Source definition. See `:h MiniPick-source`.
        source = {
          items = nil,
          name  = nil,
          cwd   = nil,

          match  = function(stritems, inds, query, do_sync)
            query = vim.tbl_map(function(c)
              -- if c == "/" then 
              --   return "\\"
              -- end
              if c == "/" then 
                return "Z"
              end
              
              if c == "\\" then 
                return "Z"
              end

              return c
            end, query)
            
            return MiniPick.default_match(stritems, inds, query, do_sync) 
          end,
          --
          -- show = function (buf_id, items, query, opts)
          --   -- query = vim.tbl_map(function(c)
          --   --   if c == "/" then 
          --   --     return "\\"
          --   --   end
          --   --
          --   --   return c
          --   -- end, query)
          --
          --   items = vim.tbl_map(replace_backslash_with_slash, items)
          --   return MiniPick.default_show(buf_id, items, query, opts)
          -- end,

          preview = nil,

          choose        = nil,
          choose_marked = nil,
        },

        -- Window related options
        window = {
          config = function ()
            height = math.floor(0.4 * vim.o.lines)
            width = math.floor(1. * vim.o.columns)
            return {
              anchor = 'NW', height = height, width = width,
              -- row = 0,
              -- col = math.floor(0.5 * (vim.o.columns - width)),
            } 
          end,

          -- String to use as cursor in prompt
          -- prompt_cursor = '',

          -- String to use as prefix in prompt
          -- prompt_prefix = '> ',
        },
      })

      MiniPick.registry.xbuffers = function(local_opts, opts)
        local_opts = vim.tbl_deep_extend('force', {
          include_current = true,
          include_unlisted = false,
        }, local_opts or {})

        local buffers_output = vim.api.nvim_exec(
          'buffers' .. (local_opts.include_unlisted and '!' or ''),
          true
        )

        local cur_buf_id, include_current = vim.api.nvim_get_current_buf(), local_opts.include_current
        local alt
        local items = {}
        for _, l in ipairs(vim.split(buffers_output, '\n')) do
          local buf_str, name = l:match('^%s*%d+'), l:match('"(.*)"')
          local is_alt = l:match('#')
          local buf_id = tonumber(buf_str)
          local item = { text = name, bufnr = buf_id }
          if is_alt then 
            alt = item
          elseif buf_id ~= cur_buf_id or include_current then 
            table.insert(items, item) 
          end
        end

        -- Prepend the alt buffer to the start of the items
        if alt then 
          table.insert(items, 1, alt)
        end

        local show = require'mini.pick'.show_with_icons
        local default_opts = { source = { name = 'Buffers', show = show } }
        opts = vim.tbl_deep_extend('force', default_opts, opts or {}, { source = { items = items } })
        return MiniPick.start(opts)
      end
  
      -- require"mini.pick".registry.xfiles = function(local_opts, opts)
      --   local is_executable = function(tool)
      --     if tool == 'fallback' then return true end
      --     return vim.fn.executable(tool) == 1
      --   end
      --   
      --   local files_get_tool = function()
      --     if is_executable('rg') then return 'rg' end
      --     if is_executable('fd') then return 'fd' end
      --     if is_executable('git') then return 'git' end
      --     return 'fallback'
      --   end
      --
      --   local files_get_command = function(tool)
      --     if tool == 'rg' then return { 'rg', '--files', '--no-follow', '--color=never' } end
      --     if tool == 'fd' then return { 'fd', '--type=f', '--no-follow', '--color=never' } end
      --     if tool == 'git' then return { 'git', 'ls-files', '--cached', '--others', '--exclude-standard' } end
      --   end
      --
      --   local cli_postprocess = function(items)
      --     while items[#items] == '' do
      --       items[#items] = nil
      --     end
      --     return items
      --   end
      --   
      --   -- local H = require"mini.pick"
      --
      --   local_opts = vim.tbl_deep_extend('force', { tool = nil }, local_opts or {})
      --   local tool = local_opts.tool or files_get_tool()
      --   local show = MiniPick.show_with_icons
      --   local default_opts = { source = { name = string.format('Files (%s)', tool), show = show } }
      --   opts = vim.tbl_deep_extend('force', default_opts, opts or {})
      --
      --   -- if tool == 'fallback' then
      --     -- opts.source.items = function() H.files_fallback_items(opts.source.cwd) end
      --     -- return MiniPick.start(opts)
      --   -- end
      --   local postprocess = function(lines)
      --     local res = cli_postprocess(lines)
      --     -- Repace all with backslash
      --     for i = 1, #res do
      --       res[i] = {
      --         path = res[i],
      --         text = replace_backslash_with_slash(res[i])  
      --       }
      --     end
      --    
      --     -- Correctly process files with `:` without sacrificing much performance
      --     -- for i = 1, #res do
      --     --   if res[i]:find(':') ~= nil then res[i] = {
      --     --     path = res[i],
      --     --     text = replace_backslash_with_slash(res[i]),
      --     --   } end
      --     -- end
      --     
      --     return res
      --   end
      --
      --   return MiniPick.builtin.cli({
      --     command = files_get_command(tool),
      --     postprocess = postprocess,
      --   }, opts)
      -- end

      -- require"mini.pick".registry.xexplorer = function(local_opts, opts)
      --   local H = require"mini.pick"
      --   
      --   local pick = H.validate_pick('explorer')
      --
      --   local_opts = vim.tbl_deep_extend('force', { cwd = nil, filter = nil, sort = nil }, local_opts or {})
      --   local cwd = local_opts.cwd or vim.fn.getcwd()
      --   if vim.fn.isdirectory(cwd) == 0 then H.error('`local_opts.cwd` should be valid directory path.') end
      --   -- - Call twice "full path" to make sure that possible '..' are collapsed
      --   cwd = H.full_path(vim.fn.fnamemodify(cwd, ':p'))
      --   local filter = local_opts.filter or function() return true end
      --   if not vim.is_callable(filter) then H.error('`local_opts.filter` should be callable.') end
      --   local sort = local_opts.sort or H.explorer_default_sort
      --   if not vim.is_callable(sort) then H.error('`local_opts.sort` should be callable.') end
      --
      --   -- Define source
      --   local choose = function(item)
      --     local path = item.path
      --     if vim.fn.filereadable(path) == 1 then return pick.default_choose(path) end
      --     if vim.fn.isdirectory(path) == 0 then return false end
      --
      --     pick.set_picker_items(H.explorer_make_items(path, filter, sort))
      --     pick.set_picker_opts({ source = { cwd = path } })
      --     pick.set_picker_query({})
      --     return true
      --   end
      --
      --   local show = H.pick_get_config().source.show or H.show_with_icons
      --
      --   local items = H.explorer_make_items(cwd, filter, sort)
      --   local source = { items = items, name = 'File explorer', cwd = cwd, show = show, choose = choose }
      --   opts = vim.tbl_deep_extend('force', { source = source }, opts or {})
      --   return pick.start(opts)
      -- end


    end



  },
}


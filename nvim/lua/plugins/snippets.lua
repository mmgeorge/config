return {
  "L3MON4D3/LuaSnip",
  -- follow latest release.
  version = "v2.*",
  dependencies = { "rafamadriz/friendly-snippets" },
  -- install jsregexp (optional!).
  -- build = "make install_jsregexp"
  config = function()
    local luasnip = require("luasnip");

    luasnip.setup({
      keep_roots = true,
      link_roots = true,
      link_children = true,

      -- Update more often, :h events for more info.
      update_events = "TextChanged,TextChangedI",
      -- Snippets aren't automatically removed if their text is deleted.
      -- `delete_check_events` determines on which events (:h events) a check for
      -- deleted snippets is performed.
      -- This can be especially useful when `history` is enabled.
      delete_check_events = "TextChanged",
      -- treesitter-hl has 100, use something higher (default is 200).
      ext_base_prio = 300,
      -- minimal increase in priority.
      ext_prio_increase = 1,
      enable_autosnippets = true,
      -- mapping for cutting selected text so it's usable as SELECT_DEDENT,
      -- SELECT_RAW or TM_SELECTED_TEXT (mapped via xmap).
      store_selection_keys = "<Tab>",
      -- luasnip uses this function to get the currently active filetype. This
      -- is the (rather uninteresting) default, but it's possible to use
      -- eg. treesitter for getting the current filetype by setting ft_func to
      -- require("luasnip.extras.filetype_functions").from_cursor (requires
      -- `nvim-treesitter/nvim-treesitter`). This allows correctly resolving
      -- the current filetype in eg. a markdown-code block or `vim.cmd()`.
      ft_func = function()
        return vim.split(vim.bo.filetype, ".", true)
      end,
    })
    -- See https://github.com/L3MON4D3/LuaSnip/blob/master/Examples/snippets.lua#L277

    -- require("luasnip.loaders.from_vscode").lazy_load()       

    local ls = require("luasnip")
    local s = ls.snippet
    local sn = ls.snippet_node
    local isn = ls.indent_snippet_node
    local t = ls.text_node
    local i = ls.insert_node
    local f = ls.function_node
    local c = ls.choice_node
    local d = ls.dynamic_node
    local r = ls.restore_node
    local events = require("luasnip.util.events")
    local ai = require("luasnip.nodes.absolute_indexer")
    local extras = require("luasnip.extras")
    local l = extras.lambda
    local rep = extras.rep
    local p = extras.partial
    local m = extras.match
    local n = extras.nonempty
    local dl = extras.dynamic_lambda
    local fmt = require("luasnip.extras.fmt").fmt
    local fmta = require("luasnip.extras.fmt").fmta
    local conds = require("luasnip.extras.expand_conditions")
    local postfix = require("luasnip.extras.postfix").postfix
    local treesitter_postfix = require("luasnip.extras.treesitter_postfix").treesitter_postfix
    local types = require("luasnip.util.types")
    local parse = require("luasnip.util.parser").parse_snippet
    local ms = ls.multi_snippet
    local k = require("luasnip.nodes.key_indexer").new_key
    -- See https://github.com/L3MON4D3/LuaSnip/blob/master/Examples/snippets.lua, 
    -- for example snippets
    local function get_position_before_postfix()
      local pos = vim.api.nvim_win_get_cursor(0) -- get current cursor position
      local row = pos[1] - 1 -- must convert to 0 indexed
      local line = vim.api.nvim_get_current_line()
      local dot_position = line:find("%.") 

      if dot_position then 
        return { row, dot_position - 2 } -- Before dot + 1 based index
      end
     
      return nil
    end
   
    local function type_node()
      local node = vim.treesitter.get_node({
        pos = get_position_before_postfix()
      })

      local ty = node:type()
      if 
        ty == "type_identifier" or 
        ty == "type_item" or 
        ty == "type_arguments" 
      then
        return node
      end

      return nil
    end 
    
    local function expr_node()
      local node = vim.treesitter.get_node({
        pos = get_position_before_postfix()
      })

      while node do 
        local ty = node:type()
        print(ty)
       
        if 
          ty == "call_expression" or 
          ty == "integer_literal" or 
          ty == "string_literal" or 
          ty == "float_literal" then 
          return node
        end 

        node = node:parent()
      end 

      return nil
    end

    ls.add_snippets('rust', {
      postfix(
        { 
          trig =".rccell",
          match_pattern = "[%w%.%_%-<>%(%)%:]+$", 
        }, 
        {
          f(function(_, parent)
            return "Rc<RefCell<" .. parent.snippet.env.POSTFIX_MATCH .. ">>"
          end, {}),
        }, 
        {
          show_condition = function(line_to_cursor)
            return type_node() ~= nil 
          end,
        }
      ),
      postfix(
        { 
          trig =".rccell",
          match_pattern = "[%w%.%_%-<>%(%)%:]+$", 
        }, 
        {
          f(function(_, parent)
            return "Rc::new(RefCell::new(" .. parent.snippet.env.POSTFIX_MATCH .. "))"
          end, {}),
        }, 
        {
          show_condition = function(line_to_cursor)
            return expr_node() ~= nil  
          end,
        }
      ),
    })

    luasnip.add_snippets(
      "all",
      {
        s("la ", t"<- "),
        s("ra ", t"-> "),
        s("udef ", t"self.useDefaults?.(arguments)"),
        s("sf ", {
          t({"<script>", 
            "  var esriConfig = {",
            "    has: {",
            "      \"esri-2d-update-debug\": 1,",
            "      \"esri-2d-debug\": 1,",
            "      \"esri-tiles-debug\": 1,",
            "      \"featurelayer-pbf\": 1,",
            "    }",
            "  }",
            "</script>"}), 
        }) ,
        s("dl ", {
          t({"//--------------------------------------------------------------------------", 
            "//",
            "//  Lifecycle",
            "//",
            "//--------------------------------------------------------------------------",
          }), 
        }),
        s("dpm ", {
          t({"//--------------------------------------------------------------------------", 
            "//",
            "//  Private Methods",
            "//",
            "//--------------------------------------------------------------------------",
          }), 
        }), 
        s("dm ", {
          t({"//--------------------------------------------------------------------------", 
            "//",
            "//  Public Methods",
            "//",
            "//--------------------------------------------------------------------------",
          }), 
        }), 
        s("dp ", {
          t({"//--------------------------------------------------------------------------", 
            "//",
            "//  Properties",
            "//",
            "//--------------------------------------------------------------------------",
          }), 
        }), 
      },
      {
        type = "autosnippets",
        key = "all_auto"
      }
    ) 
  end
}

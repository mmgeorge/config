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

    require("luasnip.loaders.from_vscode").lazy_load()       

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
    local types = require("luasnip.util.types")
    local parse = require("luasnip.util.parser").parse_snippet
    local ms = ls.multi_snippet
    local k = require("luasnip.nodes.key_indexer").new_key

    -- luasnip.add_snippets(
    --   "all",
    --     s("ra ", t"-> "),
    --     s("udef ", t"self.useDefaults?.(arguments)"),
    --   -- {
    --     -- s("fflags ", t"<script>var esriConfig = { has: {} }</script>") 
    --   -- },
    --   {
    --     type = "autosnippets",
    --     key = "all_auto"
    --   }
    -- )

   luasnip.add_snippets(
      "all",
      {
        s("ra ", t"-> "),
        s("udef ", t"self.useDefaults?.(arguments)"),
      },
      {
        type = "autosnippets",
        key = "all_auto"
      }
    ) 
  end
}

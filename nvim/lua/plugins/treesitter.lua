return {
  {
    "nvim-treesitter/nvim-treesitter",
    branch = "main",
    lazy = false,
    build = ":TSUpdate",
    config = function()
      vim.filetype.add({ extension = { frag = "frag" } })
      vim.filetype.add({ extension = { vert = "vert" } })
      vim.filetype.add({ extension = { wgslx = "wgslx" } })
      vim.treesitter.language.register("glsl", { "frag", "vert" })
      vim.treesitter.language.register("wgsl", { "wgslx" })
      vim.treesitter.language.register("markdown", "octo")

      local parsers = {
        "rust",
        "typescript",
        "tsx",
        "lua",
        "vim",
        "vimdoc",
        "json",
        "query",
        "javascript",
        "css",
        "html",
        "wgsl",
        "glsl",
        "c_sharp",
        "toml",
        "slang",
        "yaml",
        "nu",
        "markdown",
        "markdown_inline",
        "latex"
      }

      local installed = require("nvim-treesitter.config").get_installed()
      local parsers_to_install = vim.iter(parsers)
          :filter(function(parser)
            return not vim.tbl_contains(installed, parser)
          end)
          :totable()
      require("nvim-treesitter").install(parsers_to_install)

      vim.api.nvim_create_autocmd("FileType", {
        group = vim.api.nvim_create_augroup("UserTreesitter", { clear = true }),
        callback = function(args)
          pcall(vim.treesitter.start, args.buf)
          vim.bo[args.buf].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
        end,
      })
    end
  },
  {
    "windwp/nvim-ts-autotag",
    opts = {}
  },
}

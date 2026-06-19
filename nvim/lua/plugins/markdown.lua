return {
  {
    'MeanderingProgrammer/render-markdown.nvim',
    dependencies = {
      'nvim-treesitter/nvim-treesitter',
      'nvim-tree/nvim-web-devicons',
    },
    opts = {},
  },
  {
    'jmbuhr/otter.nvim',
    dependencies = {
      'nvim-treesitter/nvim-treesitter',
    },
    ft = { 'markdown', 'GithubIssue' },
    opts = {
      verbose = {
        no_code_found = false,
      },
    },
    config = function(_, opts)
      require('otter').setup(opts)
      local markdown_code = require('markdown_code')
      local group = vim.api.nvim_create_augroup('MarkdownCodeOtter', { clear = true })
      vim.api.nvim_create_autocmd('FileType', {
        group = group,
        pattern = { 'markdown', 'GithubIssue' },
        callback = function(args)
          markdown_code.activate(args.buf, {
            filetype = vim.bo[args.buf].filetype,
            register_as_markdown = vim.bo[args.buf].filetype ~= 'markdown',
          })
        end,
      })
      local buf = vim.api.nvim_get_current_buf()
      local filetype = vim.bo[buf].filetype
      if filetype == 'markdown' or filetype == 'GithubIssue' then
        markdown_code.activate(buf, {
          filetype = filetype,
          register_as_markdown = filetype ~= 'markdown',
        })
      end
    end,
  },
}

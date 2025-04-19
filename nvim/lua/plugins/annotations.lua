return { 
  "danymat/neogen", 
  version = "2.19.3",
  keys = {
    {
      "<leader>a",
      "<cmd>Neogen<CR>",
      mode = { "n", "x" },
      desc = "Document (Neogen)",
    }
  },
  config = {
    languages = {
      typescript = {
        templates = {
          annotation_convention = "jsdoc"
        }
      }
    }
  } 
 }

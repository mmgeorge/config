-- mcphub.nvim manages MCP server lifecycles (spawn, handshake, schemas,
-- tools/call) through the mcp-hub node service. The `ai` prompt library's
-- generate_with_tools({ mcps = { "server" } }) resolves tools through it via
-- nvim/lua/ai/mcp.lua.
--
-- Servers are defined in mcphub/servers.json at the root of this config repo
-- (mcphub's default path ignores XDG_CONFIG_HOME, so it is set explicitly).
return {
  {
    "ravitemer/mcphub.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
    },
    build = "npm install -g mcp-hub@latest",
    event = "VeryLazy",
    config = function()
      require("mcphub").setup({
        config = vim.fs.joinpath(vim.fs.dirname(vim.fn.stdpath("config")), "mcphub", "servers.json"),
      })
    end,
  },
}

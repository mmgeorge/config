return {
  {
    "forge-local",
    dir = vim.fs.dirname(vim.fs.dirname(vim.fs.dirname(debug.getinfo(1, "S").source:sub(2)))),
    dependencies = { "folke/snacks.nvim" },
    cmd = {
      "ForgeBranchDiff", "ForgeBranchDiffFile", "ForgeFileRevision", "ForgeDiffCompactPreview",
      "ForgeHarness", "ForgeHarnessNew", "ForgeHarnessLog", "ForgePermissions", "ForgeStartupLog",
      "ForgeGitConfigCacheReset",
    },
    init = function()
      vim.api.nvim_create_user_command("ForgeStatus", function()
        local started_at = vim.uv.hrtime()
        local log = require("forge.startup_log")
        log.write("status.command.invoked", nil, started_at)
        require("lazy").load({ plugins = { "forge-local" } })
        log.write("status.plugin.loaded", { elapsed_us = math.floor((vim.uv.hrtime() - started_at) / 1000) })
        require("forge").open(started_at)
      end, { desc = "Review git changes" })
    end,
    opts = {
      diff_logging = true,
      harness_logging = false,
      status_cursor_prewarm = true,
      status_cursor_prewarm_max_hunks = 2,
      walkthrough_inventory = "sem",
    },
    config = function(_, opts)
      local forge = require("forge")
      forge.setup(opts)
      vim.api.nvim_create_user_command("ForgeGitConfigCacheReset", function()
        require("forge.git_config_cache").reset()
      end, { desc = "Clear the cached Git configuration location" })
      vim.api.nvim_create_user_command("ForgeStartupLog", function()
        local log = require("forge.startup_log")
        log.write("log.opened")
        vim.cmd("botright split " .. vim.fn.fnameescape(log.path()))
        vim.cmd("normal! G")
      end, { desc = "Open Forge startup diagnostics" })
      vim.api.nvim_create_user_command("ForgeHarness", function()
        forge.open_harness()
      end, { desc = "Open the Forge AI Harness" })
      vim.api.nvim_create_user_command("ForgeHarnessNew", function()
        forge.new_harness_session()
      end, { desc = "Create a fresh Harness session" })
      vim.api.nvim_create_user_command("ForgeHarnessLog", function(command)
        local client = require("forge.client")
        local action = command.args
        local method = ({ on = "trace.configure", off = "trace.configure", toggle = "trace.toggle", clear = "trace.clear" })[action]
        if action ~= "" and not method then
          vim.notify("Usage: ForgeHarnessLog [on|off|toggle|clear]", vim.log.levels.WARN, { title = "ForgeHarness" })
          return
        end
        local function open_trace(status, request_error)
          if request_error then
            vim.notify(request_error, vim.log.levels.ERROR, { title = "ForgeHarness" })
            return
          end
          vim.cmd.edit(vim.fn.fnameescape(status.path))
          vim.bo.readonly = true
          vim.bo.modifiable = false
        end
        if action == "" then
          client.request("trace.status", {}, open_trace)
          return
        end
        local params = action == "on" and { enabled = true } or action == "off" and { enabled = false } or {}
        client.request(method, params, function(status, request_error)
          if request_error then
            vim.notify(request_error, vim.log.levels.ERROR, { title = "ForgeHarness" })
            return
          end
          vim.notify("Harness trace " .. (status.enabled and "enabled" or "disabled"), vim.log.levels.INFO, { title = "ForgeHarness" })
        end)
      end, {
        nargs = "?",
        complete = function() return { "on", "off", "toggle", "clear" } end,
        desc = "Open or control the Harness protocol trace",
      })
      vim.api.nvim_create_user_command("ForgePermissions", function()
        require("forge.views.permissions").open()
      end, { desc = "Edit Harness permissions" })
      local revisions = require("forge.revisions")
      revisions.setup()
      local complete_branches = revisions.values

      vim.api.nvim_create_user_command("ForgeBranchDiff", function(command)
        forge.open_branch_diff(command.args)
      end, {
        nargs = 1,
        complete = complete_branches,
        desc = "Diff the working tree against a branch or revision",
      })
      vim.api.nvim_create_user_command("ForgeBranchDiffFile", function(command)
        local file, branch = command.fargs[1], command.fargs[2]
        if not (file and branch) then
          vim.notify("Usage: ForgeBranchDiffFile <file> <branch>", vim.log.levels.WARN, { title = "ForgeBranchDiff" })
          return
        end
        forge.open_branch_diff(branch, { file = file })
      end, {
        nargs = "+",
        complete = function(arglead, cmdline)
          local args = vim.split(vim.trim((cmdline:gsub("^%S+%s*", ""))), "%s+", { trimempty = true })
          local completing_file = #args == 0 or (#args == 1 and arglead ~= "")
          if completing_file then
            return vim.fn.getcompletion(arglead, "file")
          end
          return complete_branches(arglead)
        end,
        desc = "Diff one file in the working tree against a branch or revision",
      })
      vim.api.nvim_create_user_command("ForgeFileRevision", function(command)
        local file, rev = command.fargs[1], command.fargs[2]
        if not (file and rev) then
          vim.notify("Usage: ForgeFileRevision <file> <commit>", vim.log.levels.WARN, { title = "ForgeFileRevision" })
          return
        end
        forge.open_file_revision(file, rev)
      end, {
        nargs = "+",
        complete = function(arglead, cmdline)
          local args = vim.split(vim.trim((cmdline:gsub("^%S+%s*", ""))), "%s+", { trimempty = true })
          local completing_file = #args == 0 or (#args == 1 and arglead ~= "")
          if completing_file then
            return vim.fn.getcompletion(arglead, "file")
          end
          return complete_branches(arglead)
        end,
        desc = "Open a file read-only as it exists at a git revision",
      })
      vim.api.nvim_create_user_command("ForgeDiffCompactPreview", function(command)
        forge.open_compact_preview({ staged = command.bang })
      end, { bang = true, desc = "Preview compacted git diff; use bang for staged diff" })
    end,
  },
}

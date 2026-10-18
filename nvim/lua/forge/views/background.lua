local M = { views = {} }
local client = require("forge.client")
local notifications = require("forge.infra.notifications")

--- Open a provider-free inspection view of a durable Background conversation.
---@param session_id? string
function M.open(session_id)
  local generation, failure = client.generation_config()
  if not generation then notifications.error(failure or "Background configuration unavailable", "Background timeline") return end
  local workspace = vim.fn.getcwd()
  local function request(extra, callback)
    client.request_host("background.timeline", vim.tbl_extend("force", {
      workspace = workspace, data_root = generation.data_root, session_id = session_id,
    }, extra), callback)
  end
  request({}, function(snapshot, request_failure)
    if request_failure then notifications.error(request_failure, "Background timeline") return end
    if not snapshot.session or snapshot.session == vim.NIL then
      vim.notify("No Background session for this branch yet", vim.log.levels.INFO)
      return
    end
    session_id = snapshot.session.id
    local existing = M.views[session_id]
    if existing and vim.api.nvim_win_is_valid(existing) then vim.api.nvim_set_current_win(existing) return end
    vim.cmd.tabnew()
    local window = vim.api.nvim_get_current_win()
    local buffer = vim.api.nvim_get_current_buf()
    M.views[session_id] = window
    local composer = vim.api.nvim_create_buf(false, true)
    vim.bo[buffer].filetype = "Harness"
    vim.bo[buffer].buftype = "nofile"
    vim.bo[buffer].bufhidden = "wipe"
    vim.bo[buffer].swapfile = false
    vim.api.nvim_buf_set_name(buffer, "ForgeBackground://" .. session_id)
    vim.wo[window].winbar = "Background · " .. snapshot.session.model .. " · c config · H history · q close"
    local owner
    local timer = vim.uv.new_timer()
    local function alive() return vim.api.nvim_buf_is_valid(buffer) and vim.api.nvim_win_is_valid(window) end
    local function notice(message) notifications.error(message, "Background timeline") end
    local function close()
      M.views[session_id] = nil
      if timer then timer:stop() timer:close() timer = nil end
      if owner then owner.close() owner = nil end
    end
    require("forge.views.harness.presentation").open({
      read_only = true, session_id = session_id, transcript_buffer = buffer, transcript_window = window, composer_buffer = composer,
      is_alive = alive, notice = notice,
      request = function(params, callback)
        request({ presentation = params }, function(result, failure)
          if result then snapshot = result end
          callback(result and result.result, failure)
        end)
      end,
    }, function(presentation, failure)
      if failure then close() notice(failure) return end
      owner = presentation
      timer:start(750, 750, vim.schedule_wrap(function()
        if alive() and owner and vim.api.nvim_get_current_tabpage() == vim.api.nvim_win_get_tabpage(window) then owner.sync() end
      end))
    end)
    vim.keymap.set("n", "q", function() close() if vim.api.nvim_win_is_valid(window) then vim.api.nvim_win_close(window, true) end end, { buffer = buffer })
    vim.keymap.set("n", "<Tab>", function() if owner then owner.toggle_heading(window) end end, { buffer = buffer })
    vim.keymap.set("n", "R", function() if owner then owner.sync() end end, { buffer = buffer })
    vim.keymap.set("n", "H", function()
      vim.ui.select(snapshot.sessions, { prompt = "Background history", format_item = function(item)
        return (item.background.branch or "detached") .. " · " .. item.model .. " · " .. item.id
      end }, function(selected)
        if selected then close() if vim.api.nvim_win_is_valid(window) then vim.api.nvim_win_close(window, true) end M.open(selected.id) end
      end)
    end, { buffer = buffer })
    vim.keymap.set("n", "c", function()
      local policy = vim.deepcopy(snapshot.settings.desired.auto_compaction ~= vim.NIL and snapshot.settings.desired.auto_compaction
        or snapshot.session.auto_compaction)
      vim.ui.select({ "Auto compact: " .. (policy.enabled and "On" or "Off"), "Compact at tokens: " .. policy.threshold_tokens }, { prompt = "Background /config" }, function(choice, selected_index)
        if not choice then return end
        local function save()
          request({ auto_compaction = policy, expected_revision = snapshot.settings.revision }, function(result, failure)
            if failure then notice(failure) elseif alive() then snapshot = result end
          end)
        end
        if selected_index == 1 then policy.enabled = not policy.enabled save()
        else vim.ui.input({ prompt = "Compact at tokens: ", default = tostring(policy.threshold_tokens) }, function(value)
          if not value then return end
          local amount = tonumber(value)
          if not amount or amount <= 0 or amount % 1 ~= 0 then notice("Enter a positive whole token count") return end
          policy.threshold_tokens = amount save()
        end) end
      end)
    end, { buffer = buffer })
    vim.api.nvim_create_autocmd("BufWipeout", { buffer = buffer, once = true, callback = close })
  end)
end

return M

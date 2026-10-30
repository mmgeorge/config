local M = {}
local directory = vim.fs.dirname(debug.getinfo(1, "S").source:sub(2))
---@class ForgeHarperRuleRequest
---@field rule string
---@field callback fun(failure?: string)
---@type table<string, ForgeHarperRuleRequest[]>
local pending = {}
local sequence = 0

--- Repository-owned rule overrides shared by every project.
M.default_path = vim.fs.joinpath(vim.fs.dirname(vim.fs.dirname(vim.fs.dirname(directory))), "spell", "harper-rules.json")

--- Identify a rule name supplied by Harper, never by diagnostic message text.
---@param rule any
---@return boolean
function M.valid_rule(rule)
  return type(rule) == "string" and rule:match("^[A-Za-z][A-Za-z0-9_]*$") ~= nil
end

--- Load disabled rules without replacing live settings on malformed or unreadable files.
---@param path string|false
---@param callback fun(rules?: table<string, false>, failure?: string)
function M.load(path, callback)
  if path == false then callback({}) return end
  vim.uv.fs_open(path, "r", 438, function(open_error, descriptor)
    if open_error then
      vim.schedule(function()
        if open_error:find("ENOENT", 1, true) then callback({})
        else callback(nil, "Cannot read Harper rules: " .. open_error) end
      end)
      return
    end
    local chunks, offset = {}, 0
    local function finish(failure)
      vim.uv.fs_close(descriptor, function(close_error)
        vim.schedule(function()
          if failure or close_error then callback(nil, failure or close_error) return end
          local success, rules = pcall(vim.json.decode, table.concat(chunks))
          if not success or type(rules) ~= "table" or vim.islist(rules) then
            callback(nil, "Harper rules must be a JSON object mapping rule names to false: " .. path)
            return
          end
          for rule, value in pairs(rules) do
            if not M.valid_rule(rule) or value ~= false then
              callback(nil, "Invalid disabled Harper rule in " .. path .. ": " .. tostring(rule))
              return
            end
          end
          callback(rules)
        end)
      end)
    end
    local function advance()
      vim.uv.fs_read(descriptor, 65536, offset, function(read_error, content)
        if read_error then finish("Cannot read Harper rules: " .. read_error) return end
        if content == "" then finish() return end
        chunks[#chunks + 1], offset = content, offset + #content
        advance()
      end)
    end
    advance()
  end)
end

--- Reject persistence over an editor buffer with unsaved rule changes.
---@param path string
---@return string?
local function unsaved(path)
  for _, buffer in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_loaded(buffer) and vim.bo[buffer].modified
      and vim.fs.normalize(vim.fn.resolve(vim.api.nvim_buf_get_name(buffer))) == path then
      return "Save the modified Harper rules file before disabling a rule: " .. path
    end
  end
end

--- Persist one rule under an exclusive writer lock and publish only a complete JSON document.
---@param path string
---@param rule string
---@param callback fun(failure?: string)
local function persist(path, rule, callback)
  local conflict = unsaved(path)
  if conflict then callback(conflict) return end
  local lock = path .. ".lock"
  vim.uv.fs_open(lock, "wx", 438, function(lock_error, descriptor)
    if lock_error then
      vim.schedule(function() callback("Cannot lock Harper rules " .. lock .. ": " .. lock_error) end)
      return
    end
    local function finish(failure)
      vim.uv.fs_close(descriptor, function(close_error)
        vim.uv.fs_unlink(lock, function(unlink_error)
          vim.schedule(function()
            callback(failure or close_error or unlink_error)
          end)
        end)
      end)
    end
    vim.schedule(function()
      M.load(path, function(rules, failure)
        if failure then finish(failure) return end
        if rules[rule] == false then finish() return end
        rules[rule] = false
        local names, lines = vim.tbl_keys(rules), { "{" }
        table.sort(names)
        for index, name in ipairs(names) do
          lines[#lines + 1] = '  "' .. name .. '": false' .. (index < #names and "," or "")
        end
        lines[#lines + 1] = "}"
        local content = table.concat(lines, "\n") .. "\n"
        sequence = sequence + 1
        local temporary = path .. ".tmp." .. vim.fn.getpid() .. "." .. sequence
        vim.uv.fs_open(temporary, "wx", 438, function(write_open_error, writer)
          if write_open_error then finish(write_open_error) return end
          local offset = 0
          local function publish(write_error)
            vim.uv.fs_close(writer, function(close_error)
              vim.schedule(function()
                local failure = write_error or close_error or unsaved(path)
                local function discard(reason)
                  vim.uv.fs_unlink(temporary, function(unlink_error)
                    finish(unlink_error and (reason .. "; cleanup: " .. unlink_error) or reason)
                  end)
                end
                if failure then discard(failure) return end
                vim.uv.fs_rename(temporary, path, function(rename_error)
                  if rename_error then discard(rename_error) else finish() end
                end)
              end)
            end)
          end
          local function advance()
            if offset == #content then publish() return end
            vim.uv.fs_write(writer, content:sub(offset + 1), offset, function(write_error, count)
              if write_error or not count or count == 0 then
                publish(write_error or "Harper rules write made no progress")
                return
              end
              offset = offset + count
              advance()
            end)
          end
          advance()
        end)
      end)
    end)
  end)
end

--- Serialize this editor's rule additions and report competing writers instead of losing updates.
---@param path string|false
---@param rule string
---@param callback fun(failure?: string)
function M.disable(path, rule, callback)
  if path == false then callback("Configure harper.disabled_rules to persist disabled rules") return end
  if not M.valid_rule(rule) then callback("Harper diagnostic has no valid rule identifier") return end
  path = vim.fs.normalize(vim.fn.resolve(vim.fn.fnamemodify(path, ":p")))
  if pending[path] then
    pending[path][#pending[path] + 1] = { rule = rule, callback = callback }
    return
  end
  local queue = { { rule = rule, callback = callback } }
  pending[path] = queue
  local function advance()
    local item = queue[1]
    if not item then pending[path] = nil return end
    persist(path, item.rule, function(failure)
      table.remove(queue, 1)
      if not failure then vim.cmd("checktime") end
      item.callback(failure)
      advance()
    end)
  end
  advance()
end

return M

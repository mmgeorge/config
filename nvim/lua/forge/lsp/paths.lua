local M = {}

local function key(node) return node.path .. ":" .. node.line .. ":" .. node.column end

---@class ForgeLspPathArguments
---@field from ForgeLspTarget
---@field to ForgeLspTarget
---@field max_depth? integer
---@field max_paths? integer
---@field scope? string

---@param arguments ForgeLspPathArguments
---@param query function
---@param receive fun(paths: table[][], partial: boolean, failure: string?, reasons: string[]?)
function M.search(arguments, query, receive)
  local maximum_depth, maximum_paths = arguments.max_depth or 5, arguments.max_paths or 3
  local queue, head, explored, paths, partial = {}, 1, 0, {}, false
  local reasons, seen = {}, {}
  local function incomplete(reason)
    partial = true
    if not seen[reason] then seen[reason] = true reasons[#reasons + 1] = reason end
  end
  local function finish() receive(paths, partial, nil, reasons) end
  local function prepare(target, callback)
    query({ operation = "call_root", path = target.path, line = target.line, column = target.column, limit = 50 }, function(reply, failure)
      if failure then receive({}, false, failure) return end
      if reply.next_offset or reply.warnings and #reply.warnings > 0 then
        receive({}, false, "incomplete_call_search: Callable endpoint lookup is incomplete.") return
      end
      if #reply.rows == 0 then
        receive({}, false, "not_callable: " .. (target.symbol or target.path .. ":" .. target.line .. ":" .. target.column)) return
      end
      if #reply.rows > 1 then
        local candidates = { "ambiguous_symbol: " .. #reply.rows .. " callable matches" }
        for _, row in ipairs(reply.rows) do candidates[#candidates + 1] = "  " .. row.text .. "  " .. row.path .. ":" .. row.line .. ":" .. row.column end
        receive({}, false, table.concat(candidates, "\n")) return
      end
      callback(reply.rows[1])
    end)
  end
  prepare(arguments.from, function(start)
    prepare(arguments.to, function(destination)
      local destination_key = key(destination)
      queue[1] = { start }
      local function next_route()
        local route = queue[head]
        if not route then finish() return end
        head = head + 1
        explored = explored + 1
        if explored > 512 then incomplete("Call-path route limit reached. Search incomplete.") finish() return end
        local current = route[#route]
        if key(current) == destination_key then
          paths[#paths + 1] = route
          if #paths >= maximum_paths then
            if head <= #queue then incomplete("Call-path count limit reached. Search incomplete.") end
            finish()
          else next_route() end
          return
        end
        if #route - 1 >= maximum_depth then next_route() return end
        local offset = 0
        local function next_page()
          query({ operation = "calls", path = current.path, line = current.line, column = current.column,
            direction = "outgoing", scope = arguments.scope or "workspace", limit = 50, offset = offset }, function(reply, failure)
            if failure then
              if failure:find("budget_exhausted:", 1, true) == 1 then
                incomplete("LSP request limit reached. Search incomplete.") finish()
              elseif #paths > 0 and (failure:match("^server_error:") or failure:match("^timeout:") or failure:match("^disconnected:")) then
                incomplete(failure) finish()
              else receive(paths, true, failure) end
              return
            end
            for _, warning in ipairs(reply.warnings or {}) do incomplete(warning) end
            for _, child in ipairs(reply.rows) do
              if not child.is_root then
                local cycle = false
                for _, ancestor in ipairs(route) do if key(ancestor) == key(child) then cycle = true break end end
                if not cycle then
                  if #queue >= 512 then incomplete("Call-path route limit reached. Search incomplete.")
                  else
                    local branch = vim.list_slice(route)
                    branch[#branch + 1] = child
                    queue[#queue + 1] = branch
                  end
                end
              end
            end
            if reply.next_offset then offset = reply.next_offset next_page() else next_route() end
          end)
        end
        next_page()
      end
      next_route()
    end)
  end)
end

return M

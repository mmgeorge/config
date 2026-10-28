local source = require("forge.lsp.source")
local snapshots = require("forge.lsp.snapshot")
local semantic = require("forge.lsp.semantic")
local target = require("forge.lsp.target")
local M = {}

---@class ForgeLspArguments
---@field operation string
---@field symbol? string
---@field from? ForgeLspTarget
---@field to? ForgeLspTarget
---@field max_depth? integer
---@field max_paths? integer
---@field max_requests? integer
---@field path? string
---@field line? integer
---@field column? integer
---@field query? string
---@field kind? string
---@field direction? string
---@field limit? integer
---@field offset? integer
---@field token_budget? integer
---@field depth? integer
---@field declaration_depth? integer
---@field visibility? string
---@field scope? string
---@field include? string[]
---@field start_line? integer
---@field include_declaration? boolean
---@field severity? string

local function declaration_row(snapshot, declaration)
  return { path = snapshot.path, line = declaration.line, column = declaration.column,
    text = vim.fn.strcharpart(declaration.signature, 0, 4096), modified = snapshot.modified }
end

local function excerpt(snapshot, line, marked)
  local lines = {}
  for number = math.max(1, line - 1), math.min(#snapshot.lines, line + 1) do
    lines[#lines + 1] = { line = number, text = snapshot.lines[number], hit = marked and number == line or false }
  end
  return lines
end

local function merge_lines(target, added)
  local by_line = {}
  for _, line in ipairs(target) do by_line[line.line] = line end
  for _, line in ipairs(added) do
    if by_line[line.line] then by_line[line.line].hit = by_line[line.line].hit or line.hit
    else target[#target + 1] = line by_line[line.line] = line end
  end
  table.sort(target, function(left, right) return left.line < right.line end)
end

---@param workspace string
---@param arguments ForgeLspArguments
---@param authorize function
---@param analyze function
---@param receive function
---@return fun()
function M.request(workspace, arguments, authorize, analyze, receive)
  local finished, active_cancel, active_expire = false, nil, nil
  local startup_issues, draining
  local result = { rows = {}, source = "live source", warnings = {}, omitted_visibility = {}, omitted_visibility_count = 0 }
  local captured = snapshots.new(authorize, analyze)
  local budget = arguments.operation == "paths" and { remaining = arguments.max_requests or 64 } or nil
  local timer = assert(vim.uv.new_timer())
  local function finish(failure)
    if finished then return end
    local current, stale = captured.current()
    finished = true
    timer:stop() timer:close()
    if active_cancel then active_cancel() end
    local warnings, seen = {}, {}
    for _, warning in ipairs(result.warnings or {}) do
      if not seen[warning] then warnings[#warnings + 1] = warning seen[warning] = true end
    end
    if #warnings > 16 then
      local count = #warnings - 15
      warnings = vim.list_slice(warnings, 1, 15)
      warnings[#warnings + 1] = count .. " additional omissions."
    end
    result.warnings = #warnings > 0 and warnings or nil
    if arguments.operation == "context" and result.rows[1] then
      local row = result.rows[1]
      while #row.excerpt > 0 and #vim.json.encode(result) > 60000 do result.next_line = table.remove(row.excerpt).line end
    end
    while #result.rows > 0 and #vim.json.encode(result) > 60000 do
      local row = table.remove(result.rows)
      if arguments.operation == "paths" then
        while row.indent and row.indent > 0 and #result.rows > 0 do row = table.remove(result.rows) end
        result.warnings = result.warnings or {}
        if result.warnings[#result.warnings] ~= "Call-path output budget reached." then result.warnings[#result.warnings + 1] = "Call-path output budget reached." end
      elseif arguments.operation ~= "context" then result.next_offset = row.offset or arguments.offset or 0 end
    end
    receive(not failure and current and result or nil, failure or stale)
  end
  local function active() return not finished end
  local function query(request, callback, optional, controlled)
    if finished then return end
    local workspace_query = not request.path and (request.operation == "workspace_symbols" or request.operation == "diagnostics")
    if workspace_query then request.startup_issues = startup_issues end
    local completed = false
    local cancel, expire = semantic.request(workspace, request, authorize, function(reply, failure)
      completed = true
      if finished then return end
      active_cancel, active_expire = nil, nil
      if failure then
        if controlled then callback({ rows = {} }, failure) return end
        if arguments.operation == "paths" and failure:match("^budget_exhausted:") then
          result.warnings[#result.warnings + 1] = "LSP request limit reached. Search incomplete."
          finish() return
        end
        if optional and not failure:match("^source_changed:") and not failure:match("^cancelled:") then
          result.warnings[#result.warnings + 1] = (request.relation or request.operation) .. " unavailable: " .. failure:gsub("^[%w_]+:%s*", "") callback({ rows = {} })
        else finish(failure) end
        return
      end
      for _, warning in ipairs(reply.warnings or {}) do result.warnings[#result.warnings + 1] = warning end
      callback(reply)
    end, budget)
    if not completed then active_cancel, active_expire = cancel, workspace_query and expire or nil end
  end
  local function get(path, callback, optional)
    if finished then return end
    captured.get(path, function(snapshot, failure)
      if finished then return end
      if failure then
        if optional and not failure:match("^source_changed:") and not failure:match("^access_denied:") and not failure:match("^cancelled:") then
          result.warnings[#result.warnings + 1] = failure callback(nil) else finish(failure) end
        return
      end
      if not snapshot.complete then result.warnings[#result.warnings + 1] = snapshot.warning or ("Structural analysis incomplete for " .. path .. ".") end
      callback(snapshot)
    end)
  end
  local omitted = {}
  local function omit_visibility(snapshot, declaration)
    local key = snapshot.path .. ":" .. declaration.line .. ":" .. declaration.column
    if omitted[key] then return end
    omitted[key] = true
    result.omitted_visibility_count = result.omitted_visibility_count + 1
    if #result.omitted_visibility < 16 then
      local row = declaration_row(snapshot, declaration)
      row.text = vim.fn.strcharpart(target.name(snapshot, declaration), 0, 256)
      result.omitted_visibility[#result.omitted_visibility + 1] = row
    end
  end
  local function materialize(reply, operation, callback, section)
    if not section then result.next_offset = reply.next_offset end
    result.freshness = reply.freshness or result.freshness
    result.coverage = reply.coverage or result.coverage
    local index = 1
    local groups = {}
    local function next_row()
      if finished then return end
      local row = reply.rows[index]
      if not row then callback() return end
      if operation == "summary" and row.is_root then index = index + 1 next_row() return end
      local offset = (arguments.offset or 0) + index - 1
      index = index + 1
      get(row.path, function(snapshot)
        if not snapshot then
          result.rows[#result.rows + 1] = { path = row.path, line = row.line, column = row.column,
            text = row.text, offset = row.offset or offset, section = section, indent = operation == "calls" and (row.is_root and 0 or 1) or 0 }
          next_row() return
        end
        local declaration = snapshots.enclosing(snapshot, row.line, row.column)
        local item = declaration and declaration_row(snapshot, declaration) or {
          path = snapshot.path, line = row.line, column = row.column, text = row.text, modified = snapshot.modified }
        item.offset, item.section = offset, section
        if operation == "definition" and (not declaration or row.line > declaration.line) then item.line, item.column = row.line, row.column end
        if operation == "symbols" and declaration then
          local visibility = arguments.visibility or "all"
          if visibility ~= "all" and declaration.visibility ~= visibility then
            if declaration.visibility == "unknown" then omit_visibility(snapshot, declaration) end
            next_row() return
          end
          if declaration.visibility == "unknown" then item.text = item.text .. " [visibility unknown]" end
        end
        if operation == "references" then
          if not arguments.include_declaration and declaration and row.line == declaration.line and row.column == declaration.column then next_row() return end
          local key = snapshot.path .. ":" .. item.line
          local group = groups[key]
          if not group then group = item group.excerpt = {} groups[key] = group result.rows[#result.rows + 1] = group end
          merge_lines(group.excerpt, excerpt(snapshot, row.line, true))
          next_row()
        elseif operation == "calls" then
          item.text = declaration and declaration.name == row.text and target.name(snapshot, declaration) or row.text
          item.line, item.column = row.line, row.column
          item.indent = row.is_root and 0 or 1
          item.offset = row.offset
          result.rows[#result.rows + 1] = item
          next_row()
        elseif operation == "diagnostics" then
          item = { path = snapshot.path, line = row.line, column = row.column, text = row.text:gsub("%s+", " "),
            modified = snapshot.modified, offset = offset,
            excerpt = { { line = row.line, text = snapshot.lines[row.line] or "" } } }
          result.rows[#result.rows + 1] = item next_row()
        else
          result.rows[#result.rows + 1] = item next_row()
        end
      end, true)
    end
    next_row()
  end
  local function list()
    require("forge.lsp.discovery").files(arguments.path or workspace, arguments.depth or 2, authorize, active, function(paths, warnings, failure)
      if finished then return end
      if failure then finish(failure) return end
      vim.list_extend(result.warnings, warnings)
      local index, all = 1, {}
      local visibility = arguments.visibility or (arguments.operation == "overview" and "public" or "all")
      local function next_file()
        local path = paths[index]
        index = index + 1
        if not path then
          local offset, limit = arguments.offset or 0, math.min(arguments.limit or 20, 64)
          for row_index = offset + 1, math.min(#all, offset + limit) do
            all[row_index].offset = row_index - 1 result.rows[#result.rows + 1] = all[row_index]
          end
          if offset + limit < #all then result.next_offset = offset + limit end
          finish() return
        end
        get(path, function(snapshot)
          if not snapshot then next_file() return end
          local selected = {}
          for declaration_index, declaration in ipairs(snapshot.declarations) do
            local depth = arguments.operation == "overview" and (arguments.declaration_depth or 2) or 8
            local matches = not arguments.query or target.name(snapshot, declaration):lower():gsub("::", "."):find(arguments.query:lower():gsub("::", "."), 1, true)
            if matches and declaration.depth <= depth and (visibility == "all" or declaration.visibility == visibility) then
              selected[declaration_index] = true
              if not arguments.query then
                local parent = declaration.parent
                while parent ~= nil and parent ~= vim.NIL do selected[parent + 1] = true parent = snapshot.declarations[parent + 1].parent end
              end
            elseif matches and declaration.depth <= depth and declaration.visibility == "unknown" and visibility ~= "all" then
              omit_visibility(snapshot, declaration)
            end
          end
          for declaration_index, declaration in ipairs(snapshot.declarations) do
            if selected[declaration_index] then
              local item = declaration_row(snapshot, declaration)
              item.indent = arguments.query and 0 or declaration.depth - 1
              if declaration.visibility == "unknown" then item.text = item.text .. " [visibility unknown]" end
              all[#all + 1] = item
            end
          end
          next_file()
        end, true)
      end
      next_file()
    end)
  end
  local function context()
    get(assert(arguments.path), function(snapshot)
      local target_line = snapshot.lines[arguments.line]
      if not target_line or arguments.column > vim.fn.strchars(target_line) + 1 then finish("invalid_position: " .. arguments.path .. ":" .. arguments.line .. ":" .. arguments.column .. " is outside the source.") return end
      local declaration = snapshots.enclosing(snapshot, arguments.line, arguments.column)
      if not declaration then finish((snapshot.complete and "no_declaration: " or "incomplete_symbol_search: Structural analysis unavailable at ") .. arguments.path .. ":" .. arguments.line .. ":" .. arguments.column) return end
      local first = arguments.start_line or declaration.start_line
      if first < declaration.start_line or first > declaration.end_line then finish("invalid_position: start_line is outside the declaration") return end
      local item = declaration_row(snapshot, declaration)
      item.text, item.excerpt = "", {}
      local available, used, wire_bytes = math.max(0, (arguments.token_budget or 2000) * 4 - 600), 0, 0
      for number = first, declaration.end_line do
        local text = snapshot.lines[number] or ""
        if used + #text + 18 > available or wire_bytes + #vim.json.encode({ line = number, text = text }) > 45000 then result.next_line = number break end
        item.excerpt[#item.excerpt + 1] = { line = number, text = text }
        used = used + #text + 18
        wire_bytes = wire_bytes + #vim.json.encode(item.excerpt[#item.excerpt])
      end
      result.rows[#result.rows + 1] = item
      local relationships, relation_index = arguments.include or {}, 1
      local function next_relation()
        local relation = relationships[relation_index]
        relation_index = relation_index + 1
        if not relation then finish() return end
        local operation = ({ references = "references", callers = "calls", callees = "calls", implementations = "implementation" })[relation]
        query({ operation = operation, path = arguments.path, line = declaration.line, column = declaration.column,
          direction = relation == "callers" and "incoming" or relation == "callees" and "outgoing" or nil,
          limit = 5, scope = "workspace", relation = relation }, function(reply)
          for _, row in ipairs(reply.rows) do row.sites = nil end
          materialize(reply, "summary", next_relation, relation)
        end, true)
      end
      -- Hover is supplementary: source remains useful when a server has no hover support.
      local client = source.select_client(assert(arguments.path), vim.filetype.match({ filename = arguments.path }) or "", source.loaded(arguments.path), "textDocument/hover", workspace)
      local callable = vim.tbl_contains({ "function_item", "function_signature_item", "function_declaration",
        "generator_function_declaration", "method_definition", "method_signature", "abstract_method_signature" }, declaration.kind)
      if client and not callable then
        local completed = false
        local cancel = semantic.request(workspace, { operation = "hover", path = arguments.path, line = arguments.line, column = arguments.column }, authorize, function(reply)
          completed = true
          if finished then return end
          active_cancel = nil
          if reply and reply.hover then
            local hover = reply.hover:gsub("```[^\n]*\n", ""):gsub("```", "")
            local compact = hover:gsub("%s+", " ")
            if not compact:find(declaration.signature, 1, true) then result.hover = hover end
          end
          next_relation()
        end)
        if not completed then active_cancel = cancel end
      else next_relation() end
    end)
  end
  local function resolve(selector, callback)
    target.resolve(workspace, selector, get, query, function(resolved, failure)
      if failure then
        if arguments.operation == "paths" and failure:match("^budget_exhausted:") then
          result.warnings[#result.warnings + 1] = "LSP request limit reached. Search incomplete."
          finish()
        else finish(failure) end
      else callback(resolved) end
    end)
  end
  local function paths()
    resolve(arguments.from, function(start)
      resolve(arguments.to, function(destination)
        local search = vim.tbl_extend("force", arguments, { from = start, to = destination })
        require("forge.lsp.paths").search(search, function(request, callback) get(request.path, function() query(request, callback, false, true) end) end,
          function(routes, partial, failure, reasons)
            if finished then return end
            if failure and not failure:find("budget_exhausted", 1, true) then finish(failure) return end
            if failure then partial = true reasons = { "LSP request limit reached. Search incomplete." } end
            result.message = #routes == 0 and not partial and ("No call path found within depth " .. (arguments.max_depth or 5) .. ".") or nil
            for _, reason in ipairs(reasons or {}) do result.warnings[#result.warnings + 1] = reason end
            local route_index, node_index = 1, 1
            local function next_node()
              local route = routes[route_index]
              if not route then finish() return end
              local node = route[node_index]
              if not node then route_index = route_index + 1 node_index = 1 next_node() return end
              get(node.path, function(snapshot)
                local declaration = snapshots.enclosing(snapshot, node.line, node.column)
                result.rows[#result.rows + 1] = { path = node.path, line = node.line, column = node.column,
                  text = declaration and declaration.name == node.text and target.name(snapshot, declaration) or node.text,
                  modified = snapshot.modified, indent = node_index - 1 }
                node_index = node_index + 1
                next_node()
              end)
            end
            next_node()
          end)
      end)
    end)
  end
  local function start()
    if finished then return end
    if arguments.operation == "paths" then paths()
    elseif arguments.operation == "overview" or arguments.operation == "symbols" and (arguments.path or not arguments.query) then list()
    elseif arguments.operation == "context" then context()
    else
      local request = vim.deepcopy(arguments) --[[@as ForgeLspSemanticArguments]]
      request.limit = arguments.limit or 20
      if arguments.operation == "symbols" or arguments.operation == "references" or arguments.operation == "calls" or arguments.operation == "diagnostics" then
        request.scope = arguments.scope or "workspace"
      end
      if arguments.operation == "definition" then request.operation = ({ type = "type_definition", implementation = "implementation" })[arguments.kind] or "definition"
      elseif arguments.operation == "symbols" then
        request.operation = "workspace_symbols"
        request.search_path, request.name_filter = arguments.path, arguments.query
        -- Directory paths scope workspace search without pretending to be source files.
        local stat = request.path and vim.uv.fs_stat(request.path)
        if stat and stat.type == "directory" then request.path = nil end
      elseif arguments.operation == "diagnostics" and request.path then
        local stat = vim.uv.fs_stat(request.path)
        if stat and stat.type == "directory" then request.search_path, request.path = request.path, nil end
      end
      query(request, function(reply)
        if arguments.operation == "calls" and not reply.next_offset and not (reply.warnings and #reply.warnings > 0) then
          local children = false
          for _, row in ipairs(reply.rows) do if not row.is_root then children = true break end end
          if not children then result.message = "No " .. arguments.direction .. " calls found" .. (request.scope == "workspace" and " in workspace." or ".") end
        end
        if arguments.operation == "status" then
          result = reply
          for _, row in ipairs(result.rows) do
            local capabilities = row.text:match("\n  (.*)") or ""
            local supported = {}
            for name in capabilities:gmatch("[^, ]+") do supported[name] = true end
            local mapped = { "overview", "symbols", "context", "diagnostics" }
            for _, pair in ipairs({ { "definition", "definition" }, { "type_definition", "type" }, { "implementation", "implementation" }, { "references", "references" }, { "calls", "calls" }, { "calls", "paths" } }) do
              if supported[pair[1]] then mapped[#mapped + 1] = pair[2] end
            end
            row.text = row.text:match("^[^\n]+") .. "\n  " .. table.concat(mapped, ", ")
          end
          finish()
        elseif draining then result = reply finish()
        else materialize(reply, arguments.operation, function() finish() end) end
      end)
    end
  end
  local function resolved_start()
    local duration = arguments.operation == "paths" and 30 or 10
    timer:start(duration * 1000, 0, vim.schedule_wrap(function()
      if active_expire then draining = true active_expire()
      else finish("timeout: " .. arguments.operation .. " lookup exceeded " .. duration .. " seconds.") end
    end))
    if vim.tbl_contains({ "definition", "context", "references", "calls" }, arguments.operation) then
      resolve(arguments, function(resolved)
        arguments.path, arguments.line, arguments.column = resolved.path, resolved.line, resolved.column
        start()
      end)
    else start() end
  end
  if arguments.operation == "status" then start()
  else
    local completed = false
    local cancel = require("forge.lsp.startup").wait(workspace, arguments.path, function(failure, issues)
      completed = true
      if finished then return end
      active_cancel = nil
      startup_issues = issues or {}
      if failure then finish(failure) else resolved_start() end
    end)
    if not completed then active_cancel = cancel end
  end
  return function() finish("cancelled: LSP request cancelled.") end
end

return M

local M = {}
local source = require("forge.lsp.source")
local captures = { [2] = "@module", [3] = "@module", [5] = "@type", [6] = "@function.method",
  [7] = "@variable.member", [8] = "@variable.member", [9] = "@constructor", [10] = "@type",
  [11] = "@type", [12] = "@function", [13] = "@variable", [14] = "@constant", [22] = "@constant",
  [23] = "@type", [26] = "@type" }

---@class ForgeNarrativeSymbolQuery
---@field name string
---@field workspace string
---@field excluded string[]

---@param query ForgeNarrativeSymbolQuery
---@param current fun(): boolean
---@param receive fun(target: ForgeDefinitionLocation?, failure: string?, message: string?)
---@return fun() cancel
function M.request(query, current, receive)
  local finished, pending = false, 0
  local requests, choices, seen, failures = {}, {}, {}, {}
  local deadline, guard
  local function finish(target, failure, message)
    if finished then return end
    finished = true
    if deadline then deadline:stop() deadline:close() end
    if guard then guard:stop() guard:close() end
    for _, request in ipairs(requests) do request.client:cancel_request(request.id) end
    receive(target, failure, message)
  end
  local function valid() return not finished and current() end
  local function selected(choice)
    if not valid() or not choice then finish(nil, nil, "cancelled") return end
    deadline:start(5000, 0, vim.schedule_wrap(function() finish(nil, "Symbol target loading timed out after five seconds") end))
    local function loaded(contents, failure)
      if not valid() then finish(nil, nil, "cancelled") return end
      if failure then finish(nil, failure) return end
      local line = source.rows(contents)[choice.position.line + 1]
      if not line then finish(nil, "Symbol position is outside the target file") return end
      local ok, column = pcall(vim.str_byteindex, line, choice.encoding, choice.position.character, false)
      if not ok then finish(nil, tostring(column)) return end
      finish({ path = choice.path, line = choice.position.line + 1, column = column, capture = choice.capture })
    end
    local buffer = source.loaded(choice.path)
    if buffer then loaded(table.concat(vim.api.nvim_buf_get_lines(buffer, 0, -1, false), "\n"))
    else source.read(choice.path, loaded) end
  end
  local function complete()
    if pending > 0 or finished then return end
    if #failures > 0 then finish(nil, table.concat(failures, "\n")) return end
    table.sort(choices, function(left, right)
      if left.path ~= right.path then return left.path < right.path end
      return left.position.line < right.position.line
    end)
    if #choices == 0 then finish(nil, nil, "No exact workspace symbol named " .. query.name)
    elseif #choices == 1 then selected(choices[1])
    else
      deadline:stop()
      vim.ui.select(choices, { prompt = "Definition of " .. query.name, format_item = function(choice)
        return choice.path .. ":" .. (choice.position.line + 1)
      end }, selected)
    end
  end
  local clients = {}
  for _, client in ipairs(vim.lsp.get_clients()) do
    if not client:is_stopped() and source.belongs(client, query.workspace) and client:supports_method("workspace/symbol") then
      clients[#clients + 1] = client
    end
  end
  if #clients == 0 then finish(nil, nil, "No project language server supports workspace symbols") return function() end end
  if #clients > 4 then finish(nil, "Workspace symbol lookup supports at most four project language servers") return function() end end
  local excluded = {}
  for _, path in ipairs(query.excluded) do excluded[source.canonical(path)] = true end
  local normalized = query.name:gsub("::", ".")
  local leaf = normalized:match("([^.]+)$")
  pending = #clients
  deadline = vim.uv.new_timer()
  deadline:start(5000, 0, vim.schedule_wrap(function() finish(nil, "Workspace symbol lookup timed out after five seconds") end))
  guard = vim.uv.new_timer()
  guard:start(50, 50, vim.schedule_wrap(function() if not valid() then finish(nil, nil, "cancelled") end end))
  for _, client in ipairs(clients) do
    local sent, id = client:request("workspace/symbol", { query = leaf }, function(failure, result)
      if not valid() then finish(nil, nil, "cancelled") return end
      pending = pending - 1
      if failure then failures[#failures + 1] = client.name .. ": " .. (failure.message or vim.inspect(failure))
      elseif result and result ~= vim.NIL then
        if type(result) ~= "table" or not vim.islist(result) then failures[#failures + 1] = client.name .. ": invalid workspace symbol response"
        elseif #result > 256 then failures[#failures + 1] = client.name .. ": symbol result exceeds 256 entries"
        else
          for _, symbol in ipairs(result) do
            if type(symbol) ~= "table" or type(symbol.name) ~= "string" then
              failures[#failures + 1] = client.name .. ": invalid workspace symbol"
              break
            end
            local name = symbol.name:gsub("::", ".")
            local qualified = type(symbol.containerName) == "string" and (symbol.containerName:gsub("::", ".") .. "." .. name) or name
            if name == normalized or (normalized:find(".", 1, true) and qualified == normalized) then
              local location = symbol.location
              if type(location) ~= "table" or type(location.uri) ~= "string" or location.uri:sub(1, 7) ~= "file://"
                or type(location.range) ~= "table" or type(location.range.start) ~= "table"
                or type(location.range.start.line) ~= "number" or type(location.range.start.character) ~= "number"
                or location.range.start.line < 0 or location.range.start.character < 0 then
                failures[#failures + 1] = client.name .. ": symbol has no resolved file location"
              else
                local path = source.canonical(vim.uri_to_fname(location.uri))
                local position = location.range.start
                local key = path .. ":" .. position.line .. ":" .. position.character
                if source.inside(path, query.workspace) and not excluded[path] and not seen[key] then
                  seen[key] = true
                  choices[#choices + 1] = { path = path, position = position, encoding = client.offset_encoding or "utf-16", capture = captures[symbol.kind] }
                end
              end
            end
          end
        end
      end
      complete()
    end)
    if sent then requests[#requests + 1] = { client = client, id = id }
    else pending = pending - 1 failures[#failures + 1] = client.name .. ": rejected workspace symbol lookup" complete() end
  end
  return function() finish(nil, nil, "cancelled") end
end

return M

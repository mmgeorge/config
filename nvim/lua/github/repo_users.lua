---@class GithubRepoUsersFetchOptions
---@field cwd string?
---@field repo string
---@field system_async fun(command: string[], input: string?, cwd: string?, callback: fun(result: table))
---@field decode_json fun(stdout: string): table?, string?
---@field result_error fun(result: table): string
---@field callback fun(result: { ok: boolean, contributors?: table[], message?: string, code?: integer })

local M = {}

---@param repo string
---@return { name: string, command: string[] }[]
local function user_endpoints(repo)
  return {
    {
      name = "contributors",
      command = { "gh", "api", "/repos/" .. repo .. "/contributors", "--paginate", "--slurp" },
    },
    {
      name = "collaborators",
      command = { "gh", "api", "/repos/" .. repo .. "/collaborators", "--paginate", "--slurp" },
    },
  }
end

---@param users table<string, table>
---@param raw any
local function add_user(users, raw)
  if type(raw) ~= "table" then return end
  if type(raw.login) == "string" and raw.login ~= "" then
    local key = raw.login:lower()
    if not users[key] then
      users[key] = {
        login = raw.login,
        name = type(raw.name) == "string" and raw.name or nil,
      }
    elseif not users[key].name and type(raw.name) == "string" then
      users[key].name = raw.name
    end
    return
  end
  for _, item in ipairs(raw) do
    add_user(users, item)
  end
end

---@param users table<string, table>
---@return table[]
local function user_list(users)
  local items = {}
  for _, user in pairs(users) do
    items[#items + 1] = user
  end
  table.sort(items, function(left, right)
    return tostring(left.login or ""):lower() < tostring(right.login or ""):lower()
  end)
  return items
end

---@param opts GithubRepoUsersFetchOptions
function M.fetch_async(opts)
  local endpoints = user_endpoints(opts.repo)
  local users = {}
  local errors = {}
  local successful_requests = 0
  local failure_code = 1

  local function fetch_next(index)
    local endpoint = endpoints[index]
    if not endpoint then
      if successful_requests == 0 then
        opts.callback({
          ok = false,
          message = table.concat(errors, "; "),
          code = failure_code,
        })
        return
      end
      opts.callback({ ok = true, contributors = user_list(users) })
      return
    end

    opts.system_async(endpoint.command, nil, opts.cwd, function(result)
      if result.code ~= 0 then
        failure_code = tonumber(result.code) or failure_code
        errors[#errors + 1] = endpoint.name .. ": " .. opts.result_error(result)
        fetch_next(index + 1)
        return
      end
      local decoded, decode_error = opts.decode_json(result.stdout)
      if not decoded then
        failure_code = tonumber(result.code) or failure_code
        errors[#errors + 1] = endpoint.name .. ": " .. (decode_error or "gh api returned invalid JSON")
        fetch_next(index + 1)
        return
      end
      successful_requests = successful_requests + 1
      for _, raw in ipairs(decoded) do
        add_user(users, raw)
      end
      fetch_next(index + 1)
    end)
  end

  fetch_next(1)
end

return M

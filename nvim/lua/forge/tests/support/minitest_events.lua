local M = {}

---@class ForgeMiniCase
---@field desc string[]
---@field exec? {state?:string,fails?:string[]}
---@class ForgeMiniEventReporter
---@field update fun(self:ForgeMiniEventReporter,index:integer)
---@field timeout fun(self:ForgeMiniEventReporter)

---Create one top-level-entry reporter from mini.test's collected case order.
---@param cases ForgeMiniCase[]
---@param path string Repository-relative test file.
---@param emit fun(event:table)
---@param clock fun():number Monotonic nanoseconds.
---@return ForgeMiniEventReporter
function M.new(cases, path, emit, clock)
  local groups, by_index, current = {}, {}, nil
  for index, case in ipairs(cases) do
    local name = assert(case.desc[2], "mini.test case has no top-level entry")
    local group = groups[name]
    if not group then
      group = { id = path .. "::" .. name, last = index, failures = {}, finished = false }
      groups[name] = group
    end
    group.last = index
    by_index[index] = group
  end
  local reporter = {}
  local terminal_cases = {}

  local function finish(group, status)
    if group.finished then return end
    group.finished = true
    emit({ event = "finished", id = group.id, status = status,
      duration_ms = math.floor((clock() - group.started) / 1000000), failures = group.failures })
  end

  function reporter:update(index)
    local group, case = assert(by_index[index]), cases[index]
    current = group
    if not group.started then
      group.started = clock()
      emit({ event = "started", id = group.id })
    end
    local state = case.exec and case.exec.state or ""
    if terminal_cases[index] or not (state:match("^Pass") or state:match("^Fail")) then return end
    terminal_cases[index] = true
    for _, raw in ipairs(case.exec.fails or {}) do
      local normalized = tostring(raw):gsub("\\", "/")
      local source, line
      for trace in normalized:gmatch("[^\n]+") do
        local candidate, position = trace:match("(nvim/lua/[^\n:]+%.lua):(%d+)")
        if candidate and not candidate:match("/support/minitest_shard%.lua$") then
          source, line = candidate, position
          break
        end
        local suffix, suffix_line = trace:match("%.%.%./([^:]+%.lua):(%d+)")
        if suffix and path:sub(-#suffix) == suffix then
          source, line = path, suffix_line
          break
        end
      end
      local failure = { message = table.concat(case.desc, " | ", 2) .. ": " .. (normalized:match("([^\n]+)") or normalized) }
      if source then failure.path, failure.line = source, tonumber(line) end
      group.failures[#group.failures + 1] = failure
    end
    if index == group.last then finish(group, #group.failures == 0 and "passed" or "failed") end
  end

  function reporter:timeout()
    if current and current.started and not current.finished then finish(current, "timed_out") end
  end

  return reporter
end

return M

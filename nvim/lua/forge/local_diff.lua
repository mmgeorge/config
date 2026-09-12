local status = require("forge.status")
local M = {}
---@class ForgeLocalPreview
---@field filename string
---@field used integer
---@field state ForgeNativeStatus

---@type table<string, ForgeLocalPreview>
local by_file = {}
---@type table<integer, ForgeLocalPreview>
local by_buffer = {}
local sequence = 0

local function key(filename)
  return vim.fs.normalize(vim.fn.fnamemodify(filename, ":p"))
end

local function remove(owner)
  if by_file[owner.filename] == owner then by_file[owner.filename] = nil end
  by_buffer[owner.state.replica.buffer] = nil
end

function M.open(filename)
  filename = key(filename)
  sequence = sequence + 1
  local existing = by_file[filename]
  if existing and existing.state.active and vim.api.nvim_buf_is_valid(existing.state.replica.buffer) then
    existing.used = sequence
    return existing.state.replica.buffer
  end
  local count, oldest = 0, nil
  for _, owner in pairs(by_file) do
    if owner.state.active and vim.api.nvim_buf_is_valid(owner.state.replica.buffer) then
      count = count + 1
      if #vim.fn.win_findbuf(owner.state.replica.buffer) == 0 and (not oldest or owner.used < oldest.used) then oldest = owner end
    else remove(owner) end
  end
  if count >= 6 and not oldest then error("All six local diff previews are visible") end
  local owner = { filename = filename, used = sequence }
  owner.state = status.open_local({ filename = filename, bind = false, filetype = "ForgeStatus",
    before_open = count >= 6 and function(start)
      local retiring = assert(oldest, "local preview eviction requires a hidden owner")
      remove(retiring)
      status.close(retiring.state, function(failure) if not failure and owner.state.active then start() end end)
    end or nil,
    handler = { close = function() remove(owner) end },
  })
  by_file[filename], by_buffer[owner.state.replica.buffer] = owner, owner
  vim.api.nvim_create_autocmd("BufWipeout", { buffer = owner.state.replica.buffer, once = true,
    callback = function() remove(owner) end })
  return owner.state.replica.buffer
end

function M.owner(buffer)
  return by_buffer[buffer]
end

function M.refresh(filename, callback)
  local owner = by_file[key(filename)]
  if not owner or not owner.state.active then return false end
  if owner.state.replica.status == "Applied" then status.refresh(owner.state, callback)
  elseif callback then callback(false, "Local diff is still opening") end
  return true
end

function M.close(filename)
  local owner = by_file[key(filename)]
  if not owner then return end
  remove(owner)
  status.close(owner.state)
end

function M.close_all()
  local retained = {}
  for _, owner in pairs(by_file) do retained[#retained + 1] = owner end
  by_file, by_buffer = {}, {}
  for _, owner in ipairs(retained) do status.close(owner.state) end
end

return M

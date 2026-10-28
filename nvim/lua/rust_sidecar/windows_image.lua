local M = {}
local library

---@param path string
---@return fun()?, string?, integer?
function M.pin(path)
  local ffi = require("ffi")
  if not library then
    ffi.cdef([[
      void* __stdcall CreateFileW(const uint16_t*, uint32_t, uint32_t, void*, uint32_t, uint32_t, void*);
      int __stdcall CloseHandle(void*);
      uint32_t __stdcall GetLastError(void);
    ]])
    library = ffi.load("kernel32")
  end
  local encoded = vim.iconv(path, "utf-8", "utf-16le")
  if not encoded then return nil, "Cannot encode executable path as UTF-16" end
  local name = ffi.new("uint16_t[?]", #encoded / 2 + 1)
  ffi.copy(name, encoded, #encoded)
  -- A shared read handle pins the cached image against writes and deletion until host exit.
  local handle = library.CreateFileW(name, 0x80000000, 1, nil, 3, 0x80, nil)
  if handle == ffi.cast("void*", -1) then
    local code = tonumber(library.GetLastError())
    return nil, ("Cannot pin executable %s (Windows error %d)"):format(path, code), code
  end
  local released = false
  return function()
    if released then return end
    if library.CloseHandle(handle) == 0 then
      error(("Cannot release executable %s (Windows error %d)"):format(path, tonumber(library.GetLastError())))
    end
    released = true
  end
end

return M

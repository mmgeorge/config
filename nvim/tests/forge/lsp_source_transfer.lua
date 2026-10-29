local source = string.rep("α\n", 200000)
local parts, offset, replied = {}, 0, false
package.loaded["forge.client"] = {
  host_get_generation = function() return 1 end,
  host_is_accepting = function() return true end,
  request = function(request_specification, receive)
  local method = request_specification.method
  local arguments = request_specification.params or {}

    if method == "session.lsp.analyze" then
      assert(arguments.source_offset == offset and arguments.source_total == #source)
      assert(#vim.json.encode(arguments) < 512 * 1024)
      assert(vim.str_utfindex(arguments.source, "utf-32"))
      parts[#parts + 1] = arguments.source
      offset = offset + #arguments.source
      vim.schedule(function() receive(offset < #source and { pending = true } or { declarations = {}, complete = true }) end)
    elseif method == "session.lsp.respond" then replied = true receive({ accepted = true })
    else error(method) end
  end,
}
package.loaded["forge.lsp.query"] = { request = function(_, _, _, analyze, receive)
  analyze("large.rs", source, function(result, failure)
    assert(not failure and result.complete)
    receive({ rows = {}, source = "test" })
  end)
  return function() end
end }
require("forge.lsp.bridge").handle("lsp.request", { id = "large-source", workspace = "/repo", arguments = {} }, "session", 1)
assert(vim.wait(2000, function() return replied end, 5))
assert(#parts > 1 and table.concat(parts) == source)
print("lsp_source_transfer: passed")

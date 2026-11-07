-- Lease the built Forge executable for a host fixture through the production executable cache.
--
-- Copying the 182 MB executable into each fixture directory creates a new image on every run,
-- and Windows scans that new image before its first launch, which routinely exceeds a host
-- fixture's startup deadline. The builder's content-addressed cache creates one copy per build
-- and pins it, so every fixture run of the same build launches an already-scanned image.

---@return string path Pinned executable path that the fixture's host launcher must use.
return function()
  local completion = {}
  require("forge.builder").acquire(require("forge.builder").binary_path(), function(lease, failure)
    completion.lease, completion.failure, completion.done = lease, failure, true
  end)
  assert(vim.wait(120000, function() return completion.done end, 10), "Forge executable lease did not finish")
  assert(completion.lease, "Forge executable lease failed: " .. tostring(completion.failure))
  return completion.lease.path
end

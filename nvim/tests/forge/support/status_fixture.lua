local M = {}

function M.file(path, id)
  return { id = id or 1, generation = 1, section = "unstaged", change = "modified", path = path or "source.txt",
    untracked = false, stats = { state = "unknown" } }
end

function M.snapshot(document, options)
  options = options or {}
  return { document = document, revision = options.revision or 0, view = options.view or { kind = "status" },
    head = { state = "attached", reference = "main", object = "abc" }, context = options.context or vim.NIL,
    section = { { kind = "unstaged", file = { 1 } } }, file = { M.file(options.path) } }
end

function M.metadata(target, rows)
  return { target = target and { { id = target, range = { start = { row = 0, column = 0 },
    ["end"] = { row = rows, column = 0 } } } } or {}, decoration = {}, editable_region = {} }
end

function M.body(document, text, options)
  options = options or {}
  return { document = document, file = 1, generation = 1, more = options.more or false,
    state = { state = options.more and "partial" or "ready" }, snapshot = { document = "body:1:1", revision = 1,
      block = { { id = options.block or "body:1", text = text, metadata = M.metadata(options.target or "hunk:1", #text) } } } }
end

return M

local M = {}

---@class ForgeNativeEffect
---@field id string
---@field document string
---@field revision integer
---@field view string
---@field sequence integer
---@field kind "cursor"|"notify"|"open_file"|"open_source"|"open_commit"|"browser"
---@field url? string
---@field block? string
---@field position? {row: integer, column: integer}
---@field message? string
---@field level? integer
---@field path? string
---@field row? integer
---@field column? integer
---@field layout? "current"|"split"|"vsplit"|"tab"
---@field workspace? string
---@field oid? string
---@field source_revision? string

---@param session table
---@param view ForgeInputView
---@param effect ForgeNativeEffect
---@return "Applied"|"Discarded"|"Duplicate"|"Failed"
---@return string?
function M.apply(session, view, effect)
  if not view.active or view.document ~= session.document or session.status ~= "Applied" or session.applying
    or effect.document ~= session.document or effect.revision ~= session.revision
    or effect.view ~= view.id or effect.sequence ~= view.sequence
    or not vim.api.nvim_win_is_valid(view.window) or vim.api.nvim_win_get_buf(view.window) ~= session.buffer
  then
    return "Discarded"
  end
  if view.effect[effect.id] then return "Duplicate" end
  if effect.kind ~= "notify" and session.editable
    and require("forge.editable").suspend_generated_text(session.editable) then return "Discarded" end
  if effect.kind ~= "notify" and view.cursor
    and not vim.deep_equal(vim.api.nvim_win_get_cursor(view.window), view.cursor)
  then return "Discarded" end
  local ok, failure = pcall(function()
    assert(type(effect.id) == "string" and #effect.id > 0 and #effect.id <= 256 and not effect.id:find("%c"), "invalid effect identity")
    assert(vim.tbl_count(view.effect) < 64, "input effect limit exceeded")
    assert(not vim.in_fast_event(), "effects require the main loop")
    if effect.kind == "cursor" then
      local node = assert(session.sequence.node[effect.block], "cursor block was removed")
      local position = assert(effect.position, "cursor position is missing")
      assert(position.row >= 0 and position.row < node.entry.row_count and position.row == math.floor(position.row), "cursor row is outside block")
      local _, start_row = session.sequence:position(effect.block)
      local row = vim.api.nvim_buf_get_lines(session.buffer, start_row + position.row, start_row + position.row + 1, true)[1]
      local byte = row:byte(position.column + 1)
      assert(position.column >= 0 and position.column <= #row and position.column == math.floor(position.column)
        and (not byte or byte < 128 or byte >= 192), "cursor column splits UTF-8")
      view.effect[effect.id] = true
      vim.api.nvim_win_set_cursor(view.window, { start_row + position.row + 1, position.column })
      vim.api.nvim_win_call(view.window, function() vim.cmd("normal! zv") end)
      view.cursor = vim.api.nvim_win_get_cursor(view.window)
    elseif effect.kind == "open_source" then
      assert(type(effect.workspace) == "string" and #effect.workspace <= 32768
        and vim.fs.abspath(effect.workspace) == vim.fs.normalize(effect.workspace), "invalid source workspace")
      assert(type(effect.path) == "string" and #effect.path <= 65536, "invalid encoded source path")
      assert(type(effect.source_revision) == "string" and #effect.source_revision <= 256, "invalid source revision")
      local row = effect.row or 0
      assert(type(row) == "number" and row >= 0 and row == math.floor(row), "invalid source row")
      local path = vim.base64.decode(effect.path)
      assert(#path > 0 and not path:find("%z"), "invalid source path")
      view.effect[effect.id] = true
      require("forge.source_document").open({ workspace = effect.workspace, path = path,
        revision = effect.source_revision, line = row + 1, window = view.window,
        is_current = function()
          return view.active and view.sequence == effect.sequence and session.revision == effect.revision
            and session.status == "Applied" and vim.api.nvim_win_is_valid(view.window)
            and vim.api.nvim_win_get_buf(view.window) == session.buffer
            and (not view.cursor or vim.deep_equal(vim.api.nvim_win_get_cursor(view.window), view.cursor))
        end,
        on_error = session.notice or function(message) vim.notify(message, vim.log.levels.ERROR) end })
    elseif effect.kind == "open_commit" then
      assert(type(effect.workspace) == "string" and #effect.workspace <= 32768
        and vim.fs.abspath(effect.workspace) == vim.fs.normalize(effect.workspace), "invalid commit workspace")
      assert(type(effect.oid) == "string" and #effect.oid == 40 and effect.oid:match("^%x+$"), "invalid commit object ID")
      view.effect[effect.id] = true
      require("forge.source_document").open_commit({ workspace = effect.workspace, oid = effect.oid, window = view.window,
        is_current = function()
          return view.active and view.sequence == effect.sequence and session.revision == effect.revision
            and session.status == "Applied" and vim.api.nvim_win_is_valid(view.window)
            and vim.api.nvim_win_get_buf(view.window) == session.buffer
            and (not view.cursor or vim.deep_equal(vim.api.nvim_win_get_cursor(view.window), view.cursor))
        end,
        on_error = session.notice or function(message) vim.notify(message, vim.log.levels.ERROR) end })
    elseif effect.kind == "open_file" then
      assert(type(effect.path) == "string" and #effect.path > 0 and #effect.path <= 32768
        and not effect.path:find("%z") and vim.fs.abspath(effect.path) == vim.fs.normalize(effect.path), "invalid absolute file path")
      local row, column = effect.row or 0, effect.column or 0
      assert(type(row) == "number" and row >= 0 and row == math.floor(row)
        and type(column) == "number" and column >= 0 and column == math.floor(column), "invalid source position")
      local command = ({ current = "edit", split = "split", vsplit = "vsplit", tab = "tabedit" })[effect.layout or "current"]
      assert(command, "invalid file opening layout")
      view.effect[effect.id] = true
      vim.api.nvim_win_call(view.window, function()
        vim.api.nvim_cmd({ cmd = command, args = { effect.path } }, {})
        require("forge.folds").restore_inherited(view.window, vim.api.nvim_get_current_win())
        local target = vim.api.nvim_get_current_buf()
        row = math.min(row, vim.api.nvim_buf_line_count(target) - 1)
        local text = vim.api.nvim_buf_get_lines(target, row, row + 1, true)[1]
        column = math.min(column, #text)
        while column > 0 and text:byte(column + 1) and text:byte(column + 1) >= 128 and text:byte(column + 1) < 192 do
          column = column - 1
        end
        vim.api.nvim_win_set_cursor(0, { row + 1, column })
      end)
    elseif effect.kind == "browser" then
      assert(type(effect.url) == "string" and #effect.url <= 65536 and not effect.url:find("%c")
        and (effect.url:match("^https://[^/]+") or effect.url:match("^http://[^/]+")), "invalid browser URL")
      view.effect[effect.id] = true
      local _, failure = vim.ui.open(effect.url)
      if failure then error(failure) end
    elseif effect.kind == "notify" then
      assert(type(effect.message) == "string" and #effect.message <= 65536, "invalid notification text")
      view.effect[effect.id] = true
      vim.notify(effect.message, effect.level or vim.log.levels.INFO)
    else
      error("unsupported native effect")
    end
  end)
  if not ok then
    if session.notice then session.notice(tostring(failure)) else vim.notify(tostring(failure), vim.log.levels.ERROR) end
    return "Failed", tostring(failure)
  end
  return "Applied"
end

return M

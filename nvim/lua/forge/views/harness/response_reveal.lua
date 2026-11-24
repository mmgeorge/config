local layout = require("forge.views.harness.layout")

---@class HarnessResponseReveal
---@field exchange? string Exchange whose progress or completion is being followed.
---@field newest? string Newest exchange seen in transcript order.
---@field settled boolean Whether that exchange had completed.
---@field revision? integer Last observed transcript revision.
---@field composer_window? integer Window whose focus enables live progress following.
---@field deferred? string Completed exchange whose reveal waits for a window of the current tab
---  page, because it finished while every transcript window was in a hidden tab page.
local Reveal = {}
Reveal.__index = Reveal

---Create a progress and completion observer without retaining viewport snapshots.
---@param composer_window? integer
---@return HarnessResponseReveal
function Reveal.new(composer_window)
  return setmetatable({ settled = false, composer_window = composer_window }, Reveal)
end

---Report whether focus is in the composer or in a popup entered from it, such as a permission
---review, so a popup that takes focus mid-stream does not stop the transcript from following.
---@return boolean
function Reveal:composer_focused()
  local current = vim.api.nvim_get_current_win()
  if current == self.composer_window then return true end
  if vim.api.nvim_win_get_config(current).relative == "" then return false end
  return self.composer_window ~= nil and vim.fn.win_getid(vim.fn.winnr("#")) == self.composer_window
end

---Select the live exchange, retaining its bounds through settlement until a newer exchange opens.
local function exchange_range(sequence, reveal)
  local finish = sequence:count() - 1
  local newest, previous
  for index = finish, 0, -1 do
    local block = sequence:at(index)
    local node = block.entry.metadata.node
    if node and node ~= vim.NIL and node.kind == "exchange" and (not node.parent or node.parent == vim.NIL) then
      newest = newest or { id = node.id, finish = finish }
      if node.lifecycle == "live" then return finish, newest.id end
      if node.id == reveal.exchange and newest.id == reveal.newest then previous = finish end
    end
    -- The projector puts a separator before each subsequent top-level exchange, including
    -- task continuations whose opening has no user prompt.
    if vim.endswith(block.id, ":separator") then finish = index - 1 end
  end
  if newest then return previous or newest.finish, newest.id end
  return sequence:count() - 1, nil
end

---Follow live progress from the composer and reveal completed exchanges outside transcript focus.
---@param document table Committed native transcript replica.
---@param views table<integer, table> Attached transcript windows.
---@param reset? boolean Adopt initial or switched history without revealing it.
---@param handoff? boolean The host reports that a queued turn follows the settled exchange. The
---successor replaces that exchange at the tail within a few owner steps, so revealing it would
---move the transcript twice; the settlement is adopted as seen instead.
function Reveal:publish(document, views, reset, handoff)
  local prompt_row, response_row
  local complete_response = true
  local exchange, settled, live
  local finish, newest = exchange_range(document.sequence, self)
  for index = finish, 0, -1 do
    local block = document.sequence:at(index)
    local metadata = block.entry.metadata
    local node = metadata.node
    if node and node ~= vim.NIL and (not node.parent or node.parent == vim.NIL) then
      local marker = metadata.layout and metadata.layout.marker
      if node.kind == "exchange" then
        if exchange then break end
        exchange, settled = node.id, node.lifecycle == "settled"
        live = node.lifecycle == "live"
      elseif exchange and marker and marker ~= vim.NIL and marker.capture == "ForgeHarnessPrompt" then
        local _, row = document.sequence:position(block.id)
        prompt_row = prompt_row or row
        break
      elseif not exchange and not prompt_row and marker and marker ~= vim.NIL
        and marker.capture == "ForgeHarnessPrompt" then
        -- Steering consumed mid-exchange starts the reply the reader is waiting for, so the
        -- newest steering row, not the exchange's original prompt, anchors the reveal.
        local _, row = document.sequence:position(block.id)
        prompt_row = row
      elseif not exchange and not prompt_row and node.kind == "message" and marker and marker ~= vim.NIL
        and marker.capture == "ForgeHarnessResponse" then
        local _, row = document.sequence:position(block.id)
        response_row = row
        complete_response = complete_response and block.entry.row_count > 0 and node.display == "full" and not node.more
      end
    end
  end
  local completed = exchange and settled and (self.exchange ~= exchange or not self.settled
    or self.deferred == exchange)
  local follow = live and self.revision ~= document.revision and self:composer_focused()
  self.exchange, self.settled, self.newest = exchange, settled == true, newest
  self.revision = document.revision
  self.deferred = nil
  if reset or handoff and not follow
    or not follow and (not completed or not response_row or not complete_response) then return end
  if vim.api.nvim_get_current_buf() == document.buffer then return end

  local first_row = not follow and require("forge.buffer").physical_row(document, prompt_row or response_row) or nil
  local last_row = vim.api.nvim_buf_line_count(document.buffer) - 1
  if finish < document.sequence:count() - 1 then
    local _, next_row = document.sequence:position(document.sequence:at(finish + 1).id)
    last_row = require("forge.buffer").physical_row(document, next_row) - 1
  end
  local function position_end(window)
    if finish == document.sequence:count() - 1 then
      layout.position_tail(document.buffer, window)
    else
      vim.api.nvim_win_set_cursor(window, { last_row + 1, 0 })
      vim.api.nvim_win_call(window, function() vim.cmd("normal! $zb") end)
    end
  end
  local tabpage = vim.api.nvim_get_current_tabpage()
  local shown = false
  for window in pairs(views) do
    if vim.api.nvim_win_is_valid(window)
      and vim.api.nvim_win_get_buf(window) == document.buffer
      and vim.api.nvim_win_get_tabpage(window) == tabpage then
      shown = true
      if follow then
        position_end(window)
      else
        local height = vim.api.nvim_win_get_height(window) - (vim.wo[window].winbar ~= "" and 1 or 0)
        -- Keep the prompt visible when the completed exchange exceeds the window.
        local measured = vim.api.nvim_win_text_height(window, { start_row = first_row, end_row = last_row })
        if measured.all <= height then
          position_end(window)
        else
          vim.api.nvim_win_set_cursor(window, { first_row + 1, 0 })
          vim.api.nvim_win_call(window, function()
            local scrolloff = vim.wo[window].scrolloff
            if scrolloff < 0 then scrolloff = vim.o.scrolloff end
            local margin = math.min(scrolloff, math.floor((height - 1) / 2))
            if margin > 0 then vim.cmd("normal! " .. margin .. "gj") end
            local view = vim.fn.winsaveview()
            view.topline, view.skipcol, view.topfill, view.leftcol = first_row + 1, 0, 0, 0
            vim.fn.winrestview(view)
          end)
        end
      end
    end
  end
  -- Nobody saw the exchange finish, so its reveal runs when its tab page is shown again.
  if not shown and not follow then self.deferred = exchange end
end

return Reveal

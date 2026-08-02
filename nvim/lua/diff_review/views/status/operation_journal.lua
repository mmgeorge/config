local section_map = require("diff_review.views.status.section_map")

local M = {}

---@alias DiffReviewOperationBurstId string|integer
---@alias DiffReviewOperationLayerState "pending"|"succeeded"

--- Represents one ordered optimistic status move owned by an operation burst.
---@class DiffReviewOperationLayer
---@field sequence integer
---@field burst_id DiffReviewOperationBurstId
---@field state DiffReviewOperationLayerState
---@field move DiffReviewStatusMove

--- Tracks immutable confirmed sections and ordered optimistic operation layers.
---@class DiffReviewOperationJournal
---@field confirmed_section_list DiffReviewStatusSection[]
---@field layer_list DiffReviewOperationLayer[]
---@field next_sequence integer

---@param journal DiffReviewOperationJournal
---@return DiffReviewOperationLayer[]
local function copy_layer_list(journal)
  local layer_list = {} ---@type DiffReviewOperationLayer[]
  for _, layer in ipairs(journal.layer_list) do
    layer_list[#layer_list + 1] = layer
  end
  return layer_list
end

---@param confirmed_section_list DiffReviewStatusSection[]
---@param layer_list DiffReviewOperationLayer[]
---@param next_sequence integer
---@return DiffReviewOperationJournal
local function journal_from_parts(confirmed_section_list, layer_list, next_sequence)
  return {
    confirmed_section_list = confirmed_section_list,
    layer_list = layer_list,
    next_sequence = next_sequence,
  }
end

--- Build a journal from an authoritative section snapshot.
---@param confirmed_section_list DiffReviewStatusSection[]
---@return DiffReviewOperationJournal
function M.new(confirmed_section_list)
  return journal_from_parts(vim.deepcopy(confirmed_section_list or {}), {}, 1)
end

--- Build a journal with one appended semantic layer without changing the prior journal.
---@param journal DiffReviewOperationJournal
---@param burst_id DiffReviewOperationBurstId
---@param entries DiffReviewStatusEntry[]
---@param target_section DiffReviewStatusStageSectionName
---@return DiffReviewOperationJournal
function M.append(journal, burst_id, entries, target_section)
  assert(burst_id ~= nil, "operation burst id is required")
  local move = section_map.capture_move(entries, target_section)
  if #move.selection_list == 0 then return journal end
  local layer_list = copy_layer_list(journal)
  layer_list[#layer_list + 1] = {
    sequence = journal.next_sequence,
    burst_id = burst_id,
    state = "pending",
    move = move,
  }
  return journal_from_parts(journal.confirmed_section_list, layer_list, journal.next_sequence + 1)
end

--- Build a journal with a completed burst retained until snapshot commit.
---@param journal DiffReviewOperationJournal
---@param burst_id DiffReviewOperationBurstId
---@return DiffReviewOperationJournal
function M.mark_succeeded(journal, burst_id)
  local layer_list = {} ---@type DiffReviewOperationLayer[]
  for _, layer in ipairs(journal.layer_list) do
    if layer.burst_id == burst_id and layer.state ~= "succeeded" then
      layer_list[#layer_list + 1] = {
        sequence = layer.sequence,
        burst_id = layer.burst_id,
        state = "succeeded",
        move = layer.move,
      }
    else
      layer_list[#layer_list + 1] = layer
    end
  end
  return journal_from_parts(journal.confirmed_section_list, layer_list, journal.next_sequence)
end

--- Build a journal without one failed or cancelled burst, preserving survivor order.
---@param journal DiffReviewOperationJournal
---@param burst_id DiffReviewOperationBurstId
---@return DiffReviewOperationJournal
function M.remove_burst(journal, burst_id)
  local layer_list = {} ---@type DiffReviewOperationLayer[]
  for _, layer in ipairs(journal.layer_list) do
    if layer.burst_id ~= burst_id then layer_list[#layer_list + 1] = layer end
  end
  return journal_from_parts(journal.confirmed_section_list, layer_list, journal.next_sequence)
end

--- Merge an authoritative baseline and retire only its resolved burst.
---@param journal DiffReviewOperationJournal
---@param authoritative_section_list DiffReviewStatusSection[]
---@param resolved_burst_id? DiffReviewOperationBurstId
---@return DiffReviewOperationJournal
function M.commit(journal, authoritative_section_list, resolved_burst_id)
  local layer_list = {} ---@type DiffReviewOperationLayer[]
  for _, layer in ipairs(journal.layer_list) do
    if resolved_burst_id == nil or layer.burst_id ~= resolved_burst_id then
      layer_list[#layer_list + 1] = layer
    end
  end
  return journal_from_parts(vim.deepcopy(authoritative_section_list or {}), layer_list, journal.next_sequence)
end

--- Build a clean journal from a full snapshot after coordinator work drains.
---@param journal DiffReviewOperationJournal
---@param authoritative_section_list DiffReviewStatusSection[]
---@return DiffReviewOperationJournal
function M.reset(journal, authoritative_section_list)
  return journal_from_parts(vim.deepcopy(authoritative_section_list or {}), {}, journal.next_sequence)
end

--- Build the visible section model by replaying every surviving layer in order.
---@param journal DiffReviewOperationJournal
---@return DiffReviewStatusSection[]
function M.project(journal)
  local projected_section_list = vim.deepcopy(journal.confirmed_section_list)
  for _, layer in ipairs(journal.layer_list) do
    projected_section_list = section_map.apply_move(projected_section_list, layer.move)
  end
  return projected_section_list
end

--- Build a detached copy of the confirmed baseline for path-authoritative replacement.
---@param journal DiffReviewOperationJournal
---@return DiffReviewStatusSection[]
function M.confirmed(journal)
  return vim.deepcopy(journal.confirmed_section_list)
end

--- Build a path set from every unresolved optimistic layer.
---@param journal DiffReviewOperationJournal
---@return DiffReviewAffectedPathSet
function M.pending_path_set(journal)
  local path_set = {} ---@type DiffReviewAffectedPathSet
  for _, layer in ipairs(journal.layer_list) do
    for _, selection in ipairs(layer.move.selection_list) do
      path_set[selection.filename] = true
    end
  end
  return path_set
end

--- Resolve whether any optimistic layer still requires resolution.
---@param journal DiffReviewOperationJournal
---@return boolean
function M.has_layers(journal)
  return #journal.layer_list > 0
end

return M

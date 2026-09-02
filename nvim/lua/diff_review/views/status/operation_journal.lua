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

--- Creates a shallow clone of the journal's operation layer list.
---@param journal DiffReviewOperationJournal Source journal record.
---@return DiffReviewOperationLayer[] layer_list Cloned layer list.
local function copy_layer_list(journal)
  local layer_list = {} ---@type DiffReviewOperationLayer[]
  for _, layer in ipairs(journal.layer_list) do
    layer_list[#layer_list + 1] = layer
  end
  return layer_list
end

--- Instantiates an immutable journal record from confirmed sections and operation layers.
---@param confirmed_section_list DiffReviewStatusSection[] Authoritative baseline sections.
---@param layer_list DiffReviewOperationLayer[] Ordered operation layers.
---@param next_sequence integer Monotonic sequence counter.
---@return DiffReviewOperationJournal journal Instantiated journal record.
local function journal_from_parts(confirmed_section_list, layer_list, next_sequence)
  return {
    confirmed_section_list = confirmed_section_list,
    layer_list = layer_list,
    next_sequence = next_sequence,
  }
end

--- Initializes a new operation journal from an authoritative section snapshot.
---@param confirmed_section_list DiffReviewStatusSection[] Baseline status section array.
---@return DiffReviewOperationJournal journal New operation journal instance.
function M.new(confirmed_section_list)
  return journal_from_parts(vim.deepcopy(confirmed_section_list or {}), {}, 1)
end

--- Appends an optimistic status move layer to the journal without modifying the input journal.
---@param journal DiffReviewOperationJournal Source operation journal.
---@param burst_id DiffReviewOperationBurstId Unique burst transaction identifier.
---@param entries DiffReviewStatusEntry[] Target status entries being moved.
---@param target_section DiffReviewStatusStageSectionName Destination section name (`"staged"` or `"unstaged"`).
---@return DiffReviewOperationJournal journal Updated operation journal instance.
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

--- Marks an operation burst as succeeded while retaining it until snapshot commit.
---@param journal DiffReviewOperationJournal Source operation journal.
---@param burst_id DiffReviewOperationBurstId Target burst identifier.
---@return DiffReviewOperationJournal journal Updated operation journal instance.
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

--- Removes a failed or cancelled operation burst from the journal layers.
---@param journal DiffReviewOperationJournal Source operation journal.
---@param burst_id DiffReviewOperationBurstId Burst identifier to remove.
---@return DiffReviewOperationJournal journal Updated operation journal instance.
function M.remove_burst(journal, burst_id)
  local layer_list = {} ---@type DiffReviewOperationLayer[]
  for _, layer in ipairs(journal.layer_list) do
    if layer.burst_id ~= burst_id then layer_list[#layer_list + 1] = layer end
  end
  return journal_from_parts(journal.confirmed_section_list, layer_list, journal.next_sequence)
end

--- Merges an authoritative baseline snapshot and retires the resolved operation burst.
---@param journal DiffReviewOperationJournal Source operation journal.
---@param authoritative_section_list DiffReviewStatusSection[] Authoritative status sections.
---@param resolved_burst_id? DiffReviewOperationBurstId Optional completed burst identifier.
---@return DiffReviewOperationJournal journal Updated operation journal instance.
function M.commit(journal, authoritative_section_list, resolved_burst_id)
  local layer_list = {} ---@type DiffReviewOperationLayer[]
  for _, layer in ipairs(journal.layer_list) do
    if resolved_burst_id == nil or layer.burst_id ~= resolved_burst_id then
      layer_list[#layer_list + 1] = layer
    end
  end
  return journal_from_parts(vim.deepcopy(authoritative_section_list or {}), layer_list, journal.next_sequence)
end

--- Resets the journal with a clean authoritative section snapshot and empty operation layers.
---@param journal DiffReviewOperationJournal Source operation journal.
---@param authoritative_section_list DiffReviewStatusSection[] Authoritative status sections.
---@return DiffReviewOperationJournal journal Reset operation journal instance.
function M.reset(journal, authoritative_section_list)
  return journal_from_parts(vim.deepcopy(authoritative_section_list or {}), {}, journal.next_sequence)
end

--- Replays all surviving optimistic operation layers in sequence over the confirmed baseline.
---@param journal DiffReviewOperationJournal Source operation journal.
---@return DiffReviewStatusSection[] sections Renderable status sections array.
function M.project(journal)
  local projected_section_list = vim.deepcopy(journal.confirmed_section_list)
  for _, layer in ipairs(journal.layer_list) do
    projected_section_list = section_map.apply_move(projected_section_list, layer.move)
  end
  return projected_section_list
end

--- Returns a detached clone of the confirmed baseline section array.
---@param journal DiffReviewOperationJournal Source operation journal.
---@return DiffReviewStatusSection[] sections Detached baseline status sections.
function M.confirmed(journal)
  return vim.deepcopy(journal.confirmed_section_list)
end

--- Collects the set of affected file paths across all pending operation layers.
---@param journal DiffReviewOperationJournal Source operation journal.
---@return DiffReviewAffectedPathSet path_set Set of affected relative file paths.
function M.pending_path_set(journal)
  local path_set = {} ---@type DiffReviewAffectedPathSet
  for _, layer in ipairs(journal.layer_list) do
    for _, selection in ipairs(layer.move.selection_list) do
      path_set[selection.filename] = true
    end
  end
  return path_set
end

--- Determines whether any optimistic operation layers remain active in the journal.
---@param journal DiffReviewOperationJournal Source operation journal.
---@return boolean has_layers True if one or more layers exist.
function M.has_layers(journal)
  return #journal.layer_list > 0
end

return M

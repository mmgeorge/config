vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
vim.loader.enable(false)
local fixture = vim.json.decode(table.concat(vim.fn.readfile("nvim/rust/forge/target/timeline-semantic-fixture.json"), "\n"))
local client = require("forge.client")
local accepting = client.host_is_accepting
client.host_is_accepting = function() return true end
local interactive = vim.g.forge_timeline_semantic_interactive == true
local stage, pending, owner, controller = 0, nil, nil, nil
local notices = {}
local transcript = vim.api.nvim_create_buf(false, true)
local composer = vim.api.nvim_create_buf(false, true)
local window = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_buf(window, transcript)
local function bind(value, native, document)
  for key, child in pairs(value) do
    if child == native then value[key] = document
    elseif type(child) == "table" then bind(child, native, document) end
  end
end
owner = require("forge.views.harness.presentation").open({ session_id = "session", transcript_buffer = transcript,
  composer_buffer = composer, transcript_window = window, read_only = true, is_alive = function() return true end,
  notice = function(message) notices[#notices + 1] = message end,
  on_update = function() if controller then controller.refresh_winbar() end end,
  request = function(params, callback)
    if params.operation == "open" then
      bind(fixture, fixture.initial.transcript.document .. ":status", params.document .. ":status")
      bind(fixture, fixture.initial.transcript.document, params.document)
      fixture.initial.syntax_pending = false
      callback(fixture.initial)
    elseif params.operation == "node" or params.operation == "diff_input" then
      stage = stage + 1
      local transition = assert(fixture.transition[stage], "unexpected timeline action")
      local expected = ({ changes = "node", file = "node", semantic = "diff_input", public = "diff_input", visible = "node", full = "diff_input", all = "diff_input" })[transition.step]
      assert(params.operation == expected, params.operation .. " instead of " .. expected)
      if params.input then
        assert(params.input.action == ((transition.step == "semantic" or transition.step == "full") and "toggle_presentation" or "toggle_visibility"))
      end
      if stage == 3 and not interactive then pending = function() callback(transition.result) end else callback(transition.result) end
    elseif params.operation == "sync" then
      local delivery = vim.deepcopy(stage == 0 and { snapshot = fixture.initial.transcript, status = fixture.initial.status, diff_view = fixture.initial.diff_view } or fixture.transition[stage].sync)
      delivery.syntax_pending = false
      if params.revision == (stage == 0 and fixture.initial.transcript.revision or fixture.transition[stage].expected.revision) then delivery.patch, delivery.snapshot = {}, vim.NIL end
      callback(delivery)
    elseif params.operation == "snapshot" then
      local snapshot = vim.deepcopy(stage == 0 and fixture.initial.transcript or fixture.transition[stage].expected)
      snapshot.diff_view = stage == 0 and fixture.initial.diff_view or fixture.transition[stage].result.diff_view
      callback(snapshot)
    else callback({}) end
  end,
}, function(value, failure)
  assert(value and not failure, failure)
  value.observe_sections = function() end
end)
local function settled(expected)
  assert(vim.wait(3000, function() return stage == expected and owner.ready and not owner.pending and not owner.syncing and not owner.applying and not owner.diff_inflight end), table.concat(notices, "\n"))
  assert(owner.transcript.status == "Applied" and #notices == 0, table.concat(notices, "\n"))
end
settled(0)
local session = require("forge.session")
local prior = session.harness.presentation
session.harness.presentation = owner
session.harness.transcript_win = window
controller = require("forge.views.harness.controller")
controller.attach_transcript(transcript)
controller.refresh_winbar()
local function select(kind, text)
  for row, line in ipairs(vim.api.nvim_buf_get_lines(transcript, 0, -1, false)) do
    local location = require("forge.buffer").locate(owner.transcript, row - 1, 0)
    local node = location and owner.transcript.sequence.node[location.block].entry.metadata.node
    if node and node.kind == kind and (not text or line:find(text, 1, true)) then vim.api.nvim_win_set_cursor(0, {row, 0}) return node.id end
  end
  error("missing native " .. kind .. " heading")
end
local function invoke(key, expected)
  local mapping = vim.fn.maparg(key, "n", false, true)
  assert(mapping.callback and mapping.buffer == 1, "missing " .. key)
  mapping.callback()
  if expected then settled(expected) end
end
if interactive then
  _G.ForgeTimelineSemanticFixture = { owner = owner, stage = function() return stage end, notices = notices }
  vim.cmd("redraw")
  return
end
local changes = select("changes")
owner.toggle_heading(window)
settled(1)
local file = select("file")
owner.toggle_heading(window)
settled(2)
invoke("ox")
assert(vim.wait(1000, function() return pending ~= nil end))
invoke("ov")
invoke("ov")
pending()
settled(3)
assert(owner.diff_view.presentation == "semantic" and owner.diff_view.visibility == "all")
assert(table.concat(vim.api.nvim_buf_get_lines(transcript, 0, -1, false), "\n"):find("1 change", 1, true), "collapsed semantic heading lost its change count")
invoke("ov", 4)
select("group", "pub fn visible()")
owner.toggle_heading(window)
settled(5)
local rows = table.concat(vim.api.nvim_buf_get_lines(transcript, 0, -1, false), "\n")
assert(rows:find("updated();", 1, true) and not rows:find("hidden", 1, true))
invoke("ox", 6)
assert(owner.diff_view.visibility == "public" and owner.diff_view.presentation == "full")
invoke("ov", 7)
assert(owner.diff_view.visibility == "all")
assert(require("forge.nodes").closed(owner.transcript, changes) == false,
  "diff controls collapsed the containing changes section")
assert(require("forge.nodes").closed(owner.transcript, file) == false,
  "diff controls collapsed the loaded file")
owner.close()
session.harness.presentation = prior
client.host_is_accepting = accepting
print("timeline_semantic: native patches, deferred files, controls, parity, unfolding, and cached presentations passed")

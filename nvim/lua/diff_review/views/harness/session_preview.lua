local SessionPreview = {}

local layout = require("diff_review.views.harness.layout")
local main_timeline = require("diff_review.views.harness.timeline")
local markdown = require("diff_review.render.harness.markdown")
local render_transaction = require("diff_review.render.harness.transaction")
local renderer = require("diff_review.render.harness.interaction_tree")

local namespace = vim.api.nvim_create_namespace("DiffReviewHarnessSessionPreview")
local active = nil

---@param transcript_win integer
---@return table
function SessionPreview.open(transcript_win)
  SessionPreview.close()
  local source_buf = vim.api.nvim_win_get_buf(transcript_win)
  local source_view = vim.api.nvim_win_call(transcript_win, vim.fn.winsaveview)
  local preview_buf = vim.api.nvim_create_buf(false, true)
  vim.bo[preview_buf].buftype = "nofile"
  vim.bo[preview_buf].bufhidden = "hide"
  vim.bo[preview_buf].swapfile = false
  vim.bo[preview_buf].filetype = "Harness"
  vim.bo[preview_buf].modifiable = false
  vim.api.nvim_win_set_buf(transcript_win, preview_buf)
  vim.wo[transcript_win].wrap = true
  vim.wo[transcript_win].linebreak = true
  vim.wo[transcript_win].breakindent = true
  vim.wo[transcript_win].breakindentopt = "shift:0"
  vim.wo[transcript_win].foldmethod = "manual"
  vim.wo[transcript_win].foldenable = true
  vim.wo[transcript_win].foldtext = "v:lua.require'diff_review.render.harness.interaction_tree'.foldtext()"
  vim.wo[transcript_win].fillchars = "fold: "
  layout.configure_gutterless_window(transcript_win)
  layout.attach_scroll_boundary(preview_buf, transcript_win)
  active = {
    transcript_win = transcript_win,
    source_buf = source_buf,
    source_view = source_view,
    preview_buf = preview_buf,
    render_state = {
      transcript_buf = preview_buf,
      transcript_win = transcript_win,
      render_namespace = namespace,
    },
  }
  return active
end

---@param snapshot table
function SessionPreview.render(snapshot)
  if not active or not vim.api.nvim_buf_is_valid(active.preview_buf) then return end
  local state = {
    timeline = vim.deepcopy(snapshot.timeline or {}),
    interaction = vim.deepcopy(snapshot.interaction or {}),
    agent = vim.deepcopy(snapshot.agent or { definition = {}, run = {}, turn = {} }),
    agent_live = {},
  }
  local timeline = main_timeline.project(state)
  local render = renderer.build(timeline, {
    content_width = vim.api.nvim_win_is_valid(active.transcript_win)
        and vim.api.nvim_win_get_width(active.transcript_win)
      or nil,
    cwd = snapshot.session and snapshot.session.workspace or vim.uv.cwd(),
    expanded = {},
    now_ms = os.time() * 1000,
  })
  render_transaction.apply(active.render_state, render, { reset = true })
  markdown.render(active.preview_buf, active.transcript_win, render.markdown_ranges)
end

function SessionPreview.close()
  local preview = active
  active = nil
  if not preview then return end
  if vim.api.nvim_win_is_valid(preview.transcript_win) and vim.api.nvim_buf_is_valid(preview.source_buf) then
    vim.api.nvim_win_set_buf(preview.transcript_win, preview.source_buf)
    vim.api.nvim_win_call(preview.transcript_win, function()
      local line_count = math.max(1, vim.api.nvim_buf_line_count(preview.source_buf))
      preview.source_view.lnum = math.min(preview.source_view.lnum, line_count)
      preview.source_view.topline = math.min(preview.source_view.topline, line_count)
      vim.fn.winrestview(preview.source_view)
    end)
  end
  if vim.api.nvim_buf_is_valid(preview.preview_buf) then
    vim.api.nvim_buf_delete(preview.preview_buf, { force = true })
  end
end

return SessionPreview

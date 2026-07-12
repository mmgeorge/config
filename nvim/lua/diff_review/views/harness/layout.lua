local M = {}

local config = require("diff_review.infra.config")

---@param win integer
local function configure_gutterless_window(win)
  vim.wo[win].number = false
  vim.wo[win].relativenumber = false
  vim.wo[win].signcolumn = "no"
  vim.wo[win].foldcolumn = "0"
  vim.wo[win].statuscolumn = ""
end

---@param name string
---@return integer
local function named_buffer(name)
  local existing = vim.fn.bufnr(name)
  if existing >= 0 and vim.api.nvim_buf_is_valid(existing) then return existing end
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_name(buf, name)
  return buf
end

---@return integer, integer, integer, integer
function M.open()
  local options = config.options.harness
  vim.cmd("tabnew")
  local transcript_win = vim.api.nvim_get_current_win()
  local transcript_buf = named_buffer(options.buffer_name)
  vim.api.nvim_win_set_buf(transcript_win, transcript_buf)
  vim.bo[transcript_buf].buftype = "nofile"
  vim.bo[transcript_buf].bufhidden = "hide"
  vim.bo[transcript_buf].swapfile = false
  vim.bo[transcript_buf].filetype = "Harness"
  vim.bo[transcript_buf].modifiable = false
  vim.wo[transcript_win].wrap = true
  vim.wo[transcript_win].linebreak = true
  vim.wo[transcript_win].breakindent = true
  vim.wo[transcript_win].breakindentopt = ""
  vim.wo[transcript_win].foldmethod = "manual"
  vim.wo[transcript_win].foldenable = true
  vim.wo[transcript_win].foldtext = "v:lua.require'diff_review.render.harness.transcript'.foldtext()"
  vim.wo[transcript_win].fillchars = "fold: "
  configure_gutterless_window(transcript_win)

  vim.cmd("belowright " .. tostring(options.composer_min_height) .. "split")
  local composer_win = vim.api.nvim_get_current_win()
  local composer_buf = named_buffer(options.composer_name)
  vim.api.nvim_win_set_buf(composer_win, composer_buf)
  vim.bo[composer_buf].buftype = "nofile"
  vim.bo[composer_buf].bufhidden = "hide"
  vim.bo[composer_buf].swapfile = false
  vim.bo[composer_buf].filetype = "HarnessInput"
  vim.bo[composer_buf].modifiable = true
  vim.api.nvim_buf_set_lines(composer_buf, 0, -1, false, { "" })
  vim.wo[composer_win].wrap = true
  vim.wo[composer_win].linebreak = true
  configure_gutterless_window(composer_win)
  vim.wo[composer_win].foldcolumn = "1"
  return transcript_buf, transcript_win, composer_buf, composer_win
end

---@param composer_buf integer
---@param composer_win integer
function M.attach_auto_height(composer_buf, composer_win)
  local group = vim.api.nvim_create_augroup("DiffReviewHarnessComposer" .. composer_buf, { clear = true })
  local function resize()
    M.resize_composer(composer_buf, composer_win)
  end
  vim.api.nvim_create_autocmd({ "TextChanged", "TextChangedI", "BufWinEnter" }, {
    group = group,
    buffer = composer_buf,
    callback = resize,
  })
end

---@param composer_buf integer
---@param composer_win integer
function M.resize_composer(composer_buf, composer_win)
  if not (vim.api.nvim_buf_is_valid(composer_buf) and vim.api.nvim_win_is_valid(composer_win)) then return end
  local options = config.options.harness
  local count = vim.api.nvim_buf_line_count(composer_buf)
  local queue_rows = vim.b[composer_buf].diff_review_queue_rows or 0
  vim.api.nvim_win_set_height(
    composer_win,
    math.max(options.composer_min_height, math.min(options.composer_max_height, count + queue_rows + 1))
  )
end

---@param transcript_buf integer
---@param transcript_win integer
---@return integer
function M.maximum_content_topline(transcript_buf, transcript_win)
  local line_count = vim.api.nvim_buf_line_count(transcript_buf)
  local window_height = vim.api.nvim_win_get_height(transcript_win)
  local function tail_height(start_row)
    local ok, height = pcall(vim.api.nvim_win_text_height, transcript_win, {
      start_row = start_row,
      end_row = line_count - 1,
    })
    return ok and height.all or (line_count - start_row)
  end
  if tail_height(0) <= window_height then return 1 end
  local low = 0
  local high = line_count - 1
  local last_full_row = 0
  while low <= high do
    local middle = math.floor((low + high) / 2)
    if tail_height(middle) >= window_height then
      last_full_row = middle
      low = middle + 1
    else
      high = middle - 1
    end
  end
  return last_full_row + 1
end

---@param transcript_buf integer
---@param transcript_win integer
function M.attach_scroll_boundary(transcript_buf, transcript_win)
  local group = vim.api.nvim_create_augroup("DiffReviewHarnessScroll" .. transcript_buf, { clear = true })
  local clamping = false
  vim.api.nvim_create_autocmd("WinScrolled", {
    group = group,
    buffer = transcript_buf,
    callback = function()
      if clamping or not vim.api.nvim_win_is_valid(transcript_win) then return end
      local maximum_topline = M.maximum_content_topline(transcript_buf, transcript_win)
      vim.api.nvim_win_call(transcript_win, function()
        local view = vim.fn.winsaveview()
        if view.topline <= maximum_topline then return end
        clamping = true
        view.topline = maximum_topline
        vim.fn.winrestview(view)
        clamping = false
      end)
    end,
  })
end

return M

local autosnippets = {
  {
    trigger = "la ",
    body = "<- "
  },
  --         {
  --           trigger = "efn ",
  --           body = [[export function $1($2): $3 {
  --   $4
  -- }]],
  --           files = { "typescript", "typescriptreact" }
  --         },
  --         {
  --           trigger = "fn ",
  --           body = [[function $1($2): $3 {
  --   $4
  -- }]],
  --           files = { "typescript", "typescriptreact" }
  --         },
  --         {
  --           trigger = "cfn ",
  --           body = [[const $1 = (props: { $2 }) => {
  --   $4
  -- }]],
  --           files = { "typescriptreact" }
  --         },
  --         {
  --           trigger = "fn ",
  --           body = [[fn $1($2) -> $3 {
  --   $4
  -- }]],
  --           files = { "rust" }
  --         },
  {
    trigger = "todo: ",
    body = "TODO: "
  },
  {
    trigger = "perf: ",
    body = "PERF: "
  },
  {
    trigger = "bug: ",
    body = "BUG: "
  },
  {
    trigger = "warn: ",
    body = "WARN: "
  },
  {
    trigger = "hack: ",
    body = "HACK: "
  },
  {
    trigger = "note: ",
    body = "NOTE: "
  },
  {
    trigger = "ra ",
    body = "-> "
  },
  {
    trigger = "rra ",
    body = "=> "
  },
  {
    trigger = "udef ",
    body = "self.useDefaults?.(arguments)"
  },
  {
    trigger = "sf",
    body = [[<script>
var esriConfig = {
  has: {
    "esri-2d-update-debug": 1,
    "esri-2d-debug": 1,
    "esri-tiles-debug": 1,
    "featurelayer-pbf": 1,
  },
},
</script>]]
  },
  {
    trigger = "doch ",
    body = [[//--------------------------------------------------------------------------"
//
//  $1
//
//--------------------------------------------------------------------------"
]]
  }
}

function valid_for_file(item, ftype)
  if not item.file then
    return true
  end

  for _, v in ipairs(item.files) do
    if v == ftype then
      return true
    end
  end
  return false
end

vim.api.nvim_create_autocmd("TextChangedI", {
  pattern = "*",
  callback = function()
    local line = vim.api.nvim_get_current_line()
    local col = vim.api.nvim_win_get_cursor(0)[2] -- Current column

    -- Extract the last entered token
    local start_pos, end_pos = line:sub(1, col):find("([^%s]+)%s?$")
    local ftype = vim.bo.filetype

    if start_pos and end_pos then
      local last_token = line:sub(start_pos, end_pos)

      -- OPTIMIZE: Inefficent. Use map as snippets list grows?
      for index, item in ipairs(autosnippets) do
        if valid_for_file(item, ftype) and last_token == item.trigger then
          local pos = vim.api.nvim_win_get_cursor(0) -- get current cursor position
          local row = pos[1] - 1;
          local buf = vim.api.nvim_get_current_buf()
          -- Remove match token
          vim.api.nvim_buf_set_text(buf, row, start_pos - 1, row, end_pos, {})
          -- Insert text at cursor
          -- vim.api.nvim_put(vim.split(item.body, "\n"), "", true, true)
          vim.snippet.expand(item.body)
          break
        end
      end
    end
  end,
})

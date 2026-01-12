-- Pull in the wezterm API
local wezterm                               = require 'wezterm'
local action                                = wezterm.action

-- This will hold the configuration.
local config                                = wezterm.config_builder()

config.default_cwd                          = "D:/"

config.default_prog = { "D:/config/windows/nudevcmd.bat" }
-- config.default_prog = { "nu" }
-- config.default_prog                         = {
--   "C:/Program Files/PowerShell/7/pwsh.exe",
--   "-NoExit",
--   "-NoProfile",
--   "-File", "D:/config/windows/init.ps1"
-- }

-- config.unix_domains = {
--   {
--     name = 'unix',
--   },
-- }
-- config.default_domain = 'unix'
config.font                                 = wezterm.font('Cascadia Code NF')
config.font_rules                           = {}

config.line_height                          = 1.0
config.cursor_blink_rate                    = 0

config.colors                               = {
  -- cursor_fg = 'red',
  cursor_fg = 'black',
  cursor_bg = 'white',
  cursor_border = 'white',

  tab_bar = {
    background = 'black',
    active_tab = {
      bg_color = 'black',
      fg_color = 'white',
    },
    inactive_tab_edge = 'black',
    inactive_tab = {
      bg_color = 'black',
      fg_color = 'gray',
    },
    inactive_tab_hover = {
      bg_color = 'black',
      fg_color = 'white',
    },
    new_tab = {
      bg_color = 'black',
      fg_color = 'black',
    },
    new_tab_hover = {
      bg_color = 'black',
      fg_color = 'black',
    },
  },
}

config.window_decorations                   = "INTEGRATED_BUTTONS|RESIZE"

config.enable_tab_bar                       = true
config.hide_tab_bar_if_only_one_tab         = false
config.tab_and_split_indices_are_zero_based = true
config.show_tab_index_in_tab_bar            = false
config.show_new_tab_button_in_tab_bar       = false
-- nightly only:
-- config.show_close_tab_button_in_tabs        = false

-- This function returns the suggested title for a tab.
-- It prefers the title that was set via `tab:set_title()`
-- or `wezterm cli set-tab-title`, but falls back to the
-- title of the active pane in that tab.
-- function tab_title(tab_info)
--   local title = tab_info.tab_title
--   -- if the tab title is explicitly set, take that
--   if title and #title > 0 then
--     return title
--   end
--   -- Otherwise, use the title from the active pane
--   -- in that tab
--   return tab_info.active_pane.title
-- end

wezterm.on('format-tab-title', function(tab, tabs, panes, config, hover, max_width)
  local user_title = tab.active_pane.title
  local pwd = tab.active_pane.current_working_dir

  -- If a specific title hasn't been set by a process, use the directory
  if pwd then
    -- Convert the URL object to a local path string
    local path = pwd.file_path

    -- Handle the special case for the home directory (~)
    if path == wezterm.home_dir then
      return " ~ "
    end

    -- Match everything after the last slash
    local last_dir = path:match("([^/]+)$")
    return " " .. last_dir .. " "
  end

  return " " .. user_title .. " "
end)

config.window_padding = {
  left = 4,
  right = 4,
  top = 2,
  bottom = 0,
}

config.window_frame   = {
  font = wezterm.font('Cascadia Code NF'),
  font_size = 10.0,
  active_titlebar_bg = 'black',
  inactive_titlebar_bg = 'black',
}

-- config.inactive_pane_hsb = {
-- saturation = 1.,
-- brightness = .6,
-- }
-- config.integrated_title_buttons     = { 'Close' }

config.leader         = { key = 'n', mods = 'CTRL', timeout_milliseconds = 1000 }
config.keys           = {
  -- {
  --   key = 's',
  --   mods = 'LEADER',
  --   action = wezterm.action.ShowTabNavigator,
  --   -- action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' },
  -- },
  {
    key = 'y',
    mods = 'CTRL',
    action = action.PasteFrom 'Clipboard',
  },
  {
    key = 'c',
    mods = 'LEADER',
    action = wezterm.action.SpawnTab 'CurrentPaneDomain',
  },
  {
    key = '\'',
    mods = 'LEADER',
    action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' },
    -- action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' },
  },
  {
    key = 'm',
    mods = 'LEADER',
    action = wezterm.action.TogglePaneZoomState,
    -- action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' },
  },
  -- {
  --   key = 'i',
  --   mods = 'LEADER',
  --   action = wezterm.action.SwitchToWorkspace,

  -- },
  {
    key = 's',
    mods = 'LEADER',
    action = wezterm.action.ShowLauncherArgs { flags = 'FUZZY|WORKSPACES' },
  },
  {
    key = 'y',
    mods = 'LEADER',
    action = wezterm.action.ActivateTabRelative(-1),
  },
  {
    key = 'z',
    mods = 'LEADER',
    action = wezterm.action.ActivateTabRelative(1),
  },
  { key = "n", mods = "LEADER", action = wezterm.action.ActivatePaneDirection("Left") },
  { key = "a", mods = "LEADER", action = wezterm.action.ActivatePaneDirection("Down") },
  { key = "e", mods = "LEADER", action = wezterm.action.ActivatePaneDirection("Up") },
  { key = "i", mods = "LEADER", action = wezterm.action.ActivatePaneDirection("Right") },
  {
    key = 'x',
    mods = 'LEADER',
    action = wezterm.action.CloseCurrentPane { confirm = false },
  },
  -- {
  --   key = 'o',
  --   mods = 'LEADER',
  --   action = wezterm.action.PaneSelect {
  --     alphabet = 'rsthneai'
  --   },
  -- },
  {
    key = 's',
    mods = 'ALT|SHIFT',
    action = wezterm.action.AdjustPaneSize { 'Up', 2 }
  },
  {
    key = 't',
    mods = 'ALT|SHIFT',
    action = wezterm.action.AdjustPaneSize { 'Down', 2 }
  },
  {
    key = 'o',
    mods = 'LEADER',
    action = wezterm.action.ActivatePaneDirection "Next"
  },
  {
    key = 'O',
    mods = 'LEADER',
    action = wezterm.action.PaneSelect {
      alphabet = 'rsthneai',
      mode = "SwapWithActiveKeepFocus"
    },
  },
  {
    key = ';',
    mods = 'LEADER',
    action = wezterm.action.ActivateCommandPalette,
  },
  {
    key = 'u',
    mods = 'LEADER',
    action = wezterm.action.PromptInputLine {
      description = 'Rename Workspace',
      action = wezterm.action_callback(function(window, pane, line)
        -- line will be `nil` if they hit escape without entering anything
        -- An empty string if they just hit enter
        -- Or the actual line of text they wrote
        if line then
          wezterm.mux.rename_workspace(
            wezterm.mux.get_active_workspace(),
            line
          )
        end
      end),
    },
  },
  {
    key = '.',
    mods = 'LEADER',
    action = wezterm.action.ActivateCopyMode,
  },
}

config.key_tables     = {
  copy_mode = {
    -- Navigation
    { key = 'h', mods = 'NONE', action = action.CopyMode 'MoveForwardWordEnd' },
    { key = 'r', mods = 'NONE', action = action.CopyMode 'MoveBackwardWord' },
    { key = 't', mods = 'NONE', action = action.CopyMode 'MoveDown' },
    { key = 's', mods = 'NONE', action = action.CopyMode 'MoveUp' },
    { key = 'n', mods = 'NONE', action = action.CopyMode 'MoveLeft' },
    { key = 'i', mods = 'NONE', action = action.CopyMode 'MoveRight' },
    { key = 'f', mods = 'ALT',  action = action.CopyMode 'MoveForwardWord' },
    {
      key = ';',
      mods = 'NONE',
      action = action.CopyMode 'MoveToStartOfLineContent',
    },
    -- {
    --   key = '^',
    --   mods = 'SHIFT',
    --   action = action.CopyMode 'MoveToStartOfLineContent',
    -- },
    {
      key = '/',
      mods = 'NONE',
      action = action.CopyMode 'MoveToEndOfLineContent',
    },
    {
      key = 'Space',
      mods = 'NONE',
      action = action.CopyMode { SetSelectionMode = 'Cell' },
    },
    {
      key = 'Escape',
      mods = 'NONE',
      action = action.Multiple {
        { CopyMode = 'Close' },
      },
    },
    {
      key = 'q',
      mods = 'NONE',
      action = action.Multiple {
        { CopyMode = 'Close' },
      },
    },
    { key = ',', mods = 'NONE', action = action.CopyMode 'JumpReverse' },
    { key = '0', mods = 'NONE', action = action.CopyMode 'MoveToStartOfLine' },
    { key = ';', mods = 'NONE', action = action.CopyMode 'JumpAgain' },
    {
      key = 'k',
      mods = 'NONE',
      action = action.CopyMode 'MoveToScrollbackTop',
    },
    {
      key = 'K',
      mods = 'NONE',
      action = action.CopyMode 'MoveToScrollbackBottom',
    },
    {
      key = 'K',
      mods = 'SHIFT',
      action = action.CopyMode 'MoveToScrollbackBottom',
    },
    {
      key = 'p',
      mods = 'NONE',
      action = action.CopyMode { SetSelectionMode = 'Cell' },
    },
    {
      key = 'P',
      mods = 'NONE',
      action = action.CopyMode { SetSelectionMode = 'Line' },
    },
    {
      key = 'P',
      mods = 'SHIFT',
      action = action.CopyMode { SetSelectionMode = 'Line' },
    },
    {
      key = 'l',
      mods = 'NONE',
      action = action.Multiple {
        { CopyTo = 'ClipboardAndPrimarySelection' },
        { CopyMode = 'ClearSelectionMode' },
        -- { CopyMode = 'Close' },
      },
    },
    {
      key = 'e',
      mods = 'NONE',
      -- action = action.CopyMode { MoveByPage = -0.25 },
      action = action.Multiple {
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
      },
    },
    {
      key = 'a',
      mods = 'NONE',
      -- action = action.CopyMode { MoveByPage = 0.25 },
      action = action.Multiple {
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
      },
    },
    {
      key = 'u',
      mods = 'NONE',
      action = action.Multiple {
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
        { CopyMode = 'MoveUp' },
      },
    },
    {
      key = 'o',
      mods = 'NONE',
      action = action.Multiple {
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
        { CopyMode = 'MoveDown' },
      },
    },
    -- {
    --   key = '$',
    --   action = action.CopyMode 'MoveToEndOfLineContent',
    -- },
    -- {
    --   key = 'F',
    --   mods = 'NONE',
    --   action = action.CopyMode { JumpBackward = { prev_char = false } },
    -- },
    -- { key = 'H', mods = 'NONE', action = action.CopyMode 'MoveToViewportTop' },
    -- {
    --   key = 'H',
    --   mods = 'SHIFT',
    --   action = action.CopyMode 'MoveToViewportTop',
    -- },
    -- {
    --   key = 'L',
    --   mods = 'NONE',
    --   action = action.CopyMode 'MoveToViewportBottom',
    -- },
    -- {
    --   key = 'L',
    --   mods = 'SHIFT',
    --   action = action.CopyMode 'MoveToViewportBottom',
    -- },
    -- {
    --   key = 'M',
    --   mods = 'NONE',
    --   action = action.CopyMode 'MoveToViewportMiddle',
    -- },
    -- {
    --   key = 'M',
    --   mods = 'SHIFT',
    --   action = action.CopyMode 'MoveToViewportMiddle',
    -- },
    -- {
    --   key = 'O',
    --   mods = 'NONE',
    --   action = action.CopyMode 'MoveToSelectionOtherEndHoriz',
    -- },
    -- {
    --   key = 'O',
    --   mods = 'SHIFT',
    --   action = action.CopyMode 'MoveToSelectionOtherEndHoriz',
    -- },
    -- {
    --   key = 'T',
    --   mods = 'NONE',
    --   action = action.CopyMode { JumpBackward = { prev_char = true } },
    -- },
    -- {
    --   key = 'T',
    --   mods = 'SHIFT',
    --   action = action.CopyMode { JumpBackward = { prev_char = true } },
    -- },
    -- { key = 'b', mods = 'NONE', action = action.CopyMode 'MoveBackwardWord' },
    -- { key = 'b', mods = 'ALT', action = action.CopyMode 'MoveBackwardWord' },
    -- { key = 'b', mods = 'CTRL', action = action.CopyMode 'PageUp' },
    -- {
    --   key = 'c',
    --   mods = 'CTRL',
    --   action = action.Multiple {
    --     { CopyMode = 'ScrollToBottom' },
    --     { CopyMode = 'Close' },
    --   },
    -- },
    -- {
    --   key = 'e',
    --   mods = 'NONE',
    --   action = action.CopyMode 'MoveForwardWordEnd',
    -- },
    -- {
    --   key = 'f',
    --   mods = 'NONE',
    --   action = action.CopyMode { JumpForward = { prev_char = false } },
    -- },
    -- { key = 'f', mods = 'CTRL', action = action.CopyMode 'PageDown' },
    -- {
    --   key = 'm',
    --   mods = 'ALT',
    --   action = action.CopyMode 'MoveToStartOfLineContent',
    -- },
    -- {
    --   key = 'o',
    --   mods = 'NONE',
    --   action = action.CopyMode 'MoveToSelectionOtherEnd',
    -- },
    -- {
    --   key = 't',
    --   mods = 'NONE',
    --   action = action.CopyMode { JumpForward = { prev_char = true } },
    -- },
    -- {
    --   key = 'u',
    --   mods = 'CTRL',
    --   action = action.CopyMode { MoveByPage = -0.5 },
    -- },
    -- {
    --   key = 'v',
    --   mods = 'CTRL',
    --   action = action.CopyMode { SetSelectionMode = 'Block' },
    -- },
    -- { key = 'w', mods = 'NONE', action = action.CopyMode 'MoveForwardWord' },
    -- { key = 'PageUp', mods = 'NONE', action = action.CopyMode 'PageUp' },
    -- { key = 'PageDown', mods = 'NONE', action = action.CopyMode 'PageDown' },
    { key = 'LeftArrow', mods = 'NONE', action = action.CopyMode 'MoveLeft' },
    {
      key = 'LeftArrow',
      mods = 'ALT',
      action = action.CopyMode 'MoveBackwardWord',
    },
    {
      key = 'RightArrow',
      mods = 'NONE',
      action = action.CopyMode 'MoveRight',
    },
    {
      key = 'RightArrow',
      mods = 'ALT',
      action = action.CopyMode 'MoveForwardWord',
    },
    { key = 'UpArrow',   mods = 'NONE', action = action.CopyMode 'MoveUp' },
    { key = 'DownArrow', mods = 'NONE', action = action.CopyMode 'MoveDown' },
    {
      key = "Enter",
      mods = "ALT",
      action = wezterm.action.DisableDefaultAssignment,
    },
  }
}


-- and finally, return the configuration to wezterm
return config

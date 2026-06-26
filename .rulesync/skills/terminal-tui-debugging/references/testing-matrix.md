# TUI Testing Matrix

Use this reference when choosing regression coverage for terminal UI work. The
goal is to test most behavior below the PTY layer, then reserve real terminal
tests for failures that require terminal mechanics.

## Universal Pattern

1. **State/model layer**: test update functions, command dispatch, keymaps,
   parsers, reducers, stores, and layout math with ordinary unit tests.
2. **Render layer**: render one frame at fixed width and height, then assert a
   buffer, string, snapshot, SVG, or component tree.
3. **PTY layer**: spawn the real app under a pseudo-terminal when raw input,
   ANSI escapes, alternate screen, resize, shell behavior, or process lifecycle
   controls the bug.
4. **Recording layer**: use asciicast or video tools for human replay and docs,
   not as the primary correctness oracle.

## Rust

For **Ratatui** apps, prefer `ratatui::backend::TestBackend` plus snapshot
testing for render output. Use `insta` when reviewable snapshots help catch
frame regressions.

For terminal-driver behavior, follow the Codex-style pattern: wrap a backend
around a VT100 parser, render through the real terminal backend, then inspect
the parsed screen. Use `tmux` or a PTY harness only for smoke tests that require
real resize or process behavior.

Useful coverage:

- Unit-test layout structs and widget state before rendering.
- Snapshot rendered frames at representative sizes.
- Assert Unicode, emoji, CJK, ANSI style spans, wrapping, and cursor position
  with a VT100 parser.
- Mark real `tmux` or external-terminal smoke tests as ignored/manual unless
  the project already runs them reliably in CI.

## Go

For **Bubble Tea** apps, test `Update` and `View` logic directly before adding a
terminal harness. Use `teatest` when you need fixed terminal size, final output,
final model assertions, intermediate output waits, or synthetic key messages.

Useful coverage:

- Unit-test commands and messages that drive the model.
- Use fixed terminal dimensions for golden output.
- Send `tea.KeyMsg` values instead of driving a shell when key handling is a
  Bubble Tea concern.
- Use VHS for demos and repro videos, not as the first regression-test layer.

## TypeScript

For **OpenTUI** or Solid-based TUIs, prefer the framework renderer. Opencode
uses `createTestRenderer` and `testRender` to validate app lifecycle, keymaps,
and components without launching a real terminal for every case.

Useful coverage:

- Mock renderer creation for lifecycle tests.
- Test keymap configuration and mode stacks as data.
- Render components with fixed width and height.
- Escalate to PTY tests only for raw terminal behavior.

## Python

For **Textual**, use async app tests and the framework pilot for key presses,
clicks, and screen-size changes. Use `pytest-textual-snapshot` when visual SVG
snapshots catch layout regressions.

For generic Python CLIs and TUIs, use `pexpect` for process interaction and
`pyte` when you need a screen buffer instead of stream text.

Useful coverage:

- Use `pytest-asyncio` or the framework's async test mode.
- Drive interactions through the app's pilot when available.
- Use snapshot comparison for stable visual states.
- Use `pexpect` plus a terminal emulator for non-framework TUIs.

## Neovim

For plugins and dotfiles, do not default to vendoring Neovim's internal
`screen.lua` harness. Neovim core uses RPC-driven functional tests and
`screen:expect()` because it owns the UI protocol, but those utilities are not
currently a stable public plugin-testing API.

Useful coverage:

- Use pure Lua tests for parsing, transforms, layout math, and request
  normalization.
- Use isolated headless Neovim child processes for plugin APIs, buffers,
  extmarks, diagnostics, autocmds, and commands.
- Use RPC for inspecting the user's existing editor state.
- Use Terminal MCP when the visible terminal surface matters: floating windows,
  splits, prompts, cursor, colors, redraw, and terminal size.

## Generic CLI And TUI

Use stream matching for line-oriented tools and screen-buffer matching for full
screen TUIs.

Useful coverage:

- Use subprocess tests for non-interactive CLI output.
- Use PTY tests for prompts, passwords, keyboard interaction, and terminal mode.
- Use a screen emulator for curses-style redraws and position-sensitive UI.
- Use `tmux capture-pane` or Terminal MCP for manual repros that depend on a
  real terminal multiplexer or rendered interactive state.

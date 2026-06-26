---
name: terminal-tui-debugging
description: >-
  Debug and verify interactive terminal UIs, CLIs, prompts, pagers, shells,
  editors, and TUI applications. Use when a task involves rendered terminal
  state, VT100/ANSI behavior, cursor position, keyboard input, resize behavior,
  alternate-screen behavior, fzf, nvim, less, tmux, curses, ratatui, Bubble Tea,
  Textual, OpenTUI, or integration testing for terminal applications.
---

# Terminal TUI Debugging

Use this skill when the bug lives in the rendered terminal state. Terminal UIs
draw by sending bytes to a pseudo-terminal, so plain command output cannot prove
cursor motion, redraw timing, color spans, alternate screens, or resize behavior.

## Testing Ladder

Choose the lowest layer that can prove the failure.

1. **Model tests** validate state transitions, command routing, keymap lookup,
   parsing, and layout math without a terminal.
2. **Render tests** validate a deterministic frame at fixed dimensions with a
   framework test backend or snapshot.
3. **PTY tests** validate stdin/stdout, ANSI escape handling, terminal size,
   resize signals, alternate screens, and cursor behavior.
4. **Terminal MCP** validates a live reproduction when an agent needs to inspect
   or drive an interactive terminal session.

Use Terminal MCP for diagnosis and human-visible verification. Prefer committed
model, render, or PTY tests for regression coverage.

## Terminal MCP

Use the `terminal` MCP (`terminal-mcp --headless`) to debug interactive terminal
UIs: nvim, fzf, pagers, prompts, and custom TUI apps. It holds a persistent
pseudo-terminal behind a real VT100/ANSI emulator, so it can launch a TUI, send
keystrokes, and read the live rendered screen. Plain shell commands cannot do
this. Reach for Terminal MCP only when the bug lives in that rendered
interactive state. Keep using normal shell commands for plain command output.

- **Reproduce at the right size.** TUI layout bugs depend on terminal
  dimensions. Create a session with the failing `cols` and `rows`
  (`createSession`), or rely on the default session sized by the config. State
  the size used when reporting a layout bug.
- **Drive, then observe, every step.** Send input with `type` for text and
  `sendKey` for keys such as `Enter`, `Escape`, `ArrowUp`, `Ctrl+C`, and
  function keys. After each key, re-read the screen before sending the next. A
  TUI needs a moment to redraw, and acting on a stale frame hides the real state.
- **Pick the observation that answers the question.** Use `getContent` for
  buffer text and cursor position. Use `takeScreenshot` with `ansi` to verify
  colors and SGR styling. Use `takeScreenshot` with `png` when the visual layout
  itself is the question.
- **Isolate with sessions.** Pass a `sessionId` from `createSession` to run the
  TUI in one session while inspecting in another, so commands never interleave.
  Call `destroySession` when done, or let it idle out.
- **Capture hard repros.** Wrap a flaky interaction in `startRecording` and
  `stopRecording` to save an asciicast the user can replay with asciinema.

## Workflow

1. Launch the TUI in an isolated session at the suspected size.
2. Wait for the first stable screen before sending input.
3. Send one key or text chunk.
4. Capture the visible state.
5. Compare the capture against the expected user-visible state.
6. Repeat until the failure mechanism is clear.
7. Close only the process or session you created.

Always report the terminal size, command used to launch the app, interaction
sequence, and the exact observation that proved the result.

## Regression Coverage

After reproducing a TUI bug, add regression coverage at the lowest stable layer.
Do not turn every Terminal MCP repro into a fragile end-to-end test.

- Use **model tests** for reducers, update loops, command dispatch, keymaps,
  parsed rows, and layout calculations.
- Use **render snapshots** for deterministic frame output at fixed sizes.
- Use **PTY tests** for terminal driver behavior, resize handling, alternate
  screen behavior, process lifecycle, and raw input handling.
- Use **manual or Terminal MCP verification** for exploratory debugging,
  flaky timing, and one-off visual inspection.

Read `references/testing-matrix.md` when choosing framework-specific tools or
writing integration tests for a terminal UI stack.

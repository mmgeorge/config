---
name: terminal-tui-debugging
description: >-
  Debug and verify interactive terminal UIs, CLIs, prompts, pagers, shells,
  editors, and TUI applications. Use when a task involves rendered terminal
  state, VT100/ANSI escape sequences, cursor positioning, keyboard input, resize
  behavior, alternate-screen buffers, fzf, nvim, less, tmux, curses, ratatui,
  Bubble Tea, Textual, OpenTUI, or terminal integration testing.
targets:
  - '*'
---

# Terminal TUI Debugging

Use this skill when verifying or debugging rendered terminal state. Terminal UIs draw by emitting byte streams to a pseudo-terminal (PTY), which requires testing cursor motion, redraw timing, color attributes, alternate screen buffers, and resize event propagation.

## Testing Hierarchy

Evaluate defects at the lowest isolated layer capable of reproducing the failure:

1. **Model Tests:** Validate state transitions, command routing, keymap resolution, text parsing, and layout geometry without a terminal driver.
2. **Render Tests:** Validate deterministic frame output at fixed dimensions using framework test backends or snapshot comparisons.
3. **PTY Tests:** Validate stdin/stdout streams, ANSI escape handling, terminal dimensions, resize signals, alternate screen transitions, and cursor positioning.
4. **Terminal MCP Sessions:** Validate live interactive behavior when diagnosing issues in running terminal processes.

Use Terminal MCP for diagnosis and interactive verification. Use automated model, render, or PTY tests for persistent regression suites.

## Terminal MCP Integration

Use `terminal-mcp` (`terminal-mcp --headless`) to debug interactive terminal applications: Neovim, fzf, pagers, shell prompts, and custom TUIs. It runs a persistent pseudo-terminal behind an ANSI/VT100 emulator, enabling keystroke dispatch and rendered screen inspection. Use standard shell commands for batch execution, and use Terminal MCP when interactive terminal state must be inspected.

- **Explicit Dimensions:** Layout behavior depends on terminal dimensions. Create sessions with failing `cols` and `rows` (`createSession`) or record the default configured dimensions when reporting defects.
- **Sequential Input and Screen Capture:** Dispatch inputs using `type` for text and `sendKey` for control keys (`Enter`, `Escape`, `ArrowUp`, `Ctrl+C`). Re-read the screen buffer after each input to observe redraw output before sending subsequent keys.
- **Targeted Output Inspection:** Use `getContent` for buffer text and cursor coordinates. Use `takeScreenshot` with `ansi` format to verify SGR styling and colors. Use `takeScreenshot` with `png` format to inspect visual rendering.
- **Session Isolation:** Allocate dedicated sessions via `createSession` to prevent command interleaving. Terminate sessions with `destroySession` when finished.
- **Recording Repro Sequences:** Capture complex or timing-dependent defects with `startRecording` and `stopRecording` to produce asciicast artifacts.

## Debugging Workflow

1. Spawn the TUI application in an isolated session at specific dimensions.
2. Wait for the initial stable frame before dispatching inputs.
3. Send a single key event or input chunk.
4. Capture the resulting screen buffer and cursor coordinates.
5. Compare the output against expected visual state.
6. Repeat until the defect mechanism is isolated.
7. Terminate the test process and session.

Report the terminal dimensions, startup command line, dispatched input sequence, and observed terminal buffer state.

## Regression Test Boundaries

- **Model Tests:** Unit test state reducers, event loops, command dispatchers, and coordinate calculations.
- **Render Tests:** Snapshot deterministic frames across fixed dimensions.
- **PTY Tests:** Test terminal driver integration, SIGWINCH resize handling, and alternate screen buffer lifecycles.
- **Manual/MCP Verification:** Conduct exploratory debugging and timing validation.

Read `references/testing-matrix.md` when selecting framework-specific test backends or implementing terminal integration test suites.

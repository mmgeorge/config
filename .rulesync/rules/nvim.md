---
globs:
  - 'nvim/**/*'
---
# Neovim Configuration

Use the `neovim-lua-dev` skill for all Lua and Neovim plugin work. It owns cursor and selection preservation, live-editor safety, LuaLS annotations, asynchronous processes, and failure notifications.

- **Persisted data:** Backward compatibility is not a goal for disposable state. Never migrate incompatible sessions, transcripts, caches, or journals; select a fresh compatibility-version directory and provide cleanup for obsolete versions without blocking new versions on old editors. Preserve user settings and configuration outside that namespace and try to migrate them gracefully, reporting unsupported formats and failures. Reuse the owning store's cleanup boundary, protect whole versions held by live users, and leave unrelated files untouched.
- **Test layout:** Each plugin keeps its tests in `nvim/lua/<plugin>/tests/`, and every test file ends in `.test.lua`. Unit tests live under `tests/unit/` and integration tests under `tests/integration/`; both mirror the plugin's source tree. Shared helpers live in `tests/support/`, test data and manual scripts in `tests/fixtures/`, and notes in `tests/plans/`. These are plain `.lua` or Markdown files and never run as tests.
- **Unit tests:** A test of one source file's behavior is a unit test, at that file's mirrored path: `nvim/lua/forge/views/status/status_context.lua` → `nvim/lua/forge/tests/unit/views/status/status_context.test.lua`, and `init.lua` → `init.test.lua`. Each test is one row of the file's `scenarios` table, `{ name = "...", run = function() ... end },`, starting at two-space indentation, which is how the runner discovers rows. Add a new unit test as a row in the existing file, never as a separate file. When a source file moves or is renamed, move its unit test file to the matching path.
- **mini.test files (pilot):** A test file may instead return a `MiniTest.new_set()` whose cases are top-level `T["name"] = function() ... end` entries, as `nvim/lua/forge/tests/unit/lsp/startup.test.lua` does. The runner fetches the pinned mini.test commit itself and runs such a file as one shard, its cases sequentially in a single Neovim through `tests/support/minitest_shard.lua`, with a five-second deadline per case; `file::case` runs one case. Because cases share the process, restore every replaced function in a `post_case` hook and delete temporary state through `MiniTest.finally`.
- **Integration tests:** A test that drives several modules together, the Forge host, or Git is an integration test. It is a scenario row in the file named after the source area it exercises, in that area's mirrored directory: `views/status/` → `nvim/lua/forge/tests/integration/views/status/status.test.lua`, and a top-level module's area is a directory named after it, such as `integration/client/client.test.lua` for `client.lua`. Add a new integration test as a row in its area's file. Only a test that must reload its own file in a child Neovim, or that dispatches its own scenarios, stays a standalone file in the area directory. Load helpers by repository path, such as `dofile("nvim/lua/forge/tests/support/state.lua")`, at file scope before the test changes directory.
- **Running Lua tests:** Run Lua tests only through `.forge/checks/lua-tests.nu`, never a hand-written `nvim -l` command. Tests that launch the Forge host assume a current executable, so run `rust-build` after Rust changes and before the Lua tests. Each scenario row and each standalone test runs in its own headless Neovim, in parallel, and is killed at five seconds. Select tests by test file, source file, directory, test name (the file name without `.test.lua`, which selects every file with that name), or `name::scenario`. A source directory selects both of its mirrors:

  ```nu
  nu .forge/checks/lua-tests.nu                                   # every test
  nu .forge/checks/lua-tests.nu client                            # unit/client.test.lua and integration/client/client.test.lua
  nu .forge/checks/lua-tests.nu client::admission_queue           # one scenario
  nu .forge/checks/lua-tests.nu nvim/lua/forge/client.lua         # a source file's unit test
  nu .forge/checks/lua-tests.nu nvim/lua/forge/lsp                # unit and integration tests for lsp/
  nu .forge/checks/lua-tests.nu --suite status                    # a source area's unit and integration tests
  nu .forge/checks/lua-tests.nu --affected nvim/lua/forge/client.lua  # tests covering changed paths
  nu .forge/checks/lua-tests.nu --list client                     # print the selection without running it
  ```

  `--affected` runs a changed source file's mirrored unit test plus its integration area (the nearest mirrored integration directory of the file or an ancestor), or the plugin's whole test directory when it has no area; a change under `tests/` or to the runner runs every test.
- **LuaLS check:** Check the whole plugin with `lua-language-server --check nvim/lua/forge --logpath <scratch directory outside the repository>`, using the scoop build, because the Mason build crashes on `--check`. After a refactor that moves files with `git mv`, run `:LspRestart` to clear duplicate warnings from the stale index.

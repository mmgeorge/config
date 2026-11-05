---
globs:
  - 'nvim/**/*'
---
# Neovim Configuration

Use the `neovim-lua-dev` skill for all Lua and Neovim plugin work. It owns cursor and selection preservation, live-editor safety, LuaLS annotations, asynchronous processes, and failure notifications.

- **Persisted data:** Backward compatibility is not a goal for disposable state. Never migrate incompatible sessions, transcripts, caches, or journals; select a fresh compatibility-version directory and provide cleanup for obsolete versions without blocking new versions on old editors. Preserve user settings and configuration outside that namespace and try to migrate them gracefully, reporting unsupported formats and failures. Reuse the owning store's cleanup boundary, protect whole versions held by live users, and leave unrelated files untouched.
- **LuaLS check:** Check the whole plugin with `lua-language-server --check nvim/lua/forge --logpath <scratch directory outside the repository>`, using the scoop build, because the Mason build crashes on `--check`. After a refactor that moves files with `git mv`, run `:LspRestart` to clear duplicate warnings from the stale index.

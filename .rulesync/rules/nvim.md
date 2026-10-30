---
globs:
  - 'nvim/**/*'
---
# Neovim Configuration

Use the `neovim-lua-dev` skill for all Lua and Neovim plugin work. It owns cursor and selection preservation, live-editor safety, LuaLS annotations, asynchronous processes, and failure notifications.

- **No compatibility code:** This configuration is personal infrastructure that may break its stored state between revisions. Do not add migrations, compatibility shims, legacy decoders, or fallbacks for old private formats unless asked. Version durable payloads with one exact current format, hide data in any other format instead of upgrading it, and delete obsolete compatibility code when a format changes.
- **LuaLS check:** Check the whole plugin with `lua-language-server --check nvim/lua/forge --logpath <scratch directory outside the repository>`, using the scoop build, because the Mason build crashes on `--check`. After a refactor that moves files with `git mv`, run `:LspRestart` to clear duplicate warnings from the stale index.

# Plan navigation fixtures

These projects exercise definition jumps from captured plan references using running
rust-analyzer, native TypeScript/tsgo, and LuaLS clients. Rust and npm dependencies are
tiny relative local packages. No registry package is needed.

`plan_navigation_languages_host.lua` copies one project into a disposable workspace,
starts a mock Harness session, and uses `plan_navigation_fixture` to seed a valid saved
plan through the Rust storage API. It opens PlanReview and invokes its navigation command.
The language server sees the real project files in hidden buffers.

Build `forge` and the `forge-harness` example `plan_navigation_fixture` before running.
Set `vim.g.forge_test_language`, `vim.g.forge_test_executable`,
`vim.g.forge_test_fixture_generator`, and `vim.g.forge_test_tsgo` in the test Neovim.
Set `vim.g.forge_test_scratch` to an existing persistent parent directory to retain
the workspace and Harness data after Neovim exits. Set `vim.g.forge_test_rust_analyzer`
when the installed analyzer needs an explicit executable path.
The TypeScript test runs an offline npm install with lifecycle scripts disabled.
The mock Harness bootstrap uses a minimal Rust file before the generator replaces
the saved plan with the selected language. This setup belongs only to the test.
Runs with `forge_test_scratch` retain their files after failure for inspection.

The fixtures cover local declarations, a relative dependency, imported members,
Rust enum variants, TypeScript dollar identifiers and optional chains, and Lua modules.

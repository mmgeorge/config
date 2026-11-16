# Building and testing Forge

Neovim launches the built Forge executable; type-checking and test runs do not update it. Inside Harness, use configured checks; outside Harness, run the commands below from the repository root with Nushell and the pinned Rust toolchain.

## Build

Run `rust-build` inside Harness, or build the development executable with Cargo. The output is `nvim/rust/forge/target/debug/forge` (`forge.exe` on Windows).

```nu
cd nvim/rust/forge
cargo build --profile dev --locked --bin forge
```

`:ForgeBuild` performs the same build. Restart Neovim after rebuilding because a running host retains its executable.

For an explicit release build, use `rust-build-release` or `:ForgeBuildRelease`. Set `vim.g.forge_build_profile = "release"` before Forge loads to select `target/release`; the default is `dev`.

## Tests

Lua tests that launch Forge require a current executable. `lua-host-test` declares `rust-build` as a prerequisite; `lua-test` runs without it.

Use `rust-test` for Rust tests and [Editor tests](editor-tests.md) for Lua selection and fixtures. See [Verification by area](verification-by-area.md) for targeted coverage and [Check adapter integration](extending-project-checks.md#check-adapter-process-integration) for runner and process tests.

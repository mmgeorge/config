# Neovim Rust Extensions

This document summarizes what we learned about writing Neovim extensions in Rust, with emphasis on the current ecosystem, `nvim-oxi`, async/background work, testing, packaging, and practical tradeoffs.

## Executive summary

Rust is a viable way to build Neovim extensions, but it is still a niche path compared to Lua.

The main takeaways are:

- Lua is still the default and most widely supported way to write Neovim plugins.
- Rust is attractive when you want stronger typing, better structure for complex logic, or access to the Rust ecosystem.
- The two most relevant Rust paths are:
  - `nvim-oxi`: in-process Rust bindings to Neovim
  - `nvim-rs`: external Msgpack-RPC client / remote plugin approach
- For new plugin development inside Neovim, `nvim-oxi` currently looks like the better fit.
- For complex background I/O, the sweet spot is often:
  - Neovim/UI boundary in `nvim-oxi`
  - background async work in threads or Tokio
  - communication via channels back to the main thread

## Current state of Rust for Neovim

Rust-based Neovim extensions are real and supported, but they are not the mainstream path.

### What is mainstream?

The mainstream path is still:

- Lua plugin code
- Lua configuration surface
- optional helper binaries in Rust, Go, or Node when needed

### When does Rust make sense?

Rust starts to make sense when the plugin needs one or more of the following:

- heavy parsing or transformation logic
- more complex internal architecture than is pleasant in Lua
- robust typed data models
- concurrent background work
- reusable libraries from the Rust ecosystem
- lower overhead than remote RPC plugins

### Common architectural patterns

The practical patterns today are:

1. Pure Lua plugin
   - simplest
   - most compatible
   - easiest for users to install

2. Lua plugin + Rust helper binary
   - common pragmatic compromise
   - UI remains easy in Lua
   - performance-heavy logic moves to Rust

3. `nvim-oxi` plugin
   - the plugin itself is written in Rust
   - loaded from Neovim via Lua `require(...)`
   - most “native Rust plugin” feeling path today

4. `nvim-rs` remote plugin or client
   - Rust process communicates with Neovim via Msgpack-RPC
   - more decoupled, but more IPC overhead and serialization ceremony

## `nvim-oxi` vs `nvim-rs`

### `nvim-oxi`

What it is:

- Rust bindings to Neovim's API
- designed for in-process plugin authoring
- exposes Rust-side access to Neovim API, LuaJIT state, and optional libuv bindings

Why it stands out:

- closer to writing a native plugin than a remote plugin
- better fit for directly exposing `require("my_plugin")` style modules
- avoids Msgpack-RPC overhead for ordinary Neovim API calls

### `nvim-rs`

What it is:

- a library for writing Neovim RPC clients in Rust
- built around Neovim's Msgpack-RPC API

When it fits:

- if you want an external process model
- if your plugin is more naturally a separate daemon/client
- if decoupling from Neovim's in-process ABI matters more than directness

### Which is more supported?

For new in-process plugin work, `nvim-oxi` looks like the better path.

Reasoning:

- `nvim-oxi` is explicitly aimed at plugin authors
- it tracks Neovim versions with feature flags
- it has current examples, tests, and active nightly maintenance

`nvim-rs` is still useful, but it feels more specialized toward RPC/client scenarios.

## What `nvim-oxi` actually does

A key question was whether `nvim-oxi` mostly talks to Lua, or whether it binds to Neovim directly.

The answer is:

- plugin loading happens through Lua module loading conventions
- ordinary editor API calls are mostly direct Rust-to-Neovim C FFI

### Evidence from the implementation

The important split in `nvim-oxi` is:

- `api`: bindings to the Neovim C API
- `lua`: low-level bindings to the LuaJIT state used by Neovim
- `libuv`: optional bindings to Neovim's event loop layer

The `#[nvim_oxi::plugin]` macro generates a `luaopen_<name>` entrypoint, so the resulting dynamic library can be loaded with normal Lua module loading.

But the API layer itself uses direct `extern "C"` declarations for functions such as:

- `nvim_create_buf`
- `nvim_buf_get_lines`
- `nvim_buf_attach`
- `nvim_set_keymap`
- `nvim_echo`
- many more

So the high-level picture is:

- loading and Lua interoperability use LuaJIT state
- ordinary Neovim API calls are direct C FFI, not RPC and not merely `vim.api` Lua wrappers

### What still uses Lua state?

Lua is still part of the story for:

- module loading (`require("my_plugin")`)
- callbacks and Lua function references
- push/pop of values on the Lua stack
- interop with Lua modules
- optional `mlua` integration

### Efficiency implications

Compared to a remote plugin, `nvim-oxi` should be much closer to an in-process Lua plugin in overhead.

That means:

- no Msgpack-RPC round trip for normal API calls
- no external process boundary for ordinary editor operations
- remaining overhead is mostly type conversion, Lua interop, and whatever work your own plugin does

## How a user installs a `nvim-oxi` plugin

### Packaging model

A `nvim-oxi` plugin is generally built as a shared library (`cdylib`).

The artifact is placed so Neovim can load it as a Lua module, for example:

- `lua/my_plugin.so` on Linux
- `lua/my_plugin.dylib` on macOS
- `lua/my_plugin.dll` on Windows

Then from the user's perspective, loading looks like:

```lua
require("my_plugin")
```

### Does it “just work”?

It can, if the plugin author packages the built dynamic library in the expected place.

If the artifact is already present and loadable, then yes, it can effectively “just work.”

### Is Lua glue required?

Not strictly.

A Rust plugin can return a Lua-facing module table directly from Rust. That means APIs like this can be implemented entirely on the Rust side:

```lua
require("my_plugin").setup({
  foo = true,
  bar = "baz",
})
```

However, authors often still include small Lua wrappers for:

- lazy-loading ergonomics
- plugin-manager integration
- platform-specific artifact loading
- friendlier configuration defaults
- startup/build checks

## Built-in package manager (`vim.pack`) and Rust plugins

The new built-in package manager manages plugin repositories on disk. It does not do anything special for Rust plugins.

### What this means in practice

If the repository already contains a loadable artifact like `lua/my_plugin.so`, then `vim.pack` can fetch the repo and the plugin can be required normally.

If the repository requires `cargo build`, then either:

- the plugin author needs to ship built artifacts, or
- the user/config needs a build step/hook

### Minimal config-side example with `vim.pack`

A simple config can add the repo and run a build when the plugin changes on disk:

```lua
vim.pack.add({ 'https://github.com/me/my_plugin' })

vim.api.nvim_create_autocmd('PackChanged', {
  callback = function(ev)
    if ev.data.spec.name == 'my_plugin' and ev.data.kind ~= 'delete' then
      vim.system({ 'cargo', 'build', '--release' }, { cwd = ev.data.path }):wait()
    end
  end,
})

require('my_plugin')
```

So: `vim.pack` manages the repo; build orchestration is still the plugin author's or user's job.

## Configuration surface from Rust

A Rust plugin can expose its config surface directly, without Lua glue.

For example, the Rust module returned from `require("my_plugin")` can include:

- `setup(opts)`
- `open()`
- `toggle()`
- `run(...)`

This feels the same from the user's perspective as a Lua plugin.

Lua wrappers are optional, not required.

## Neovim API support cadence in `nvim-oxi`

### Version flags seen in `nvim-oxi`

At the time of research, `nvim-oxi` exposes feature flags for:

- `neovim-0-10`
- `neovim-0-11`
- `neovim-nightly`

We did **not** see an explicit `neovim-0-12` feature yet.

### What that means

This does not necessarily mean everything breaks on 0.12.

It means:

- `nvim-oxi` has explicit stable coverage for 0.10/0.11
- it also tracks unstable/newer APIs through `nightly`
- but there is not yet a formal published “0.12 feature” knob in the version we examined

### Does `nvim-oxi` need to update immediately after Neovim updates?

Not always.

Whether things break depends on what changed in Neovim and what your plugin uses.

If your plugin uses stable APIs that did not change in a breaking way, it may continue to work fine on a newer Neovim.

If your plugin depends on:

- newly added APIs
- changed function signatures
- changed structs/options/enum representations
- newly added fields your bindings do not yet know about

then `nvim-oxi` may need updates.

### Is nightly tracking useful here?

Yes. Nightly support is valuable because it allows maintainers to adapt before a stable release lands.

But it is not a guarantee that a released Neovim version already has fully polished stable support in the latest published crate.

### Evidence of maintenance cadence

What we found:

- `nvim-oxi` has around 1.1k GitHub stars
- the crates.io release we saw was `0.6.0`
- there are repeated PRs tracking nightly changes over time
- the repo shows multiple “Fix Nightly” or similar PRs across 2024 and 2025

This suggests:

- active maintenance
- real attention to API drift
- but also a maintenance model that tracks Neovim changes incrementally rather than magically abstracting them away forever

## Neovim 0.12 and plugin-author impact

Neovim 0.12 adds a wide range of features, especially in:

- LSP
- UI/API
- floating windows
- diagnostics
- Lua helpers
- statusline and messaging

Examples of notable additions relevant to plugin authors include:

- richer `nvim_echo()` support
- `nvim_open_tabpage()`
- improved floating window behavior
- more message/progress events
- new LSP capabilities and helpers
- `vim.net.request()`

If you want to use brand new 0.12-only APIs through the nice Rust wrapper surface, you generally need `nvim-oxi` to expose them first.

Possible exceptions:

- some APIs may already have been added under `neovim-nightly`
- you can declare your own `extern "C"` binding as a temporary workaround
- you can sometimes reach functionality through Lua interop if needed

## Background work, async, and Tokio

### Can a `cdylib` plugin use Tokio?

Yes.

You can absolutely use Tokio in a `cdylib` plugin.

The main caveat is architectural:

- do not block Neovim's main thread
- do not perform long-running async waits directly on the UI thread
- do Neovim API/UI mutation on the main thread or in scheduled callbacks
- do background I/O in worker threads / Tokio runtime

### Is Tokio created on the main thread?

Not necessarily.

A good plugin pattern is:

- main Neovim thread stays responsive
- a worker OS thread is spawned
- a Tokio runtime is created inside that worker thread
- background tasks run there
- results are sent back via channels

### Real example found

We found a real example in `Ruddickmg/hermes.nvim`.

That repo uses both:

- `nvim-oxi`
- `tokio`

Its `Cargo.toml` includes both crates, and its ACP connection manager creates a Tokio runtime inside a spawned thread using a pattern like:

```rust
let runtime = tokio::runtime::Builder::new_current_thread()
    .enable_all()
    .build()?;
runtime.block_on(async {
    stdio::connect(handler, thread_agent, receiver).await
})
```

That is a strong example of the intended separation:

- `nvim-oxi` for plugin surface and UI-side integration
- Tokio for ACP/background async work

## How the main thread communicates with the Tokio thread

A good mental model is:

- Neovim main thread = UI/event-loop side
- Tokio thread = async worker side

Typical flow:

1. User action in Neovim calls a Rust function exposed through `nvim-oxi`.
2. That function enqueues a request to a background worker.
3. Tokio does async I/O or ACP work.
4. When a result is ready, Tokio sends a plain event/message back through a channel.
5. Neovim-side code polls or schedules a callback, receives the event, and updates the UI.

### Why not `await` on the main thread?

Because that would block or complicate Neovim's responsiveness.

The usual design is:

- no waiting on the main thread
- use nonblocking enqueue/send
- receive results via a queue
- apply UI changes from a libuv/timer/scheduled callback

## `nvim-oxi` and libuv integration

`nvim-oxi` does not appear to provide a full high-level async runtime abstraction equivalent to Tokio that “just integrates everything” for you.

What it does provide:

- Neovim bindings
- LuaJIT bindings
- optional `libuv` bindings

So the practical pattern is:

- use `nvim-oxi` + `libuv` for UI-thread scheduling/polling
- use Tokio/threads for background I/O
- bridge them with channels or shared state

## Buffer creation and watching buffer changes

`nvim-oxi` exposes direct wrappers for the usual Neovim buffer primitives.

### Creating and opening a buffer

Typical workflow:

- create a buffer with `api::create_buf(...)`
- set lines with `Buffer::set_lines(...)`
- show it by setting current buffer or opening a window

### Watching for user edits

`Buffer::attach(...)` wraps Neovim's `nvim_buf_attach()`.

This allows you to observe buffer events such as:

- line changes (`on_lines`)
- byte-level changes (`on_bytes`)
- changedtick-only events
- reload
- detach

### Example

The following example creates a scratch buffer, opens it, and watches for line changes:

```rust
use nvim_oxi as nvim;
use nvim::api::{self, opts::BufAttachOpts};
use nvim::Dictionary;

#[nvim::plugin]
fn watch_demo() -> nvim::Result<Dictionary> {
    Ok(Dictionary::from_iter([(
        "open_watched_buffer",
        nvim::Function::from_fn(open_watched_buffer),
    )]))
}

fn open_watched_buffer() -> nvim::Result<()> {
    let mut buf = api::create_buf(true, true)?;
    buf.set_lines(.., true, ["edit me", "more text here"])?;
    api::set_current_buf(&buf)?;

    let opts = BufAttachOpts::builder()
        .utf_sizes(false)
        .on_lines(|(_kind, buf, changedtick, first, last, new_last, _old_bytes, _, _)| {
            let msg = format!(
                "buffer {} changed: tick={}, rows {}..{} -> {}",
                buf.handle(),
                changedtick,
                first,
                last,
                new_last
            );

            let _ = api::echo([(msg, None::<&str>)], true, &Default::default());
            false
        })
        .on_detach(|(_kind, buf)| {
            let _ = api::echo(
                [(format!("detached from buffer {}", buf.handle()), None::<&str>)],
                true,
                &Default::default(),
            );
            false
        })
        .build();

    buf.attach(false, &opts)?;
    Ok(())
}
```

We also annotated this pattern in detail during the investigation to show what every part is doing.

## Example pattern for Tokio worker -> Neovim UI update

A common architecture for agent-like plugins looks like this:

1. Neovim command/function triggers ACP work.
2. Main thread enqueues request.
3. Tokio worker talks to ACP agent.
4. Worker sends `PlanFinished` or similar event back.
5. Main thread drains a queue from a libuv timer or scheduled callback.
6. UI is updated on the main thread.

### Why polling with libuv is okay

This polling is cheap because:

- the libuv callback is integrated with Neovim's event loop
- the callback uses nonblocking receive like `try_recv()`
- if nothing is available, it returns immediately

So it does not “block the main thread” in the usual bad sense.

## Testing story in `nvim-oxi`

One of the better parts of `nvim-oxi` is its built-in testing support.

### How tests work

`nvim-oxi` provides a test macro that runs code inside a real Neovim instance.

That means you can write tests that assert real editor behavior rather than just unit-testing pure helper logic.

### Built-in test macro

You can write tests like:

```rust
use nvim_oxi as nvim;
use nvim::api;

#[nvim::test]
fn opens_buffer() -> nvim::Result<()> {
    let before = api::get_current_buf().handle();

    open_watched_buffer()?;

    let buf = api::get_current_buf();
    assert_ne!(buf.handle(), before);

    let lines: Vec<String> = buf
        .get_lines(.., true)?
        .map(|s| s.to_string_lossy().into_owned())
        .collect();

    assert_eq!(lines, vec!["edit me", "more text here"]);
    Ok(())
}
```

### Integration test setup

For integration tests, `nvim-oxi` expects them in a separate `cdylib` crate, with a build script like:

```rust
fn main() -> Result<(), nvim_oxi::tests::BuildError> {
    nvim_oxi::tests::build()
}
```

This is a very solid story for real plugin integration tests.

## Examples of projects using `nvim-oxi`

Examples we found include:

- `saghen/blink.pick`
- `Ruddickmg/hermes.nvim`
- `fusillicode/dotfiles` (`noxi` module)

These show that `nvim-oxi` is not just theoretical, though it is still far from the scale of mainstream Lua plugins.

## Is Rust a good fit specifically for Neovim extensions?

Yes, if your plugin has real internal complexity.

It is especially appealing when the plugin needs:

- state machines
- protocol clients
- background concurrency
- structured data processing
- correctness under asynchronous event handling

For simple UI sugar or small editor tweaks, Lua is still usually the better choice.

For protocol-heavy or architecture-heavy work, Rust becomes compelling.

## Recommendations

### If you are evaluating Rust for a Neovim plugin

Use Rust if:

- the plugin has significant internal logic
- you want typed protocol/state handling
- you are comfortable shipping a compiled artifact
- you are okay with somewhat more complex packaging

Stay in Lua if:

- the plugin is mostly editor glue
- startup simplicity matters more than architecture
- you need maximum ecosystem familiarity

### If you are building something ACP/agent-shaped

A strong architecture would be:

- `nvim-oxi` for Neovim surface and buffer/window integration
- ACP client implemented in Rust
- Tokio for background async work
- libuv or scheduled callbacks to flow results back into the editor

That is likely the cleanest serious Rust-native path for a Neovim agent plugin today.

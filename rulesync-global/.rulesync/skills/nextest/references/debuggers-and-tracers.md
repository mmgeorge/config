# Debuggers and Tracers

*When to read this:* When running a single test under an interactive debugger such as gdb, lldb, or WinDbg, or under a system-call tracer such as strace, dtruss, or truss.

`--debugger "<command>"` (since 0.9.113) and `--tracer "<command>"` (since 0.9.114) run exactly one selected test under the given tool, so the filter must match a single test. Both modes keep nextest's test environment, including setup-script variables, and both disable timeouts, retries, leak detection, output capture, and parallelism. They are intended for local use; nextest may adjust their environment details between releases.

| Behavior | `--debugger` | `--tracer` |
| --- | --- | --- |
| Standard input | Passed through for interactive commands. | Null. |
| Unix process group | Not created, so the debugger controls the terminal. | Created, isolating the test. |
| Unix signal handling | Mostly disabled. | Standard. |
| Interactive `t` key | Disabled. | Enabled. |

nextest splits the argument with Unix shell rules and appends the test command, such as `<tool...> <test-binary> --exact <test-name> --nocapture`. End a debugger command with `--` or `--args` so the debugger does not parse `--exact` as its own option:

```sh
cargo nextest run --debugger "rust-gdb --args" parser::rejects_empty
cargo nextest run --debugger "rust-lldb --" parser::rejects_empty
cargo nextest run --debugger windbgx parser::rejects_empty
cargo nextest run --tracer "strace -f" parser::rejects_empty   # Linux; dtruss -f on macOS, truss -f elsewhere
```

Use `--tracer` rather than `--debugger` for tracers, because the null standard input, standard signal handling, and process group suit non-interactive tracing. Wrap a tool that expects a different argument layout in a small script. Visual Studio Code's CodeLLDB attaches through `codelldb-launch`.

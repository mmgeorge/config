# Reference Profile

*Read when writing complete lookup documentation for an API, a schema, CLI commands, configuration, a protocol, or error codes.*

## Goal

Describe every item in scope accurately—with exact syntax, types, defaults, side effects, and errors—in an order a reader can scan while coding or debugging.

## Sections

Organize the reference the way the interface itself is organized.

### Scope and source

- State the exact version covered.
- Name the source the reference is generated from or checked against, such as an OpenAPI file or a protobuf definition.

### Structure

- Mirror the interface's modules, namespaces, or command groups in the headings.
- Start each large group with a summary table or index.

### Entries

For every function, field, setting, flag, or error code, give:

- Its full name or command syntax.
- Its type and allowed values.
- Whether it is required, its default value, and what replaces it if it is deprecated.
- What it does, including side effects.
- Every error or exit code it can produce.

### Examples

- Give a short, working example for each entry, checked with the same compiler, schema, or tests that produce the reference.

## Leave out

- Do not include tutorials, design history, or arguments. Link the how-to guide, explanation, or RFC.
- Do not skip rarely used items. A reference covers everything in its stated scope.

## Example

One entry from a configuration reference:

> `sync.retry_delay`
>
> The time the sync worker waits before retrying a failed save, in milliseconds. It is optional and defaults to 500. Each retry doubles the delay, up to `sync.max_retry_delay`. A value below 100 fails validation at startup with `config error: sync.retry_delay must be at least 100`.
>
> ```toml
> [sync]
> retry_delay = 1000
> ```

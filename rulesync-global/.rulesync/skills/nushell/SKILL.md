---
name: nushell
description: Use when Codex works with Nushell scripts, modules, one-liners, commands, configuration, nu-check validation, or source-backed Nushell behavior. Reference nushell/nushell and nushell/nushell.github.io through GitHub MCP for current source, command signatures, internals, user-facing docs, cookbook examples, migration guides, and language reference.
---

# Nushell

Use this skill for Nushell scripts, modules, one-liners, commands, configuration, and syntax-sensitive guidance.

## Source Reference

- Use GitHub MCP for `nushell/nushell` when checking source code, builtins, command signatures, or internals.
- Use GitHub MCP for `nushell/nushell.github.io` when checking user-facing docs, cookbook examples, migration guides, or language reference.
- Prefer source-backed answers when syntax, command signatures, parsing behavior, or current behavior matters.

## Script Validation

Before returning any Nushell script:

1. Ensure `nu` is installed.
2. Run `nu -c 'nu-check script.nu'` for standalone scripts.
3. Run `nu -c 'nu-check --as-module module.nu'` for module files.
4. For one-liners, validate with `"your code here" | nu-check`.
5. Fix every parse error before presenting the script.

If validation cannot run, say exactly why and include the command that failed.

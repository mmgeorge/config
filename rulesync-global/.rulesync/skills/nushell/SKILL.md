---
name: nushell
description: >-
  Author, inspect, validate, and debug Nushell scripts, modules, pipelines, commands,
  configuration, and nu-check syntax assertions. Use when working with Nushell code,
  converting shell scripts to Nushell, or validating script syntax.
targets:
  - '*'
---

# Nushell

Use this skill for authoring and validating Nushell scripts, modules, pipelines, commands, and configuration.

## Source Reference

- Consult `nushell/nushell` via GitHub MCP for source implementations, builtin command signatures, and language internals.
- Consult `nushell/nushell.github.io` via GitHub MCP for official documentation, cookbook patterns, and migration guides.
- Rely on verified command signatures and language grammar when resolving parsing ambiguities.

## Script Validation

Validate all Nushell scripts before presenting or executing them:

1. Verify `nu` binary is accessible in the environment.
2. For standalone scripts: run `nu -c 'nu-check path/to/script.nu'`.
3. For module definitions: run `nu -c 'nu-check --as-module path/to/module.nu'`.
4. For pipeline expressions: run `"<pipeline-string>" | nu-check`.
5. Resolve all parse errors identified by `nu-check` before deployment.

If validation is blocked by missing runtime dependencies, report the exact missing command and diagnostic output.

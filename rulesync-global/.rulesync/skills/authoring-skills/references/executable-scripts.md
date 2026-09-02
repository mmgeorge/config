# Executable Scripts in Agent Skills

*When to read this:* When authoring skills that bundle deterministic Python, Shell, or CLI scripts under `scripts/`.

## Purpose of Bundled Scripts

Bundle executable scripts for operations requiring deterministic execution, schema validation, data migration, or repeatable parsing. Pre-tested scripts eliminate generation variance, reduce context token consumption, and enforce invariant validation. Keep scripts structured as single-purpose command-line utilities.

## Execution vs Reading Intent

Explicitly define whether a script is intended for execution or reference inspection:

- **Execute (Standard):** "Execute `analyze_form.py` to extract form field coordinates."
- **Reference Inspection:** "Inspect `analyze_form.py` for the coordinate normalization algorithm."

## Structured Error Handling

Scripts must handle internal exceptions and output actionable diagnostic text rather than terminating with unhandled stack traces:

```python
def process_file(path: str) -> str:
    try:
        with open(path, "r", encoding="utf-8") as file_handle:
            return file_handle.read()
    except FileNotFoundError:
        sys.stderr.write(f"Error: Target file {path} does not exist.\n")
        sys.exit(1)
    except PermissionError:
        sys.stderr.write(f"Error: Insufficient read permissions for {path}.\n")
        sys.exit(1)
```

Ensure validation utilities output the expected field schema and list missing or invalid keys explicitly.

## Justified Configuration Constants

Document the technical justification for operational constants and numeric thresholds:

```python
# HTTP request timeout: allows slow upstream API responses while bounding stalled connections
REQUEST_TIMEOUT_SECONDS = 30

# Maximum retry count: handles transient network blips without unbounded loop execution
MAX_RETRY_ATTEMPTS = 3
```

## Plan-Validate-Execute Sequence

For batch, destructive, or complex multi-file mutations, use an intermediate structured plan:

1. **Analyze:** Parse input state and generate a proposed change manifest (`changes.json`).
2. **Validate:** Execute a validation script against `changes.json` to verify references, schemas, and permissions before mutating state.
3. **Execute:** Apply changes from the validated manifest.
4. **Verify:** Run verification assertions on the resulting state.

## Runtime Dependencies and Environment

- **Document Prerequisites:** Declare required language runtimes and package dependencies in `SKILL.md`.
- **Environment Constraints:** State network and filesystem requirements explicitly.
- **Cross-Platform Compatibility:** Use POSIX path conventions (`scripts/validate.py`) across all scripts and instructions.

# Skills That Bundle Executable Code

*When to read this:* only if the skill ships a `scripts/` directory. Markdown-only skills can
skip this entirely.

## Why bundle a script at all

Even when the agent *could* write the code, a pre-made, tested script is **more reliable**
(no generation variance), **cheaper** (its source never enters context — only its output),
**faster**, and **consistent** across runs. Bundle a script for any deterministic, repeated,
or fragile operation where variation is a bug (validation, packing/unpacking, migrations,
transforms). Do **not** dump general library code in `scripts/` — keep each one a tiny,
single-purpose CLI.

## Make the execution intent explicit

For every script, say whether the agent should **run** it or **read** it — the default is run:

- Run (most common): "Run `analyze_form.py` to extract fields."
- Read as reference (only for complex logic the agent must understand): "See
  `analyze_form.py` for the extraction algorithm."

Running keeps the code out of context; reading pulls it in. Pick deliberately.

## Solve, don't punt

A script must handle its own error conditions, not fail and leave the agent to improvise.
Return a useful message and a sane fallback:

```python
def process_file(path):
    try:
        with open(path) as f:
            return f.read()
    except FileNotFoundError:
        print(f"File {path} not found, creating default")
        with open(path, "w") as f:
            f.write("")
        return ""
    except PermissionError:
        print(f"Cannot access {path}, using default")
        return ""
```

Make validators **verbose and specific** so the agent can fix the input:
`"Field 'signature_date' not found. Available: customer_name, order_total, signature_date_signed"`
beats `"invalid field"`.

## No voodoo constants

Every magic value must justify itself (Ousterhout's law: if you don't know the right value,
how will the agent?):

```python
# HTTP requests usually finish within 30s; longer covers slow links
REQUEST_TIMEOUT = 30
# 3 retries balances reliability vs latency; most blips clear by retry 2
MAX_RETRIES = 3
```

Not `TIMEOUT = 47  # why 47?`.

## Plan → validate → execute (verifiable intermediate outputs)

For batch, destructive, or high-stakes operations, don't let the agent act in one shot. Have
it write a structured **plan file**, validate the plan with a script, *then* execute:

```
analyze → write changes.json → validate changes.json → apply → verify
```

Validation catches bad references and conflicts before anything is written, the plan is
reversible/iterable without touching originals, and errors point to a specific line. Use this
for: batch edits, destructive changes, complex validation rules, high-stakes runs.

## Dependencies and environment

- **List required packages** in the SKILL.md and show the install before use; never assume a
  binary is on PATH. To check a tool, prefer a no-spawn check (e.g. an `executable`/`which`
  API your runtime provides) over shelling out.
- **Know the runtime limits.** Some execution environments have **no network and no runtime
  install** (e.g. the Claude API code tool); others can install from npm/PyPI. State your
  skill's requirement so it isn't run where it can't work.
- **Forward-slash paths only** in scripts and instructions (`scripts/x.py`), even on Windows.

For the validate→fix→repeat loop pattern in prose, see `references/writing-and-content.md`. Ship gate:
`references/checklist-and-anti-patterns.md`.

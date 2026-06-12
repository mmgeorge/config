---
name: walkthrough
description: Generate .walkthrough.json at the git repo root — a guided, ordered review walkthrough of the current changes (staged + unstaged vs HEAD, plus untracked files) for the DiffReview Neovim plugin. Use after finishing a multi-file change, or when asked to create a review walkthrough from the current diff.
---

# Generate a Review Walkthrough

Produce `.walkthrough.json` at the repository root so a reviewer can step
through the current changes in order inside the DiffReview Neovim plugin.
Each step points at a region of a changed file and explains it; the plugin
jumps the reviewer from step to step.

## 1. Gather context

Run from the repository:

- `git rev-parse --show-toplevel` — the directory where `.walkthrough.json`
  must be written.
- `git rev-parse HEAD` — the full 40-character sha for the `commit` field.
- `git diff HEAD` — one view covering both staged and unstaged changes.
- `git ls-files --others --exclude-standard` — untracked files are part of
  the change set too; read the relevant ones.

If you just finished making the changes yourself, you already know the
narrative — use the diff only to confirm files and line numbers.

## 2. Author the steps

- **Order tells the story.** Sequence steps as a narrative: entry point or
  API surface first, then core logic, then call sites, then tests/config.
  Not file order, not diff order.
- **One logical idea per step.** A step covers one cohesive region; aim for
  3-15 steps total. Skip mechanical churn (renames, lockfiles, formatting)
  unless it is the point of the change.
- **Positions describe the file as it exists on disk right now** (the NEW,
  post-change content): 1-based `line` and `col`, `start` to `end`
  inclusive, `end` >= `start`.
- **Keep the region tight: only the changed lines being discussed, roughly
  40 lines at most.** Do not stretch a range over a whole table/function to
  cover a two-line edit — the region is highlighted during review, and a
  sprawling highlight buries the point. When one logical change is two small
  edits far apart, prefer two steps over one loose range.
- **`start.line` must be a changed (added) line whenever possible.** The
  reviewer is jumped to that exact line inside the diff; anchoring on an
  unchanged context line lands the jump approximately and gets flagged as
  stale.
- `file` is repo-relative with forward slashes.
- `comment` explains **why** — what the region does and why it changed,
  1-4 sentences. The reviewer can already see the code, and the reviewer UI
  displays the step's file name, so do not repeat the file path in the
  comment.
- `title` is an optional short heading (a few words).
- `summary` is shown before the first step. Open with a 1-3 sentence prose
  overview, then include an **ASCII code-flow diagram** of the change from
  the relevant entry point (not the entire system): one node per
  function/module with its file path in brackets, `*` marking modified/new
  nodes, `~` marking removed ones, and data flow (input/output types) where
  it clarifies. Keep diagram lines under 76 characters - the reviewer UI
  preserves preformatted lines verbatim. Example shape:

  ```
  API Request (POST /documents)
    │ (DocumentRequest)
    ├── validate_request() → ValidatedRequest   [api/handlers.rs]
    ├── *process_document() → StoredDocument    [processing/mod.rs]
    │     ├── ~save_and_respond() → Response    [processing/mod.rs]
    │     └── *save_result() → StoredDocument   [processing/store.rs]
    └── *notify() → NotifyResult                [notifications/mod.rs] (NEW)
  ```

## 3. Schema

The document must validate against `walkthrough.schema.json` (shipped
alongside this skill). The schema, inlined:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "diff-review-walkthrough.schema.json",
  "title": "DiffReview Walkthrough",
  "type": "object",
  "required": ["version", "summary", "commit", "steps"],
  "additionalProperties": false,
  "properties": {
    "version": { "const": 1 },
    "summary": { "type": "string", "minLength": 1 },
    "commit": { "type": "string", "pattern": "^[0-9a-fA-F]{40}$" },
    "steps": {
      "type": "array",
      "minItems": 1,
      "items": { "$ref": "#/$defs/step" }
    }
  },
  "$defs": {
    "position": {
      "type": "object",
      "required": ["line", "col"],
      "additionalProperties": false,
      "properties": {
        "line": { "type": "integer", "minimum": 1 },
        "col": { "type": "integer", "minimum": 1 }
      }
    },
    "step": {
      "type": "object",
      "required": ["file", "start", "end", "comment"],
      "additionalProperties": false,
      "properties": {
        "file": { "type": "string", "minLength": 1 },
        "start": { "$ref": "#/$defs/position" },
        "end": { "$ref": "#/$defs/position" },
        "comment": { "type": "string", "minLength": 1 },
        "title": { "type": "string" }
      }
    }
  }
}
```

## 4. Example

```json
{
  "version": 1,
  "summary": "Adds rate limiting to the public API. A new token-bucket middleware guards every /api route; configuration lives in config/rate_limit.toml and the middleware is exercised by new integration tests.\n\nRequest (POST /api/*)\n  ├── *rate_limit() → Decision        [src/middleware/rate_limit.rs] (NEW)\n  │     └── *TokenBucket::take()      [src/middleware/rate_limit.rs]\n  └── handle() → Response             [src/router.rs]",
  "commit": "8f14e45fceea167a5a36dedd4bea2543c6a04c33",
  "steps": [
    {
      "title": "Token bucket middleware",
      "file": "src/middleware/rate_limit.rs",
      "start": { "line": 18, "col": 1 },
      "end": { "line": 41, "col": 2 },
      "comment": "The core token-bucket implementation. Buckets are keyed by client IP and refill lazily on access, so there is no background task; the trade-off is a small burst allowance after idle periods."
    },
    {
      "title": "Wire into the router",
      "file": "src/router.rs",
      "start": { "line": 41, "col": 5 },
      "end": { "line": 44, "col": 60 },
      "comment": "The middleware is layered onto the /api scope only — health checks and static assets stay unthrottled."
    }
  ]
}
```

## 5. Self-check before writing

- Every `step.file` appears in `git diff HEAD` or the untracked list.
- Every `start.line`/`end.line` is within the file's current line count
  (verify against the file on disk, not the diff hunk headers).
- Every `start.line` points at a changed (added) line, not unchanged
  context; every region is tight (~40 lines max) around the discussed code.
- `commit` is the full 40-character sha from `git rev-parse HEAD`.
- The JSON is valid: no trailing commas, no comments, double-quoted keys.
- Write to `<repo root>/.walkthrough.json`, overwriting any existing file.
- If a JSON Schema validator is available, run it against the schema shipped
  alongside this skill and fix any reported violation, e.g.:
  - `check-jsonschema --schemafile <skill dir>/walkthrough.schema.json .walkthrough.json`
  - `npx ajv-cli validate --spec=draft2020 -s <skill dir>/walkthrough.schema.json -d .walkthrough.json`

  Skip silently when neither tool is installed - do not install one.

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
- Author the top-down review map as structured `overview`, `root`, and
  `parts[]`; the DiffReview plugin derives the displayed summary graph from
  this structure. Do not write a `summary` field.
- `overview` is 1-3 prose sentences explaining what changed and why.
- `root` is one active author-context sentence for the major feature/fix/refactor.
- Each `parts[].title` is an active author-context sentence such as `Add...`,
  `Update...`, `Move...`, `Split...`, `Remove...`, or `Preserve...`. These
  titles become the top-level graph children and the `Major changes:` roots.
- Keep `parts[]` focused on the true major review claims, usually 2-4 entries.
  Do not promote every meaningful diff cluster into a part. Supporting work
  like demo wiring, shader helper splits, tests, docs/plans, preservation of an
  old path, or follow-up notes belongs under the relevant major part unless it
  is the main purpose of the change.
- Inside each part, prefer real repo boundaries for group titles: top-level
  modules, plugins, packages, crates, apps, or stable subsystems. In plugin
  repos, use plugin names such as `Physics plugin`, `Render plugin`, or
  `Model plugin` before abstract buckets like `API`, `storage`, or `renderer`.
  Use abstract group labels only when there is no clearer module boundary.
  Each item has:
  - `marker`: `+` new, `*` modified, `~` removed/split, `>` ownership moved.
  - `title`: the important module/function/concept in the graph.
  - `note`: a short annotation explaining why it changed. Notes should be
    descriptive fragments that render cleanly under the item; do not include
    line breaks, tree characters, file paths, or imperative broken phrases.
  - `steps`: one or more exact file/range walkthrough comments for that item.
- Exact file paths and ranges belong only in nested `steps`; do **not** include
  repo paths or bracketed file names in `overview`, `root`, part titles, group
  titles, item titles, or item notes. The plugin displays nested steps as
  `Part X.Y`, where `X` is the part and `Y` is the step within that part.
  Derived summary example:

  ```
  Adds model-backed particle rendering while preserving billboard particles.
  The PR separates storage/render ownership so physics, render, and model
  plugins communicate through a narrow instance-source handoff.

  Add model-backed particle rendering for live particles.
  ├── Update particle spawns to choose between billboard and model rendering.
  ├── Move particle GPU buffers behind a render-facing handoff.
  └── Update model renderer to draw live particles through the PBR path.

  Major changes:

  1. Update particle spawns to choose between billboard and model rendering.
     ├── Physics plugin
     │   ├── +ParticleRenderMode
     │   │   chooses Billboard vs Model(asset) at spawn time
     │   ├── *ParticleSystem::run()
     │   │   simulates particles, then publishes render instances
     │   └── +ParticleBillboardRenderSystem::run()
     │       preserves the legacy triangle path as a separate system
     └── Demo/app wiring
         └── *drop3d particle spawn
             opts into Model(SmoothSphere) to exercise PBR particles

  2. Move particle GPU buffers behind a render-facing handoff.
     ├── Physics plugin
     │   └── >ParticleStorage
     │       moves GPU buffers out of the solver into shared storage
     ├── Render plugin
     │   └── +ParticleInstanceSources
     │       owns the render-facing handoff for live particle buffers
     └── Model plugin
         └── *ModelRenderSystem::render()
             reads the handoff before issuing particle model draws

  3. Update model renderer to draw live particles through the PBR path.
     ├── Model plugin
     │   ├── +ParticlePbRenderPipeline
     │   │   binds particle storage into the model render pass
     │   ├── *model primitive draw commands
     │   │   reuse existing primitive/material stores for particles
     │   └── *ModelRenderSystem::render()
     │       draws each primitive across the live particle count
     └── Shader path
         ├── *pbr_vertex helpers
         │   share vertex-output construction with normal model rendering
         └── +particle_render.entry.slang
             turns particle positions/radii into PBR mesh instances

  Legend: + new, * modified, ~ removed/split, > ownership moved
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
  "required": ["version", "overview", "root", "commit", "parts"],
  "additionalProperties": false,
  "properties": {
    "version": { "const": 2 },
    "overview": { "type": "string", "minLength": 1 },
    "root": { "type": "string", "minLength": 1 },
    "commit": { "type": "string", "pattern": "^[0-9a-fA-F]{40}$" },
    "parts": {
      "type": "array",
      "minItems": 1,
      "items": { "$ref": "#/$defs/part" }
    }
  },
  "$defs": {
    "part": {
      "type": "object",
      "required": ["title", "groups"],
      "additionalProperties": false,
      "properties": {
        "title": { "type": "string", "minLength": 1 },
        "groups": {
          "type": "array",
          "minItems": 1,
          "items": { "$ref": "#/$defs/group" }
        }
      }
    },
    "group": {
      "type": "object",
      "required": ["title", "items"],
      "additionalProperties": false,
      "properties": {
        "title": { "type": "string", "minLength": 1 },
        "items": {
          "type": "array",
          "minItems": 1,
          "items": { "$ref": "#/$defs/item" }
        }
      }
    },
    "item": {
      "type": "object",
      "required": ["marker", "title", "note", "steps"],
      "additionalProperties": false,
      "properties": {
        "marker": { "enum": ["+", "*", "~", ">"] },
        "title": { "type": "string", "minLength": 1 },
        "note": { "type": "string", "minLength": 1 },
        "steps": {
          "type": "array",
          "minItems": 1,
          "items": { "$ref": "#/$defs/step" }
        }
      }
    },
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
  "version": 2,
  "overview": "Adds rate limiting to the public API while keeping health checks and static assets unthrottled. The change introduces a middleware boundary around API routes, stores token buckets per client, and adds configuration plus integration coverage.",
  "root": "Add token-bucket rate limiting to API requests.",
  "commit": "8f14e45fceea167a5a36dedd4bea2543c6a04c33",
  "parts": [
    {
      "title": "Add middleware that decides whether a request can continue.",
      "groups": [
        {
          "title": "API module",
          "items": [
            {
              "marker": "*",
              "title": "api route stack",
              "note": "wraps /api requests without throttling health/static routes",
              "steps": [
                {
                  "title": "Wire into the router",
                  "file": "src/router.rs",
                  "start": { "line": 41, "col": 5 },
                  "end": { "line": 44, "col": 60 },
                  "comment": "The middleware is layered onto the /api scope only; health checks and static assets stay unthrottled."
                }
              ]
            }
          ]
        },
        {
          "title": "Rate-limit module",
          "items": [
            {
              "marker": "+",
              "title": "rate_limit()",
              "note": "returns allow/deny decisions before request handling",
              "steps": [
                {
                  "title": "Token bucket middleware",
                  "file": "src/middleware/rate_limit.rs",
                  "start": { "line": 18, "col": 1 },
                  "end": { "line": 41, "col": 2 },
                  "comment": "The middleware keys buckets by client IP and refills lazily on access, so there is no background task; the trade-off is a small burst allowance after idle periods."
                }
              ]
            }
          ]
        }
      ]
    },
    {
      "title": "Add per-client token buckets with lazy refill.",
      "groups": [
        {
          "title": "Rate-limit module",
          "items": [
            {
              "marker": "+",
              "title": "TokenBucket::take()",
              "note": "refills on access and consumes one token per accepted request",
              "steps": [
                {
                  "file": "src/middleware/rate_limit.rs",
                  "start": { "line": 52, "col": 3 },
                  "end": { "line": 73, "col": 4 },
                  "comment": "The bucket implementation combines lazy refill and consumption in one operation, which keeps request-time behavior deterministic and easy to test."
                }
              ]
            }
          ]
        }
      ]
    }
  ]
}
```

## 5. Self-check before writing

- Every nested `steps[].file` appears in `git diff HEAD` or the untracked list.
- Every nested `start.line`/`end.line` is within the file's current line count
  (verify against the file on disk, not the diff hunk headers).
- Every nested `start.line` points at a changed (added) line, not unchanged
  context; every region is tight (~40 lines max) around the discussed code.
- The document has no `summary` field; DiffReview derives the summary from
  `overview`, `root`, and `parts`.
- `overview`, `root`, part titles, group titles, item titles, and item notes
  have no repo-relative paths or bracketed file names.
- `root` and each `parts[].title` are active author-context sentences, not
  noun-only labels.
- `parts[]` captures the few major top-down review claims, not every support
  change. If there are more than four parts, merge by feature/fix/refactor
  outcome before writing the JSON.
- Group titles identify real module/plugin/package/app boundaries whenever
  possible. Do not use abstract labels such as `API`, `storage`, `renderer`, or
  `validation` when the repo has a clearer boundary such as `Physics plugin`,
  `Model plugin`, `Render plugin`, or a named crate/package.
- Item notes are single-line descriptive fragments that render cleanly beneath
  the item; no tree characters, file paths, or broken imperative phrases.
- `parts[].groups[].items[]` explain the most important top-down design changes;
  they are not exhaustive inventories of changed files/functions.
- `commit` is the full 40-character sha from `git rev-parse HEAD`.
- The JSON is valid: no trailing commas, no comments, double-quoted keys.
- Write to `<repo root>/.walkthrough.json`, overwriting any existing file.
- If a JSON Schema validator is available, run it against the schema shipped
  alongside this skill and fix any reported violation, e.g.:
  - `check-jsonschema --schemafile <skill dir>/walkthrough.schema.json .walkthrough.json`
  - `npx ajv-cli validate --spec=draft2020 -s <skill dir>/walkthrough.schema.json -d .walkthrough.json`

  Skip silently when neither tool is installed - do not install one.

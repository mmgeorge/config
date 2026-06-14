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
- `summary` is shown before the first step. It is the reviewer's top-down map
  of the change:
  1. Open with a 1-3 sentence prose overview explaining what changed and why.
  2. Add a compact top-level graph rooted at the major feature/fix/refactor.
     Each child is an active author-context sentence such as `Add...`,
     `Update...`, `Move...`, `Split...`, `Remove...`, or `Preserve...`.
  3. Add `Major changes:` and expand each child sentence into a small graph.
     Group nodes by top-level subsystem/module when that best explains the
     design, and add short inline annotations that say why the important nodes
     changed.
  4. End with `Legend: + new, * modified, ~ removed/split, > ownership moved`.
  The walkthrough steps already carry exact file and range targets, so do
  **not** include repo paths or bracketed file names in the summary. Keep graph
  lines under 76 characters - the reviewer UI preserves preformatted lines
  verbatim. Example shape:

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
  "summary": "Adds rate limiting to the public API while keeping health checks and static assets unthrottled. The change introduces a middleware boundary around API routes, stores token buckets per client, and adds configuration plus integration coverage.\n\nAdd token-bucket rate limiting to API requests.\n├── Add middleware that decides whether a request can continue.\n├── Add per-client token buckets with lazy refill.\n└── Wire rate-limit configuration into the API route stack.\n\nMajor changes:\n\n1. Add middleware that decides whether a request can continue.\n   ├── HTTP routing\n   │   └── *api route stack\n   │       wraps /api requests without throttling health/static routes\n   └── Middleware\n       └── +rate_limit()\n           returns allow/deny decisions before request handling\n\n2. Add per-client token buckets with lazy refill.\n   └── Rate-limit core\n       └── +TokenBucket::take()\n           refills on access and consumes one token per accepted request\n\n3. Wire rate-limit configuration into the API route stack.\n   ├── Configuration\n   │   └── +RateLimitConfig\n   │       controls bucket size and refill rate\n   └── Integration tests\n       └── +API throttling workflow\n           covers accepted bursts, throttled overflow, and refill behavior\n\nLegend: + new, * modified, ~ removed/split, > ownership moved",
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
- `summary` has no repo-relative paths or bracketed file names; exact locations
  belong in `steps`.
- `summary` starts with prose, then a top-level graph, then `Major changes:`
  with expanded graphs, then the legend at the end.
- Major-change roots are active author-context sentences, not noun-only labels.
- The expanded graphs are not exhaustive inventories of changed files/functions;
  they explain the most important top-down design changes.
- `commit` is the full 40-character sha from `git rev-parse HEAD`.
- The JSON is valid: no trailing commas, no comments, double-quoted keys.
- Write to `<repo root>/.walkthrough.json`, overwriting any existing file.
- If a JSON Schema validator is available, run it against the schema shipped
  alongside this skill and fix any reported violation, e.g.:
  - `check-jsonschema --schemafile <skill dir>/walkthrough.schema.json .walkthrough.json`
  - `npx ajv-cli validate --spec=draft2020 -s <skill dir>/walkthrough.schema.json -d .walkthrough.json`

  Skip silently when neither tool is installed - do not install one.

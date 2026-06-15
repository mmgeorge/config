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
  `parts[]`; the DiffReview plugin derives the displayed walkthrough summary
  from this structure. Do not write a `summary` field.
- `overview` is 2-3 prose sentences that tell the story. Start with one
  active sentence that succinctly encapsulates the whole change, such as `Add
  support for full model-backed particle rendering.` Then use `Before...` and
  `Now...` sentences to explain what changed in concrete terms.
- `root` is one active author-context sentence for the major feature/fix/refactor.
- Each `parts[].title` is an active author-context sentence such as `Add...`,
  `Update...`, `Move...`, `Split...`, `Remove...`, or `Preserve...`. These
  titles become the numbered review sections in the walkthrough summary.
- Keep `parts[]` focused on the true major review claims, usually 2-4 entries.
  Do not promote every meaningful diff cluster into a part. Supporting work
  like demo wiring, shader helper splits, tests, docs/plans, preservation of an
  old path, or follow-up notes belongs under the relevant major part unless it
  is the main purpose of the change.
- Inside each part, prefer exact code boundaries for group titles: concrete
  class/type/module/crate/package/plugin symbols such as `PhysicsPlugin`,
  `RenderPlugin`, `ModelPlugin`, `rate_limit`, or `AppBrowserPhysicsDemo`.
  Use humanized labels like `Physics plugin`, `Render plugin`, or abstract
  buckets like `API`, `storage`, `renderer`, and `validation` only when there is
  no clearer owning symbol/module in the code.
  Each item has:
  - `action`: one of `Add`, `Update`, `Move`, `Remove`, or `Split`.
  - `title`: a concrete story claim or code symbol in the graph.
  - `note`: a short imperative annotation that reads as part of the author's
    change, e.g. `bind particle storage during model-backed particle draws` or
    `reuse primitive and material uploads for particle instances`. Do not use
    third-person wording like `binds` or `reuses`.
  - `steps`: one or more exact file/range walkthrough comments for that item.
  - `children`: optional nested subchanges under a story item.
- Use `children` when a where-boundary contains a design move plus concrete
  implementation symbols. The parent item should say the responsibility shift,
  and child items should name the concrete symbols/functions that implement it.
  Parent items may omit `steps` when their children carry the exact file ranges.
  Do not flatten related subchanges into siblings when the relationship matters.
- Exact file paths and ranges belong only in nested `steps`; do **not** include
  repo paths or bracketed file names in `overview`, `root`, part titles, group
  titles, item titles, or item notes. The plugin displays nested steps as
  `Part X.Y`, where `X` is the part and `Y` is the step within that part.
  Derived summary example:

  ```
  Add support for full model-backed particle rendering. Before, simulated
  particles could only render through the legacy triangle billboard path. Now,
  each particle spawn can choose billboard or model rendering, physics publishes
  live particle buffers through a render-facing handoff, and the model/PBR
  renderer draws the selected mesh once per live particle.

  1. Let particle spawns choose billboard or model rendering.
     ├── PhysicsPlugin
     │   └── Add    selectable render mode for particle spawns
     │       │     make the render path explicit instead of assuming billboard particles
     │       ├── Add    ParticleRenderMode
     │       │     represent billboard vs model-backed rendering at spawn time
     │       └── Update ParticleSpawn
     │             carry the selected render mode with spawn data
     └── AppBrowserPhysicsDemo
         └── Update drop3d particle spawn
               switch the stress scene to SmoothSphere model particles

  2. Move live particle data into render-facing resources.
     ├── PhysicsPlugin
     │   ├── Move   particle simulation buffers into ParticleStorage
     │   │   │     make ParticleStorage the shared buffer owner for simulation and rendering
     │   │   ├── Add    ParticleStorage
     │   │   │     own particle GPU buffers and expose a render instance view
     │   │   └── Update ParticleSystem::run()
     │   │         advance simulation through shared storage and publish the current particle source
     │   ├── Update PhysicsPlugin::install()
     │   │     register shared storage and preserve billboard rendering as its own system
     │   └── Add    ParticleBillboardRenderSystem
     │         keep the legacy triangle path behind billboard mode
     └── RenderPlugin
         └── Add    ParticleInstanceSources
               share live particle buffers and optional model metadata with render systems

  3. Draw the selected model once per live particle through the PBR path.
     ├── ModelPlugin
     │   ├── Add    particle PBR render pipeline
     │   │   │     bind particle storage during the model render pass
     │   │   └── Add    ParticlePbRenderPipeline
     │   │         create the pipeline variant for particle-instanced model draws
     │   ├── Update model renderer to issue particle model draws
     │   │   │     load the selected model asset and draw each primitive across the live particle count
     │   │   ├── Update ModelRenderSystem::new()
     │   │   │     initialize the particle pipeline state
     │   │   ├── Update ModelRenderSystem::update_entities()
     │   │   │     prepare particle model draw commands from the render-facing handoff
     │   │   └── Update ModelRenderSystem::render()
     │   │         issue indexed draws across the live particle count
     │   └── Update model stores to reuse primitive and material uploads
     │       │     record per-primitive draw metadata for particle-instanced models
     │       ├── Add    ParticleModelDrawCommand
     │       │     describe one primitive/material draw for live particle instances
     │       ├── Update ModelStore::insert_particle_model()
     │       │     reuse model primitives while returning particle draw metadata
     │       └── Update PrimitiveMeshStore
     │             expose primitive buffers in the layout expected by the particle entry point
     └── particle_render shaders
         └── Add    particle PBR shader entry
             │     expand particle positions and radii into lit mesh instances
             ├── Add    particle_render entry point
             │     draw each live particle as the selected model primitive
             └── Add    PBR vertex helpers
                   share vertex-output construction with normal model rendering
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
    "version": { "const": 4 },
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
      "required": ["action", "title", "note"],
      "additionalProperties": false,
      "anyOf": [
        { "required": ["steps"] },
        { "required": ["children"] }
      ],
      "properties": {
        "action": { "enum": ["Add", "Update", "Move", "Remove", "Split"] },
        "title": { "type": "string", "minLength": 1 },
        "note": { "type": "string", "minLength": 1 },
        "steps": {
          "type": "array",
          "minItems": 1,
          "items": { "$ref": "#/$defs/step" }
        },
        "children": {
          "type": "array",
          "minItems": 1,
          "items": { "$ref": "#/$defs/item" }
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
  "version": 4,
  "overview": "Add token-bucket rate limiting to public API requests. Before, API handlers accepted every request that reached the route stack. Now, API routes pass through a middleware boundary that checks per-client token buckets while health checks and static assets stay unthrottled.",
  "root": "Add token-bucket rate limiting to API requests.",
  "commit": "8f14e45fceea167a5a36dedd4bea2543c6a04c33",
  "parts": [
    {
      "title": "Add middleware that decides whether a request can continue.",
      "groups": [
        {
          "title": "Router",
          "items": [
            {
              "action": "Update",
              "title": "API route middleware boundary",
              "note": "route public API requests through the limiter while leaving health/static paths alone",
              "children": [
                {
                  "action": "Update",
                  "title": "api route stack",
                  "note": "wrap /api requests without throttling health/static routes",
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
            }
          ]
        },
        {
          "title": "rate_limit",
          "items": [
            {
              "action": "Add",
              "title": "rate_limit()",
              "note": "return allow/deny decisions before request handling",
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
          "title": "rate_limit",
          "items": [
            {
              "action": "Add",
              "title": "TokenBucket::take()",
              "note": "refill on access and consume one token per accepted request",
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
- Group titles identify concrete code boundaries whenever possible: class/type
  names, module names, plugin structs, crates, packages, or apps. Prefer
  `PhysicsPlugin`, `RenderPlugin`, `ModelPlugin`, `Router`, or `rate_limit` over
  humanized labels like `Physics plugin` or abstract labels such as `API`,
  `storage`, `renderer`, and `validation`.
- Nested item structure preserves the story inside each where-boundary: parent
  items describe responsibility shifts or design moves; child items name the
  concrete symbols/functions. Parent items may omit `steps` only when they have
  `children`.
- Item actions are title-cased verbs from the allowed set: `Add`, `Update`,
  `Move`, `Remove`, `Split`.
- Item notes are single-line imperative fragments that render cleanly beneath
  the item; no third-person verbs (`binds`, `reuses`), tree characters, file
  paths, or broken phrases.
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

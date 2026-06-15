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
  `tasks[]`; the DiffReview plugin derives the displayed walkthrough summary
  from this structure. Do not write a `summary` field.
- `overview` is 2-3 prose sentences that tell the story. Start with one
  active sentence that succinctly encapsulates the whole change, such as `Add
  support for full model-backed particle rendering.` Then use `Before...` and
  `Now...` sentences to explain what changed in concrete terms.
- `root` is one active author-context sentence for the major feature/fix/refactor.
- Each `tasks[].title` is an active author-context sentence such as `Add...`,
  `Update...`, `Move...`, `Split...`, `Remove...`, or `Preserve...`. These
  titles become the numbered review sections in the walkthrough summary.
- `tasks[].justification` is optional in the schema but generally expected.
  Include one unless the task title already makes the reason very obvious. Omit
  it for very obvious/mechanical tasks or when it would only restate the title.
  Keep it 80 characters or less. It renders directly under the task title as
  yellow italic text, without a `why:` label.
- Keep `tasks[]` focused on the true major review claims, usually 2-4 entries.
  Do not promote every meaningful diff cluster into a task. Supporting work
  like demo wiring, shader helper splits, tests, docs/plans, preservation of an
  old path, or follow-up notes belongs under the relevant major task unless it
  is the main purpose of the change.
- Inside each task, prefer exact code boundaries for group titles: concrete
  module, file, package, directory, crate, or app boundaries such as
  `PhysicsPlugin`, `RenderPlugin`, `ModelPlugin`, `rate_limit`, or
  `AppBrowserPhysicsDemo`.
  Use humanized labels like `Physics plugin`, `Render plugin`, or abstract
  buckets like `API`, `storage`, `renderer`, and `validation` only when there is
  no clearer owning symbol/module in the code.
  Each group has:
  - `type`: one of `Module`, `File`, `Package`, or `Directory`. Group types are
    restricted to container-level programming constructs; do not use `Class`,
    `Struct`, `Enum`, `Trait`, `Interface`, `Type`, `Test`, `Config`, `Method`,
    `Function`, `Field`, or `Constant` at the group level.
  - `title`: the concrete owning code boundary.
- Each group has one or more `subtasks[]`. A subtask is the missing intent layer
  between a container and concrete code items:
  - `title`: an active prose sentence or fragment describing what this group
    does in service of the parent task, such as `Publish live particle buffers
    for render systems.` Do not attach a type or action to a subtask.
  - `justification`: optional and exceptional. Use it only when the subtask has
    non-obvious rationale, tradeoff, or sequencing context that the title cannot
    carry. Otherwise omit it. Keep it 80 characters or less. It renders directly
    under the subtask title as yellow italic text, without a `why:` label.
  - `items`: one or more typed code items that implement the subtask.
  Each item has:
  - `action`: one of `Add`, `Update`, `Move`, `Remove`, or `Split`.
    These render in the summary as `Add`, `Modify`, `Move`, `Remove`, and
    `Split` respectively. Use `Update` in JSON for changes that should display
    as `Modify`.
  - `type`: one of `Class`, `Struct`, `Enum`, `Trait`, `Interface`, `Test`,
    `Config`, `Function`, `Method`, `Constant`, or `Field`.
    Summary rows render types as a lowercase keyword when no `subtype` is set;
    `Function` and `Method` both render as `fn`.
  - `subtype`: optional narrower display label such as `Resource`, `Buffer`,
    `Pipeline`, or `Store`. Use it only when the item's base class, trait,
    interface, framework role, or semantic kind can be narrowed down from the
    code. When present, the summary displays `subtype` instead of the base type
    keyword.
  - `title`: a concrete story claim or code symbol in the graph.
  - `note`: a short imperative annotation, 50 characters or less, that renders
    after `to` and reads as part of the author's change, e.g.
    `bind particle storage during model draws` or
    `reuse primitive and material uploads`. Do not use third-person wording
    like `binds` or `reuses`.
  - `steps`: one or more exact file/range walkthrough comments for that item.
- Items are leaf nodes. Do not use `children`, and do not create action items as
  grouping rows. Put grouping/intent in the subtask title; reserve subtask
  justification for exceptional non-obvious context. Put concrete implementation
  symbols as sibling items under that subtask. Every item must have `steps`.
- Exact file paths and ranges belong only in nested `steps`; do **not** include
  repo paths or bracketed file names in `overview`, `root`, task titles, group
  titles, subtask titles, item titles, or item notes. The plugin displays
  nested steps as `Task X.Y`, where `X` is the task and `Y` is the step within
  that task.
  Derived summary example:

  ```
  Add support for full model-backed particle rendering. Before, simulated
  particles could only render through the legacy triangle billboard path. Now,
  each particle spawn can choose billboard or model rendering, physics publishes
  live particle buffers through a render-facing handoff, and the model/PBR
  renderer draws the selected mesh once per live particle.

  1. Let particle spawns choose billboard or model rendering.
   Spawn authors need a stable way to select the rendering path before simulation hands particles to rendering.
   module PhysicsPlugin
      └─ Make particle render paths explicit at spawn time.
         ├─ Add enum ParticleRenderMode to represent billboard vs model-backed rendering at spawn time
         └─ Modify struct ParticleSpawn to carry the selected render mode with spawn data
   module AppBrowserPhysicsDemo
      └─ Exercise the model-backed path in the drop3d scene.
         └─ Modify fn drop3d particle spawn to switch the stress scene to SmoothSphere model particles

  2. Move live particle data into render-facing resources.
   module PhysicsPlugin
      ├─ Own shared particle buffers outside the simulation loop.
      │  ├─ Add Resource ParticleStorage to own particle GPU buffers and expose a render instance view
      │  └─ Modify fn ParticleSystem::run() to advance simulation through shared storage and publish the current particle source
      └─ Keep billboard rendering as the legacy render mode.
         ├─ Modify fn PhysicsPlugin::install() to register shared storage and preserve billboard rendering as its own system
         └─ Add struct ParticleBillboardRenderSystem to keep the legacy triangle path behind billboard mode
   module RenderPlugin
      └─ Publish particle instance sources for render systems.
         └─ Add Resource ParticleInstanceSources to share live particle buffers and optional model metadata with render systems

  3. Draw the selected model once per live particle through the PBR path.
   module ModelPlugin
      ├─ Bind particle storage during model render passes.
      │  └─ Add Pipeline ParticlePbRenderPipeline to create the pipeline variant for particle-instanced model draws
      └─ Issue PBR draws across the live particle count.
         ├─ Modify fn ModelRenderSystem::new() to initialize the particle pipeline state
         ├─ Modify fn ModelRenderSystem::update_entities() to prepare particle model draw commands from the render-facing handoff
         └─ Modify fn ModelRenderSystem::render() to issue indexed draws across the live particle count
   module ModelStore
      └─ Reuse primitive and material uploads for particle instances.
         ├─ Add Command ParticleModelDrawCommand to describe one primitive/material draw for live particle instances
         ├─ Modify fn ModelStore::insert_particle_model() to reuse model primitives while returning particle draw metadata
         └─ Modify Store PrimitiveMeshStore to expose primitive buffers in the layout expected by the particle entry point
   file particle_render shaders
      └─ Share PBR vertex and fragment behavior with particle instances.
         ├─ Add fn particle_render entry point to draw each live particle as the selected model primitive
         ├─ Add fn PBR vertex helpers to share vertex-output construction with normal model rendering
         └─ Add fn PBR fragment helper to share material sampling and lighting
  ```

## 3. Schema

The document must validate against `walkthrough.schema.json` (shipped
alongside this skill). The schema, inlined:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "diff-review-walkthrough.schema.json",
  "title": "DiffReview Walkthrough",
  "description": "Guided review walkthrough of a set of changes, consumed by the DiffReview Neovim plugin. Lives at the repository root as .walkthrough.json.",
  "type": "object",
  "required": ["version", "overview", "root", "commit", "tasks"],
  "additionalProperties": false,
  "properties": {
    "version": {
      "const": 7
    },
    "overview": {
      "type": "string",
      "minLength": 1,
      "description": "Shown before the graph: 1-3 sentences explaining what changed and why."
    },
    "root": {
      "type": "string",
      "minLength": 1,
      "description": "Active author-context sentence for the top-level feature, fix, or refactor."
    },
    "commit": {
      "type": "string",
      "pattern": "^[0-9a-fA-F]{40}$",
      "description": "Full sha of HEAD at generation time (git rev-parse HEAD). Used for staleness detection; positions describe the working tree as it existed on top of this commit."
    },
    "tasks": {
      "type": "array",
      "minItems": 1,
      "description": "Major review tasks. Each task title becomes a numbered review section and Task N.* navigation context.",
      "items": { "$ref": "#/$defs/task" }
    }
  },
  "$defs": {
    "task": {
      "type": "object",
      "required": ["title", "groups"],
      "additionalProperties": false,
      "properties": {
        "title": {
          "type": "string",
          "minLength": 1,
          "description": "Active author-context sentence, e.g. Add..., Update..., Move..., Split..., Remove..., Preserve..."
        },
        "justification": {
          "type": "string",
          "minLength": 1,
          "maxLength": 80,
          "description": "Optional intent/rationale shown as yellow italic text directly under the task title; 80 characters or less."
        },
        "groups": {
          "type": "array",
          "minItems": 1,
          "items": { "$ref": "#/$defs/group" }
        }
      }
    },
    "group": {
      "type": "object",
      "required": ["type", "title", "subtasks"],
      "additionalProperties": false,
      "properties": {
        "type": {
          "$ref": "#/$defs/groupType"
        },
        "title": {
          "type": "string",
          "minLength": 1,
          "description": "Concrete owning code boundary such as a module, file, package, directory, crate, or app."
        },
        "subtasks": {
          "type": "array",
          "minItems": 1,
          "items": { "$ref": "#/$defs/subtask" }
        }
      }
    },
    "subtask": {
      "type": "object",
      "required": ["title", "items"],
      "additionalProperties": false,
      "properties": {
        "title": {
          "type": "string",
          "minLength": 1,
          "description": "Prose intent row under a group; explains the local purpose served by the concrete items."
        },
        "justification": {
          "type": "string",
          "minLength": 1,
          "maxLength": 80,
          "description": "Optional intent/rationale shown as yellow italic text directly under the subtask title; 80 characters or less."
        },
        "items": {
          "type": "array",
          "minItems": 1,
          "items": { "$ref": "#/$defs/item" }
        }
      }
    },
    "item": {
      "type": "object",
      "required": ["action", "type", "title", "note", "steps"],
      "additionalProperties": false,
      "properties": {
        "action": {
          "enum": ["Add", "Update", "Move", "Remove", "Split"],
          "description": "Title-cased action verb. Update displays as Modify in the summary."
        },
        "type": {
          "$ref": "#/$defs/itemType"
        },
        "subtype": {
          "type": "string",
          "minLength": 1,
          "description": "Optional narrower display label such as Resource or Buffer. Use only when the item's base class, trait, interface, framework role, or semantic kind is known; the summary renders this instead of the base type keyword."
        },
        "title": {
          "type": "string",
          "minLength": 1,
          "description": "Story node or concrete code symbol in the expanded graph."
        },
        "note": {
          "type": "string",
          "minLength": 1,
          "maxLength": 50,
          "description": "Short imperative annotation explaining what this node does in the change; 50 characters or less."
        },
        "steps": {
          "type": "array",
          "minItems": 1,
          "items": { "$ref": "#/$defs/step" }
        }
      }
    },
    "groupType": {
      "enum": ["Module", "File", "Package", "Directory"],
      "description": "Broad container-level construct for group headers. The summary renders this as a lowercase keyword before the group title."
    },
    "itemType": {
      "enum": [
        "Class",
        "Struct",
        "Enum",
        "Trait",
        "Interface",
        "Test",
        "Config",
        "Function",
        "Method",
        "Constant",
        "Field"
      ],
      "description": "Broad programming construct rendered as a lowercase keyword between the action and item title unless subtype is set. Function and Method both render as fn."
    },
    "position": {
      "type": "object",
      "required": ["line", "col"],
      "additionalProperties": false,
      "properties": {
        "line": {
          "type": "integer",
          "minimum": 1,
          "description": "1-based line in the NEW (post-change) file as it exists on disk"
        },
        "col": {
          "type": "integer",
          "minimum": 1,
          "description": "1-based column (byte offset) in that line"
        }
      }
    },
    "step": {
      "type": "object",
      "required": ["file", "start", "end", "comment"],
      "additionalProperties": false,
      "properties": {
        "file": {
          "type": "string",
          "minLength": 1,
          "description": "Repo-relative path with forward slashes"
        },
        "start": {
          "$ref": "#/$defs/position",
          "description": "Start of the referenced region. The line should be a CHANGED (added) line, not unchanged context - reviewers are jumped to it inside the diff."
        },
        "end": {
          "$ref": "#/$defs/position",
          "description": "Inclusive end of the referenced region; must be >= start. Keep the region tight around the discussed change (roughly 40 lines at most)."
        },
        "comment": {
          "type": "string",
          "minLength": 1,
          "description": "What this step changes and why; 1-4 sentences. Do not repeat the file path - the reviewer UI displays the step's file name."
        },
        "title": {
          "type": "string",
          "description": "Optional short step heading"
        }
      }
    }
  }
}
```

## 4. Example

```json
{
  "version": 7,
  "overview": "Add token-bucket rate limiting to public API requests. Before, API handlers accepted every request that reached the route stack. Now, API routes pass through a middleware boundary that checks per-client token buckets while health checks and static assets stay unthrottled.",
  "root": "Add token-bucket rate limiting to API requests.",
  "commit": "8f14e45fceea167a5a36dedd4bea2543c6a04c33",
  "tasks": [
    {
      "title": "Add middleware that decides whether a request can continue.",
      "justification": "Public API handlers need one shared gate before route-specific work starts.",
      "groups": [
        {
          "type": "Module",
          "title": "Router",
          "subtasks": [
            {
              "title": "Route public API requests through the limiter.",
              "items": [
                {
                  "action": "Update",
                  "type": "Function",
                  "title": "api route stack",
                  "note": "wrap /api without throttling health routes",
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
          "type": "Module",
          "title": "rate_limit",
          "subtasks": [
            {
              "title": "Return allow and deny decisions before request handling.",
              "items": [
                {
                  "action": "Add",
                  "type": "Function",
                  "subtype": "Middleware",
                  "title": "rate_limit()",
                  "note": "return allow/deny before request handling",
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
        }
      ]
    },
    {
      "title": "Add per-client token buckets with lazy refill.",
      "justification": "Buckets need deterministic refill without background work.",
      "groups": [
        {
          "type": "Module",
          "title": "rate_limit",
          "subtasks": [
            {
              "title": "Refill buckets lazily as requests arrive.",
              "items": [
                {
                  "action": "Add",
                  "type": "Method",
                  "title": "TokenBucket::take()",
                  "note": "refill on access and consume one token",
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
  `overview`, `root`, and `tasks`.
- `overview`, `root`, task titles, group titles, subtask titles, item titles,
  and item notes have no repo-relative paths or bracketed file names.
- `root` and each `tasks[].title` are active author-context sentences, not
  noun-only labels.
- Task `justification` fields are generally present unless the title makes the
  reason very obvious. Subtask justifications are exceptional; use them only for
  non-obvious rationale, tradeoffs, or sequencing context. Use single-sentence
  rationale lines of 80 characters or less. They do not start with `why:`
  because the UI renders them as yellow italic text.
- `tasks[]` captures the few major top-down review claims, not every support
  change. If there are more than four tasks, merge by feature/fix/refactor
  outcome before writing the JSON.
- Group titles identify concrete code boundaries whenever possible: module
  names, files, crates, packages, directories, or apps. Prefer
  `PhysicsPlugin`, `RenderPlugin`, `ModelPlugin`, `Router`, or `rate_limit` over
  humanized labels like `Physics plugin` or abstract labels such as `API`,
  `storage`, `renderer`, and `validation`.
- Group `type` values are restricted to container-level programming constructs:
  `Module`, `File`, `Package`, or `Directory`.
- Item `type` values use common programming constructs: `Class`, `Struct`,
  `Enum`, `Trait`, `Interface`, `Test`, `Config`, `Function`, `Method`,
  `Constant`, or `Field`.
- Optional item `subtype` values are narrow semantic labels like `Resource`,
  `Buffer`, or `Middleware`; omit `subtype` unless the code clearly supports
  that narrower label.
- Each group has `subtasks[]`; each subtask has a prose `title` and concrete
  `items[]`.
- Items are leaf nodes: every item has `steps`, and no item has `children`.
  When a change needs grouping, use the subtask title and sibling items instead
  of nesting action items; add subtask justification only for exceptional
  non-obvious context.
- Item actions are title-cased verbs from the allowed set: `Add`, `Update`,
  `Move`, `Remove`, `Split`.
- Item notes are single-line imperative fragments of 50 characters or less that
  render cleanly after the summary's inserted `to`; no third-person verbs
  (`binds`, `reuses`), tree characters, file paths, or broken phrases.
- `tasks[].groups[].subtasks[].items[]` explain the most important top-down
  design changes; they are not exhaustive inventories of changed files/functions.
- `commit` is the full 40-character sha from `git rev-parse HEAD`.
- The JSON is valid: no trailing commas, no comments, double-quoted keys.
- Write to `<repo root>/.walkthrough.json`, overwriting any existing file.
- If a JSON Schema validator is available, run it against the schema shipped
  alongside this skill and fix any reported violation, e.g.:
  - `check-jsonschema --schemafile <skill dir>/walkthrough.schema.json .walkthrough.json`
  - `npx ajv-cli validate --spec=draft2020 -s <skill dir>/walkthrough.schema.json -d .walkthrough.json`

  Skip silently when neither tool is installed - do not install one.

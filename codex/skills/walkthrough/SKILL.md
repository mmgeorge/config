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
- **Summary stays compact; comments guide the file-by-file review.** The
  `tasks[]` summary should stay top-down and focused, but `steps[]` are the
  inline code comments a reviewer sees while expanding files. Produce enough
  comment blocks that each meaningful changed file or semantic region has local
  context. Prefer several tight steps over one sparse comment for a whole task.
- **One logical idea per step.** A step covers one cohesive region. For
  multi-file changes, expect roughly 1-3 comment steps per important file or
  changed cluster, and more when a large deletion, replacement, migration, or
  subtle behavior change needs explanation. Skip purely mechanical churn
  (renames, lockfiles, formatting) unless it is the point of the change.
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
- **Comment meaningful removals too.** When a large or important block is
  deleted, add a `Remove` item/step that explains what was removed and why. For
  replacement deletions, anchor the step to the added replacement line. For
  deletion-only hunks, anchor to the closest surviving post-change line at the
  deletion boundary and keep the range small; the comment should name the
  deleted concept, e.g. `Remove the legacy retry loop because request retries
  now run through the shared backoff policy.`
- `file` is repo-relative with forward slashes.
- `title` should be present for every step. Write it as a succinct, concrete
  local code action sentence that states what this region does, such as `Define
  cache eviction modes.`, `Set bounded retries as the default policy.`, or
  `Store sync state on JobRecord.` Prefer concrete code verbs such as
  `Define`, `Set`, `Store`, `Remove`, `Configure`, `Bind`, `Load`, `Register`,
  `Guard`, `Validate`, `Split`, `Merge`, `Extract`, `Inline`, `Read`, `Write`,
  `Allocate`, or `Clear`. Avoid abstract or vague verbs such as `Represent`,
  `Carry`, `Handle`, `Support`, `Exercise`, `Improve`, or `Update`. Do not use
  noun-only headings like `Router wiring` or `Token bucket middleware`. Use
  `Store` when data is persisted or kept on a struct/resource/cache/state
  object. Use `Set` when assigning a runtime value, default, flag, option, or
  configuration.
- `comment` is the step's mini justification: first state the problem,
  limitation, risk, or review pressure; then state the solution, what changed,
  and why that addresses it. Use 1-4 prose sentences without `Problem:` or
  `Solution:` labels, aiming for roughly 180-200 characters when clarity allows.
  The reviewer can already see the code, and the reviewer UI displays the step's
  file name, so do not repeat the file path in the comment.
- `callout` is optional, singular, and exceptional. Most steps should omit it.
  Use it only for reviewer-critical context that would be easy to miss while
  jumping between blocks: `important`, `limitation`, `temporary`, `risk`,
  `followup`, `deviation`, or `workaround`.
  Always use `deviation` when the implementation departs from the original plan;
  this is high-priority review context. Use `workaround` when the change works
  around a deeper issue that is not fully fixed here because the full fix would
  be broader. Do not use `callouts` plural, do not add more than one callout,
  and do not restate the step title or comment. Keep `callout.text` compact,
  usually 120-180 characters.
- Author the top-down review map as structured `overview`, `root`, and
  `tasks[]`; the DiffReview plugin derives the displayed walkthrough summary
  from this structure. Do not write a `summary` field.
- `overview` is 2-3 prose sentences that tell the story. Start with one
  active sentence that succinctly encapsulates the whole change, such as `Add
  offline draft sync for document editing.` Then use `Before...` and
  `Now...` sentences to explain what changed in concrete terms.
- `root` is one active author-context sentence for the major feature/fix/refactor.
- Each `tasks[].title` is an active architectural review claim, not an
  implementation row. These titles become the numbered review sections in the
  walkthrough summary, so they should name the responsibility shift, boundary,
  or capability a reviewer should understand before reading module and item
  details. Avoid abstract capability phrasing such as `Let X choose Y`,
  `Enable X`, or `Support X`, but also avoid restating concrete implementation
  rows that will appear below. Prefer theoretical examples like `Separate
  document editing from sync transport.`, `Expose draft state beyond the editor
  session.`, or `Preserve edit order across offline retries.`
- `tasks[].justification` is optional in the schema but generally expected.
  Include one unless the task title already makes the reason very obvious. Use
  it to explain what is now different from before and why the task follows from
  that change; prefer `now ... so ...` justifications when they read naturally.
  Aim for 90-140 characters, with 170 characters as the hard schema cap. Omit
  it for very obvious/mechanical tasks or when it would only restate the title.
  It renders as rationale text without a `why:` label.
- Keep `tasks[]` focused on the true major review claims, usually 2-4 entries.
  Do not promote every meaningful diff cluster into a task. Supporting work
  like demo wiring, helper splits, tests, docs/plans, preservation of an
  old path, or follow-up notes belongs under the relevant major task unless it
  is the main purpose of the change.
- Inside each task, prefer exact code boundaries for group titles: concrete
  module, file, package, directory, crate, or app boundaries such as
  `DocumentEditor`, `DraftSync`, `SearchIndexer`, or `ExportScheduler`.
  Use humanized labels like `editor layer`, `background worker`, or abstract
  buckets like `sync`, `storage`, `renderer`, and `validation` only when there
  is no clearer owning symbol/module in the code.
  Each group has:
  - `type`: one of `Module`, `File`, `Package`, or `Directory`. Group types are
    restricted to container-level programming constructs; do not use `Class`,
    `Struct`, `Enum`, `Trait`, `Interface`, `Type`, `Test`, `Config`, `Method`,
    `Function`, `Field`, or `Constant` at the group level.
  - `title`: the concrete owning code boundary.
- Each group has one or more `subtasks[]`. A subtask is the high-level code
  action between a container and concrete code items:
  - `title`: an active prose sentence or fragment describing the design move
    this group makes in service of the parent task, such as `Route draft
    changes through the sync queue.` Do not attach a type or item action
    to a subtask. Subtask titles must use one of the high-level verbs in this
    verb bank. If none seems to fit, rewrite the subtask around the closest
    listed responsibility, ownership, lifecycle, data-flow, behavior, reuse, or
    structure change instead of inventing a different leading verb:
    - Boundary / ownership: `Expose`, `Encapsulate`, `Move`, `Centralize`,
      `Distribute`
    - Structure: `Extract`, `Inline`, `Split`, `Merge`, `Compose`, `Embed`
    - Lifecycle: `Create`, `Destroy`, `Register`, `Unregister`, `Attach`,
      `Detach`, `Start`, `Stop`
    - Data / control flow: `Route`, `Resolve`, `Defer`
    - Behavior rules: `Configure`, `Relax`, `Enable`, `Disable`
    - Reuse / variants: `Reuse`, `Generalize`, `Specialize`
    Avoid vague or overloaded verbs such as `exercise`, `handle`, `support`,
    `make`, `keep`, or `publish` unless `publish` is literally the public API
    concept in the code. When responsibility moves, name the concrete destination
    or source. Good non-domain-specific examples: `Configure export presets
    for scheduled reports.`, `Expose cached metadata to picker previews.`,
    `Extract attachment validation into a shared gate.`, `Route draft changes
    through the sync queue.`, `Generalize markdown rendering across editable
    text blocks.`, and `Specialize the cache refresh path for stale records.`
  - `justification`: optional and exceptional. Use it only when the subtask has
    non-obvious rationale, tradeoff, or sequencing context that the title cannot
    carry. Otherwise omit it. Aim for 90-140 characters, with 170 characters as
    the hard schema cap. It renders as rationale text without a `why:` label.
  - `items`: one or more typed code items that implement the subtask.
  Each item has:
  - `action`: one of `Add`, `Update`, `Move`, `Remove`, or `Split`.
    These render in the summary as `Add`, `Modify`, `Move`, `Remove`, and
    `Split` respectively. For newly-authored walkthroughs, use only `Add`,
    `Update`, or `Remove` for item action rows; use `Update` in JSON for rows
    that should display as `Modify`. Do not use `Move` or `Split` as item
    actions unless preserving an older walkthrough shape; describe moves and
    splits in the subtask title, then express the concrete leaf rows as
    `Add`/`Update`/`Remove` items.
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
    `bind retry policy before handlers` or
    `reuse cached search snapshots`. Do not use third-person wording
    like `binds` or `reuses`.
  - `steps`: one or more exact file/range walkthrough comments for that item.
- Items are leaf nodes. Do not use `children`, and do not create action items as
  grouping rows. Put grouping/intent in the subtask title; reserve subtask
  justification for exceptional non-obvious context. Put concrete implementation
  symbols as sibling items under that subtask. Every item must have `steps`, and
  one item may have multiple steps when the same semantic item changes several
  important regions or removes a substantial block.
- Exact file paths and ranges belong only in nested `steps`; do **not** include
  repo paths or bracketed file names in `overview`, `root`, task titles, group
  titles, subtask titles, item titles, or item notes. The plugin displays
  nested steps as `X.Y - <step title>`, where `X` is the task and `Y` is the
  step within that task.

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
      "description": "Major architectural review claims. Each task title becomes a numbered review section and Task N.* navigation context.",
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
          "description": "Active architectural review claim naming the responsibility shift, boundary, or capability being reviewed."
        },
        "justification": {
          "type": "string",
          "minLength": 1,
          "maxLength": 170,
          "description": "Optional intent/rationale shown under the task title; aim for 90-140 characters, hard cap 170. Prefer explaining what is now different and why the change follows."
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
          "maxLength": 170,
          "description": "Optional intent/rationale shown under the subtask title; aim for 90-140 characters, hard cap 170. Use only for non-obvious local rationale."
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
    "callout": {
      "type": "object",
      "required": ["kind", "text"],
      "additionalProperties": false,
      "properties": {
        "kind": {
          "enum": ["important", "limitation", "temporary", "risk", "followup", "deviation", "workaround"],
          "description": "Reviewer-critical callout kind. Deviation is for departures from the original plan; workaround is for a temporary or indirect fix for an issue not fully solved here."
        },
        "text": {
          "type": "string",
          "minLength": 1,
          "maxLength": 180,
          "description": "Compact reviewer-critical context, 180 characters or less. Do not restate the title or comment."
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
          "description": "Start of the referenced region. Prefer a CHANGED (added) line, not unchanged context; for deletion-only comments, use the closest surviving new-side line at the deletion boundary."
        },
        "end": {
          "$ref": "#/$defs/position",
          "description": "Inclusive end of the referenced region; must be >= start. Keep the region tight around the discussed change (roughly 40 lines at most)."
        },
        "comment": {
          "type": "string",
          "minLength": 1,
          "description": "Mini justification for this step: problem or pressure first, then the solution/change and why it addresses that problem; 1-4 sentences, aiming for roughly 180-200 characters. Do not repeat the file path."
        },
        "title": {
          "type": "string",
          "description": "Succinct active action sentence for the inline step header, e.g. Emit draft changes before persistence."
        },
        "callout": {
          "$ref": "#/$defs/callout",
          "description": "Optional singular callout for reviewer-critical context. Most steps omit it."
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
  "overview": "Add offline draft sync for document editing. Before, edits stayed tied to one open editor session. Now, drafts are stored outside the editor buffer, queued through a sync boundary, and replayed in order when transport is available.",
  "root": "Add offline draft sync for document editing.",
  "commit": "8f14e45fceea167a5a36dedd4bea2543c6a04c33",
  "tasks": [
    {
      "title": "Separate document editing from sync transport.",
      "justification": "Edits now outlive one editor session, so local drafting and remote sync need a stable handoff boundary.",
      "groups": [
        {
          "type": "Module",
          "title": "DocumentEditor",
          "subtasks": [
            {
              "title": "Expose draft changes outside the editor session.",
              "items": [
                {
                  "action": "Update",
                  "type": "Function",
                  "title": "edit pipeline",
                  "note": "emit draft changes before save",
                  "steps": [
                    {
                      "title": "Emit draft changes before persistence.",
                      "file": "src/editor/document.rs",
                      "start": { "line": 38, "col": 3 },
                      "end": { "line": 45, "col": 4 },
                      "comment": "Edits previously stayed inside the active editor buffer, so closing the document dropped unsynced state. Emitting draft changes creates a stable handoff for sync and recovery."
                    }
                  ]
                }
              ]
            }
          ]
        },
        {
          "type": "Module",
          "title": "DraftSync",
          "subtasks": [
            {
              "title": "Route draft changes through the sync queue.",
              "items": [
                {
                  "action": "Add",
                  "type": "Struct",
                  "subtype": "Queue",
                  "title": "DraftSyncQueue",
                  "note": "buffer drafts until transport is ready",
                  "steps": [
                    {
                      "title": "Buffer draft changes before transport.",
                      "file": "src/sync/draft_queue.rs",
                      "start": { "line": 12, "col": 1 },
                      "end": { "line": 34, "col": 2 },
                      "comment": "Network transport can be unavailable while users keep editing. A local queue preserves draft order until sync can send changes without coupling editor code to transport state.",
                      "callout": {
                        "kind": "risk",
                        "text": "Queue ordering is now part of correctness; review conflict handling before changing replay semantics."
                      }
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
      "title": "Preserve edit order across offline retries.",
      "justification": "Offline edits can replay after reconnect, so queued drafts need deterministic ordering before conflict checks run.",
      "groups": [
        {
          "type": "Module",
          "title": "DraftSync",
          "subtasks": [
            {
              "title": "Resolve queued drafts before remote writes.",
              "items": [
                {
                  "action": "Add",
                  "type": "Method",
                  "title": "DraftSyncQueue::drain_ready()",
                  "note": "replay drafts in sequence",
                  "steps": [
                    {
                      "title": "Replay drafts in stored order.",
                      "file": "src/sync/draft_queue.rs",
                      "start": { "line": 51, "col": 3 },
                      "end": { "line": 73, "col": 4 },
                      "comment": "Reconnect can expose several pending drafts at once, and conflict checks depend on the user's edit order. Draining the queue in sequence keeps replay deterministic before remote writes begin."
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
- Every nested `start.line` points at a changed (added) line whenever possible,
  not unchanged context. For deletion-only steps, it points at the closest
  surviving new-side line at the deletion boundary. Every region is tight
  (~40 lines max) around the discussed code.
- Important deleted blocks have their own `Remove` item/step. The comment names
  what was removed and explains why that deletion matters to the design.
- Every nested `steps[].title` is present and reads as a short concrete local
  code action sentence, not a noun phrase. Prefer precise verbs like `Define`,
  `Set`, `Store`, `Remove`, `Configure`, `Bind`, `Load`, `Register`, `Guard`,
  or `Validate`; avoid vague or abstract verbs like `Represent`, `Carry`,
  `Handle`, `Support`, `Exercise`, `Improve`, or `Update`. Use `Store` for
  data kept on a struct/resource/cache/state object; use `Set` for assigning a
  value, default, flag, option, or configuration.
- Every nested `steps[].comment` is a mini justification: problem/current
  pressure first, then solution/change and why it addresses the problem. Aim for
  roughly 180-200 characters, but prefer clarity over exact length.
- Every optional `steps[].callout` is singular, uses one of the allowed kinds,
  and is reserved for reviewer-critical context. Most steps omit it. Always use
  `deviation` when the implementation departs from the original plan, and use
  `workaround` when this change works around a deeper issue that remains
  unfixed.
- The document has no `summary` field; DiffReview derives the summary from
  `overview`, `root`, and `tasks`.
- `overview`, `root`, task titles, group titles, subtask titles, item titles,
  and item notes have no repo-relative paths or bracketed file names.
- `root` is an active author-context sentence. Each `tasks[].title` is an active
  architectural review claim, not a noun-only label or implementation summary.
  Task titles should be higher level than their groups, subtasks, and items, and
  should not simply repeat a concrete row that follows.
- Task `justification` fields are generally present unless the title makes the
  reason very obvious. They explain what is now different from before and why
  the task follows from that change; prefer `now ... so ...` when natural.
  Subtask justifications are exceptional; use them only for non-obvious
  rationale, tradeoffs, or sequencing context. Use single-sentence rationale
  lines, aim for 90-140 characters, and stay within the 170-character schema
  cap. They do not start with `why:` because the UI renders them as rationale
  text.
- `tasks[]` captures the few major top-down review claims, not every support
  change. If there are more than four tasks, merge by feature/fix/refactor
  outcome before writing the JSON.
- Group titles identify concrete code boundaries whenever possible: module
  names, files, crates, packages, directories, or apps. Prefer
  `DocumentEditor`, `DraftSync`, `SearchIndexer`, `ExportScheduler`, or
  `Router` over humanized labels like `editor layer` or abstract labels such as
  `sync`, `storage`, `renderer`, and `validation`.
- Group `type` values are restricted to container-level programming constructs:
  `Module`, `File`, `Package`, or `Directory`.
- Item `type` values use common programming constructs: `Class`, `Struct`,
  `Enum`, `Trait`, `Interface`, `Test`, `Config`, `Function`, `Method`,
  `Constant`, or `Field`.
- Optional item `subtype` values are narrow semantic labels like `Resource`,
  `Buffer`, or `Middleware`; omit `subtype` unless the code clearly supports
  that narrower label.
- Each group has `subtasks[]`; each subtask has a prose high-level code-action
  `title` and concrete `items[]`. The title's leading verb must come from the
  grouped architectural verb bank in the authoring instructions.
- Items are leaf nodes: every item has `steps`, and no item has `children`.
  When a change needs grouping, use the subtask title and sibling items instead
  of nesting action items; add subtask justification only for exceptional
  non-obvious context. Use multiple steps on one item when that item has several
  review-worthy regions.
- Item actions are title-cased verbs from the schema set, but newly-authored
  action rows use only `Add`, `Update`, or `Remove`; `Update` displays as
  `Modify` in the summary. Avoid `Move` and `Split` as item actions; put that
  design intent in the subtask title instead.
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

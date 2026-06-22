---
name: walkthrough
description: Generate .walkthrough.json at the git repo root - a guided, ordered review walkthrough of the current non-excluded changes (staged + unstaged vs HEAD, plus untracked files) for the DiffReview Neovim plugin. Use after finishing a multi-file change, or when asked to create a review walkthrough from the current diff.
---

# Generate a Review Walkthrough

Produce `.walkthrough.json` at the repository root so DiffReview can guide a
reviewer through the current changes. The summary explains the design at a
high level; nested steps render as inline comments at exact code locations.

## 1. Gather Context

Run from the target repository:

- `git rev-parse --show-toplevel` - where `.walkthrough.json` must be written.
- `git rev-parse HEAD` - full 40-character sha for `commit`.
- `git diff --name-only HEAD` - tracked paths with staged or unstaged changes.
- `git ls-files --others --exclude-standard` - untracked paths.

Exclude low-value artifact paths before reading or planning. Do not inspect,
summarize, group, annotate, or validate changes whose repo-relative path:

- ends in `.md` or `.txt`
- has a path segment named `docs`, `plans`, or `codegen`

Treat excluded files as if they are not part of the change. They do not need
diff coverage, tasks, groups, changes, annotations, or validation.

After applying exclusions, run the diff only for the remaining tracked paths and
read only remaining untracked files. Do not run or inspect an unrestricted full
diff before filtering.

If you made the changes yourself, use your implementation knowledge for the
review story and the filtered diff only to verify files and line numbers.

## 2. Output Shape

Validate against `walkthrough.schema.json` shipped with this skill. Read that
file when exact enum values or required fields are unclear.

Required top-level shape:

```text
version: 10
flow[]:
  text: compact data-flow node label, without file paths
  children?: next flow nodes or branch consumers
overview: 2-3 sentence before/now story with precise, accessible prose
root: active author-context sentence for the overall change
commit: full HEAD sha
tasks[]:
  title: architectural review claim, often a plain-language role
  justification?: why this task exists now; name and explain new constructs
  groups[]:
    type: Module | File | Package | Directory
    title: concrete owning boundary
    subtasks[]:
      title: high-level local design move
      justification?: exceptional local rationale
      changes[]:
        action: Add | Modify | Remove
        kind: Class | Struct | Enum | Trait | Interface | Test | Config | Function | Method | Constant | Field
        role?: optional code-proven role label, such as Cache or Adapter
        target: concrete code construct, symbol, or story node
        note: imperative fragment, 50 chars or less
        annotations[]:
          title: local code action sentence
          file: repo-relative path with forward slashes
          start/end: current file positions, 1-based line and column
          comment: mini justification in problem/solution form
          callout?: one exceptional reviewer note
```

## 3. Writing Rules

**Data flow**
- Always organize the walkthrough as a data-flow review. Pick the state,
  request, event, buffer, record, or artifact the review follows and use that
  path to order the graph and task list.
- The task list must visibly follow that flow from producer or entry point,
  through transforms/stores, to consumers. Do not switch to unrelated file,
  layer, artifact-type, or implementation buckets.
- Add a top-level `flow` tree that can be rendered as a compact graph in
  Neovim. Use one primary root when possible, then branch only where the data
  splits to multiple consumers. Keep labels short and concrete, and do not
  include file paths.
- The `flow` tree and tasks must agree. The graph is the quick orientation; the
  tasks are the same path expanded into review responsibilities.

**Prose**
- Use precise, accessible language: concrete nouns, plain verbs, short
  sentences, and causal before/now framing. Avoid semicolons.
- Match data-flow verbs to the code. Use `publish` only for public API, event,
  or subscription-style publication. Prefer `stores`, `writes`, `updates`,
  `provides`, or `reads` when data is kept in a resource, assigned into shared
  state, refreshed for consumers, exposed through an accessor, or consumed.
- Be precise about ownership. Do not infer ownership from the package/module
  name or primary consumer. Use `shared`, `written by`, `read by`, or `updated
  by` when state crosses systems.
- Before writing final prose, check each architecture noun phrase. If a reviewer
  could ask "what is that?" because the phrase is new, generic, or project-local,
  either replace it with a clearer role or explain it in the same sentence. For
  example, prefer `a queue that stores pending review comments` over `sync
  channel` when the channel is new to this change.
- In the overview, do not introduce a new connector label unless the sentence
  says what it carries, owns, updates, or coordinates. Prefer the role over the
  label when the details will be introduced later.
- Introduce named code constructs or symbols when they clarify ownership or a
  review boundary. On first mention, explain what the construct does and use
  normal articles in prose, such as `the DraftCache`. Do not list construct
  names just to prove coverage; details belong in tasks and items.
- For ownership or boundary changes, describe the architectural role in plain
  language, then name the construct that now owns that role: `Move draft state
  into a sync cache.` plus `The new DraftCache stores pending edits...`.

**Overview**
- `overview` is 2-3 sentences for the top-level review frame, not a mechanism
  inventory. Start with the primary feature, fix, or capability and the
  reviewer-visible outcome: `<feature/change> so <outcome>`.
- Then explain the before/now shift. `Before` names the old owner, limitation,
  or behavior. `Now` describes the new architecture at the role level and may
  name one or two central constructs only when they make the role clearer.
- Keep detailed mechanisms, secondary constructs, and pipeline steps out of the
  overview. Put those in tasks, groups, and items.

**Tasks**
- Tasks are the major review responsibilities in the data-flow review, usually
  3-5. They are not one row per file, artifact type, or implementation step.
- Use tasks to split the overview into what a reviewer should understand before
  reading groups and changes.
- Task titles use this shape: `<Active verb> <domain object> <with|through|in|across> <architectural role>.`
- Task justifications use this shape: `<Same subject> now <new behavior/capability>, so <old limitation or review concern> is resolved.`
- Keep titles active and reviewer-facing. Prefer `Share particle GPU buffers across render paths.` over `Move particle buffers into a shared storage resource.`
- Keep justifications concrete but not change-level. They may name the central construct responsible for the task, but only after explaining the role in plain language.
- Titles should read as what the change makes true for the codebase, not as a
  description of the implementation route. Use the domain object as the subject
  and a concrete architectural role as the destination so the reviewer knows
  what changed before reading change details.
- Do not create top-level tasks for excluded artifact paths or for supporting
  demos and tests. Demos and tests belong under the feature, boundary, behavior,
  or contract they validate or demonstrate.
- Example: `Share draft state through the sync cache.` plus `The sync cache now
  stores pending edits, so editor buffers and background sync use the same
  recovery point.`

**Groups**
- Groups use concrete owning boundaries: module, file, package, directory,
  crate, or app names. Group types are only `Module`, `File`, `Package`, or
  `Directory`.

**Subtasks**
- Subtasks are high-level local design moves between a group and concrete changes.
  Do not attach a kind or change action to a subtask.
- Start subtask titles with one of these verbs: `Expose`, `Encapsulate`, `Move`,
  `Centralize`, `Distribute`, `Extract`, `Inline`, `Split`, `Merge`, `Compose`,
  `Embed`, `Create`, `Destroy`, `Register`, `Unregister`, `Attach`, `Detach`,
  `Start`, `Stop`, `Route`, `Resolve`, `Defer`, `Configure`, `Relax`, `Enable`,
  `Disable`, `Reuse`, `Generalize`, or `Specialize`.
- Avoid vague verbs such as `exercise`, `handle`, `support`, `make`, or `keep`.
- Use subtask `justification` only for non-obvious rationale, tradeoff, or
  sequencing context. Otherwise omit it.

**Changes**
- Changes are concrete changed constructs or artifacts.
- Change `action` must be one of `Add`, `Modify`, or `Remove`.
- Use `role` only for a narrower role proven by the code: an implemented
  trait/interface, base class, framework registration, or strong repo
  convention. If no code evidence proves the role, omit it so the renderer uses
  `kind`.
- `note` is a short imperative fragment that reads after `to`, such as `emit
  draft changes before save`.

**Annotations**
- Create enough annotations that each meaningful changed file or semantic region
  has local review context. Use more annotations for large deletions, replacements,
  migrations, or subtle behavior changes.
- Keep ranges tight around changed lines, roughly 40 lines at most. `start.line`
  should point at an added line when possible; deletion-only steps anchor to the
  closest surviving post-change line.
- Annotation `title` is required by convention. Use concrete local action verbs such
  as `Define`, `Set`, `Store`, `Remove`, `Configure`, `Bind`, `Load`,
  `Register`, `Guard`, `Validate`, `Split`, `Merge`, `Extract`, `Inline`,
  `Read`, `Write`, `Allocate`, or `Clear`. Avoid `Represent`, `Carry`,
  `Handle`, `Support`, `Exercise`, `Improve`, or `Update`.
- Use `Store` for data kept on a struct/resource/cache/state object. Use `Set`
  for assigning a runtime value, default, flag, option, or configuration.
- Annotation `comment` is a mini justification: state the problem, limitation,
  risk, or review pressure first, then the solution and why it addresses the problem.
  Aim for roughly 150-180 characters when clarity allows.
- `callout` is optional, singular, and exceptional. Use only `important`,
  `limitation`, `temporary`, `risk`, `followup`, `deviation`, or `workaround`.
  Use `deviation` for departures from the original plan and `workaround` for
  temporary or indirect fixes.

## 4. Mini Example

```json
{
  "version": 10,
  "flow": [
    {
      "text": "DocumentEditor::apply_edit()",
      "children": [
        {
          "text": "DraftChange",
          "children": [
            {
              "text": "DraftCache",
              "children": [
                { "text": "editor recovery" },
                {
                  "text": "SyncWorker::drain_cache()",
                  "children": [
                    { "text": "retry save request" }
                  ]
                }
              ]
            }
          ]
        }
      ]
    }
  ],
  "overview": "Add offline draft sync so document edits survive closed editor sessions. Before, unsaved edits lived only in the active editor buffer. Now, the editor writes draft changes into a cache and the sync worker drains that cache for retry.",
  "root": "Add offline draft sync.",
  "commit": "8f14e45fceea167a5a36dedd4bea2543c6a04c33",
  "tasks": [
    {
      "title": "Write editor changes into durable draft state.",
      "justification": "The editor now records draft changes before save, so closing the buffer no longer drops unsynced work.",
      "groups": [
        {
          "type": "Module",
          "title": "DocumentEditor",
          "subtasks": [
            {
              "title": "Route draft changes out of the editor session.",
              "changes": [
                {
                  "action": "Modify",
                  "kind": "Function",
                  "target": "edit pipeline",
                  "note": "write edits to DraftCache",
                  "annotations": [
                    {
                      "title": "Write draft changes before persistence.",
                      "file": "src/editor/document.rs",
                      "start": { "line": 38, "col": 3 },
                      "end": { "line": 45, "col": 4 },
                      "comment": "Edits previously stayed in local buffer state until save completed. Writing each draft change to DraftCache gives recovery a durable record before persistence runs."
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
              "title": "Create the cache that carries drafts to sync.",
              "changes": [
                {
                  "action": "Add",
                  "kind": "Struct",
                  "role": "Cache",
                  "target": "DraftCache",
                  "note": "store pending editor drafts",
                  "annotations": [
                    {
                      "title": "Store pending drafts for retry.",
                      "file": "src/sync/draft_cache.rs",
                      "start": { "line": 12, "col": 1 },
                      "end": { "line": 34, "col": 2 },
                      "comment": "Background sync previously had no durable source after an editor closed. DraftCache stores pending edits and retry metadata so the worker can resume later."
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
      "title": "Drain cached drafts through background sync.",
      "justification": "The sync worker now reads DraftCache entries, so failed saves can retry without reopening the editor.",
      "groups": [
        {
          "type": "Module",
          "title": "DraftSync",
          "subtasks": [
            {
              "title": "Resolve queued drafts into sync attempts.",
              "changes": [
                {
                  "action": "Modify",
                  "kind": "Function",
                  "target": "sync worker",
                  "note": "retry cached draft edits",
                  "annotations": [
                    {
                      "title": "Read cached drafts before sync.",
                      "file": "src/sync/worker.rs",
                      "start": { "line": 57, "col": 3 },
                      "end": { "line": 72, "col": 4 },
                      "comment": "The worker previously only saw edits from live editor sessions. Reading DraftCache first lets failed saves move back into the normal sync path."
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

## 5. Validate Before Writing

- Diff coverage: every annotation file is in the filtered review set, ranges
  exist in the current file, added-line anchors are used when possible, and
  important deletions have `Remove` annotations.
- Narrative quality: prose has no repo paths, bracketed file names, or
  semicolons; tasks are architectural claims; named constructs are explained;
  subtasks use the verb bank.
- Schema contract: `version` is 10, `flow` is non-empty, `commit` is the full
  HEAD sha, change actions are only `Add`, `Modify`, or `Remove`, notes are
  imperative fragments of 50 characters or less, and JSON has no comments or
  trailing commas.
- Are all ownership claims correct?

Write `<repo root>/.walkthrough.json` with LF (`\n`) line endings, overwriting
any existing file. If
available, validate with one of:

```bash
check-jsonschema --schemafile <skill dir>/walkthrough.schema.json .walkthrough.json
npx ajv-cli validate --spec=draft2020 -s <skill dir>/walkthrough.schema.json -d .walkthrough.json
```

Skip schema validation silently when neither tool is installed; do not install
one just for this skill.

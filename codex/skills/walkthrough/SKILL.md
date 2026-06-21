---
name: walkthrough
description: Generate .walkthrough.json at the git repo root - a guided, ordered review walkthrough of the current changes (staged + unstaged vs HEAD, plus untracked files) for the DiffReview Neovim plugin. Use after finishing a multi-file change, or when asked to create a review walkthrough from the current diff.
---

# Generate a Review Walkthrough

Produce `.walkthrough.json` at the repository root so DiffReview can guide a
reviewer through the current changes. The summary explains the design at a
high level; nested steps render as inline comments at exact code locations.

## 1. Gather Context

Run from the target repository:

- `git rev-parse --show-toplevel` - where `.walkthrough.json` must be written.
- `git rev-parse HEAD` - full 40-character sha for `commit`.
- `git diff HEAD` - staged and unstaged changes together.
- `git ls-files --others --exclude-standard` - untracked files are part of the
  review set; read relevant ones.

If you made the changes yourself, use your implementation knowledge for the
narrative and the diff only to verify files and line numbers.

## 2. Output Shape

Validate against `walkthrough.schema.json` shipped with this skill. Read that
file when exact enum values or required fields are unclear.

Required top-level shape:

```text
version: 7
overview: 2-3 sentence before/now story with precise, accessible prose
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
      items[]:
        action: Add | Modify | Remove
        type: Class | Struct | Enum | Trait | Interface | Test | Doc | Plan | App | Config | Function | Method | Constant | Field
        subtype?: narrow semantic codebase-specific label, such a specific subclass or interface.
        title: concrete code construct, symbol, or story node
        note: imperative fragment, 50 chars or less
        steps[]:
          title: local code action sentence
          file: repo-relative path with forward slashes
          start/end: current file positions, 1-based line and column
          comment: mini justification in problem/solution form
          callout?: one exceptional reviewer note
```

## 3. Writing Rules

**Story order**
- Sequence tasks by review narrative, not raw file or diff order: public
  surface, core logic, call sites, then tests/config.
- Keep `tasks[]` to the major review claims, usually 2-4. Place demos, docs,
  tests, and follow-up plans into the feature they validate or explain
  unless they are the main change.

**Prose**
- Use precise, accessible language: concrete nouns, plain verbs, short
  sentences, and causal before/now framing. Avoid semicolons.
- Match data-flow verbs to the code. Use `publish` only for public API, event,
  or subscription-style publication. Prefer `stores`, `writes`, `updates`,
  `provides`, or `reads` when data is kept in a resource, assigned into shared
  state, refreshed for consumers, exposed through an accessor, or consumed.
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
- Tasks are major review claims, not every changed file or artifact type. Use
  them to split the overview into the few responsibilities a reviewer needs to
  understand.
- Task titles and justifications must not center demos, notes, docs, generated
  bindings, tests, or follow-up plans. Nest those artifacts under the feature or
  boundary they demonstrate, validate, document, or generate.
- Do not use demos, tests, docs, or plans as the reason for a feature task. When
  those artifacts exercise a change, describe the actual API, behavior, or
  boundary they exercise.
- Task titles use this shape: `<Active verb> <domain object> <with|through|in|across> <architectural role>.`
- Task justifications use this shape: `<Same subject> now <new capability>, so <old limitation or reason> no longer applies.`
- Keep titles active and reviewer-facing. Prefer `Share particle GPU buffers across render paths.` over `Move particle buffers into a shared storage resource.`
- Keep justifications concrete but not item-level. They may name the central construct responsible for the task, but only after explaining the role in plain language.
- Titles should read as what the change makes true for the codebase, not as a
  description of the implementation route. Use the domain object as the subject
  and a concrete architectural role as the destination so the reviewer knows
  what changed before reading item details.
- Example: `Share draft state through the sync cache.` plus `The sync cache now
  stores pending edits, so editor buffers and background sync use the same
  recovery point.`

**Groups**
- Groups use concrete owning boundaries: module, file, package, directory,
  crate, or app names. Group types are only `Module`, `File`, `Package`, or
  `Directory`.

**Subtasks**
- Subtasks are high-level local design moves between a group and concrete items.
  Do not attach a type or item action to a subtask.
- Start subtask titles with one of these verbs: `Expose`, `Encapsulate`, `Move`,
  `Centralize`, `Distribute`, `Extract`, `Inline`, `Split`, `Merge`, `Compose`,
  `Embed`, `Create`, `Destroy`, `Register`, `Unregister`, `Attach`, `Detach`,
  `Start`, `Stop`, `Route`, `Resolve`, `Defer`, `Configure`, `Relax`, `Enable`,
  `Disable`, `Reuse`, `Generalize`, or `Specialize`.
- Avoid vague verbs such as `exercise`, `handle`, `support`, `make`, or `keep`.
- Use subtask `justification` only for non-obvious rationale, tradeoff, or
  sequencing context. Otherwise omit it.

**Items**
- Items are concrete leaf rows.
- Item `action` must be one of `Add`, `Modify`, or `Remove`.
- Use `Test`, `Doc`, `Plan`, or `App` as the item `type` when the artifact
  belongs to that repo convention. `Doc` means `docs/` or a stronger repo
  documentation convention; `Plan` means `plans/` or a stronger planning/design
  convention.
- Use `subtype` only for clear narrower roles, such as a base class or interface
  the type implements.
- `note` is a short imperative fragment that reads after `to`, such as `emit
  draft changes before save`.

**Steps**
- Create enough steps that each meaningful changed file or semantic region has
  local review context. Use more steps for large deletions, replacements,
  migrations, or subtle behavior changes.
- Keep ranges tight around changed lines, roughly 40 lines at most. `start.line`
  should point at an added line when possible; deletion-only steps anchor to the
  closest surviving post-change line.
- Step `title` is required by convention. Use concrete local action verbs such
  as `Define`, `Set`, `Store`, `Remove`, `Configure`, `Bind`, `Load`,
  `Register`, `Guard`, `Validate`, `Split`, `Merge`, `Extract`, `Inline`,
  `Read`, `Write`, `Allocate`, or `Clear`. Avoid `Represent`, `Carry`,
  `Handle`, `Support`, `Exercise`, `Improve`, or `Update`.
- Use `Store` for data kept on a struct/resource/cache/state object. Use `Set`
  for assigning a runtime value, default, flag, option, or configuration.
- Step `comment` is a mini justification: state the problem, limitation, risk,
  or review pressure first, then the solution and why it addresses the problem.
  Aim for roughly 150-180 characters when clarity allows.
- `callout` is optional, singular, and exceptional. Use only `important`,
  `limitation`, `temporary`, `risk`, `followup`, `deviation`, or `workaround`.
  Use `deviation` for departures from the original plan and `workaround` for
  temporary or indirect fixes.

## 4. Mini Example

```json
{
  "version": 7,
  "overview": "Add offline draft sync so document edits survive closed editor sessions. Before, edits lived only in the active editor buffer. Now, draft state moves into a sync cache so editor buffers and background sync share one recovery point.",
  "commit": "8f14e45fceea167a5a36dedd4bea2543c6a04c33",
  "tasks": [
    {
      "title": "Share draft state through a sync cache.",
      "justification": "The new DraftCache stores pending edits, so closing the editor no longer drops unsynced work.",
      "groups": [
        {
          "type": "Module",
          "title": "DraftSync",
          "subtasks": [
            {
              "title": "Create the sync cache for editor drafts.",
              "items": [
                {
                  "action": "Add",
                  "type": "Struct",
                  "subtype": "Resource",
                  "title": "DraftCache",
                  "note": "store pending editor drafts",
                  "steps": [
                    {
                      "title": "Store pending drafts for retry.",
                      "file": "src/sync/draft_cache.rs",
                      "start": { "line": 12, "col": 1 },
                      "end": { "line": 34, "col": 2 },
                      "comment": "Draft state previously lived only inside the open editor. The new DraftCache stores pending edits and retry metadata so recovery does not depend on the buffer staying open."
                    }
                  ]
                }
              ]
            }
          ]
        },
        {
          "type": "Module",
          "title": "DocumentEditor",
          "subtasks": [
            {
              "title": "Route draft changes out of the editor session.",
              "items": [
                {
                  "action": "Modify",
                  "type": "Function",
                  "title": "edit pipeline",
                  "note": "send edits to DraftCache",
                  "steps": [
                    {
                      "title": "Write draft changes before persistence.",
                      "file": "src/editor/document.rs",
                      "start": { "line": 38, "col": 3 },
                      "end": { "line": 45, "col": 4 },
                      "comment": "The editor previously kept unsaved changes in local buffer state. Sending each edit to the DraftCache gives sync and recovery one durable handoff before persistence runs."
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

- Diff coverage: every step file is changed or untracked, ranges exist in the
  current file, added-line anchors are used when possible, and important
  deletions have `Remove` steps.
- Narrative quality: prose has no repo paths, bracketed file names, or
  semicolons; tasks are architectural claims; named constructs are explained;
  subtasks use the verb bank.
- Schema contract: `commit` is the full HEAD sha, item actions are only `Add`,
  `Modify`, or `Remove`, notes are imperative fragments of 50 characters or
  less, and JSON has no comments or trailing commas.

Write `<repo root>/.walkthrough.json` with LF (`\n`) line endings, overwriting
any existing file. If
available, validate with one of:

```bash
check-jsonschema --schemafile <skill dir>/walkthrough.schema.json .walkthrough.json
npx ajv-cli validate --spec=draft2020 -s <skill dir>/walkthrough.schema.json -d .walkthrough.json
```

Skip schema validation silently when neither tool is installed; do not install
one just for this skill.

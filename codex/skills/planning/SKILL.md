---
name: planning
description: Use when Codex is asked to create, draft, write, review, or update an implementation plan, execution plan, refactor plan, test plan, or reviewer-readable code-flow walkthrough before making changes.
---

# Planning

Plans follow the walkthrough artifact model. A plan is a reviewer-readable
implementation walkthrough, not a generic proposal. Start with the review
overview, then a compact code-flow diagram, then a numbered task tree. Those
three sections must describe the same data-flow narrative from producer or entry
point, through stores and transforms, into consumers. Do not start with generic
Problem, Refactoring, or Design patterns sections. Do not organize a plan as a
file inventory unless the real review path is file-based.

When creating a plan, use this template. Output each section in order:

1. **Overview** - 2-3 sentence before/now story with precise, accessible prose. Start with the feature, fix, or capability and the reviewer-visible outcome. Then explain the old limitation and the new role-level architecture. Name central constructs only when they clarify ownership or a review boundary.
2. **Code flow diagram** - Compact ASCII diagram that follows the same path as the tasks. Start at the relevant producer, entry point, state owner, request, event, buffer, record, or artifact. Continue through stores/transforms and end at the consumers. Include input/output types or state names when they clarify the flow. Mark modified/new nodes with `*` and removed nodes with `~`. Keep file paths out of the diagram.
3. **Tasks** - Numbered walkthrough tree. Each task is an active architectural review claim, not a file bucket. Use the shape `<Active verb> <domain object> <with|through|in|across> <architectural role>.` Add one justification sentence after the title using the shape `<same subject> now <new behavior>, so <old limitation or review concern> is resolved.`
   - `Task` is a numbered architectural claim that advances the code-flow narrative.
   - `Group` is an owning boundary and starts with one of these colorizable type terms: `module`, `file`, `package`, or `directory`.
   - `Subtask` is a local design move under a group. Start with one of these verbs: `Expose`, `Encapsulate`, `Move`, `Centralize`, `Distribute`, `Extract`, `Inline`, `Split`, `Merge`, `Compose`, `Embed`, `Create`, `Destroy`, `Register`, `Unregister`, `Attach`, `Detach`, `Start`, `Stop`, `Route`, `Resolve`, `Defer`, `Configure`, `Relax`, `Enable`, `Disable`, `Reuse`, `Generalize`, or `Specialize`.
   - `Change` is a concrete construct edit. Start with one of these actions: `Add`, `Modify`, or `Remove`.
   - After the action, include a standalone colorizable type/kind term before the target. Use one of these kind terms when possible: `class`, `struct`, `enum`, `trait`, `interface`, `test`, `app`, `config`, `fn`, `method`, `constant`, or `field`. Use a code-proven role term such as `Resource`, `Cache`, or `Adapter` only when that role is clearer than the broad kind.
   - Keep the group type term and change kind/role term separate from the target so renderers can colorize words like `module`, `file`, `struct`, `fn`, `config`, and `Resource`.
   - Include every function, type, config, app, test, or field that will be added, modified, or deleted.
   - Keep tasks in the same order as the code-flow diagram. Do not switch to unrelated file, layer, artifact-type, or implementation buckets.
4. **Modularity, testability, and plan validation** - Are ownership claims correct? Are boundaries clean, interfaces narrow, internals hidden, and behavior testable in isolation? If the plan touches many unrelated files, revise the task/group boundaries before presenting it. Check that the overview, code-flow diagram, tasks, and tests agree. Every changed construct appears under a task. Task titles are architectural claims. Groups are owning boundaries. Subtasks are local design moves. Changes are concrete edits. Ownership claims are explicit. No section collapses request failures, validation, or user-visible behavior into vague `handle`, `support`, `make`, or `update` wording.
5. **Test plan** - Specific tests tied to the walkthrough flow:
   - **Unit tests**: What to test, what to mock, what behavior each validates.
   - **Integration tests**: End-to-end workflows with real modules, covering key scenarios and edge cases.

Example:
```
1. **Overview** - Add offline draft sync so editor changes survive closed buffers and failed saves. Before, unsaved edits lived only in the active editor session and retry workers had no durable source. Now, the editor writes draft changes into a cache and the sync worker drains that cache into retryable save requests.

2. **Code flow diagram**
DocumentEditor::apply_edit()
  └─ *DraftChange
     └─ *DraftCache
        ├─ editor recovery
        └─ *SyncWorker::drain_cache() -> SaveRequest
           └─ retry save request

3. **Tasks**
1. Write editor changes through durable draft state. Editor changes now become cached draft records before save, so closing the buffer no longer drops unsynced work.
module editor session
└─ Route draft changes out of the active buffer.
   ├─ Add struct DraftChange to describe pending editor edits
   └─ Modify fn DocumentEditor::apply_edit to write changes to DraftCache
module draft sync
└─ Create the cache that carries drafts to retry.
   └─ Add Resource DraftCache to store pending draft records

2. Drain cached drafts through background sync. The sync worker now reads cached draft records, so failed saves can retry without reopening the editor.
module draft sync
└─ Resolve cached drafts into save attempts.
   ├─ Modify fn SyncWorker::drain_cache to build retry save requests
   └─ Add fn DraftCache::mark_saved to clear durable records after persistence
file editor recovery
└─ Reuse cached drafts for reopened sessions.
   └─ Modify fn restore_editor_session to load pending DraftChange records

5. **Test plan**
- **Unit tests**: DraftCache stores pending DraftChange records, SyncWorker::drain_cache turns cached drafts into SaveRequest values, and DraftCache::mark_saved clears saved records.
- **Integration tests**: Apply an edit, close and reopen the editor, then verify the session restores the cached draft and the background sync drains it into a retry save request.
```

Every plan must end with these lines:
```
+After completing each task, output a status line ([3/7 tasks complete — next: Task 4: ...]) and immediately proceed. Do not pause, summarize progress, or ask to continue.
+Finish the entire plan. No partial completions. Output DONE when all phases pass.
```

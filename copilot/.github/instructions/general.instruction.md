# General Guidelines
- Always complete the full task. **Never** stop partway ("good stopping point", "I'll leave the rest to you", etc.). Do not output summaries like "the remaining steps would follow the same pattern" or "you can apply the same approach to X, Y, Z." Execute every step literally. If you hit an error, debug it. If a fix causes a new problem, fix that too. Done means it works — including a green build and passing tests — not that you've made an attempt.
- **Never** refuse to fix something because it's "out of scope" or "in a different module." Chase bugs to their root cause regardless of module, layer, or crate.
- **Never claim something is impossible without evidence.** Find a link, issue, or error message that proves it. Historically, every "this won't work" or "known limitation" claim has been wrong. If stuck, say so honestly and present options — never silently switch approaches.
- If instructions are ambiguous or equally viable paths exist, stop and ask.
- When asked a question, answer it — don't automatically start writing code.
- Never use semicolons. Split sentences instead.

# Subagent Research Workflow
- Use the built-in `explorer` agent for local codebase exploration. Use `remote-code-explorer` for remote source, external library, GitHub, docs, examples, issues, or PR research when that custom agent is available.
- Do not spawn explore agents for tiny lookups, single-file reads, tightly coupled debugging, direct implementation work, or the next critical-path step when the main agent is blocked on the answer. Do that work locally.
- Before delegating, decide the immediate local task and keep working on non-overlapping work while subagents run. Do not duplicate a subagent's assignment in the main thread.
- Make each delegated exploration task concrete and bounded. Include the target, the question to answer, desired thoroughness (`quick`, `medium`, or `thorough`), and the expected output format.
- Ask explore agents to return compact evidence-backed findings with exact file paths, symbols, URLs, and gaps. The main agent owns synthesis, decisions, edits, and verification.

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

# Programming
- **Engineering over hacking.** When you spot a design issue, stop and refactor — fix the real problem even if it's substantial. Duplicated code should be shared. Minimize accumulated tech debt.
- **Fix at the source.** Fix problems where they originate — never monkey-patch. Fixes must apply to all codepaths, current and future. Flawed abstraction? Fix the abstraction. Wrong data? Fix where it's produced. Adding an `if` for a case that "shouldn't happen"? Fix why it happens.
- **No shortcuts or workarounds.** Never use serde rename, compatibility shims, adapter layers, or similar hacks to avoid real restructuring. Rename everywhere. Refactor if the structure doesn't support the change. Leave surrounding code better than you found it.
- **Think like a computer scientist.** Apply design patterns (strategy, builder, observer, etc.) when they fit naturally. Recognize implicit patterns — GoF, data-driven design, or other established paradigms — and make them explicit in the code's structure. When code reinvents a known pattern poorly, refactor to the proper one. Don't force patterns where they add complexity without clarity.
- **Modularity and encapsulation.** Keep concerns separated, interfaces narrow, internals hidden. If adding a feature requires touching many unrelated files, the boundaries are wrong — fix them.
- **Documentation templates.** Use these templates when adding docs or comments.
  - **Class/struct/trait docs**: Use third-person present tense with the type as the implied subject.
    Template: `<Stores|Owns|Coordinates|Tracks|Represents|Defines|Provides|Resolves> <state, responsibility, or capability> for <consumer or boundary> [<because|so|when|before|after|while|without|by|for|to> <justification>] [, <-ing phrase describing outcome or reason>].`
    Optional newline + second paragraph: `<Preserves|Enforces|Maintains> <invariant or lifecycle contract>.`
  - **Method/function docs**: Use imperative action wording with implied subject "you". Add these docs only when behavior is not obvious. Keep them to at most two lines.
    Template: `<Build|Split|Merge|Resolve|Route|Validate|Load|Write> <result or action> from <input or source> [<because|so|when|before|after|while|without|by|for|to> <justification>] [, <-ing phrase describing outcome or reason>].`
    Optional second line: `<Preserve|Enforce|Maintain|Validate|Avoid> <invariant, boundary, or edge case>.`
  - **Inline comments**: Use imperative action wording for non-obvious local constraints, ordering requirements, edge cases, external API quirks, or invariants. Inline comments should usually be one line. Use a block comment only when a local invariant needs multiple related facts.
    Template: `// <Keep|Avoid|Preserve|Defer|Normalize|Clamp|Cache|Skip|Retry|Guard> <local action or constraint> [<because|when|before|after|while|without|by|for|to> <reason>] [, <-ing phrase describing outcome or reason>].`
  - Do not add comments that merely restate the code. Do not write "this is non-trivial" or similar labels.
- **Naming.** Never use single-letter variable names. Avoid abbreviations unless widely understood (e.g., `url`, `id`, `config`). Long type names can be shortened to a clear word — e.g., `FoundationalVectorStore` → `store` — but never to a letter like `r` or `s`.
- **Testing strategy**: Unit tests per module with mocks for isolation. Integration tests for end-to-end workflows with real modules — these catch boundary issues mocks hide.
- Keep functions focused and reasonably sized. A function whose body is shorter than its signature is a smell — if it isn't doing meaningful work beyond a direct call, it shouldn't exist.

# Shell and Environment
- **Never** read or set environment variables via shell commands. If one is required and unset, ask the user.
- Do **not** prefix commands with `cmd /c`, just call them.
- Unless there are compelling performance reasons, prefer using simple commands serially instead of complex combined ones. This are more likely to be auto approved.

# Searching
- For researching or finding new Rust crates, use `docs-mcp`.
- When you need to view the source code:
  - For Rust or Typescript, first check locally if it already exist.
  - Otherwise, when you need to view source code, use `github` mcp.
- Also search issues and PR description with the `github` mcp to augment understanding.
- For CLI tools or APIs, prefer searching the source code to get a deep understanding.

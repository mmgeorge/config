# Plan a software change in Harness

Produce a reviewer-readable implementation walkthrough as a canonical PlanDocument JSON.
The document must connect the requested outcome to concrete owners, typed flows, and executable
tasks. Harness renders the review view and records execution progress from this same document.
Optimize for removing avoidable waiting between required deliverables. Do not optimize for the
number of tasks, subtasks, or siblings. Additional work is not a concurrency improvement.

Harness has already created the document. Use its current `plan_id` and `version`. Harness owns
`schema_version`, `plan_id`, `version`, and the original `prompt`. Follow the advertised tool schema
for PlanDocument schema version 5. Author semantic data, not Markdown diagrams, indentation,
numbering, or provider checklists. Planning and approval do not widen command or file permissions.

## Planning procedure

1. **Inspect the affected code.** Identify existing owners, callers, interfaces, manifests, and
   verification commands. Resolve discoverable facts from source before asking questions. Separate
   observed behavior from assumptions. Inspect only the boundaries needed for this change.
2. **Establish the outcome.** Define observable success, scope, and constraints in `overview` and
   `usage`. Ask only when an unresolved product decision changes implementation. Use
   `harness_question_ask` with one to three concise questions, two or three mutually exclusive
   choices per structured question, and a recommended choice first. End the turn after asking.
3. **Design the ownership model.** Populate `entity_changes` and `dependencies`. Reuse existing
   owners and contracts. Place state and behavior together until a distinct responsibility or
   reuse boundary justifies separation. Specify the interfaces consumers will need.
4. **Trace the behavior.** Populate `flows` from actual entry points through changed and unchanged
   owners to observable results. Include material failure paths. Reconcile flow inputs and outputs
   with the entity declarations before dividing the work.
5. **Define complete tasks.** Give each task one cohesive deliverable, owned files, and a concrete
   completion check. Put source changes and required verification in its `files[].subtasks[]`.
   Keep changes that cannot be implemented and verified separately in one task.
6. **Determine prerequisites.** For each task, identify the earlier deliverables required to
   implement and verify it. Record their task IDs in `requires`. A runtime call does not by itself
   imply an implementation dependency. An existing usable interface can allow separate work.
   For each edge, identify the concrete declaration, implementation, generated artifact, or test
   capability the consumer needs. Remove edges justified only by a feature's runtime order or
   by a broad label such as backend, frontend, storage, or integration.
7. **Group independent work.** Place each task in the earliest stage where its prerequisites are
   complete and it can finish without sibling changes. Review shared files, module registration,
   manifests, generated artifacts, unfinished interfaces, and shared mutable resources. Move
   dependent work later or combine coupled tasks. Do not add interfaces, scaffolding, tiny tasks,
   or unrelated files solely to manufacture parallelism.
   After assigning stages, examine each task again: what prevents it from starting one stage
   earlier? If no required artifact, verification dependency, or file conflict does, move it.
   A task waits for every earlier stage even when its `requires` does not name those tasks.
8. **Audit and submit.** Validate the document as one ownership model using the checklist below.
   Apply coherent changes through `harness_plan_edit`, resolve all reported violations, and call
   `harness_plan_submit` with the exact current ID and version. Stop at submitted review. Approval
   and execution are separate lifecycle transitions.

## Document fields

### `title`, `overview`, `usage`, and `assumptions`

- Replace the provisional title with a concise capability or change derived from the request.
- Write two or three overview sentences. Start with the feature, fix, or capability and its
  reviewer-visible outcome. Then explain the limitation or architectural motivation and the
  resulting ownership model. Use a before/now contrast only for changed existing behavior.
- Set `usage` to `{ "command": "...", "expected_result": "..." }` for caller-facing behavior.
  Show one actual command, API call, or interaction and its observable result. For a CLI, include
  the full executable command and a literal stdout/stderr transcript with exit status, for example
  `status: ready\nitem_count: 42\nexit status: 0`. Use `<no stdout>` or `<no stderr>` when applicable.
  Do not replace literal CLI output with an instruction such as “Print the result”.
- For non-text results, use a compact placeholder such as
  `<visual result: reopened editor displays the unsaved draft>`.
- Encode omitted Usage as JSON null. Harness supplies the rendered `<Omitted>` marker.
- Record unresolved, consequential assumptions as strings. Verify discoverable facts instead of
  labeling them assumptions. Do not use assumptions to conceal unanswered scope decisions.

### `entity_changes`: owners and interfaces

Model each changed construct once as a ProgramEntityChange. Describe what it owns, exposes, or
coordinates and why that boundary belongs together. Keep identifiers and signatures precise.

- Supply `action`, `kind`, `name`, `description`, and repository-relative `path`. Use the actual
  construct kind, such as `struct`, `class`, `enum`, `trait`, `interface`, `function`, or `config`.
  Use a role such as `resource`, `cache`, or `adapter` only when it describes the construct better.
- Use `add`, `modify`, `remove`, or `rename`. For renames, put the prior identifier in
  `renamed_from` and the destination in `name`. Do not encode one rename as removal plus addition.
- Nest fields, methods, functions, constants, and properties under their owner in `members`.
  Include each member's action, kind, name, visibility, and applicable type, parameters, and
  return type. Omit a meaningless unit or void return. Member descriptions are optional.
- Put enum cases in `variants` and named payload fields in each variant's `fields`. Cases and
  payload fields carry their own actions and optional descriptions. Payload fields accept optional
  `kind: "field"` and visibility metadata. Treat variant payload as exposed contract data.
- Use `extends` for inheritance and `conforms_to` for contracts. Declare shared operations on the
  contract once. Add concrete-only operations only when they matter to the planned behavior.
  Express capability differences through explicit state, strategies, or results.
- Use retained-state notation consistently: `Type` means owned, `&Type` retained non-owning,
  `@Type` retained shared ownership, `Type?` optional, and `Type[]` a collection. Parameters and
  returns express transient dependencies. Do not invent a `uses` field or ownership metadata.
- Keep private implementation details with their owner. Do not infer intentional exclusivity
  merely because a type currently has one caller. Harness derives diagram layout from the model.
- Name direct accessors `noun()`, `noun_mut()`, and `set_noun(value)` where applicable. Do not add
  a mutable-reference accessor when the language or interface has no such operation.
- Send complete `members`, `variants`, `conforms_to`, and variant `fields` arrays, including empty
  arrays. A replacement must retain the declarations that still belong in the plan.

### `dependencies`: package decisions

- Record every added, modified, or removed package with `action`, `name`, `version`, `manifest`,
  `license`, and `justification`. A dependency's name identifies its manifest declaration.
- Explain its architectural role and why existing code or the standard library cannot satisfy
  that role. Trace domain APIs through external flow edges. For runtime, derive, build, or test
  support, name the owning entity and integration mechanism in the entity or task description.
- For Rust, use a valid Cargo version requirement. Harness resolves and preserves an exact
  published version for referenced API checks. Do not invent versions or resolved evidence.
- When package declarations change, make their configuration the first task. Match each dependency
  manifest to exactly one task file. Keep package configuration separate from unrelated interface
  design, and list it as a prerequisite where compilation or verification requires it.

### `flows`: typed behavior across owners

Use one to three flows for the major affected runtime, data, request, event, persistence, recovery,
or configuration paths. Give each a natural title such as Capture, Sync, or Recovery. In its
description, state the entry point, observable outcome, and ownership boundary or failure risk
that makes the flow distinct. Keep independent flows separate.

- Supply `title`, `description`, `source`, and ordered `edges`. Use the same entity names as the
  object model. Show concrete construction and calls for changed orchestration such as `main`.
- Reference changed constructs with `{"kind":"planned_entity","entity":"TypeName"}`.
  Reference unchanged repository constructs with
  `{"kind":"workspace_entity","entity_kind":"type","name":"TypeName","path":"src/file.rs","line":42}`.
  Use the actual one-indexed declaration line. Reference library types with
  `{"kind":"external_entity","entity_kind":"type","name":"TypeName","dependency":"package-name"}`.
  Omit or null `dependency` only when no package provenance applies.
- Put a `relation` on each edge: `construct` creates, `call` invokes, `read` obtains data through
  a callable, `write` mutates through a callable, `send` transfers a payload, `emit` produces a
  payload, and `return` transfers a payload back to its target.
- Target exactly one type entity for `construct`, `call`, `read`, and `write`. Reserve
  `entity_kind: "endpoint"` for external actors or destinations such as schedulers or storage.
  `send`, `emit`, and `return` may target endpoints and require `payload_type`.
- Each `call`, `read`, or `write` needs a sibling `callable` with `kind: "function"` or `"method"`
  and a bare identifier in `name`, without parentheses, arguments, or a receiver prefix.
- For callable inputs, `payload_type` describes all non-receiver parameters: one type, a tuple
  for multiple parameters, or `()` for none. Model outputs with
  `return_type: {"value_type":"TypeName","error_type":"ErrorType"}`. Omit `error_type` for
  infallible calls and omit meaningless infallible unit outputs. For a fallible unit result,
  retain `value_type: "()"` alongside `error_type`. Represent material error behavior in branches.
- Put work performed within a call in its `expansion`. Put alternatives in `branches`, each with
  a `condition` and nonempty `edges`. Supply complete `edges`, `expansion`, and `branches` arrays.
  Nest explicitly. An expansion must contain material work, not only repeat the parent's return.
- Before adding an external Rust callable, verify its public identifier and declared input and
  output types against source or documentation for the planned version. Use the receiver's owning
  Cargo package in `dependency`. Preserve a public receiver alias and declared generic parameter
  names. Harness resolves aliases and re-exports for source navigation, but signature comparison
  does not compile the project or prove trait bounds and concrete generic substitutions.
- If an external API remains unverified, record that limitation. Keep the boundary within a
  planned owner, explain its concrete work with nested edges where known, and name the dependency
  responsibility. Do not fabricate a callable or hide missing design behind “handle” or “support”.

### `stages[].tasks[]`: executable ownership boundaries

A stage is an ordered execution barrier. Its tasks are independent implementation units that can
each finish, including their required verification, after earlier stages complete. Stage numbers
and task letters come from array position. Stable IDs identify work across revisions.

- Give each stage an `id`, a concise outcome-oriented `title`, and nonempty `tasks`.
- Give each task an `id`, `requires`, `title`, `description`, and nonempty `files`. Keep IDs when
  renaming or regrouping the same work. Never put display numbers or letters in titles or IDs.
- Prefer independent sibling tasks over unnecessary sequential stages. Use `requires: []` when
  a task needs no prior deliverable. Otherwise list actual prerequisite task IDs from earlier
  stages. Do not chain tasks merely because they appear consecutively in a runtime flow.
- Build the prerequisite graph before naming stages. Place work after its actual predecessors,
  then group the ready tasks that can coexist. Stage titles describe the resulting work, rather
  than imposing a generic sequence of architecture layers. Explain non-obvious prerequisites in
  the affected task's description by naming the artifact or check that requires them.
- Apply this independence check: with all earlier stages complete and every sibling absent,
  can this task implement its deliverable and pass its completion checks? If not, move the needed
  deliverable earlier, move the dependent task later, or combine the coupled work.
- Siblings cannot edit the same file, including either endpoint of a rename. Disjoint files are
  necessary but do not prove independence. Check build registration, generated output, schema
  migrations, shared fixtures, and external resources as well as API contracts.
- Establish shared contracts earlier only when consumers genuinely need a changed contract.
  Include usable declarations, exports, and verification infrastructure needed by those consumers.
  A sentence promising an interface does not make sibling implementations independent.
- Keep ownership responsibilities cohesive. Do not create extra layers or split one small change
  into artificial tasks to increase the sibling count. A sequential plan is correct when concrete
  prerequisites require it. Explain such constraints in the affected task description.
- Review broad tasks as well as narrow ones. If a task combines substantial required deliverables
  with distinct owners and independent completion checks, separate that existing work where the
  file and compilation boundaries support it. Keep coupled operations together. Do not create
  speculative abstractions or count file-level subtasks as concurrent execution units.
- Write task titles as active architectural claims, for example “Preserve pending edits through
  durable draft state.” Follow with one or two sentences explaining effect, motivation, constraint,
  or completion evidence. Do not repeat the title. Use “now” only for changed existing behavior.
- Put assembled verification after all implementation tasks it exercises. Verification that uses
  only earlier artifacts belongs with its implementation task. Do not promise a sibling's future
  integration as a task's completion check.
- Distinguish checking a consumer against a completed contract from checking the assembled runtime.
  Existing fixtures or controlled collaborator responses may support local completion before the
  collaborator implementation exists. State that scope and keep real integration verification
  after all participants. Do not weaken a test, duplicate infrastructure, or invent a contract
  merely to remove a prerequisite. Check each test's `covers_entities` and actual collaborators
  against the tasks that provide them.
- In an empty repository, establish the declarations and usable build/test entry points needed by
  subsequent work. Do not register absent modules or require future CLI behavior in an earlier
  test. Do not combine an entire application solely because its final module registry or entry
  point is shared. First check whether completed shared types and ordinary targeted test entry
  points let substantial consumers finish separately, with final registration owned by a later
  integration task. For example, a Rust integration test can include its owned source module with
  `#[path]` and use the completed library contracts before that module is publicly registered.
  Name the actual test target and imports that make this possible. Do not prescribe this technique
  when normal module tests already work, introduce placeholder behavior, or add production layers
  solely for concurrency. If no concrete verification boundary works, retain the dependency.
- Harness currently executes one lettered task at a time. Independence describes readiness and
  future concurrency, not permission to start sibling tasks during execution.

### `files[].subtasks[]`: changes and verification evidence

Files establish concrete edit ownership. Subtasks record local design moves or meaningful tests
within the task. They remain completion evidence for the whole task, not separate execution goals.

- For file additions, modifications, or removals, supply `action` and repository-relative `path`.
  For a rename, supply `action: "rename"`, `from`, and `to`. Place renamed entities at `to`.
- For implementation work, supply `operation`, a complementary `description`, and `entities`.
  Reference each top-level entity in exactly one implementation subtask at its declared path.
  Nested members and variants remain attached through that owner.
- Use one supported operation: expose, encapsulate, move, centralize, distribute, extract, inline,
  split, merge, compose, embed, create, destroy, register, unregister, attach, detach, start, stop,
  route, resolve, defer, configure, relax, enable, disable, reuse, generalize, or specialize.
  For `operation: "route"`, write `"draft changes into DraftStore."` without repeating “Route”.
- Represent each concrete test as a flat subtask with `operation: "test"`, `action`, `name`,
  `category: "unit"` or `"integration"`, and `behavior`. Use `covers_entities` to trace production
  owners. Put it in its actual test file. Never model a concrete test as an `entity_change`, a
  nested `tests` collection, or a top-level resource.
- Prefer integration tests through real modules for ownership boundaries, persistence, recovery,
  and end-to-end behavior. Specify scenarios, observable results, and relevant failure paths.
- Use unit tests for algorithms, parsers, data structures, state machines, or complex isolated
  behavior. Identify collaborators to mock when needed. Omit tests when no meaningful runtime
  behavior needs verification. Do not add tests for trivial accessors, delegation, field assignment,
  or properties already enforced by the type system. Strengthen types before testing avoidable
  invalid states.

## Edit and submission protocol

The PlanDocument describes the result. `harness_plan_edit` accepts an atomic patch to that result.
Use the supplied active document or `harness_plan_read` to obtain current state. Do not create a
second plan or write the rendered review file to change the canonical document.

- Send `plan_id`, `expected_version`, and the applicable patch fields: `plan`, `set`, `rename`,
  `delete`, `stages`, and `assumptions`. Do not send the full PlanDocument as the tool request.
- Set `plan.title`, `plan.overview`, and `plan.usage` directly. Explicit null clears usage.
  Replace `stages` and `assumptions` as complete arrays. Include retained tasks and nested content.
- Under `set`, group complete resources in `entity_changes`, `dependencies`, and `flows` arrays.
  Entity and dependency `name`, and flow `title`, are semantic keys. Existing keys replace in
  place and new keys append. Include every retained member, variant, field, edge, and branch.
- To change a resource's identifying key, use `rename` with `{"from":"old key","to":"new key"}`
  under its collection, then address the destination under `set` if replacing its contents.
  Example: `"rename":{"flows":[{"from":"Draft capture","to":"Capture"}]}`.
- To retract a plan resource, list its current semantic key under `delete`, for example
  `"delete":{"flows":["Obsolete recovery"]}`. To remove actual code or a package, instead `set`
  a complete resource with implementation `action: "remove"`. These are different operations.
- Do not wrap set resources in `key`/`value`, operation envelopes, or JSON Patch `op`/`path` pairs.
  Do not repeat a semantic key or both delete and set/rename the same resource in one patch.
- Apply interdependent changes together so references remain valid. Every rejected control call
  returns one JSON object. Read `phase`, `code`, the complete `violation` array, and optional
  `retry.expected_version`. Correct the reported violations together. Use the returned version,
  rereading current state when needed. Never keep retrying a stale version or identical patch.
- When repairing a same-stage prerequisite error, reorder the actual producer and consumer, then
  reassess unaffected tasks for their earliest valid placement. Do not add unsupported prerequisites
  or serialize every task merely to satisfy validation.
- Successful editing does not submit the plan. Call `harness_plan_submit` with its exact current
  `plan_id` and `expected_version`. A prose answer or provider task update is not submission.

## Submission checklist

Validate the PlanDocument as one ownership model before submission:

- Does `usage` demonstrate the promised outcome, and do assumptions expose remaining uncertainty?
- Does every changed construct appear once, with accurate action, ownership, path, and signature?
- Do flow targets and callable types agree with declarations and verified dependency APIs?
- Does every dependency justification map to concrete behavior or an explicit support mechanism?
- Does every entity belong to exactly one implementation subtask, and every dependency manifest
  to exactly one task file?
- Can every sibling task finish with its siblings absent, including its stated verification?
- Do `requires` IDs resolve to earlier stages, and have unnecessary stage barriers been removed?
- For each dependency edge, can the task name an artifact or check it needs from that predecessor?
- Does any stage delay otherwise ready work, or does one task conceal independently useful work?
- Do tasks cover exports, wiring, migration, and assembled verification required for the outcome?
- Can execution report each whole task with concrete subtask, entity, path, and test evidence?

After approval, the scheduler selects one whole task. Report completion with
`harness_plan_task_report` using the active task ID, current plan version, and version-scoped JSON
pointers such as `/stages/1/tasks/0/files/0/subtasks/0`. Record departures from accepted intent
through `harness_plan_deviation` before proceeding. A stage completes only after all its tasks
have persisted completion. Do not report sibling work as completed or bypass the scheduler.

## Example: establish shared storage, implement independent consumers, verify the journey

This is one complete `harness_plan_edit` request for an existing editor. Substitute the active
plan ID and version. The example assumes the repository already registers the listed modules,
provides its storage and save boundaries, and can compile each consumer with the others unchanged.
It adds no packages. These are example-specific starting conditions to verify, not defaults.

The storage task establishes usable behavior first. Capture, sync, and recovery then own distinct
files and can each be verified against that storage without sibling changes. The final test needs
all three consumers. Harness renders those positions as `1a`, `2a`/`2b`/`2c`, and `3a`.


```json
{
  "plan_id": "plan-uuid",
  "expected_version": 1,
  "plan": {
    "title": "Preserve editor drafts across restart and failed saves",
    "overview": "Preserve unsaved edits when editor buffers close or saving fails. A durable draft owner supplies independent capture, sync, and recovery boundaries, so retries and reopened sessions use the same pending revisions.",
    "usage": {
      "command": "Edit a document, close the editor before saving, and reopen it.",
      "expected_result": "<visual result: the reopened editor restores the unsaved draft>"
    }
  },
  "set": {
    "entity_changes": [
      {
        "action": "modify",
        "kind": "struct",
        "name": "DraftStore",
        "description": "Owns durable pending edits independently from editor buffers. Existing storage commits a draft before capture returns and acknowledges only the saved revision.",
        "path": "src/draft_store.rs",
        "members": [
          {
            "action": "add",
            "kind": "method",
            "name": "persist",
            "visibility": "public",
            "type": null,
            "parameters": [{"name": "draft", "type": "Draft"}],
            "return_type": "Result<DraftId, DraftError>"
          },
          {
            "action": "add",
            "kind": "method",
            "name": "pending",
            "visibility": "public",
            "type": null,
            "parameters": [],
            "return_type": "Result<Vec<Draft>, DraftError>"
          },
          {
            "action": "add",
            "kind": "method",
            "name": "acknowledge",
            "visibility": "public",
            "type": null,
            "parameters": [{"name": "id", "type": "DraftId"}],
            "return_type": "Result<(), DraftError>"
          }
        ],
        "variants": [],
        "extends": null,
        "conforms_to": []
      },
      {
        "action": "modify",
        "kind": "struct",
        "name": "DocumentEditor",
        "description": "Captures edits through DraftStore before the source buffer can close.",
        "path": "src/editor.rs",
        "members": [
          {
            "action": "add",
            "kind": "method",
            "name": "capture",
            "visibility": "public",
            "type": null,
            "parameters": [{"name": "draft", "type": "Draft"}],
            "return_type": "Result<DraftId, DraftError>"
          }
        ],
        "variants": [],
        "extends": null,
        "conforms_to": []
      },
      {
        "action": "modify",
        "kind": "struct",
        "name": "SyncWorker",
        "description": "Drains pending drafts through the existing save boundary and retains records when saving fails.",
        "path": "src/sync.rs",
        "members": [
          {
            "action": "add",
            "kind": "method",
            "name": "sync",
            "visibility": "public",
            "type": null,
            "parameters": [],
            "return_type": "Result<(), DraftError>"
          }
        ],
        "variants": [],
        "extends": null,
        "conforms_to": []
      },
      {
        "action": "modify",
        "kind": "struct",
        "name": "EditorRecovery",
        "description": "Restores pending draft bodies through the existing buffer-opening boundary after restart.",
        "path": "src/recovery.rs",
        "members": [
          {
            "action": "add",
            "kind": "method",
            "name": "restore",
            "visibility": "public",
            "type": null,
            "parameters": [],
            "return_type": "Result<(), DraftError>"
          }
        ],
        "variants": [],
        "extends": null,
        "conforms_to": []
      }
    ],
    "dependencies": [],
    "flows": [
      {
        "title": "Capture",
        "description": "An editor edit reaches durable storage before capture returns. A storage failure remains visible to the caller.",
        "source": {"kind": "planned_entity", "entity": "DocumentEditor"},
        "edges": [
          {
            "relation": "call",
            "target": {"kind": "planned_entity", "entity": "DocumentEditor"},
            "callable": {"kind": "method", "name": "capture"},
            "payload_type": "Draft",
            "expansion": [
              {
                "relation": "write",
                "target": {"kind": "planned_entity", "entity": "DraftStore"},
                "callable": {"kind": "method", "name": "persist"},
                "payload_type": "Draft",
                "expansion": [],
                "branches": [],
                "return_type": {"value_type": "DraftId", "error_type": "DraftError"}
              }
            ],
            "branches": [],
            "return_type": {"value_type": "DraftId", "error_type": "DraftError"}
          }
        ]
      },
      {
        "title": "Sync",
        "description": "The worker reads pending drafts and saves them through the existing persistence boundary. Only successful saves acknowledge the exact draft revision.",
        "source": {"kind": "planned_entity", "entity": "SyncWorker"},
        "edges": [
          {
            "relation": "call",
            "target": {"kind": "planned_entity", "entity": "SyncWorker"},
            "callable": {"kind": "method", "name": "sync"},
            "payload_type": "()",
            "expansion": [
              {
                "relation": "read",
                "target": {"kind": "planned_entity", "entity": "DraftStore"},
                "callable": {"kind": "method", "name": "pending"},
                "payload_type": "()",
                "expansion": [],
                "branches": [],
                "return_type": {"value_type": "Vec<Draft>", "error_type": "DraftError"}
              },
              {
                "relation": "send",
                "target": {"kind": "external_entity", "entity_kind": "endpoint", "name": "Existing save boundary"},
                "payload_type": "Draft",
                "expansion": [],
                "branches": [
                  {
                    "condition": "save succeeds for this draft revision",
                    "edges": [
                      {
                        "relation": "write",
                        "target": {"kind": "planned_entity", "entity": "DraftStore"},
                        "callable": {"kind": "method", "name": "acknowledge"},
                        "payload_type": "DraftId",
                        "expansion": [],
                        "branches": []
                      }
                    ]
                  }
                ]
              }
            ],
            "branches": []
          }
        ]
      },
      {
        "title": "Recovery",
        "description": "After restart, recovery loads pending drafts and opens their contents in editor buffers. Recovery does not acknowledge unsaved records.",
        "source": {"kind": "planned_entity", "entity": "EditorRecovery"},
        "edges": [
          {
            "relation": "call",
            "target": {"kind": "planned_entity", "entity": "EditorRecovery"},
            "callable": {"kind": "method", "name": "restore"},
            "payload_type": "()",
            "expansion": [
              {
                "relation": "read",
                "target": {"kind": "planned_entity", "entity": "DraftStore"},
                "callable": {"kind": "method", "name": "pending"},
                "payload_type": "()",
                "expansion": [],
                "branches": [],
                "return_type": {"value_type": "Vec<Draft>", "error_type": "DraftError"}
              },
              {
                "relation": "send",
                "target": {"kind": "external_entity", "entity_kind": "endpoint", "name": "Existing editor buffer boundary"},
                "payload_type": "Draft",
                "expansion": [],
                "branches": []
              }
            ],
            "branches": []
          }
        ]
      }
    ]
  },
  "stages": [
    {
      "id": "storage",
      "title": "Establish durable draft storage",
      "tasks": [
        {
          "id": "draft-storage",
          "requires": [],
          "title": "Preserve pending edits through durable draft state.",
          "description": "Implement the shared storage operations before consumers use them. Verify persistence across reopen and revision-specific acknowledgement with the existing storage fixture.",
          "files": [
            {
              "action": "modify",
              "path": "src/draft_store.rs",
              "subtasks": [
                {
                  "operation": "expose",
                  "description": "durable capture, pending reads, and revision-specific acknowledgement.",
                  "entities": ["DraftStore"]
                },
                {
                  "operation": "test",
                  "action": "add",
                  "name": "pending_drafts_survive_reopen",
                  "category": "integration",
                  "behavior": "Persist two draft revisions, reopen the real storage fixture, acknowledge one, and verify the other remains pending.",
                  "covers_entities": ["DraftStore"]
                }
              ]
            }
          ]
        }
      ]
    },
    {
      "id": "consumers",
      "title": "Connect independent draft consumers",
      "tasks": [
        {
          "id": "capture-edits",
          "requires": ["draft-storage"],
          "title": "Capture editor changes through the draft owner.",
          "description": "Commit each edit before capture returns. Verify storage failure propagation through the editor and real draft fixture without sync or recovery.",
          "files": [
            {
              "action": "modify",
              "path": "src/editor.rs",
              "subtasks": [
                {
                  "operation": "route",
                  "description": "editor edits into durable draft capture.",
                  "entities": ["DocumentEditor"]
                },
                {
                  "operation": "test",
                  "action": "add",
                  "name": "capture_commits_before_returning",
                  "category": "integration",
                  "behavior": "Capture an edit through DocumentEditor, reopen its storage, and verify the draft body. Make storage unwritable and verify capture returns an error.",
                  "covers_entities": ["DocumentEditor", "DraftStore"]
                }
              ]
            }
          ]
        },
        {
          "id": "sync-drafts",
          "requires": ["draft-storage"],
          "title": "Retain failed saves through the sync worker.",
          "description": "Save pending revisions through the existing save boundary. Verify retry behavior using the storage fixture and the existing local save test server without editor capture or recovery.",
          "files": [
            {
              "action": "modify",
              "path": "src/sync.rs",
              "subtasks": [
                {
                  "operation": "route",
                  "description": "pending drafts through save and acknowledgement.",
                  "entities": ["SyncWorker"]
                },
                {
                  "operation": "test",
                  "action": "add",
                  "name": "failed_save_remains_pending",
                  "category": "integration",
                  "behavior": "Seed a draft directly in real storage, reject the first save at the local test server, then retry successfully. Verify it remains pending after failure and clears only after success.",
                  "covers_entities": ["SyncWorker", "DraftStore"]
                }
              ]
            }
          ]
        },
        {
          "id": "recover-drafts",
          "requires": ["draft-storage"],
          "title": "Restore pending edits through session recovery.",
          "description": "Open pending draft bodies through the existing buffer boundary. Seed storage directly and verify restored contents without capture or sync.",
          "files": [
            {
              "action": "modify",
              "path": "src/recovery.rs",
              "subtasks": [
                {
                  "operation": "route",
                  "description": "pending draft bodies into reopened buffers.",
                  "entities": ["EditorRecovery"]
                },
                {
                  "operation": "test",
                  "action": "add",
                  "name": "recovery_restores_pending_bodies",
                  "category": "integration",
                  "behavior": "Seed real storage, reopen the recovery session, and verify the expected buffer body while the draft remains pending.",
                  "covers_entities": ["EditorRecovery", "DraftStore"]
                }
              ]
            }
          ]
        }
      ]
    },
    {
      "id": "verification",
      "title": "Verify the assembled draft lifecycle",
      "tasks": [
        {
          "id": "verify-journey",
          "requires": ["capture-edits", "sync-drafts", "recover-drafts"],
          "title": "Verify draft survival across restart and retry.",
          "description": "Exercise capture, recovery, and sync together after their implementations complete. Verify the user-visible recovered body and durable state across a failed save and successful retry.",
          "files": [
            {
              "action": "add",
              "path": "tests/draft_journey.rs",
              "subtasks": [
                {
                  "operation": "test",
                  "action": "add",
                  "name": "draft_survives_restart_and_failed_save",
                  "category": "integration",
                  "behavior": "Capture an edit, close the editor, reopen recovery, and verify its body. Fail the first save, confirm the draft survives another reopen, retry successfully, and confirm it is no longer pending.",
                  "covers_entities": ["DocumentEditor", "DraftStore", "SyncWorker", "EditorRecovery"]
                }
              ]
            }
          ]
        }
      ]
    }
  ],
  "assumptions": [
    "The repository already registers src/draft_store.rs, src/editor.rs, src/sync.rs, and src/recovery.rs and exposes the listed owners.",
    "Draft, DraftId, and DraftError already exist, and DraftId identifies one immutable revision.",
    "Existing storage and local save-server fixtures support failure injection. Each consumer can compile and be tested with sibling implementations unchanged."
  ]
}
```

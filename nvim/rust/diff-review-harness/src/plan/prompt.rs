use super::document::{PlanDocument, PlanSubtask, PlanTask};
use anyhow::Result;
use serde::{Deserialize, Serialize};

/// Builds the Harness-owned planning and revision contracts.
pub struct PlanPrompt;

const USAGE_CONTRACT: &str = "Encode omitted Usage as JSON null. The <Omitted> text belongs only to rendered Markdown and is never a structured tool argument.";
const WALKTHROUGH_CONTRACT: &str = r#"Plan the work as a reviewer-readable implementation walkthrough, not a generic proposal. The PlanDocument JSON remains canonical and Harness renders Markdown as a read-only reviewer projection. Frame the plan around the domain object model and its ownership relationships, then describe each major runtime or data flow that crosses those boundaries. Do not force unrelated behavior into one linear flow. Connect every flow to the objects and owners in the model. Do not organize the plan around generic Problem, Refactoring, or Design Patterns sections.

Title:
- Replace the provisional title with a concise, specific plan name derived from the requested outcome.
- Name the resulting capability or change. Do not copy the original prompt or use a generic label such as Implementation plan.

Overview:
- Write exactly two or three sentences.
- Start with the feature, fix, or capability and its reviewer-visible outcome.
- Then explain the relevant limitation, unmet capability, or architectural motivation and the resulting role-level architecture.
- Use a before/now contrast only when the plan changes existing behavior.
- Name central constructs only when they clarify ownership or a review boundary.

Usage:
- Populate `usage` immediately after the overview for every caller-facing command, API, function, UI action, configuration, or text-producing workflow.
- Put one concrete call, command, or interaction in `command` and its expected observable result in `expected_result`.
- For a CLI, put the full executable command in `command`. Put a representative literal terminal transcript in `expected_result` that shows the actual stdout or stderr line structure with concrete sample values and the resulting exit status.
- Never use an imperative or prose summary such as `Print the geometry column...` as a CLI `expected_result`. Show the output itself. If the command intentionally emits no terminal output, write `<no stdout>` or `<no stderr>` and still include the exit status.
- Example CLI `expected_result`: `geometry_column: geometry\ncrs: EPSG:4326\nrow_count: 42\nexit status: 0`.
- For a visual, audio, hardware, or other non-text result, use a compact placeholder such as `<visual result: rendered preview updates with the selected theme>`.
- Encode `usage` as JSON null only when no caller-facing behavior applies. Harness alone renders that null as `<Omitted>`.

Dependencies:
- Write every dependency justification as exactly two sentences.
- Use the first sentence to state the dependency's architectural role and the second to explain why the existing code or standard library cannot satisfy that role.
- For Rust dependencies, write a valid Cargo version requirement. Harness resolves and preserves one exact published version when validating referenced APIs.

Object model and ownership:
- Establish ownership before describing flows. Model every relevant changed construct exactly once.
- Represent traits, interfaces, and abstract base classes as contract entities. Represent structs, classes, enums, configs, resources, caches, adapters, applications, and other instantiable or value roles as concrete entities.
- Set an entity `action` to add, modify, remove, or rename. A rename keeps the destination name in `name` and the existing source name in `renamed_from`. The projection marks added, modified, and renamed declarations with `*` and removed declarations with `~`.
- Give every entity its complete declaration identity, semantic kind, repository-relative path, and architectural description.
- The projection aligns repository-relative paths as bounded inline suffixes on entity declarations. Member and enum lines retain the full object-model column width, so do not shorten meaningful signatures merely to match the path alignment.
- Nest fields, methods, functions, constants, and properties under their owning entity. Record visibility, type, parameters, and meaningful return type. Omit a meaningless void or unit return.
- Declare shared operations once on their trait, interface, or abstract base. Do not repeat inherited operations on concrete implementations.
- Put concrete-only operations on an implementation only when they matter to the plan. Model capability differences as fields, capability values, strategies, or explicit results instead of asymmetric subsets of contract operations.
- Use `extends` for inheritance and `conforms_to` for contract conformance. Parameters and return types already express transient dependencies, so do not invent `uses` relationships.
- Write retained-state types with the walkthrough notation: unqualified `Type` for owned state, `&Type` for a retained non-owning reference, `@Type` for retained shared ownership, `Type?` for optional state, and `Type[]` for zero or more values.
- Harness derives UML indentation from concrete entities referenced by exactly one concrete entity. Keep type expressions precise so the reviewer projection can recover that dependency hierarchy without a separate ownership field.
- Put enum cases in `variants`. Put named payload fields under their variant because the variant exposes that data. Reserve private member visibility for state hidden by its owning entity.
- When direct accessors belong in the design, name them `noun()`, `noun_mut()`, and `set_noun(value)`, not `get_noun()`. Omit `noun_mut()` when the language or API does not expose a distinct mutable-reference operation.

Flows:
- Add one to three separate flows for the major affected runtime, data, request, event, persistence, recovery, or configuration paths.
- Give every flow a natural title such as Capture, Sync, or Recovery and exactly two substantive description sentences. Use the first sentence to state the actual entry point and observable outcome. Use the second sentence to explain the ownership boundary, architectural risk, or independent review reason that makes this flow distinct. Do not merely repeat the title or enumerate steps already visible in the diagram.
- Harness renders every flow as a rooted execution tree. Top-level steps are independent roots, edge `expansion` steps execute inside that relationship before its result returns, and step `branches` represent labeled alternative continuations.
- Give every step a concise action and the planned, workspace, or external entity that performs it. Use the step's `edges` for material runtime relationships and `branches` for mutually exclusive outcomes. Always send `edges`, `branches`, and each edge's `expansion` as complete arrays, including empty arrays.
- Put work performed by a callee inside that edge's `expansion`. Put every branch condition in `condition` and its nonempty continuation in `steps`. Never infer nesting from adjacent top-level steps.
- Planned and workspace repository paths occupy the aligned right column. External participants remain inline without repository paths.
- Choose the exact edge relation: `construct` creates the target, `call` invokes a callable, `read` obtains data through a callable, `write` mutates or persists through a callable, `send` transfers its named request or event, `emit` produces an observable effect, and `return` sends a result back to the target.
- Give every `call`, `read`, and `write` relation one structured `callable` with `kind: "function"` or `"method"` and its bare identifier in `name`. Never put parentheses, arguments, receiver names, or prose in the callable name.
- Put exactly one type entity in the target of every `construct`, `call`, `read`, and `write` edge. A changed target resolves through `{"kind":"planned_entity","entity":"entity_id"}`. An unchanged repository target uses `{"kind":"workspace_entity","entity_kind":"type","name":"TypeName","path":"src/file.rs","line":42}` with a repository-relative path and one-indexed declaration line. An external target uses `{"kind":"external_entity","entity_kind":"type","name":"TypeName","dependency":"package-name"}`; omit or null the dependency only when no package provenance applies.
- For a Rust callable, use the receiver type's owning Cargo package in `dependency` and the exact public Rust identifier in `callable.name`. Harness validates inherent methods, associated functions, and extension-trait methods against the plan's resolved dependency versions.
- Use `entity_kind: "endpoint"` only for external actors or destinations such as terminals, workers, storage boundaries, or schedulers. `send`, `emit`, and `return` may target endpoint entities.
- Put the value, event, result, or observable effect produced by each relationship in `result`. Use `{"kind":"type","name":"TypeName"}` for a named type and `{"kind":"text","text":"observable value"}` for other results. Inputs such as paths and files belong in the step action or callable context, never as a callable receiver.
- For changed orchestration functions such as `main`, show concrete construction and invocation edges instead of jumping from input parsing directly to a downstream result. Reuse planned entity names and real member names so reviewers can see who constructs, calls, reads, writes, sends, emits, or returns each value.
- Top-level step order controls reviewer presentation only. Runtime meaning comes from explicit edges, expansions, and branches, never adjacency or array position.
- Reuse entity names from the object model. Keep independent flows separate.
- Use compact action, result, value, and entity names so the vertical projection keeps its aligned owner column within 100 characters.

Tasks:
- Organize tasks by domain object and ownership responsibility, not by implementation phase, file bucket, or one forced code-flow sequence.
- When the plan adds, modifies, or removes any dependency, make the task that configures those dependency changes the first task in the plan.
- Make every task title an active architectural review claim. Prefer `<Active verb> <domain object> <with|through|in|across> <architectural role>.` Vary that shape only when another concise active construction states the ownership change more precisely.
- Follow every task title with one or two sentences that add architectural effect, motivation, constraint, or reviewer-visible consequence. Do not merely paraphrase the title.
- Use `now` only when contrasting changed existing behavior. For additions, state what the construct owns, enables, or connects without inventing prior behavior.
- Treat each task as an architectural claim that advances the object model or clarifies an ownership boundary.
- Treat each task file as a concrete source-file boundary. Use `action: "add"`, `"modify"`, or `"remove"` with one repository-relative `path`. Use `action: "rename"` with distinct repository-relative `from` and `to` paths. Renamed entities and subtasks belong to the destination path.
- Treat each implementation subtask as a local design move. Use exactly one supported operation: expose, encapsulate, move, centralize, distribute, extract, inline, split, merge, compose, embed, create, destroy, register, unregister, attach, detach, start, stop, route, resolve, defer, configure, relax, enable, disable, reuse, generalize, or specialize.
- Write each subtask description as the grammatical complement to its structured operation because renderers prepend the operation label. For `operation: "route"`, write `"the command path into GeoParquetInspector."`, not `"Route the command path into GeoParquetInspector."`.
- Express every concrete construct edit through an entity or nested member with an accurate kind such as class, struct, enum, trait, interface, test, app, config, function, method, constant, field, Resource, Cache, or Adapter. Top-level entities may use action add, modify, remove, or rename; rename requires `renamed_from` and the destination identifier in `name`. Nested members, variants, fields, dependencies, and tests remain add, modify, or remove.
- Keep file boundaries, construct kinds, and target names distinct so the renderer can colorize and navigate them independently.
- Include every function, type, configuration, application, test, and field that will be added, modified, or removed.
- Attach every planned entity to exactly one implementation subtask. Order tasks by the object model and ownership relationships. Within a task, order changes in the sequence that makes the ownership change easiest to review.

Modularity:
- Keep each ownership responsibility behind a narrow boundary and place related behavior with its owner.
- Do not scatter ownership across unrelated files, expose broad interfaces, hide capability differences in asymmetric implementations, or imply accidental exclusivity.
- Keep contract operations on their contract and preserve retained-state ownership notation.

Tests:
- Add only high-value tests tied to ownership boundaries and relevant flows. Omit tests when no meaningful runtime behavior needs verification.
- Prefer integration tests that exercise real ownership boundaries, failure paths, and end-to-end behavior through concrete modules.
- Add unit tests only for algorithms, data structures, state machines, parsers, or other complex isolated behavior.
- Do not test properties enforced by the language type system, exhaustive enums, constructors that only assign fields, trivial accessors, direct delegation, or static schema shape.
- When an invariant could be enforced by the type system but requires a test, strengthen the types or restructure the API first, then test only the remaining runtime behavior.
- For each unit test, identify the algorithm, data structure, state machine, parser, or isolated behavior under test, the collaborators mocked when applicable, and the behavior it validates.
- For each integration test, describe the end-to-end workflow through real modules, including key scenarios, observable results, failure paths, and edge cases.
- Put each test in its real repository-relative test file, classify it as unit or integration, describe its concrete behavior, and link it through `covers_entities` to the production entities it exercises.

Validation:
- Validate the PlanDocument as one ownership model before submission.
- Ensure every changed construct appears under exactly one task subtask and every file uses a repository-relative path.
- Ensure every major independent flow has its own description.
- Ensure task claims, entity fields, flow values, dependencies, tests, and assumptions describe the same boundaries.
- Revise any plan that violates the modularity rules, produces flow lines over 100 characters, or collapses failures and user-visible behavior into vague `handle`, `support`, `make`, or `update` wording."#;
const EDIT_CONTRACT: &str = r#"Use harness_plan_edit as one resource-oriented mutation. Put additions under `add`, field patches under `modify`, and semantic names under `remove`. Every resource supports the same add, modify, and remove lifecycle. Nested resources use the same structure inside their owner. A complete task file uses its tagged `action` plus `path`, or `from` and `to` for a rename. A `files.modify` patch selects the current destination with `path` and replaces its tagged operation through `change` when needed. Use `members` for fields, methods, functions, constants, and properties. Use `variants` only on enum entities. Use `name` when adding an entity, member, variant, or dependency. Use `entity`, `member`, `variant`, or `dependency` when selecting an existing value, and the corresponding singular selector `field`, `flow`, `step`, `task`, or `subtask` for other modifications. Every `subtasks.modify` entry must include its `subtask` selector. A test-subtask modification must also retain `operation: "test"`. `entities`, flow-step `edges`, flow-step `branches`, edge `expansion`, and branch `steps` are always complete replacement arrays, never add/modify/remove resource mutations. Implementation-subtask `entities` may contain only planned program entities. Never put package names there or create a synthetic config entity solely to group dependencies. A dependency-only manifest subtask may use an empty entity array because Harness derives dependency ownership from the manifest task file. Use `operation` for a subtask design move.

Represent each concrete test as its own flat task-file subtask with `operation: "test"`, `action`, `name`, `category`, and `behavior` directly on that subtask. Never model a concrete test as an `entity_change`, nest a `tests` collection, or create a top-level tests resource. `covers_entities` optionally traces one test to production entities without establishing ownership. Tests inherit their source path and architectural task from their parent file and task. Harness generates internal IDs. Never invent or send ID fields. Never use JSON Patch `op`, `path`, or `value` fields.

```json
{
  "plan_id": "plan-uuid",
  "expected_version": 1,
  "plan": {
    "modify": {
      "title": "Preserve editor drafts across closed buffers",
      "overview": "Add a durable draft cache that survives buffer closure and feeds retryable background saves.",
      "usage": {
        "command": "draft-sync status --document doc-42",
        "expected_result": "Print one pending draft for doc-42 until the background save succeeds."
      }
    }
  },
  "entity_changes": {
    "add": [
      {
        "action": "add",
        "kind": "resource",
        "name": "DraftCache",
        "description": "Owns pending drafts independently from editor buffers.",
        "path": "src/draft_sync.rs",
        "members": [
          {
            "action": "add",
            "kind": "method",
            "name": "store",
            "description": "Store one pending draft.",
            "visibility": "public",
            "parameters": [{"name": "draft", "type": "DraftChange"}]
          }
        ]
      },
      {
        "action": "modify",
        "kind": "struct",
        "name": "DocumentEditor",
        "description": "Routes edits into durable draft state.",
        "path": "src/editor/session.rs",
        "members": [
          {
            "action": "modify",
            "kind": "method",
            "name": "apply_edit",
            "description": "Persist each edit before buffer lifetime can end.",
            "visibility": "public"
          }
        ]
      },
      {
        "action": "add",
        "kind": "enum",
        "name": "DraftStatus",
        "description": "Tracks whether one durable draft still needs persistence.",
        "path": "src/draft_sync.rs",
        "variants": [
          {
            "action": "add",
            "name": "Pending",
            "description": "Marks a draft that still needs persistence.",
            "fields": []
          },
          {
            "action": "add",
            "name": "Failed",
            "description": "Carries the reason the last persistence attempt failed.",
            "fields": [
              {
                "action": "add",
                "name": "message",
                "type": "String"
              }
            ]
          }
        ]
      }
    ]
  },
  "dependencies": {
    "add": [
      {
        "action": "add",
        "name": "tokio",
        "version": "1",
        "manifest": "Cargo.toml",
        "license": "MIT",
        "justification": "Runs asynchronous draft persistence and retry work. The existing synchronous runtime cannot schedule retries without blocking editor work."
      }
    ]
  },
  "flows": {
    "add": [{
      "title": "Draft capture",
      "description": "Persist an edit before its source buffer closes. Typed edges expose the editor-to-cache boundary and its durable storage effect.",
      "steps": [
        {
          "action": "Apply editor change",
          "target": {"kind": "planned_entity", "entity": "DocumentEditor"},
          "edges": [{
            "relation": {
              "kind": "call",
              "callable": {"kind": "method", "name": "store"}
            },
            "target": {"kind": "planned_entity", "entity": "DraftCache"},
            "expansion": [{
              "action": "Persist pending draft",
              "target": {"kind": "planned_entity", "entity": "DraftCache"},
              "edges": [
                {
                  "relation": {
                    "kind": "write",
                    "callable": {"kind": "method", "name": "persist"}
                  },
                  "target": {
                    "kind": "external_entity",
                    "entity_kind": "type",
                    "name": "DraftStore",
                    "dependency": null
                  },
                  "expansion": [],
                  "result": null
                }
              ],
              "branches": []
            }],
            "result": {"kind": "type", "name": "DraftId"}
          }],
          "branches": []
        }
      ]
    }]
  },
  "tasks": {
    "add": [{
      "title": "Own pending drafts through durable state.",
      "description": "DraftCache gives unsaved edits a lifetime independent from editor buffers.",
      "files": [
        {
          "action": "add",
          "path": "src/draft_sync.rs",
          "subtasks": [{
            "operation": "create",
            "description": "the durable draft owner.",
            "entities": ["DraftCache", "DraftStatus"]
          }, {
            "operation": "test",
            "action": "add",
            "name": "retries_failed_draft_after_backoff",
            "category": "unit",
            "behavior": "The draft state machine retries a failed draft only after its backoff expires.",
            "covers_entities": ["DraftCache", "DraftStatus"]
          }]
        },
        {
          "action": "modify",
          "path": "src/editor/session.rs",
          "subtasks": [{
            "operation": "route",
            "description": "editor changes into the draft owner.",
            "entities": ["DocumentEditor"]
          }]
        },
        {
          "action": "modify",
          "path": "Cargo.toml",
          "subtasks": [{
            "operation": "configure",
            "description": "the asynchronous runtime."
          }]
        },
        {
          "path": "tests/draft_recovery.rs",
          "action": "add",
          "subtasks": [{
            "operation": "test",
            "action": "add",
            "name": "restores_pending_draft_after_reopen",
            "category": "integration",
            "behavior": "Closing and reopening a document restores its pending draft through real editor and cache modules.",
            "covers_entities": ["DraftCache", "DocumentEditor"]
          }, {
            "operation": "test",
            "action": "add",
            "name": "does_not_restore_saved_draft",
            "category": "integration",
            "behavior": "Reopening a document omits a draft that completed persistence before the original session closed.",
            "covers_entities": ["DraftCache", "DocumentEditor"]
          }]
        }
      ]
    }]
  },
  "assumptions": {
    "add": ["Draft persistence uses the existing workspace storage boundary."]
  }
}
```"#;

/// Defines the execution boundary that shapes one accepted-plan prompt.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanExecutionPromptKind {
    Start,
    Continue,
    ResumeAfterInterruption,
}

/// Build one scheduler-aware accepted-plan execution prompt.
pub fn execution_prompt(
    kind: PlanExecutionPromptKind,
    execution_id: &str,
    active_task: Option<&PlanTask>,
    document: &PlanDocument,
) -> Result<String> {
    let boundary = match kind {
        PlanExecutionPromptKind::Start => "Begin accepted-plan execution.",
        PlanExecutionPromptKind::Continue => "Continue accepted-plan execution.",
        PlanExecutionPromptKind::ResumeAfterInterruption => {
            "Resume accepted-plan execution after an interruption. Preserve completed workspace changes and evidence. Do not repeat finished actions. Continue the same active task."
        }
    };
    let active_entity_id = active_task
        .into_iter()
        .flat_map(|task| &task.files)
        .flat_map(|file| &file.subtasks)
        .flat_map(PlanSubtask::owned_entity_ids)
        .collect::<std::collections::HashSet<_>>();
    let active_entity_change = document
        .entity_changes
        .iter()
        .filter(|entity| {
            active_entity_id.contains(&entity.entity_id) || active_entity_id.contains(&entity.name)
        })
        .collect::<Vec<_>>();
    let active_work = serde_json::json!({
        "task": active_task,
        "entity_changes": active_entity_change,
    });
    Ok(format!(
        "{boundary} Execution ID: {execution_id}. Complete the active whole task before calling harness_plan_task_report with detailed subtask, entity, path, and test evidence. Reference each planned test result through its test_subtask_id. Call harness_plan_deviation before departing from accepted intent. Call harness_goal_complete only after the scheduler has no incomplete tasks.\n\nActive task:\n```json\n{}\n```\n\nEffective canonical PlanDocument:\n```json\n{}\n```",
        serde_json::to_string_pretty(&active_work)?,
        serde_json::to_string_pretty(document)?,
    ))
}

impl PlanPrompt {
    /// Build a decision-complete planning request for one user objective.
    pub fn draft(request: &str) -> String {
        format!(
            r#"You are planning a software change in Harness Plan mode. The retained execution authorization still governs every command and file operation. Planning does not imply read-only access.

Explore the repository before asking questions. Resolve discoverable facts from the code. You may write supporting planning material when the retained authorization permits it. Ask only when a product decision materially changes the implementation. When feedback is required, call harness_question_ask with one to three concise questions, two or three mutually exclusive choices per structured question, and a recommended choice first. End that turn after requesting feedback.

Harness already created the canonical PlanDocument and owns its plan ID, version, and original prompt. The prompt is Harness-owned render context and is not a model-editable field. Build the semantic plan only through harness_plan_edit. Model each changed program construct once as a ProgramEntityChange with its add, modify, remove, or rename action. For a rename, put the existing identifier in `renamed_from` and the destination identifier in `name`; never encode one rename as separate remove and add entities. Model every package decision once as a top-level dependency change with its version, manifest, license, and justification. Nest ordinary member changes under their owning entity and enum cases under the enum's dedicated variants collection. Express inheritance and conformance directly on that entity. Use tagged planned_entity, workspace_entity, or external_entity references in flows. Reserve workspace_entity for unchanged repository constructs and record their entity kind, name, repository-relative path, and one-indexed declaration line. Attach every entity to exactly one subtask. Harness derives dependency ownership by matching each manifest to exactly one task file. Keep dependencies, entities, independent flows, architectural tasks, file boundaries, optional high-value tests, and assumptions aligned. Harness owns internal IDs. {USAGE_CONTRACT} Do not use provider task updates as the plan. When the document passes submission validation, call harness_plan_submit with the exact plan_id and expected_version. Ordinary prose and provider checklists do not submit a plan.

{WALKTHROUGH_CONTRACT}

{EDIT_CONTRACT}

User request:
{request}"#
        )
    }

    /// Continue one paused planning conversation with the user's selected feedback.
    pub fn feedback(request: &str, answer: &str) -> String {
        format!(
            r#"Continue the existing Harness Plan mode conversation. The Harness already recorded and consumed the user's answers below. Do not call harness_question_answer or harness_question_withdraw for them. Incorporate the answers through semantic harness_plan_edit operations. {USAGE_CONTRACT} If another material product decision remains, call harness_question_ask and end the turn. Otherwise call harness_plan_submit with the exact canonical plan ID and version.

{WALKTHROUGH_CONTRACT}

{EDIT_CONTRACT}

Original request:
{request}

{answer}"#
        )
    }

    /// Build a follow-up turn that may preserve or explicitly mutate pending decisions.
    pub fn clarification(request: &str, elicitation_json: &str, question: &str) -> String {
        mutable_elicitation_prompt(Some(request), elicitation_json, question)
    }

    /// Build an ordinary follow-up turn against one pending Harness decision set.
    pub fn question_follow_up(elicitation_json: &str, question: &str) -> String {
        mutable_elicitation_prompt(None, elicitation_json, question)
    }

    /// Build a semantic revision request from reviewed plan state.
    pub fn revision(
        document_json: &str,
        annotation_json: &str,
        overall_comment: Option<&str>,
    ) -> String {
        let comment = overall_comment
            .filter(|value| !value.trim().is_empty())
            .unwrap_or("None");
        format!(
            r#"Revise the saved canonical plan in Harness Plan mode. Resolve every annotation and overall comment with semantic harness_plan_edit operations, then call harness_plan_submit with the exact resulting plan ID and version.

{WALKTHROUGH_CONTRACT}

{EDIT_CONTRACT}

Overall review comment:
{comment}

Current canonical PlanDocument:
```json
{document_json}
```

Semantic annotations:
{annotation_json}"#
        )
    }

    /// Prepend the complete canonical plan so every provider can discover and edit it.
    pub fn with_active_document(prompt: String, document_json: &str) -> String {
        format!("Active canonical PlanDocument:\n```json\n{document_json}\n```\n\n{prompt}")
    }
}

fn mutable_elicitation_prompt(
    planning_request: Option<&str>,
    elicitation_json: &str,
    question: &str,
) -> String {
    let workflow_boundary = if planning_request.is_some() {
        "Do not continue or submit the plan during this turn."
    } else {
        "Do not continue the original request during this turn."
    };
    let planning_context = planning_request
        .map(|request| format!("\nOriginal planning request:\n{request}\n"))
        .unwrap_or_default();
    format!(
        r#"The user is responding while a Harness question set remains pending. Treat the pending elicitation as mutable decision state, not as a modal lock.

Remain in Harness Plan mode. The retained execution authorization governs repository access. Answer the user's follow-up directly, using repository evidence when relevant. {workflow_boundary}

After answering, choose exactly one outcome:

1. Preserve
   Make no control-tool call when the existing questions and options remain material and valid.

2. Answer
   Call harness_question_answer only when the user explicitly and unambiguously answers a pending question. Do not convert tentative language, discussion, or model preference into an answer.

3. Replace
   Call harness_question_ask with the complete revised question set when the user requests changes or when clarification changes which questions or options remain material. Preserve question IDs only when their meaning remains unchanged. End the turn after replacement.

4. Withdraw
   Call harness_question_withdraw when no material user decision remains. Provide a concise reason grounded in an explicit user instruction, delegated judgment, repository evidence, or a resolved requirement. End the turn after withdrawal.

Never select an option merely because you recommend it. Never retain a question that no longer affects the implementation. Never withdraw a question merely to avoid asking for input. "Choose for me" explicitly delegates the decision and may resolve the question. Tentative language such as "I'm leaning toward" does not resolve the question. If only part of the question set changes, replace the complete set while retaining unchanged questions and stable IDs. If a new material decision emerges, include it in the complete replacement set. After any control-tool call, let the Harness present or resume the resulting workflow.
{planning_context}
Pending elicitation:
{elicitation_json}

User follow-up:
{question}"#
    )
}

#[cfg(test)]
mod test {
    use super::{PlanExecutionPromptKind, PlanPrompt, execution_prompt};

    #[test]
    fn planning_contract_requires_structured_submission_without_native_mode() {
        let prompt = PlanPrompt::draft("Refactor the renderer");
        assert!(prompt.contains("Plan mode"));
        assert!(prompt.contains("harness_plan_submit"));
        assert!(prompt.contains("harness_question_ask"));
        assert!(prompt.contains("Refactor the renderer"));
        assert!(!prompt.contains("harness_plan_create"));
        assert!(prompt.contains("ProgramEntityChange"));
        assert!(prompt.contains("dedicated variants collection"));
        assert!(prompt.contains("Omit tests when no meaningful runtime behavior"));
        assert!(prompt.contains("Never model a concrete test as an `entity_change`"));
        assert!(prompt.contains("Prefer integration tests"));
        assert!(prompt.contains("type system"));
        assert!(prompt.contains("reviewer-readable implementation walkthrough"));
        assert!(prompt.contains("Replace the provisional title"));
        assert!(prompt.contains("original prompt"));
        assert!(prompt.contains("Write exactly two or three sentences"));
        assert!(prompt.contains("representative literal terminal transcript"));
        assert!(
            prompt.contains("actual stdout or stderr line structure with concrete sample values")
        );
        assert!(prompt.contains("Never use an imperative or prose summary"));
        assert!(prompt.contains("<no stdout>"));
        assert!(prompt.contains("geometry_column: geometry\\ncrs: EPSG:4326"));
        assert!(prompt.contains("every dependency justification as exactly two sentences"));
        assert!(
            prompt
                .contains("make the task that configures those dependency changes the first task")
        );
        assert_eq!(prompt.matches("\nDependencies:\n").count(), 1);
        assert!(prompt.contains("Model every relevant changed construct exactly once"));
        assert!(prompt.contains("`@Type` for retained shared ownership"));
        assert!(prompt.contains("paths as bounded inline suffixes"));
        assert!(prompt.contains("retain the full object-model column width"));
        assert!(prompt.contains("derives UML indentation from concrete entities"));
        assert!(!prompt.contains("exclusive_owner_entity"));
        assert!(prompt.contains("one to three separate flows"));
        assert!(prompt.contains("renders every flow as a rooted execution tree"));
        assert!(prompt.contains("edge `expansion` steps execute inside that relationship"));
        assert!(prompt.contains("step `branches` represent labeled alternative continuations"));
        assert!(prompt.contains("Never infer nesting from adjacent top-level steps"));
        assert!(prompt.contains("structured `callable`"));
        assert!(prompt.contains("bare identifier"));
        assert!(prompt.contains("exactly one type entity"));
        assert!(prompt.contains("`entity_kind: \"endpoint\"`"));
        assert!(prompt.contains("\"dependency\":\"package-name\""));
        assert!(prompt.contains("exactly two substantive description sentences"));
        assert!(prompt.contains("actual entry point and observable outcome"));
        assert!(
            prompt.contains("ownership boundary, architectural risk, or independent review reason")
        );
        assert!(prompt.contains(r#"{"kind":"type","name":"TypeName"}"#));
        assert!(prompt.contains(r#"{"kind":"text","text":"observable value"}"#));
        assert!(prompt.contains("show concrete construction and invocation edges"));
        assert!(
            prompt.contains("Runtime meaning comes from explicit edges, expansions, and branches")
        );
        assert!(prompt.contains("`construct` creates the target"));
        assert!(prompt.contains("aligned owner column within 100 characters"));
        assert!(prompt.contains("Organize tasks by domain object and ownership responsibility"));
        assert!(prompt.contains("renderers prepend the operation label"));
        assert!(prompt.contains("Include every function, type, configuration"));
        assert!(prompt.contains("Validate the PlanDocument as one ownership model"));
        assert!(prompt.contains("collaborators mocked when applicable"));
        assert!(prompt.contains("end-to-end workflow through real modules"));
        assert!(prompt.contains("strengthen the types or restructure the API"));
        assert_eq!(prompt.matches("\nTests:\n").count(), 1);
        assert_eq!(prompt.matches("\nModularity:\n").count(), 1);
        assert_eq!(prompt.matches("\nValidation:\n").count(), 1);
        assert!(prompt.contains("complete replacement array"));
        assert!(prompt.contains("flow-step `branches`"));
        assert!(prompt.contains("edge `expansion`"));
        assert!(prompt.contains("Every `subtasks.modify` entry"));
        assert!(prompt.contains("Never put package names there"));
        assert!(prompt.contains(r#""entity_changes": {"#));
        assert!(prompt.contains(r#""add": [{"#));
        assert!(prompt.contains("Never use JSON Patch"));
        assert!(!prompt.contains("collaborationMode"));
    }

    #[test]
    fn feedback_continues_the_same_planning_contract() {
        let prompt = PlanPrompt::feedback(
            "Refactor the renderer",
            "Planning feedback:\n- Migration: Use a staged migration",
        );
        assert!(prompt.contains("Refactor the renderer"));
        assert!(prompt.contains("Use a staged migration"));
        assert!(prompt.contains("harness_question_ask"));
        assert!(prompt.contains("harness_plan_submit"));
        assert!(prompt.contains("Encode omitted Usage as JSON null"));
        assert!(prompt.contains(r#""operation": "test""#));
        assert!(prompt.contains(r#""category": "unit""#));
        assert!(prompt.contains(r#""category": "integration""#));
        assert!(prompt.contains(r#""covers_entities": ["DraftCache", "DocumentEditor"]"#));
        assert!(!prompt.contains(r#""tests": {"#));
        assert!(prompt.contains("draft-sync status --document doc-42"));
        assert!(prompt.contains(r#""operation": "configure""#));
        assert!(prompt.contains(r#""name": "tokio""#));
        assert!(prompt.contains("already recorded and consumed"));
        assert!(prompt.contains("Do not call harness_question_answer"));
        assert!(prompt.contains("reviewer-readable implementation walkthrough"));
        assert_eq!(prompt.matches("Planning feedback:").count(), 1);
    }

    #[test]
    fn active_artifact_context_exposes_the_canonical_document() {
        let prompt = PlanPrompt::with_active_document("Why?".into(), "{\"plan_id\":\"plan\"}");
        assert!(prompt.contains("\"plan_id\":\"plan\""));
        assert!(prompt.ends_with("Why?"));
    }

    #[test]
    fn revision_sends_canonical_json_and_semantic_annotations_without_markdown() {
        let prompt = PlanPrompt::revision(
            r#"{"plan_id":"plan","entity_changes":[]}"#,
            r#"[{"json_path":"/entity_changes/0","label":"PlanDocument","target":{"target_type":"entity","entity_id":"plan_document"}}]"#,
            Some("Tighten ownership"),
        );

        assert!(prompt.contains(r#""plan_id":"plan""#));
        assert!(prompt.contains("Semantic annotations"));
        assert!(prompt.contains("PlanDocument"));
        assert!(prompt.contains("reviewer-readable implementation walkthrough"));
        assert!(prompt.contains("Validate the PlanDocument as one ownership model"));
        assert!(!prompt.contains("Current rendered plan"));
    }

    #[test]
    fn clarification_preserves_the_pending_decision_boundary() {
        let prompt = PlanPrompt::clarification("Refactor", "{\"question\":\"Migration?\"}", "Why?");
        assert!(prompt.contains("mutable decision state"));
        assert!(prompt.contains("harness_question_answer"));
        assert!(prompt.contains("harness_question_ask"));
        assert!(prompt.contains("harness_question_withdraw"));
        assert!(prompt.contains("Tentative language"));
        assert!(prompt.contains("Why?"));
        assert!(prompt.contains("Migration?"));
    }

    #[test]
    fn ordinary_question_follow_up_omits_planning_language() {
        let prompt = PlanPrompt::question_follow_up("{\"question\":\"Format?\"}", "Use JSON");
        assert!(prompt.contains("Do not continue the original request"));
        assert!(!prompt.contains("Original planning request"));
        assert!(prompt.contains("harness_question_answer"));
    }

    #[test]
    fn execution_prompt_reanchors_start_and_resume_to_the_active_task() {
        let document = super::super::document::test_fixture("plan", "Overview");
        let task = &document.tasks[0];
        let start = execution_prompt(
            PlanExecutionPromptKind::Start,
            "execution",
            Some(task),
            &document,
        )
        .unwrap();
        assert!(start.contains("Begin accepted-plan execution"));
        assert!(start.contains(&task.title));

        let resumed = execution_prompt(
            PlanExecutionPromptKind::ResumeAfterInterruption,
            "execution",
            Some(task),
            &document,
        )
        .unwrap();
        assert!(resumed.contains("Preserve completed workspace changes"));
        assert!(resumed.contains("Do not repeat finished actions"));
        assert!(resumed.contains("Continue the same active task"));
    }
}

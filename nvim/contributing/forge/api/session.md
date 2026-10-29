# Session methods

Each method below has a reachable production or live integration-control caller recorded in the [usage ledger](../../../../md/rename-usage-ledger.json). The transport metadata comes from the shared contract. These entries provide the inspected handler summary. Field-by-field schema expansion and executable examples remain tracked in the [implementation plan](../../../../rename.md).

All calls inherit the [request lifecycle and failure contract](lua-client.md#requestspec-callback-progress). Method failures do not select another session or close an otherwise valid connection.

## `session.agent.followup`

Submit follow-up work to an existing agent.

- **Context:** Explicit existing session/provider.
- **Input:** target and message.
- **Result and effects:** Returns the follow-up operation result.
- **Boundary:** The agent's current lifecycle determines whether work starts or is queued.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/lua/forge/views/harness/controller.lua:1414](../../../../nvim/lua/forge/views/harness/controller.lua).

<!-- detailed-contract -->
### Contract

Requires nonempty `target` and `message` strings. The target resolves within this root session. Returns `{ "agent": Agent, "task": AgentTask, "status": "admitted" }` after durable admission, with `replayed: true` when the same identity and work were already admitted. The child retains its identity and receives a new task, queued if earlier work is active. Reusing an operation identity with different target or text fails. Unknown targets, invalid profiles, capacity, and persistence failures remain explicit. Supply a stable `operation_id` string for recovery. Omission generates a new UUID, so a repeated request without that identity cannot establish replay equivalence.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [session_api_examples.lua](../../../../nvim/tests/forge/session_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.agent.followup",
  "params": {
    "message": "Verify the fixture result",
    "target": "8b0b8fa9-17f5-49e2-a664-1a093708150f",
    "_command": {
      "id": "example-agent-followup"
    },
    "operation_id": "example-agent-followup"
  },
  "id": 24,
  "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72"
}
```

```json
{
  "id": 24,
  "result": {
    "task": {
      "source": "user",
      "agent_id": "8b0b8fa9-17f5-49e2-a664-1a093708150f",
      "ordinal": 2,
      "parent_exchange_id": "05fb1ada-9185-4a91-8075-6b1bb82dcc1e",
      "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72",
      "final_response": null,
      "message": "Verify the fixture result",
      "last_activity_at_ms": null,
      "completed_at_ms": null,
      "id": "e94081c6-c1ee-4edb-98d2-55d41f7dcb10",
      "operation_id": "example-agent-followup",
      "created_at_ms": 1793254175959,
      "error": null,
      "parent_reported": false,
      "state": "queued"
    },
    "warning": null,
    "agent": {
      "definition": "worker",
      "updated_at_ms": 1793254175942,
      "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72",
      "nickname": null,
      "task_name": "worker-1",
      "created_at_ms": 1793254175942,
      "task_path": "/root/worker-1",
      "id": "8b0b8fa9-17f5-49e2-a664-1a093708150f",
      "selection": {
        "model": "default",
        "reasoning_effort": "medium",
        "provider": "mock",
        "context_window": null,
        "access": {
          "readable_directory": [],
          "writable_directory": [],
          "windows_sandbox": "elevated",
          "sandbox": false,
          "read_access": "full",
          "write_access": "workspace"
        },
        "service_tier": "default",
        "profile": "worker",
        "approval_mode": "ask",
        "instructions": "Complete the assigned task and report the result and verification."
      },
      "state": "starting",
      "provider_thread_id": null,
      "inbox": [
        {
          "created_at_ms": 1793254175954,
          "task_id": null,
          "ordinal": 1,
          "id": "example-agent-message",
          "message": "Keep the result concise",
          "delivery": "queued"
        }
      ],
      "source": "user"
    },
    "status": "admitted"
  }
}
```

<!-- /captured-example -->

## `session.agent.interrupt`

Interrupt an explicitly identified agent.

- **Context:** Explicit existing session/provider.
- **Input:** target.
- **Result and effects:** Returns the interruption operation result.
- **Boundary:** Interrupting does not erase the agent's recorded history.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/lua/forge/views/harness/controller.lua:1414](../../../../nvim/lua/forge/views/harness/controller.lua).

<!-- detailed-contract -->
### Contract

Requires nonempty `target` (string). Returns `{ "status": "interrupting", "target": target }` after requesting interruption within the selected root session. This acknowledges the interruption request, not provider cleanup completion. Unknown targets and cancellation failures are explicit. The operation does not erase saved child history. Supply a stable `operation_id` string for recovery. Omission generates a new UUID, so a repeated request without that identity cannot establish replay equivalence.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [session_api_examples.lua](../../../../nvim/tests/forge/session_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.agent.interrupt",
  "params": {
    "target": "8b0b8fa9-17f5-49e2-a664-1a093708150f",
    "_command": {
      "id": "example-agent-interrupt"
    },
    "operation_id": "example-agent-interrupt"
  },
  "id": 25,
  "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72"
}
```

```json
{
  "id": 25,
  "result": {
    "status": "interrupting",
    "target": "8b0b8fa9-17f5-49e2-a664-1a093708150f"
  }
}
```

<!-- /captured-example -->

## `session.agent.interrupt_all`

Interrupt the selected session's agents.

- **Context:** Explicit existing session/provider.
- **Input:** The explicit parent-session context.
- **Result and effects:** Returns the aggregate interruption operation result.
- **Boundary:** The scope is this session's agents, not unrelated host sessions.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/lua/forge/views/harness/controller.lua:1414](../../../../nvim/lua/forge/views/harness/controller.lua).

<!-- detailed-contract -->
### Contract

Accepts an empty parameter object. Returns `{ "status": "interrupting" }` after requesting interruption for each managed child in this root session. The primary agent and unrelated root sessions are outside that iteration. A child interruption failure stops the call and can leave earlier interruption requests already applied. The response does not establish provider cleanup completion. Supply a stable `operation_id` string for recovery. Omission generates a new UUID, so a repeated request without that identity cannot establish replay equivalence.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [session_api_examples.lua](../../../../nvim/tests/forge/session_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.agent.interrupt_all",
  "params": {
    "_command": {
      "id": "example-agent-interrupt-all"
    },
    "operation_id": "example-agent-interrupt-all"
  },
  "id": 26,
  "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72"
}
```

```json
{
  "id": 26,
  "result": {
    "status": "interrupting"
  }
}
```

<!-- /captured-example -->

## `session.agent.message.send`

Send a message to an existing agent.

- **Context:** Explicit existing session/provider.
- **Input:** target and message.
- **Result and effects:** Returns the routed agent-operation result.
- **Boundary:** A message is distinct from starting a new agent or resuming an idle one.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/lua/forge/views/harness/controller.lua:1414](../../../../nvim/lua/forge/views/harness/controller.lua).

<!-- detailed-contract -->
### Contract

Requires nonempty `target` and `message` strings. Returns the saved message object with `id`, `ordinal`, `message`, `delivery`, `task_id`, and `created_at_ms`. Delivery is `steered` when an active provider accepts native steering, otherwise `queued` for an input boundary. This does not start an idle child or create a follow-up task. Repeating the same identity/text returns the saved message. Different text under that identity fails. Unknown targets and persistence failures are explicit. Provider steering rejection retains the message as queued. Supply a stable `operation_id` string for recovery. Omission generates a new UUID, so a repeated request without that identity cannot establish replay equivalence.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [session_api_examples.lua](../../../../nvim/tests/forge/session_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.agent.message.send",
  "params": {
    "message": "Keep the result concise",
    "target": "8b0b8fa9-17f5-49e2-a664-1a093708150f",
    "_command": {
      "id": "example-agent-message"
    },
    "operation_id": "example-agent-message"
  },
  "id": 23,
  "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72"
}
```

```json
{
  "id": 23,
  "result": {
    "created_at_ms": 1793254175954,
    "task_id": null,
    "ordinal": 1,
    "id": "example-agent-message",
    "message": "Keep the result concise",
    "delivery": "queued"
  }
}
```

<!-- /captured-example -->

## `session.agent.model.list`

List models usable for a child agent.

- **Context:** Explicit existing session/provider.
- **Input:** provider and the explicit parent-session context.
- **Result and effects:** Returns the agent-model catalog.
- **Boundary:** Provider availability and discovery failure remain explicit.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/subagent_selection.lua:112](../../../../nvim/lua/forge/views/harness/subagent_selection.lua).

<!-- detailed-contract -->
### Contract

Requires `provider` (string), parsed as a supported backend kind. Returns an array of [BackendModel](runtime-schemas.md#backendmodel) for launching a child using the current session's workspace/access policy. Discovery may contact or launch that provider and has a 30-second deadline. It does not create a child agent or send a prompt. Unknown providers and discovery failures fail explicitly. It emits no request progress.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [session_api_examples.lua](../../../../nvim/tests/forge/session_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.agent.model.list",
  "params": {
    "provider": "mock"
  },
  "id": 9,
  "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72"
}
```

```json
{
  "id": 9,
  "result": {
    "capability": {
      "permission_control": true,
      "background_terminals": true,
      "approval_mode_list": [
        "ask",
        "deny",
        "allow"
      ],
      "native_fork": true,
      "native_compact": true,
      "os_sandbox": false,
      "compaction": {
        "lifecycle": true,
        "background": false,
        "metrics": true,
        "summary": true
      },
      "agent": {
        "spawn": true,
        "input": true,
        "observe": true,
        "interrupt": true,
        "parallel": true
      },
      "catalog": {
        "skill": false,
        "mcp": false,
        "live_mcp_mutation": false
      },
      "native_steer": true,
      "native_turn_rollback": true,
      "native_goal": false,
      "model_selection": true,
      "effort_selection": true,
      "fast_mode": true,
      "ultrafast_mode": true
    },
    "models": [
      {
        "description": "Deterministic Harness test model.",
        "selected_reasoning": null,
        "default_context_window": "default",
        "selected_context_window": null,
        "context_window": [
          {
            "id": "default",
            "token_limit": 100000
          }
        ],
        "reasoning": [
          "low",
          "medium",
          "high"
        ],
        "is_default": true,
        "id": "mock-model",
        "default_reasoning": "medium",
        "service_tiers": [
          "default",
          "fast",
          "ultrafast"
        ],
        "vision": false
      }
    ]
  }
}
```

<!-- /captured-example -->

## `session.agent.start`

Start an agent within the selected session.

- **Context:** Explicit existing session/provider.
- **Input:** The agent launch request accepted by the coordinator.
- **Result and effects:** Returns launch information and ensuing agent state.
- **Boundary:** The agent receives its own execution identity under the parent session.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/lua/forge/views/harness/controller.lua:1396](../../../../nvim/lua/forge/views/harness/controller.lua).

<!-- detailed-contract -->
### Contract

Requires nonempty `text` (string) and accepts `profile` (string or null). Unknown fields other than the transport `operation_id` fail decoding. The profile selects the child provider/model and access configuration. This creates fresh child work without inheriting the parent transcript. Returns `{ "agent": Agent, "task": AgentTask, "status": "admitted" }` after durable admission. Repeating the same operation and captured work returns that result with `replayed: true`. Reusing the identity for different work fails. Admission may queue provider execution and does not mean the child finished. Profile/provider resolution, capacity, persistence, and ownership failures remain explicit. Supply a stable `operation_id` string for recovery. Omission generates a new UUID, so a repeated request without that identity cannot establish replay equivalence.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [session_api_examples.lua](../../../../nvim/tests/forge/session_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.agent.start",
  "params": {
    "text": "Return the fixture result",
    "_command": {
      "id": "example-agent"
    },
    "operation_id": "example-agent"
  },
  "id": 22,
  "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72"
}
```

```json
{
  "id": 22,
  "result": {
    "task": {
      "source": "user",
      "agent_id": "8b0b8fa9-17f5-49e2-a664-1a093708150f",
      "ordinal": 1,
      "parent_exchange_id": "a61d48c0-2b32-409f-9006-50f54fc25c71",
      "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72",
      "final_response": null,
      "message": "Return the fixture result",
      "last_activity_at_ms": null,
      "completed_at_ms": null,
      "id": "53418ec7-d500-43fc-9971-cb3a1e81850d",
      "operation_id": "example-agent",
      "created_at_ms": 1793254175942,
      "error": null,
      "parent_reported": false,
      "state": "queued"
    },
    "warning": null,
    "agent": {
      "definition": "worker",
      "updated_at_ms": 1793254175942,
      "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72",
      "nickname": null,
      "task_name": "worker-1",
      "created_at_ms": 1793254175942,
      "task_path": "/root/worker-1",
      "id": "8b0b8fa9-17f5-49e2-a664-1a093708150f",
      "selection": {
        "model": "default",
        "reasoning_effort": "medium",
        "provider": "mock",
        "context_window": null,
        "access": {
          "readable_directory": [],
          "writable_directory": [],
          "windows_sandbox": "elevated",
          "sandbox": false,
          "read_access": "full",
          "write_access": "workspace"
        },
        "service_tier": "default",
        "profile": "worker",
        "approval_mode": "ask",
        "instructions": "Complete the assigned task and report the result and verification."
      },
      "state": "starting",
      "provider_thread_id": null,
      "inbox": [],
      "source": "user"
    },
    "status": "admitted"
  }
}
```

<!-- /captured-example -->

## `session.approval.continue`

Continue after the approval-wait lifecycle permits it.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The explicit session approval-continuation context.
- **Result and effects:** Returns continuation state.
- **Boundary:** This follows the approval timeout/continuation rules rather than inventing an approval decision.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/rust/forge/crates/forge-harness/src/broker/mod.rs:4341](../../../../nvim/rust/forge/crates/forge-harness/src/broker/mod.rs).

<!-- detailed-contract -->
### Contract

No parameter fields. Returns [BrokerSnapshot](runtime-schemas.md#brokersnapshot). It consumes pending approval-timeout feedback for this session's root execution. Without feedback it returns the current snapshot. With a running/waiting selected task it reserves a continuation carrying that feedback; a paused task remains paused. Without a selected task it runs a new chat exchange with an approval-timeout continuation origin. This does not approve the timed-out operation. Provider/storage failures remain explicit, and task reservation is distinct from provider completion.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [session_api_examples.lua](../../../../nvim/tests/forge/session_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.approval.continue",
  "params": {
    "_command": {
      "id": "command:24460:51071682393600:2"
    }
  },
  "id": 12,
  "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72"
}
```

```json
{
  "id": 12,
  "result": {
    "access_abbreviation": "rxw",
    "lsp_tools": false,
    "session_tools": true,
    "status_thoughts": false,
    "check_editing": false,
    "runtime_epoch": "6f03dfc5-c2fd-4355-a7fe-61bc9fefa1b8",
    "task_operation": null,
    "checks_running": false,
    "check_timing": "after_queue",
    "automatic_checks": false,
    "checks_deferred": false,
    "session": {
      "name": "",
      "service_tier": "default",
      "shell": "system",
      "workspace": "/fixture/nvim.0\\XIjMZp\\0\\workspace",
      "purpose": "conversation",
      "last_compaction_turn": null,
      "primary_agent_id": "b14c3cec-edd9-49ca-a61f-467677607b72:agent:primary",
      "backend_session_id": null,
      "provider_checkpoint_id": null,
      "provider_fork_state": {
        "state": "ready"
      },
      "provider_label": "Mock CLI",
      "resolved_model": null,
      "plan_executor": {
        "enabled": false,
        "effort": null,
        "model": null
      },
      "plan_compact": false,
      "plan_auto_approve_revisions": true,
      "approval_timeout": {
        "amount": 10,
        "unit": "m"
      },
      "answer_timeout": {
        "amount": 10,
        "unit": "m"
      },
      "context_window": null,
      "access": {
        "readable_directory": [],
        "writable_directory": [],
        "windows_sandbox": "elevated",
        "sandbox": false,
        "read_access": "full",
        "write_access": "workspace"
      },
      "id": "b14c3cec-edd9-49ca-a61f-467677607b72",
      "approval_mode": "ask",
      "active_task": null,
      "default_approval": "ask",
      "plan_approval": null,
      "created_at_ms": 1793254175750,
      "updated_at_ms": 1793254175750,
      "lease_owner": "nvim-24460-51071561567100",
      "native_fork": false,
      "native_compact": true,
      "effort": "medium",
      "background": null,
      "backend": "mock",
      "model": "default",
      "auto_compaction": {
        "enabled": false,
        "threshold_tokens": 128000
      },
      "context_usage": null
    },
    "approval": [],
    "active_check_run": null,
    "artifact": [],
    "snapshot_revision": 4,
    "timeline_revision": 1,
    "active_wait": null,
    "status": {
      "kind": "idle"
    },
    "no_checkpoint": true,
    "subagent_tools": false,
    "active_elicitation": null,
    "capability": {
      "permission_control": true,
      "background_terminals": true,
      "approval_mode_list": [
        "ask",
        "deny",
        "allow"
      ],
      "native_fork": true,
      "native_compact": true,
      "os_sandbox": false,
      "compaction": {
        "lifecycle": true,
        "background": false,
        "metrics": true,
        "summary": true
      },
      "agent": {
        "spawn": true,
        "input": true,
        "observe": true,
        "interrupt": true,
        "parallel": true
      },
      "catalog": {
        "skill": false,
        "mcp": false,
        "live_mcp_mutation": false
      },
      "native_steer": true,
      "native_turn_rollback": true,
      "native_goal": false,
      "model_selection": true,
      "effort_selection": true,
      "fast_mode": true,
      "ultrafast_mode": true
    },
    "active_plan": null,
    "restore_recovery": null,
    "prompt_history": [],
    "agent": {
      "summary": {},
      "run": []
    },
    "ask_model": {}
  }
}
```

<!-- /captured-example -->

## `session.approval.resolve`

Resolve a pending approval.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The approval identity and selected decision.
- **Result and effects:** Returns approval/control state.
- **Boundary:** The decision applies only to the identified live approval.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `control`. Recovery: `session_command`.

**Current caller:** [nvim/lua/forge/views/harness/controller.lua:771](../../../../nvim/lua/forge/views/harness/controller.lua).

<!-- detailed-contract -->
### Contract

Requires `approval_id` and `answer_list` (array of [ApprovalAnswer](runtime-schemas.md#approvalanswer), each with `item_id` and `choice_id`). Answers select choices from that pending approval's [ApprovalRequestView](runtime-schemas.md#approvalrequestview). Returns `{ "resolved": true, "approval": ... }` with the resolved view. The permission coordinator validates item/choice ownership, records permitted grants, removes the pending request, and resumes its waiting operation. Missing, expired, wrong-owner, or invalid choices fail. This can authorize the operation represented by the approval, so callers must not substitute choices from another request. Resolution is not proof that the subsequent operation completed.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_checks_host.lua](../../../../nvim/tests/forge/harness_checks_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 693,
  "session_id": "5a80e192-fbb2-4f35-834d-933d3c0d9ec3",
  "params": {
    "answer_list": [
      {
        "choice_id": "allow_once",
        "item_id": "proposal"
      }
    ],
    "_command": {
      "id": "command:46220:51089453283800:12",
      "runtime_epoch": "5171c0e3-fc6a-4e2e-9d17-30a14ea14363"
    },
    "approval_id": "2fab4771-7656-43b9-ac72-020826e32c69"
  },
  "method": "session.approval.resolve"
}
```

```json
{
  "id": 693,
  "result": {
    "resolved": true,
    "approval": {
      "resolution_source": "user",
      "execution_owner": {
        "session_id": "5a80e192-fbb2-4f35-834d-933d3c0d9ec3",
        "agent_id": "5a80e192-fbb2-4f35-834d-933d3c0d9ec3:agent:primary"
      },
      "turn_id": null,
      "id": "2fab4771-7656-43b9-ac72-020826e32c69",
      "exchange_id": "114f3691-89af-4b42-aad4-9e73b66fba9d",
      "item_list": [
        {
          "id": "proposal",
          "command_list": [],
          "choice_list": [
            {
              "id": "allow_once",
              "rule_list": [],
              "resolution": "allow_once",
              "label": "Approve update"
            },
            {
              "id": "deny_once",
              "rule_list": [],
              "resolution": "deny_once",
              "label": "Reject update"
            },
            {
              "id": "cancel",
              "rule_list": [],
              "resolution": "cancel",
              "label": "Cancel update"
            }
          ],
          "title": "Update project checks",
          "detail": "Approve these project check changes and managed helper updates. No checks will run.\n\n--- .forge.json checks (current)\n+++ .forge.json checks (proposed)\n@@ -1 +1 @@\n-null\n\\ No newline at end of file\n+{}\n\\ No newline at end of file\n"
        }
      ],
      "provider": "Harness",
      "reason": null,
      "deadline": {
        "expires_at_ms": 1793254793575
      }
    }
  }
}
```

<!-- /captured-example -->

## `session.attachment.import`

Import an attachment into the session's managed attachment storage.

- **Context:** Explicit existing session.
- **Input:** Source path and the attachment number used by the caller.
- **Result and effects:** Returns imported attachment information.
- **Boundary:** I/O failure does not mean the attachment was submitted to the provider.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/attachment.lua:174](../../../../nvim/lua/forge/views/harness/attachment.lua).

<!-- detailed-contract -->
### Contract

Requires `path` (an absolute local path string). Optional `number` is an unsigned integer defaulting to 1 and determines the initial display label. The source must be a regular file with a UTF-8 filename. Returns [AttachmentSelection](runtime-schemas.md#attachmentselection): retained metadata `id`, editable `label`, original `name`, detected `media_type`, and UTF-8 byte offsets `start`/`end` for inserting the label.

Import copies the complete file into the content-addressed object store and persists attachment metadata before returning. It does not submit the attachment, invoke a provider, or change the source file. Source changes during capture, I/O failures, invalid paths, and metadata publication failures are explicit. Later composer edits must preserve the selection identity and update its byte span. It emits no request progress.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [session_api_examples.lua](../../../../nvim/tests/forge/session_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.attachment.import",
  "params": {
    "path": "/fixture/nvim.0\\XIjMZp\\0/example.txt",
    "number": 1
  },
  "id": 18,
  "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72"
}
```

```json
{
  "id": 18,
  "result": {
    "name": "example.txt",
    "id": "e1fee434a1091b0b290c1402ec6694c9425a9a1ab1004f7030058c9a5606f6f8",
    "end": 9,
    "media_type": "application/octet-stream",
    "start": 0,
    "label": "[File #1]"
  }
}
```

<!-- /captured-example -->

## `session.check.cancel`

Cancel an identified active check run.

- **Context:** Explicit existing session/check owner.
- **Input:** run_id.
- **Result and effects:** Returns cancellation/status information.
- **Boundary:** Cancellation must collect owned processes before releasing their resources.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/checks/tools.rs`](../../../rust/forge/crates/forge-harness/src/checks/tools.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/lua/forge/views/harness/checks.lua:309](../../../../nvim/lua/forge/views/harness/checks.lua).

<!-- detailed-contract -->
### Contract

Requires `run_id` (string), with optional `interrupt_execution` (false by default). Returns `{ "run_id": ..., "cancellation_requested": true }`. Active provider work or an explicit execution interrupt coordinates native cleanup before requesting cancellation; otherwise the check supervisor cancels the run directly. Recorded output and finished commands remain available. Cleanup contention, unknown runs, and native process failures are explicit. The acknowledgement does not erase history or imply every asynchronous observer has already seen completion.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_checks_host.lua](../../../../nvim/tests/forge/harness_checks_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 637,
  "session_id": "5a80e192-fbb2-4f35-834d-933d3c0d9ec3",
  "params": {
    "run_id": "1465daf1-e4c5-4954-a958-72a99b559bc6",
    "_command": {
      "id": "command:46220:51086762649800:9",
      "runtime_epoch": "56924f3d-a264-4f3e-919d-c93bb6b77b0d"
    }
  },
  "method": "session.check.cancel"
}
```

```json
{
  "id": 637,
  "result": {
    "run_id": "1465daf1-e4c5-4954-a958-72a99b559bc6",
    "cancellation_requested": true
  }
}
```

<!-- /captured-example -->

## `session.check.complete`

Complete the automatic-check continuation boundary.

- **Context:** Explicit existing session/check owner.
- **Input:** The selected session/check continuation context.
- **Result and effects:** Returns the resulting session state and continuation events.
- **Boundary:** This can resume waiting workflow work rather than merely querying status.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/checks/tools.rs`](../../../rust/forge/crates/forge-harness/src/checks/tools.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/rust/forge/crates/forge-harness/src/service/input.rs:788](../../../../nvim/rust/forge/crates/forge-harness/src/service/input.rs).

<!-- detailed-contract -->
### Contract

No parameter fields. Reconciles automatically owned check completion with the session's task lifecycle and returns [BrokerSnapshot](runtime-schemas.md#brokersnapshot). It starts no arbitrary caller-supplied command. Check outcomes can reserve additional task work, retain a blocked/paused execution, or retire a completed task. Required gate/configuration, storage, and cleanup failures remain explicit. The response describes settled native check state, not completion of later reserved provider work.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [session_api_examples.lua](../../../../nvim/tests/forge/session_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.check.complete",
  "params": {
    "_command": {
      "id": "command:24460:51071688868800:3"
    }
  },
  "id": 14,
  "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72"
}
```

```json
{
  "id": 14,
  "result": {
    "access_abbreviation": "rxw",
    "lsp_tools": false,
    "session_tools": true,
    "status_thoughts": false,
    "check_editing": false,
    "runtime_epoch": "6f03dfc5-c2fd-4355-a7fe-61bc9fefa1b8",
    "task_operation": null,
    "checks_running": false,
    "check_timing": "after_queue",
    "automatic_checks": false,
    "checks_deferred": false,
    "session": {
      "name": "",
      "service_tier": "default",
      "shell": "system",
      "workspace": "/fixture/nvim.0\\XIjMZp\\0\\workspace",
      "purpose": "conversation",
      "last_compaction_turn": null,
      "primary_agent_id": "b14c3cec-edd9-49ca-a61f-467677607b72:agent:primary",
      "backend_session_id": null,
      "provider_checkpoint_id": null,
      "provider_fork_state": {
        "state": "ready"
      },
      "provider_label": "Mock CLI",
      "resolved_model": null,
      "plan_executor": {
        "enabled": false,
        "effort": null,
        "model": null
      },
      "plan_compact": false,
      "plan_auto_approve_revisions": true,
      "approval_timeout": {
        "amount": 10,
        "unit": "m"
      },
      "answer_timeout": {
        "amount": 10,
        "unit": "m"
      },
      "context_window": null,
      "access": {
        "readable_directory": [],
        "writable_directory": [],
        "windows_sandbox": "elevated",
        "sandbox": false,
        "read_access": "full",
        "write_access": "workspace"
      },
      "id": "b14c3cec-edd9-49ca-a61f-467677607b72",
      "approval_mode": "ask",
      "active_task": null,
      "default_approval": "ask",
      "plan_approval": null,
      "created_at_ms": 1793254175750,
      "updated_at_ms": 1793254175750,
      "lease_owner": "nvim-24460-51071561567100",
      "native_fork": false,
      "native_compact": true,
      "effort": "medium",
      "background": null,
      "backend": "mock",
      "model": "default",
      "auto_compaction": {
        "enabled": false,
        "threshold_tokens": 128000
      },
      "context_usage": null
    },
    "approval": [],
    "active_check_run": null,
    "artifact": [],
    "snapshot_revision": 6,
    "timeline_revision": 1,
    "active_wait": null,
    "status": {
      "kind": "idle"
    },
    "no_checkpoint": true,
    "subagent_tools": false,
    "active_elicitation": null,
    "capability": {
      "permission_control": true,
      "background_terminals": true,
      "approval_mode_list": [
        "ask",
        "deny",
        "allow"
      ],
      "native_fork": true,
      "native_compact": true,
      "os_sandbox": false,
      "compaction": {
        "lifecycle": true,
        "background": false,
        "metrics": true,
        "summary": true
      },
      "agent": {
        "spawn": true,
        "input": true,
        "observe": true,
        "interrupt": true,
        "parallel": true
      },
      "catalog": {
        "skill": false,
        "mcp": false,
        "live_mcp_mutation": false
      },
      "native_steer": true,
      "native_turn_rollback": true,
      "native_goal": false,
      "model_selection": true,
      "effort_selection": true,
      "fast_mode": true,
      "ultrafast_mode": true
    },
    "active_plan": null,
    "restore_recovery": null,
    "prompt_history": [],
    "agent": {
      "summary": {},
      "run": []
    },
    "ask_model": {}
  }
}
```

<!-- /captured-example -->

## `session.check.editing.set`

Set whether check configuration editing is enabled.

- **Context:** Explicit existing session/check owner.
- **Input:** enabled boolean.
- **Result and effects:** Returns the resulting check-editing state.
- **Boundary:** This toggles editing state, not check execution.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/checks/tools.rs`](../../../rust/forge/crates/forge-harness/src/checks/tools.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/controller.lua:1646](../../../../nvim/lua/forge/views/harness/controller.lua).

<!-- detailed-contract -->
### Contract

Requires `enabled` (boolean). Returns `{ "check_editing": enabled }` after saving the conversation-scoped setting and publishing it to the observer. It does not run checks, start a prompt, or cancel an active turn. Concurrent settings revision changes and persistence failures fail the request.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_checks_host.lua](../../../../nvim/tests/forge/harness_checks_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 10,
  "session_id": "5a80e192-fbb2-4f35-834d-933d3c0d9ec3",
  "params": {
    "enabled": true
  },
  "method": "session.check.editing.set"
}
```

```json
{
  "id": 10,
  "result": {
    "check_editing": true
  }
}
```

<!-- /captured-example -->

## `session.check.list`

Read the session's check catalog and current selection.

- **Context:** Explicit existing session/check owner.
- **Input:** The check-list request accepted by the check service.
- **Result and effects:** Returns check definitions/selection and relevant current state.
- **Boundary:** Listing checks does not start them.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/checks/tools.rs`](../../../rust/forge/crates/forge-harness/src/checks/tools.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/checks.lua:320](../../../../nvim/lua/forge/views/harness/checks.lua).

<!-- detailed-contract -->
### Contract

No parameter fields. Returns a catalog with `configured`, configuration `revision`, optional `snapshot`/`error`, `overrides`, `history`, optional `active_run` and `task_id`, `changed_files`, and `applicability`. It also supplies effective `automatic_checks`/`check_timing` settings and, when available, `preview`, `coverage`, and automatic-run state. The service refreshes observed workspace changes and reads saved check configuration; it does not execute the returned commands. Configuration errors remain in the catalog when a usable catalog can be returned. Storage and change-capture failures can fail the request.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [semantic_execution_host.lua](../../../../nvim/tests/forge/semantic_execution_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 53,
  "method": "session.check.list",
  "params": {},
  "session_id": "34d8be31-e867-4265-9fd8-9a0db4216c1d"
}
```

```json
{
  "id": 53,
  "result": {
    "error": null,
    "automatic": {
      "delivered": [],
      "blocked": null,
      "repair_exchange": null,
      "repair_prompt": null,
      "pending": false,
      "run_id": null,
      "owner": "e0b6b5ac-0286-43fd-90aa-0cd33db5d82f"
    },
    "history": [],
    "revision": "569f9fec93e050e90d6d2d4673941c512bb8bf4e7e28dd135f99792a78ef2ed0",
    "preview": [],
    "check_timing": "after_queue",
    "automatic_checks": false,
    "snapshot": {
      "helper_digest": {},
      "digest": "d827049039f82a8b65a9b0f52e637cf3bc5d1e0dec7d6198edf901a5e3dcae7d",
      "config": {}
    },
    "overrides": {
      "enabled": {},
      "scopes": {},
      "auto": {}
    },
    "configured": true,
    "active_run": null,
    "task_id": "e0b6b5ac-0286-43fd-90aa-0cd33db5d82f",
    "changed_files": [],
    "applicability": [],
    "coverage": []
  }
}
```

<!-- /captured-example -->

## `session.check.override`

Change session/task check selection overrides.

- **Context:** Explicit existing session/check owner.
- **Input:** enabled, auto, and scopes maps as applicable.
- **Result and effects:** Returns the updated check configuration view.
- **Boundary:** Overrides do not rewrite the project's entire check definition.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/checks/tools.rs`](../../../rust/forge/crates/forge-harness/src/checks/tools.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/checks.lua:257](../../../../nvim/lua/forge/views/harness/checks.lua).

<!-- detailed-contract -->
### Contract

Accepts [CheckOverrides](runtime-schemas.md#checkoverrides): `enabled`, `auto`, and `scopes` maps, each defaulting to empty and keyed by configured check ID. Returns the updated catalog shape documented for [session.check.list](#sessionchecklist). It replaces temporary overrides for the selected task entry, or the ad hoc conversation when no entry exists. It does not rewrite project check definitions or start a run. Unknown IDs, unsupported file/case scope, an active run, or persistence failure reject the update.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [session_api_examples.lua](../../../../nvim/tests/forge/session_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.check.override",
  "params": {
    "enabled": {},
    "auto": {},
    "scopes": {}
  },
  "id": 13,
  "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72"
}
```

```json
{
  "id": 13,
  "result": {
    "overrides": {
      "enabled": {},
      "auto": {},
      "scopes": {}
    },
    "history": [],
    "configured": true,
    "task_id": null,
    "changed_files": null,
    "applicability": [],
    "revision": "569f9fec93e050e90d6d2d4673941c512bb8bf4e7e28dd135f99792a78ef2ed0",
    "error": null,
    "snapshot": {
      "helper_digest": {},
      "digest": "d827049039f82a8b65a9b0f52e637cf3bc5d1e0dec7d6198edf901a5e3dcae7d",
      "config": {}
    },
    "active_run": null
  }
}
```

<!-- /captured-example -->

## `session.check.run`

Start the selected checks.

- **Context:** Explicit existing session/check owner.
- **Input:** The supported check selection and declared input values.
- **Result and effects:** Returns run admission/state and later progress.
- **Boundary:** Only selected work is admitted; failures and cancellation retain run identity.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/checks/tools.rs`](../../../rust/forge/crates/forge-harness/src/checks/tools.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/tests/forge/harness_check_queue_host.lua:141](../../../../nvim/tests/forge/harness_check_queue_host.lua). Live integration-control use: Start a named check run independently of queued provider input.

<!-- detailed-contract -->
### Contract

Accepts [CheckRunRequest](runtime-schemas.md#checkrunrequest): `check` is one configured check ID or an array of IDs; optional `inputs` supplies configured typed input values; `all` defaults to false. `inputs` and `all: true` are mutually exclusive. Returns `{ "run_id": ... }` after starting native check work. The service refreshes workspace changes and resolves configured commands without accepting arbitrary replacement command text. Planning, active writing agents, or active source execution prevent admission. Unknown checks, invalid inputs/selections, configuration errors, and process-admission failures are explicit. Completion is observed through check events/status, not the initial response.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_checks_host.lua](../../../../nvim/tests/forge/harness_checks_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 29,
  "session_id": "5a80e192-fbb2-4f35-834d-933d3c0d9ec3",
  "params": {
    "check": [
      "failure",
      "dependent",
      "independent"
    ],
    "_command": {
      "id": "command:46220:51085558195100:3",
      "runtime_epoch": "56924f3d-a264-4f3e-919d-c93bb6b77b0d"
    },
    "all": true
  },
  "method": "session.check.run"
}
```

```json
{
  "id": 29,
  "result": {
    "run_id": "5a356c8f-350b-4d2c-b5f4-cdc8df3c92f3"
  }
}
```

<!-- /captured-example -->

## `session.check.status`

Read check-run status.

- **Context:** Explicit existing session/check owner.
- **Input:** The requested run/status selection.
- **Result and effects:** Returns current status/history for the requested run.
- **Boundary:** Running, failed, cancelled, and completed states remain distinct.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/checks/tools.rs`](../../../rust/forge/crates/forge-harness/src/checks/tools.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/tests/forge/harness_check_queue_host.lua:142](../../../../nvim/tests/forge/harness_check_queue_host.lua). Live integration-control use: Inspect the corresponding run and wait for its terminal state.

<!-- detailed-contract -->
### Contract

Requires `run_id` (string). Optional `wait_for_completion` defaults to false; `wait_ms` defaults to zero, or 60000 when completion waiting is requested. Returns the owned [CheckRun](runtime-schemas.md#checkrun), including command states, output paths/preview, and recorded results. Waiting is bounded to 30 seconds for an update or 60 seconds for completion. The response can still report running when that deadline expires. Unknown runs fail instead of returning an empty success. This is read-only and does not rerun a command.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_checks_host.lua](../../../../nvim/tests/forge/harness_checks_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 30,
  "session_id": "5a80e192-fbb2-4f35-834d-933d3c0d9ec3",
  "params": {
    "run_id": "5a356c8f-350b-4d2c-b5f4-cdc8df3c92f3"
  },
  "method": "session.check.status"
}
```

```json
{
  "id": 30,
  "result": {
    "lifecycle": [
      {
        "id": "5a356c8f-350b-4d2c-b5f4-cdc8df3c92f3:event:0",
        "kind": "started",
        "check_id": null,
        "created_at_ms": 1793254189697,
        "text": "Checks started · 3 selected"
      },
      {
        "id": "5a356c8f-350b-4d2c-b5f4-cdc8df3c92f3:event:1",
        "kind": "check_started",
        "check_id": "failure",
        "created_at_ms": 1793254189699,
        "text": "Check started · failure"
      }
    ],
    "id": "5a356c8f-350b-4d2c-b5f4-cdc8df3c92f3",
    "completed_plan": null,
    "progress": {
      "different": [],
      "unverified": [],
      "source_digest": {},
      "declaration_changes": [],
      "warning": [],
      "missing": [],
      "matched": []
    },
    "environment_digest": "b12c7ffb1bf280010df05e4eada7e53f4036e8888bcbf11c774b1923172fc3d3",
    "shell": "nushell",
    "cancelled_inputs": null,
    "cancelled_run_id": null,
    "started_at_ms": 1793254189697,
    "completed_at_ms": null,
    "context": {
      "overrides": {},
      "snapshot": {
        "config": {
          "dependent": {
            "trigger": {
              "auto": true
            },
            "action": {
              "command": "'unexpected' | save unexpected.txt",
              "category": "test",
              "after": [
                "failure"
              ]
            }
          },
          "failure": {
            "trigger": {
              "auto": true
            },
            "action": {
              "command": "exit 7",
              "category": "test"
            }
          },
          "independent": {
            "trigger": {
              "auto": true
            },
            "action": {
              "command": "sleep 200ms; print 'independent output'",
              "category": "test"
            }
          }
        },
        "helper_digest": {},
        "digest": "12910ee8034d0e1e56f3fe0c2a159d6db72113a5f642c37a13bdee97eb57d090"
      },
      "trigger": "User requested",
      "task_id": null,
      "changed_files": [],
      "automatic": false,
      "planned_cases": []
    },
    "owner": {
      "session_id": "5a80e192-fbb2-4f35-834d-933d3c0d9ec3",
      "kind": "session"
    },
    "commands": [
      {
        "reused_from": null,
        "output_bytes": 0,
        "exit_code": null,
        "output": "/fixture/nvim.0/od3SB9/1/forge/harness\\5a80e192-fbb2-4f35-834d-933d3c0d9ec3\\checks\\5a356c8f-350b-4d2c-b5f4-cdc8df3c92f3\\0.output",
        "started_at_ms": 1793254189699,
        "completed_at_ms": null,
        "evidence": null,
        "context": {
          "depends_on": [],
          "receipt": "/fixture/nvim.0/od3SB9/1/forge/harness\\5a80e192-fbb2-4f35-834d-933d3c0d9ec3\\checks\\5a356c8f-350b-4d2c-b5f4-cdc8df3c92f3\\0.selection.json",
          "selection": {
            "kind": "all"
          },
          "category": "test",
          "directory": ".",
          "reason": "Explicitly requested",
          "invocation": {
            "program": "exit 7",
            "args": [],
            "cases": [],
            "inputs": {},
            "directory": ".",
            "files": []
          },
          "resolved": null,
          "operation": {
            "command": "exit 7",
            "input": false
          },
          "definition_id": "failure",
          "area": ".",
          "name": "failure",
          "matched_files": []
        },
        "id": "5a356c8f-350b-4d2c-b5f4-cdc8df3c92f3:0",
        "preview": "",
        "error": null,
        "state": "running",
        "stdout": "/fixture/nvim.0/od3SB9/1/forge/harness\\5a80e192-fbb2-4f35-834d-933d3c0d9ec3\\checks\\5a356c8f-350b-4d2c-b5f4-cdc8df3c92f3\\0.stdout",
        "stderr": "/fixture/nvim.0/od3SB9/1/forge/harness\\5a80e192-fbb2-4f35-834d-933d3c0d9ec3\\checks\\5a356c8f-350b-4d2c-b5f4-cdc8df3c92f3\\0.stderr",
        "command": "exit 7",
        "selected": true
      },
      {
        "reused_from": null,
        "output_bytes": 0,
        "exit_code": null,
        "output": "/fixture/nvim.0/od3SB9/1/forge/harness\\5a80e192-fbb2-4f35-834d-933d3c0d9ec3\\checks\\5a356c8f-350b-4d2c-b5f4-cdc8df3c92f3\\1.output",
        "started_at_ms": null,
        "completed_at_ms": null,
        "evidence": null,
        "context": {
          "depends_on": [
            "failure"
          ],
          "receipt": "/fixture/nvim.0/od3SB9/1/forge/harness\\5a80e192-fbb2-4f35-834d-933d3c0d9ec3\\checks\\5a356c8f-350b-4d2c-b5f4-cdc8df3c92f3\\1.selection.json",
          "selection": {
            "kind": "all"
          },
          "category": "test",
          "directory": ".",
          "reason": "Explicitly requested",
          "invocation": {
            "program": "'unexpected' | save unexpected.txt",
            "args": [],
            "cases": [],
            "inputs": {},
            "directory": ".",
            "files": []
          },
          "resolved": null,
          "operation": {
            "command": "'unexpected' | save unexpected.txt",
            "input": false
          },
          "definition_id": "dependent",
          "area": ".",
          "name": "dependent",
          "matched_files": []
        },
        "id": "5a356c8f-350b-4d2c-b5f4-cdc8df3c92f3:1",
        "preview": "",
        "error": null,
        "state": "pending",
        "stdout": "/fixture/nvim.0/od3SB9/1/forge/harness\\5a80e192-fbb2-4f35-834d-933d3c0d9ec3\\checks\\5a356c8f-350b-4d2c-b5f4-cdc8df3c92f3\\1.stdout",
        "stderr": "/fixture/nvim.0/od3SB9/1/forge/harness\\5a80e192-fbb2-4f35-834d-933d3c0d9ec3\\checks\\5a356c8f-350b-4d2c-b5f4-cdc8df3c92f3\\1.stderr",
        "command": "'unexpected' | save unexpected.txt",
        "selected": true
      },
      {
        "reused_from": null,
        "output_bytes": 0,
        "exit_code": null,
        "output": "/fixture/nvim.0/od3SB9/1/forge/harness\\5a80e192-fbb2-4f35-834d-933d3c0d9ec3\\checks\\5a356c8f-350b-4d2c-b5f4-cdc8df3c92f3\\2.output",
        "started_at_ms": null,
        "completed_at_ms": null,
        "evidence": null,
        "context": {
          "depends_on": [],
          "receipt": "/fixture/nvim.0/od3SB9/1/forge/harness\\5a80e192-fbb2-4f35-834d-933d3c0d9ec3\\checks\\5a356c8f-350b-4d2c-b5f4-cdc8df3c92f3\\2.selection.json",
          "selection": {
            "kind": "all"
          },
          "category": "test",
          "directory": ".",
          "reason": "Explicitly requested",
          "invocation": {
            "program": "sleep 200ms; print 'independent output'",
            "args": [],
            "cases": [],
            "inputs": {},
            "directory": ".",
            "files": []
          },
          "resolved": null,
          "operation": {
            "command": "sleep 200ms; print 'independent output'",
            "input": false
          },
          "definition_id": "independent",
          "area": ".",
          "name": "independent",
          "matched_files": []
        },
        "id": "5a356c8f-350b-4d2c-b5f4-cdc8df3c92f3:2",
        "preview": "",
        "error": null,
        "state": "pending",
        "stdout": "/fixture/nvim.0/od3SB9/1/forge/harness\\5a80e192-fbb2-4f35-834d-933d3c0d9ec3\\checks\\5a356c8f-350b-4d2c-b5f4-cdc8df3c92f3\\2.stdout",
        "stderr": "/fixture/nvim.0/od3SB9/1/forge/harness\\5a80e192-fbb2-4f35-834d-933d3c0d9ec3\\checks\\5a356c8f-350b-4d2c-b5f4-cdc8df3c92f3\\2.stderr",
        "command": "sleep 200ms; print 'independent output'",
        "selected": true
      }
    ],
    "executable": "C:\\Users\\mattm\\AppData\\Local\\Programs\\nu\\bin\\nu.exe",
    "state": "running",
    "workspace_digest": {},
    "workspace_identity": null,
    "workspace": "/fixture/nvim.0\\od3SB9\\0",
    "revision": 0
  }
}
```

<!-- /captured-example -->

## `session.command.apply`

Apply a session command through its typed owner.

- **Context:** Explicit existing session.
- **Input:** operation_id, action, and variant fields. Task actions include goal, plan, execute, attach_plan, fork, replan, pause, resume, clear, and continue. The typed review variants include begin_acceptance, cancel_acceptance, request_changes, cancel_plan, and complete_acceptance. Existing configuration/compaction/rollback actions retain their separate service contracts.
- **Result and effects:** Task state commands return their completed receipt. Review variants return their defined snapshot/receipt response. Configuration and execution controls retain their operation lifecycle. Document each surviving action and response variant explicitly rather than assigning one shape to the whole dispatcher.
- **Boundary:** Task commands commit state and pending delivery. Pause suspends queued input but lets current execution finish. Explicit cancellation uses `session.turn.cancel` after pausing. With no live task, pause preserves the task slot rather than failing. Capture revision before preparation and validate task/artifact authority at commit. Configuration and explicit execution controls remain separate from task ownership.
- **Inspected source:** [task/command.rs](../../../rust/forge/crates/forge-harness/src/task/command.rs), [service/task_state.rs](../../../rust/forge/crates/forge-harness/src/service/task_state.rs), and [broker/configuration.rs](../../../rust/forge/crates/forge-harness/src/broker/configuration.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `control`. Recovery: `session_command`.

**Parameter schema:** [SessionAction](native-schemas.md#sessionaction).

**Configuration actions:** [permission, configuration, compaction, and rollback fields](session-command-configuration.md) share this carrier with task actions.

**Current caller:** [nvim/lua/forge/views/harness/task.lua:110](../../../../nvim/lua/forge/views/harness/task.lua).

<!-- detailed-contract -->
### Contract

Requires `action` and an explicit session. Task actions use [SessionAction](native-schemas.md#sessionaction): `goal`, `plan`, `execute`, `attach_plan`, `fork`, `replan`, `pause`, `resume`, `clear`, `continue`, `begin_acceptance`, `cancel_acceptance`, `request_changes`, `cancel_plan`, and `complete_acceptance`. Every variant's fields are in the linked declaration. Configuration actions `permission`, `configure`, `compact`, and `undo` use the separate [configuration contract](session-command-configuration.md).

Supply a stable `operation_id` string. Task actions generate one if absent, but that prevents recovery by a caller that loses the response. Optional `display_text` is presentation text. Internal `_command_response` selects `receipt` or `snapshot`; task starts default to receipt, and review acceptance actions default to snapshot. Lua owns the separate `_command` transport-recovery metadata. Do not generate new operation identities for retries.

A receipt contains string `id`, string `state`, integer `revision`, action name, and optional `error`. Snapshot mode returns [BrokerSnapshot](runtime-schemas.md#brokersnapshot) with that receipt in `task_operation`. Completion means the state transition committed, not that the provider has finished the new work. Task state, affected plan/question records, and the operation receipt commit together. Duplicate IDs with identical normalized actions return the saved receipt. Conflicting reuse fails.

The service publishes `session_changed` before the command response, then any action-specific event and `task_operation`, and wakes input delivery. Entry actions queue their generated prompt for the input coordinator. They can promote an active turn without cancelling it. Pause suspends future input admission and does not itself cancel the current provider turn. Empty or terminal task slots remain valid when pausing ordinary chat. Stale task revisions, invalid plan identities/digests, missing artifacts, and invalid transitions fail. Provider work can continue or fail after command acknowledgement, so observe the originating session rather than assuming the callback is final task completion.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [semantic_execution_host.lua](../../../../nvim/tests/forge/semantic_execution_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 11,
  "method": "session.command.apply",
  "params": {
    "operation_id": "42040:51100545358200:1",
    "action": "plan",
    "_command": {
      "id": "42040:51100545358200:1",
      "runtime_epoch": "81866b45-2336-4e0f-868f-584e6a16c8c7"
    },
    "attachments": [],
    "text": "build the feature"
  },
  "session_id": "34d8be31-e867-4265-9fd8-9a0db4216c1d"
}
```

```json
{
  "id": 11,
  "result": {
    "id": "42040:51100545358200:1",
    "action": "plan",
    "revision": 1,
    "error": null,
    "state": "completed"
  }
}
```

<!-- /captured-example -->

## `session.command.operation.get`

Read a durable command operation's recorded outcome.

- **Context:** Explicit existing session.
- **Input:** operation_id.
- **Result and effects:** Returns the stored operation view when present, including id, state, revision, action, and error.
- **Boundary:** Use the durable operation identity for recovery, not the transport request ID or task revision. A recorded task command can be complete while its provider entry is still pending.
- **Inspected source:** [service.rs](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `control`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/provider_catalog.lua:146](../../../../nvim/lua/forge/views/harness/provider_catalog.lua).

<!-- detailed-contract -->
### Contract

Requires `operation_id` (string). Returns the durable task/configuration receipt with `id`, `state`, `revision`, `action`, and optional `error`, or JSON null if no receipt exists for this session. This is distinct from the Lua transport command ID queried by `session.command.status.get`. It reads local storage without running or replaying the command and emits no progress. Resolve an uncertain mutation with its original ID before deciding whether any retry is safe.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_task_recovery_host.lua](../../../../nvim/tests/forge/harness_task_recovery_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "params": {
    "operation_id": "goal-start"
  },
  "id": 35,
  "session_id": "129c6069-3e95-4592-be61-fb6024130b8b",
  "method": "session.command.operation.get"
}
```

```json
{
  "result": {
    "revision": 1,
    "state": "completed",
    "id": "goal-start",
    "error": null,
    "action": "goal"
  },
  "id": 35
}
```

<!-- /captured-example -->

## `session.command.status.get`

Read the recorded outcome of a mutation command.

- **Context:** Explicit existing session.
- **Input:** The command id, distinct from this lookup request's correlation ID.
- **Result and effects:** Returns the command state and its recorded response when completed.
- **Boundary:** Unknown outcomes require inspection rather than blind replay.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/command.lua:50](../../../../nvim/lua/forge/command.lua).

<!-- detailed-contract -->
### Contract

Requires `id` (string), the Lua transport command identity previously carried in `_command`. An absent record returns `{ "state": "not_admitted" }`. A retained record returns `state` equal to `running`, `completed`, or `outcome_unknown`, plus `response`, which is the original terminal response or null. It does not run or replay the original command and emits no progress. The identity belongs to the explicit session and its retained command journal. Use this transport-recovery result before replaying a lost response; task transition receipts use the different `operation_id` field and `session.command.operation.get`.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [session_api_examples.lua](../../../../nvim/tests/forge/session_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.command.status.get",
  "params": {
    "id": "unsubmitted-example"
  },
  "id": 6,
  "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72"
}
```

```json
{
  "id": 6,
  "result": {
    "state": "not_admitted"
  }
}
```

<!-- /captured-example -->

## `session.compact`

Request compaction of the selected session context.

- **Context:** Lifecycle: explicit target/source where applicable.
- **Input:** The selected session context and supported compaction options.
- **Result and effects:** Returns the resulting session state through the compaction lifecycle.
- **Boundary:** Provider work may be involved and the session ID remains the owner.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/rust/forge/crates/forge-harness/src/service/input.rs:1106](../../../../nvim/rust/forge/crates/forge-harness/src/service/input.rs).

<!-- detailed-contract -->
### Contract

No parameter fields. Requires provider-native compaction support and an existing provider conversation. Returns [BrokerSnapshot](runtime-schemas.md#brokersnapshot) after compaction finishes, saved capability/context metadata is updated, and the compaction lifecycle record is finalized. It publishes timeline changes and `context_compacted` on success. Unsupported providers, absent conversation, provider failure, or persistence failure reject the request; failed attempts remain recorded. Compaction is provider work and must acquire execution admission, so it cannot overlap another root turn.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [session_api_examples.lua](../../../../nvim/tests/forge/session_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.compact",
  "params": {},
  "id": 20,
  "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72"
}
```

```json
{
  "id": 20,
  "result": {
    "access_abbreviation": "rxw",
    "lsp_tools": false,
    "session_tools": true,
    "status_thoughts": false,
    "check_editing": false,
    "runtime_epoch": "6f03dfc5-c2fd-4355-a7fe-61bc9fefa1b8",
    "task_operation": null,
    "checks_running": false,
    "check_timing": "after_queue",
    "automatic_checks": false,
    "checks_deferred": false,
    "session": {
      "name": "Establish a compactable fixtur",
      "service_tier": "default",
      "shell": "system",
      "workspace": "/fixture/nvim.0\\XIjMZp\\0\\workspace",
      "purpose": "conversation",
      "last_compaction_turn": null,
      "primary_agent_id": "b14c3cec-edd9-49ca-a61f-467677607b72:agent:primary",
      "backend_session_id": "mock-session",
      "provider_checkpoint_id": "mock-checkpoint",
      "provider_fork_state": {
        "state": "ready"
      },
      "provider_label": "Mock backend",
      "resolved_model": "default",
      "plan_executor": {
        "enabled": false,
        "effort": null,
        "model": null
      },
      "plan_compact": false,
      "plan_auto_approve_revisions": true,
      "approval_timeout": {
        "amount": 10,
        "unit": "m"
      },
      "answer_timeout": {
        "amount": 10,
        "unit": "m"
      },
      "context_window": null,
      "access": {
        "readable_directory": [],
        "writable_directory": [],
        "windows_sandbox": "elevated",
        "sandbox": false,
        "read_access": "full",
        "write_access": "workspace"
      },
      "id": "b14c3cec-edd9-49ca-a61f-467677607b72",
      "approval_mode": "ask",
      "active_task": null,
      "default_approval": "ask",
      "plan_approval": null,
      "created_at_ms": 1793254175750,
      "updated_at_ms": 1793254175914,
      "lease_owner": "nvim-24460-51071561567100",
      "native_fork": true,
      "native_compact": true,
      "effort": "medium",
      "background": null,
      "backend": "mock",
      "model": "default",
      "auto_compaction": {
        "enabled": false,
        "threshold_tokens": 128000
      },
      "context_usage": {
        "used": 20000,
        "remaining_percent": 80,
        "size": 100000
      }
    },
    "approval": [],
    "active_check_run": null,
    "artifact": [],
    "snapshot_revision": 10,
    "timeline_revision": 5,
    "active_wait": null,
    "status": {
      "kind": "idle"
    },
    "no_checkpoint": true,
    "subagent_tools": false,
    "active_elicitation": null,
    "capability": {
      "permission_control": true,
      "background_terminals": true,
      "approval_mode_list": [
        "ask",
        "deny",
        "allow"
      ],
      "native_fork": true,
      "native_compact": true,
      "os_sandbox": false,
      "compaction": {
        "lifecycle": true,
        "background": false,
        "metrics": true,
        "summary": true
      },
      "agent": {
        "spawn": true,
        "input": true,
        "observe": true,
        "interrupt": true,
        "parallel": true
      },
      "catalog": {
        "skill": false,
        "mcp": false,
        "live_mcp_mutation": false
      },
      "native_steer": true,
      "native_turn_rollback": true,
      "native_goal": false,
      "model_selection": true,
      "effort_selection": true,
      "fast_mode": true,
      "ultrafast_mode": true
    },
    "active_plan": null,
    "restore_recovery": null,
    "prompt_history": [],
    "agent": {
      "summary": {},
      "run": []
    },
    "ask_model": {}
  }
}
```

<!-- /captured-example -->

## `session.configure`

Apply session configuration.

- **Context:** Lifecycle: explicit target/source where applicable.
- **Input:** The supported configuration fields, including ask_model where supplied.
- **Result and effects:** Returns updated configuration/session state.
- **Boundary:** Configuration uses coordinator/task boundaries and can require active work to settle.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/lua/forge/views/harness/controller.lua:2297](../../../../nvim/lua/forge/views/harness/controller.lua).

<!-- detailed-contract -->
### Contract

Accepts the fields of [session command configuration](session-command-configuration.md) directly, including model/effort/context window/service tier, access and approval policy, shell, executor policy, automatic checks, input timeouts, and tool exposure. Omitted fields retain their current value. `validate: true` validates requested model choices against provider discovery. `settings_revision` instead captures an exact saved settings revision and reconciles its desired state; superseded revisions fail. `session_policy: true` saves supported settings to session scope rather than global defaults. A sole `ask_model` field uses the provider-local [AskModel](runtime-schemas.md#askmodel) setting and returns `{ "ask_model": ... }`.

Ordinary configuration returns the saved [HarnessSession](runtime-schemas.md#harnesssession) and publishes `session_configured`. Provider/resource updates can emit `configuration_progress` tied to the operation ID. Unsupported capability/model/effort/window/tier, invalid access or timeout values, concurrent settings changes, and provider reconciliation failures are explicit. Configuration can change saved global or session preferences and can require provider cleanup. Use canonical `session.command.apply` when the action must coordinate interruption/resumption of active work.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [semantic_execution_host.lua](../../../../nvim/tests/forge/semantic_execution_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 7,
  "method": "session.configure",
  "params": {
    "plan_auto_approve_revisions": false,
    "_command": {
      "id": "command:42040:51100523469000:1",
      "runtime_epoch": "81866b45-2336-4e0f-868f-584e6a16c8c7"
    }
  },
  "session_id": "34d8be31-e867-4265-9fd8-9a0db4216c1d"
}
```

```json
{
  "id": 7,
  "result": {
    "id": "34d8be31-e867-4265-9fd8-9a0db4216c1d",
    "provider_fork_state": {
      "state": "ready"
    },
    "updated_at_ms": 1793254204660,
    "service_tier": "default",
    "approval_mode": "ask",
    "auto_compaction": {
      "enabled": false,
      "threshold_tokens": 128000
    },
    "purpose": "conversation",
    "native_compact": true,
    "workspace": "/fixture/nvim.0\\S3Olz7\\0",
    "native_fork": false,
    "name": "",
    "shell": "system",
    "approval_timeout": {
      "unit": "m",
      "amount": 10
    },
    "answer_timeout": {
      "unit": "m",
      "amount": 10
    },
    "access": {
      "read_access": "full",
      "write_access": "workspace",
      "sandbox": false,
      "writable_directory": [],
      "windows_sandbox": "elevated",
      "readable_directory": []
    },
    "context_usage": null,
    "resolved_model": null,
    "provider_label": "Mock CLI",
    "background": null,
    "backend": "mock",
    "context_window": null,
    "model": "default",
    "effort": "medium",
    "active_task": null,
    "plan_auto_approve_revisions": false,
    "plan_compact": false,
    "last_compaction_turn": null,
    "default_approval": "ask",
    "plan_approval": null,
    "created_at_ms": 1793254204592,
    "lease_owner": "nvim-42040-51100399821900",
    "backend_session_id": null,
    "provider_checkpoint_id": null,
    "primary_agent_id": "34d8be31-e867-4265-9fd8-9a0db4216c1d:agent:primary",
    "plan_executor": {
      "enabled": false,
      "model": null,
      "effort": null
    }
  }
}
```

<!-- /captured-example -->

## `session.delete`

Delete an explicitly selected saved session.

- **Context:** The envelope `session_id` identifies the calling session, captured when its picker opens.
- **Input:** `params.session_id` is the distinct saved session to delete. Neither identifier defaults to the active view. The target must differ from the calling session.
- **Result and effects:** Returns the deletion outcome.
- **Boundary:** Deletion is a persistence mutation and must not target the active session implicitly.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/session_picker.lua:89](../../../../nvim/lua/forge/views/harness/session_picker.lua).

The two session identities have different roles:

| Field | Type and requirement | Meaning |
| --- | --- | --- |
| Envelope `session_id` | Required session ID string | The session whose view initiated deletion. Capture it when opening the picker. |
| `params.session_id` | Required session ID string | The saved session to delete. It must differ from the calling session. |
| Result `deleted` | Session ID string | The deleted local session. |
| Result `provider_session_preserved` | Boolean, `true` on success | The provider's own conversation remains available. |

Deletion closes an idle cached controller, releases its lease, and removes the local session and plan files. Active execution, control work, queued input, agents, or checks reject deletion. An unknown target or a conflicting lease also fails. No provider request or request-specific progress event occurs. The caller must discard a late response after its picker closes. After a lost connection, refresh the session list before deciding whether to retry because this method has no operation receipt.

For an existing caller `caller` and a distinct idle target `saved`, the complete wire exchange is:

```json
{"id":42,"method":"session.delete","session_id":"caller","params":{"session_id":"saved"}}
```

```json
{"id":42,"result":{"deleted":"saved","provider_session_preserved":true}}
```

The [picker regression](../../../tests/forge/session_picker.lua) checks both identities, and the [built-host regression](../../../tests/forge/harness_host.lua) deletes an idle cached session while another session is running and rejects deletion of the caller itself.

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_host.lua](../../../../nvim/tests/forge/harness_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 28,
  "method": "session.delete",
  "params": {
    "session_id": "251cbe68-448c-45af-bd75-fe41dafe2982"
  },
  "session_id": "7950e589-be54-4804-9e68-585010a566a5"
}
```

```json
{
  "id": 28,
  "result": {
    "deleted": "251cbe68-448c-45af-bd75-fe41dafe2982",
    "provider_session_preserved": true
  }
}
```

<!-- /captured-example -->

## `session.document`

Read or operate on a session's native presentation document.

- **Context:** Session lifecycle or explicit existing session.
- **Input:** The presentation operation and its document/view/input fields, including open_diff where selected.
- **Result and effects:** Returns presentation patches, snapshots, or a saved source-diff document.
- **Boundary:** Only the explicit session owns the presentation request.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Operation fields:** [typed discriminator and parameter tables](operation-schemas.md).

**Current caller:** [nvim/lua/forge/source_document.lua:30](../../../../nvim/lua/forge/source_document.lua).

<!-- detailed-contract -->
### Contract

The `operation` discriminator selects a [PresentationRequest](operation-schemas.md), or `open_diff` with `document` and captured `input` ([DocumentInput](repository-schemas.md#documentinput)). The operation schema specifies every input field, including flattened node requests. Every document/view/target belongs to this explicit session; revisions, sequences, and node generations must come from the current projection. Stale callbacks must not substitute the active buffer's identity.

| Operation | Result and completion boundary |
| --- | --- |
| `open` | [PresentationOpen](runtime-schemas.md#presentationopen): separate status/transcript snapshots, syntax readiness, and startup timings. |
| `sync` | [PresentationSync](runtime-schemas.md#presentationsync): status snapshot, syntax readiness, patches, and optional replacement snapshot. A stale replica can require the complete snapshot. |
| `snapshot` | Complete [BufferSnapshot](repository-schemas.md#buffersnapshot) for the selected retained document. |
| `open_view`, `node`, `select_agent` | Empty object after the local selection/expansion state is committed. Changes are consumed through subsequent document synchronization. |
| `resize` | `{ "updated": true }` after view width and resulting projection are retained. |
| `close`, `close_view` | `{ "closed": true }` after the corresponding local document/view lifecycle ends. |
| `staging` | `{ "updated": true }` after newer composer queue/steering state is retained. This projects pending text; it does not submit it to the provider. |
| `input` | [TranscriptAction](runtime-schemas.md#transcriptaction), or `{ "kind": "diff" }` for a saved diff target. |
| `status_action` | The action captured by the separate status document, after status revision and input-sequence validation. |
| `navigate_prompt` | Navigation result from the captured transcript input and requested direction. |
| `tool_open` | `{ "snapshot": ..., "more": true, "title": ... }` for a new output document. |
| `tool_demand` | [OutputDelivery](runtime-schemas.md#outputdelivery): optional patch and whether more output remains. |
| `tool_export` | `{ "path": ... }` for a complete retained output exported under the session data root. |
| `highlight` | Empty object after captured syntax analysis is applied if its document generation is still current. |
| `open_diff` | [SourceDelivery](repository-schemas.md#sourcedelivery) for complete saved diff text. |
| `recap`, `session_name` | `{ "text": ... }` after generation using the requested model and captured conversation history. This can invoke the provider. |
| `background_terminals` | Provider terminal inventory, or `{ "supported": false, "reason": ..., "terminal": [] }`. |
| `terminate_terminal` | Empty object after provider termination completes, with an eight-second deadline. |
| `plan_open` | Review snapshot, `saved_source_digest`, `plan_id`, `version`, `path`, `public_only`, annotations, source-edit metadata, and check options. Requested digest/version and optional saved-source digest must match. |
| `plan_view` | Updated plan review presentation for the captured document/view and optional width. |
| `plan_action`, `plan_definition` | The selected review action/navigation result, such as a source target, reference/rename result, projection update, or schema snapshot. These actions retain their captured review identity and do not authorize a later mutation against a different version. |
| `plan_save_annotations` | `{ "saved": true }` after saving annotations against the captured source digest. |
| `plan_close` | Empty object after cancelling and collecting the review document's admitted local work. |

Document rendering and input projection do not mutate repository source. Provider generation/terminal operations and local tool-output export have the effects described above. Request completion is separate from future transcript events or previously admitted provider work. Invalid operation fields, closed documents, stale identity/revision/sequence, unavailable source/analysis/provider capability, and I/O failures remain explicit. Buffer patches require matching document and base revision; retain the prior usable view when validation fails.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [semantic_execution_host.lua](../../../../nvim/tests/forge/semantic_execution_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 4,
  "method": "session.document",
  "params": {
    "view": "view:51100505712900:1",
    "operation": "open",
    "document": "harness:34d8be31-e867-4265-9fd8-9a0db4216c1d:51100503197800",
    "width": {
      "cell_width": [],
      "columns": 78,
      "variable_tabstop": [],
      "ambiguous": "single",
      "tabstop": 8
    }
  },
  "session_id": "34d8be31-e867-4265-9fd8-9a0db4216c1d"
}
```

```json
{
  "id": 4,
  "result": {
    "transcript": {
      "revision": 0,
      "document": "harness:34d8be31-e867-4265-9fd8-9a0db4216c1d:51100503197800",
      "block": [
        {
          "id": "34d8be31-e867-4265-9fd8-9a0db4216c1d:status:empty",
          "metadata": {
            "decoration": [],
            "editable_region": [],
            "target": [],
            "layout": {
              "indent": 2,
              "source_indent": 0,
              "marker": null
            }
          },
          "text": []
        }
      ]
    },
    "status": {
      "revision": 0,
      "document": "harness:34d8be31-e867-4265-9fd8-9a0db4216c1d:51100503197800:status",
      "block": []
    },
    "syntax_pending": false,
    "startup_timing": {
      "sections_created_ms": 0.4944,
      "snapshot_created_ms": 0.5002,
      "transcript_created_ms": 0.3891,
      "entries_projected_ms": 0.2108
    }
  }
}
```

<!-- /captured-example -->

## `session.exchange.comment.save`

Persist an exchange review comment.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The target exchange/comment payload.
- **Result and effects:** Returns the saved comment result.
- **Boundary:** This saves review text rather than submitting a provider prompt.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/tests/forge/harness_host.lua:120](../../../../nvim/tests/forge/harness_host.lua). Live integration-control use: Verify comment persistence and rejection of cross-session exchange targets.

<!-- detailed-contract -->
### Contract

Accepts [ExchangeComment](runtime-schemas.md#exchangecomment): required string `id`, `exchange_id`, `file_path`, and `body`, optional/null integer `old_line` and `new_line`, and integer `created_at_ms`. The timestamp field is decoded but replaced with server time. The exchange must belong to the explicit session. Returns the saved comment. Local persistence upserts by comment ID, so another save with that ID replaces its payload. It emits no provider prompt or progress.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_host.lua](../../../../nvim/tests/forge/harness_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 37,
  "method": "session.exchange.comment.save",
  "params": {
    "id": "active-review-comment",
    "new_line": 1,
    "body": "Comment while the provider is active",
    "created_at_ms": 0,
    "exchange_id": "7653dff4-cc94-4d4b-9747-44f009ab2bc6",
    "file_path": "example.rs"
  },
  "session_id": "7950e589-be54-4804-9e68-585010a566a5"
}
```

```json
{
  "id": 37,
  "result": {
    "id": "active-review-comment",
    "new_line": 1,
    "body": "Comment while the provider is active",
    "created_at_ms": 1793254195866,
    "file_path": "example.rs",
    "exchange_id": "7653dff4-cc94-4d4b-9747-44f009ab2bc6",
    "old_line": null
  }
}
```

<!-- /captured-example -->

## `session.exchange.list`

List exchanges available for review.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** An empty parameter object.
- **Result and effects:** Returns the exchange-review inventory.
- **Boundary:** Listing does not roll back or resume an exchange.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/controller.lua:1289](../../../../nvim/lua/forge/views/harness/controller.lua).

<!-- detailed-contract -->
### Contract

No parameter fields. Returns the selected session's saved exchange metadata as an array, including saved comments. Empty history returns `[]`. This call does not load full tool-output bodies, resume exchanges, mutate task selection, or invoke a provider. No progress is emitted. Timeline presentation remains owned by `session.document`.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_host.lua](../../../../nvim/tests/forge/harness_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 36,
  "method": "session.exchange.list",
  "params": {},
  "session_id": "7950e589-be54-4804-9e68-585010a566a5"
}
```

```json
{
  "id": 36,
  "result": [
    {
      "id": "7653dff4-cc94-4d4b-9747-44f009ab2bc6",
      "agent_id": "7950e589-be54-4804-9e68-585010a566a5:agent:primary",
      "comment": [],
      "input_message_id": "46304:51091528181800:1",
      "created_at_ms": 1793254195681,
      "check_owner": null,
      "finalization_error": null,
      "finalization_outcome": "complete",
      "checkpoint_after": null,
      "attributed_diff_text": null,
      "checkpoint_diff_text": null,
      "attributed_matches_checkpoint": false,
      "approval_resolution": [],
      "turn": [
        {
          "id": "7653dff4-cc94-4d4b-9747-44f009ab2bc6:turn:1",
          "item": [
            {
              "id": "7653dff4-cc94-4d4b-9747-44f009ab2bc6:turn:1:message:1",
              "kind": "message"
            }
          ],
          "message": [
            {
              "id": "7653dff4-cc94-4d4b-9747-44f009ab2bc6:turn:1:message:1",
              "provider_id": null,
              "text": "Mock response: literal ** prompt with native ownership",
              "kind": "assistant",
              "delivery": "final"
            }
          ],
          "current_message": null,
          "started_at_ms": 1793254195710,
          "completed_at_ms": 1793254195714,
          "provider": {
            "thread_id": "harness:7950e589-be54-4804-9e68-585010a566a5:agent:primary",
            "turn_id": "7653dff4-cc94-4d4b-9747-44f009ab2bc6:provider:1"
          },
          "usage": null,
          "state": {
            "outcome": "completed",
            "kind": "finished"
          }
        }
      ],
      "node_list": [
        {
          "id": "7653dff4-cc94-4d4b-9747-44f009ab2bc6:turn:1:content:7653dff4-cc94-4d4b-9747-44f009ab2bc6:turn:1:message:1",
          "kind": "turn_content",
          "item": {
            "id": "7653dff4-cc94-4d4b-9747-44f009ab2bc6:turn:1:message:1",
            "kind": "message"
          },
          "turn_id": "7653dff4-cc94-4d4b-9747-44f009ab2bc6:turn:1"
        }
      ],
      "state": "complete",
      "session_id": "7950e589-be54-4804-9e68-585010a566a5",
      "metrics": {
        "blocked_duration_ms": 0,
        "blocked_started_ms": null,
        "timing_complete": true,
        "tool_duration_ms": 0,
        "tool_started_ms": null,
        "reported_output_tokens": null,
        "reported_response_ms": null,
        "blocker": [],
        "sample": [],
        "tool": [],
        "cursor": {},
        "observed_elapsed_ms": 38,
        "request_count": 0
      },
      "approval_mode": "ask",
      "checkpoint_before": null,
      "execution_started_at_ms": null,
      "entry": [
        {
          "id": "7653dff4-cc94-4d4b-9747-44f009ab2bc6:turn:1:content:7653dff4-cc94-4d4b-9747-44f009ab2bc6:turn:1:message:1",
          "kind": "response"
        }
      ],
      "duration_ms": 38,
      "awaiting_input": false,
      "prompt": "literal ** prompt with native ownership",
      "origin": {
        "kind": "user"
      },
      "ordinal": 1,
      "checklist": null,
      "check_setup": null,
      "completed_at_ms": 1793254195726,
      "provider_retry": null,
      "disposition": "current",
      "elicitation": null
    },
    {
      "id": "c38128a7-fe54-4da5-8948-2cc6909f7b49",
      "agent_id": "7950e589-be54-4804-9e68-585010a566a5:agent:primary",
      "comment": [],
      "input_message_id": "46304:51091641457500:3",
      "created_at_ms": 1793254195790,
      "check_owner": null,
      "finalization_error": null,
      "finalization_outcome": null,
      "checkpoint_after": null,
      "attributed_diff_text": null,
      "checkpoint_diff_text": null,
      "attributed_matches_checkpoint": false,
      "approval_resolution": [],
      "turn": [],
      "node_list": [],
      "state": "running",
      "session_id": "7950e589-be54-4804-9e68-585010a566a5",
      "metrics": {
        "blocked_duration_ms": 0,
        "blocked_started_ms": null,
        "timing_complete": true,
        "tool_duration_ms": 0,
        "tool_started_ms": null,
        "reported_output_tokens": null,
        "reported_response_ms": null,
        "blocker": [],
        "sample": [],
        "tool": [],
        "cursor": {},
        "observed_elapsed_ms": 0,
        "request_count": 0
      },
      "approval_mode": "ask",
      "checkpoint_before": null,
      "execution_started_at_ms": 1793254195790,
      "entry": [],
      "duration_ms": 0,
      "awaiting_input": false,
      "prompt": "Keep this response active while sending follow-ups",
      "origin": {
        "kind": "user"
      },
      "ordinal": 2,
      "checklist": null,
      "check_setup": null,
      "completed_at_ms": null,
      "provider_retry": null,
      "disposition": "current",
      "elicitation": null
    }
  ]
}
```

<!-- /captured-example -->

## `session.exchange.recovery`

Inspect or resolve retained exchange recovery state.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** An optional supported recovery action such as undo.
- **Result and effects:** Returns recovery state or the action's result.
- **Boundary:** Recovery must preserve newer edits and report conflicts.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/controller.lua:1323](../../../../nvim/lua/forge/views/harness/controller.lua).

<!-- detailed-contract -->
### Contract

Accepts `action`, defaulting to `status`. Status returns null when no restore journal exists, otherwise `{ "pending": ..., "applied": ..., "files": ... }`. `continue` requires an incomplete journal and settled exchange tree, resumes filesystem restoration, and returns `{ "restored": true }` after finalization. `undo` creates a reverse restore preview and returns the same preview-summary shape as rollback preparation; it does not apply that preview. Unknown actions, missing journals, changed workspace state, and I/O failures are explicit. This recovers workspace restore, not provider or GitHub mutation delivery.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [session_api_examples.lua](../../../../nvim/tests/forge/session_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.exchange.recovery",
  "params": {},
  "id": 15,
  "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72"
}
```

```json
{
  "id": 15,
  "result": null
}
```

<!-- /captured-example -->

## `session.exchange.rollback.apply`

Apply a prepared rollback.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** preview_id.
- **Result and effects:** Returns the rollback result.
- **Boundary:** Applying mutates repository/workflow state and must validate the captured preview.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/lua/forge/views/harness/controller.lua:1219](../../../../nvim/lua/forge/views/harness/controller.lua).

<!-- detailed-contract -->
### Contract

Accepts `scope`, defaulting to `workspace`. Workspace mode requires the captured `preview_id`, revalidates workspace identity, Git HEAD, current file content, and exchange history, then applies the restore and finalizes conversation rollback. Returns `{ "restored": true }`. Conversation mode requires `exchange_id` and captured `latest_exchange_id`, changes conversation history without restoring files, and returns `{ "restored": true, "scope": "conversation" }`. Both require settled exchange trees and no active children. A partial filesystem restore retains a journal and requires recovery before new workspace work. Stale previews, conflicts, unavailable checkpoints, and I/O failures remain errors.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_checkpoint_host.lua](../../../../nvim/tests/forge/harness_checkpoint_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 15,
  "params": {
    "preview_id": "c478ae7d-bc6b-458a-9254-a4fb66b420aa",
    "_command": {
      "id": "command:45000:48997783503200:6"
    }
  },
  "session_id": "8bd64f01-33a3-4401-af8a-15d479387ede",
  "method": "session.exchange.rollback.apply"
}
```

```json
{
  "id": 15,
  "result": {
    "restored": true
  }
}
```

<!-- /captured-example -->

## `session.exchange.rollback.prepare`

Capture a rollback preview for an exchange.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** exchange_id.
- **Result and effects:** Returns a preview identity and rollback information.
- **Boundary:** Preparation does not apply the rollback.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/controller.lua:1226](../../../../nvim/lua/forge/views/harness/controller.lua).

<!-- detailed-contract -->
### Contract

Requires `exchange_id` in the current session. Resolves its checkpoint owner, requires settled target-and-later exchange trees, verifies that the target is still in current workspace history, and prepares a durable restore preview. Returns `preview_id`, total `files`, `warning_count`, first `warnings` page, and optional `next_offset`. Preparation reads current workspace/checkpoints and saves the preview but does not restore files. Missing Git/checkpoints, active child work, unresolved restore recovery, or I/O failure rejects preparation.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_checkpoint_host.lua](../../../../nvim/tests/forge/harness_checkpoint_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 12,
  "params": {
    "exchange_id": "0d04f451-6f62-490e-81ac-e55439380084"
  },
  "session_id": "8bd64f01-33a3-4401-af8a-15d479387ede",
  "method": "session.exchange.rollback.prepare"
}
```

```json
{
  "id": 12,
  "result": {
    "files": 3,
    "preview_id": "04a84c80-058c-4f3e-bc3d-0c60f6fe9a19",
    "warning_count": 1,
    "warnings": [
      "Later changes will be overwritten: a"
    ],
    "next_offset": null
  }
}
```

<!-- /captured-example -->

## `session.exchange.rollback.preview`

Read another page of a captured rollback preview.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** preview_id and offset.
- **Result and effects:** Returns the requested preview page.
- **Boundary:** Use the existing preview identity rather than recapturing unrelated state.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/controller.lua:1209](../../../../nvim/lua/forge/views/harness/controller.lua).

<!-- detailed-contract -->
### Contract

Requires `preview_id`; optional `offset` defaults to zero. Returns the retained preview's ID, total file/warning counts, a warning page, and `next_offset` when more warnings remain. The preview belongs to this session and must exist. This is a read-only page lookup and does not apply the restore. Follow offsets to retain every warning rather than treating the first page as complete.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_checkpoint_host.lua](../../../../nvim/tests/forge/harness_checkpoint_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.exchange.rollback.preview",
  "id": 13,
  "session_id": "fd19baf0-88fd-4fbf-97f3-9da4b1b169da",
  "params": {
    "offset": 0,
    "preview_id": "2fbda52b-ec91-45a5-92f8-c11063d5604d"
  }
}
```

```json
{
  "result": {
    "warnings": [
      "Later changes will be overwritten: a"
    ],
    "next_offset": null,
    "files": 3,
    "preview_id": "2fbda52b-ec91-45a5-92f8-c11063d5604d",
    "warning_count": 1
  },
  "id": 13
}
```

<!-- /captured-example -->

## `session.fork`

Create a new session from an explicitly selected source session.

- **Context:** Lifecycle: explicit target/source where applicable.
- **Input:** Source session and supported fork parameters.
- **Result and effects:** Returns the fork result/new session identity through the coordinator.
- **Boundary:** Provider-fork readiness and failure belong to the fork lifecycle.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/client.lua:917](../../../../nvim/lua/forge/client.lua).

<!-- detailed-contract -->
### Contract

Accepts optional source `session_id` and `name` in parameters, with required outer source-session context. The source must use the configured backend and support native fork. Returns a child [BrokerSnapshot](runtime-schemas.md#brokersnapshot) with `fork_performance` containing `total_ms`, phase timings, and `provider_pending: true`. The child and inherited timeline are durably created before the provider-native fork finishes. A `session_created` event identifies the child; later fork events resolve its readiness gate. Child provider work must wait for that gate and can fail independently of the initial response. Live task selection is not inherited. This operation uses the same provider, not a cross-provider replay.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [request_independence_host.lua](../../../../nvim/tests/forge/request_independence_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 34,
  "method": "session.fork",
  "session_id": "244a4ab6-1842-4013-a506-7023d7cabdc7",
  "params": {
    "name": "held fork"
  }
}
```

```json
{
  "id": 34,
  "result": {
    "session": {
      "answer_timeout": {
        "unit": "m",
        "amount": 10
      },
      "context_window": null,
      "access": {
        "read_access": "full",
        "sandbox": false,
        "write_access": "workspace",
        "readable_directory": [],
        "writable_directory": [],
        "windows_sandbox": "elevated"
      },
      "workspace": "/fixture/nvim.0\\q8FFKB\\0",
      "backend": "mock",
      "name": "held fork",
      "active_task": null,
      "default_approval": "ask",
      "plan_approval": null,
      "created_at_ms": 1793254199526,
      "updated_at_ms": 1793254199534,
      "lease_owner": "nvim-36464-51095217204200",
      "native_fork": false,
      "native_compact": true,
      "purpose": "conversation",
      "shell": "system",
      "model": "default",
      "background": null,
      "service_tier": "default",
      "effort": "medium",
      "context_usage": null,
      "primary_agent_id": "44880111-ea5e-4ba6-b9c7-bf431690ec3c:agent:primary",
      "auto_compaction": {
        "enabled": false,
        "threshold_tokens": 128000
      },
      "backend_session_id": null,
      "provider_checkpoint_id": null,
      "provider_fork_state": {
        "state": "preparing",
        "source_session_id": "244a4ab6-1842-4013-a506-7023d7cabdc7",
        "point": {
          "backend_session_id": null,
          "checkpoint_id": null
        }
      },
      "provider_label": "Mock CLI",
      "plan_compact": false,
      "resolved_model": null,
      "plan_executor": {
        "enabled": false,
        "model": null,
        "effort": null
      },
      "plan_auto_approve_revisions": true,
      "approval_timeout": {
        "unit": "m",
        "amount": 10
      },
      "last_compaction_turn": null,
      "id": "44880111-ea5e-4ba6-b9c7-bf431690ec3c",
      "approval_mode": "ask"
    },
    "approval": [],
    "fork_performance": {
      "provider_pending": true,
      "total_ms": 14.5027,
      "timing": [
        {
          "phase": "broker.persist_child",
          "duration_ms": 14.5004
        }
      ]
    },
    "status": {
      "kind": "idle"
    },
    "ask_model": {},
    "access_abbreviation": "rxw",
    "no_checkpoint": false,
    "subagent_tools": false,
    "task_operation": null,
    "session_tools": true,
    "status_thoughts": false,
    "check_editing": false,
    "runtime_epoch": "bc5c947b-ca1a-4a61-aca2-024618b0d809",
    "active_elicitation": null,
    "snapshot_revision": 3,
    "checks_running": false,
    "check_timing": "after_queue",
    "active_wait": null,
    "automatic_checks": false,
    "checks_deferred": false,
    "active_check_run": null,
    "timeline_revision": 1,
    "capability": {
      "approval_mode_list": [
        "ask",
        "deny",
        "allow"
      ],
      "native_fork": true,
      "native_compact": true,
      "os_sandbox": false,
      "compaction": {
        "background": false,
        "summary": true,
        "metrics": true,
        "lifecycle": true
      },
      "agent": {
        "input": true,
        "observe": true,
        "spawn": true,
        "parallel": true,
        "interrupt": true
      },
      "catalog": {
        "mcp": false,
        "live_mcp_mutation": false,
        "skill": false
      },
      "native_steer": true,
      "native_turn_rollback": true,
      "native_goal": false,
      "model_selection": true,
      "effort_selection": true,
      "fast_mode": true,
      "ultrafast_mode": true,
      "permission_control": true,
      "background_terminals": true
    },
    "artifact": [],
    "prompt_history": [],
    "active_plan": null,
    "restore_recovery": null,
    "agent": {
      "run": [],
      "summary": {}
    },
    "lsp_tools": false
  }
}
```

<!-- /captured-example -->

## `session.goal.clear`

Apply Clear to the session task slot through the retained goal alias.

- **Context:** Explicit existing session.
- **Input:** operation_id when supplied.
- **Result and effects:** Returns the completed command receipt. Retires owned questions and records the cleared task outcome.
- **Boundary:** Does not declare the objective achieved or cancel an active provider turn. Preserve the shared task command contract and response variant.
- **Inspected source:** [task/command.rs](../../../rust/forge/crates/forge-harness/src/task/command.rs), [service/task_state.rs](../../../rust/forge/crates/forge-harness/src/service/task_state.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/lua/forge/views/harness/controller.lua:1588](../../../../nvim/lua/forge/views/harness/controller.lua).

<!-- detailed-contract -->
### Contract

No action-specific fields. Supply optional `operation_id` for reliable recovery. This is the `action = "clear"` form of [session.command.apply](#sessioncommandapply), returning its transition receipt. It clears the selected live task through the task owner without deleting historical exchanges. It inherits canonical persistence, validation, and replay semantics and emits no request-progress stream.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [session_api_examples.lua](../../../../nvim/tests/forge/session_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.goal.clear",
  "params": {
    "_command": {
      "id": "example-clear"
    },
    "operation_id": "example-clear"
  },
  "id": 34,
  "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72"
}
```

```json
{
  "id": 34,
  "result": {
    "id": "example-clear",
    "action": "clear",
    "revision": 1,
    "error": null,
    "state": "completed"
  }
}
```

<!-- /captured-example -->

## `session.goal.pause`

Apply Pause to the session task slot through the retained goal alias.

- **Context:** Explicit existing session.
- **Input:** operation_id when supplied.
- **Result and effects:** Returns the completed command receipt while preserving task input and questions.
- **Boundary:** Pauses a live task and suspends queued input through the shared command owner without stopping current execution. With no live task, the task slot remains unchanged. Preserve the shared task command contract and response variant.
- **Inspected source:** [task/command.rs](../../../rust/forge/crates/forge-harness/src/task/command.rs), [service/task_state.rs](../../../rust/forge/crates/forge-harness/src/service/task_state.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/lua/forge/views/harness/controller.lua:1588](../../../../nvim/lua/forge/views/harness/controller.lua).

<!-- detailed-contract -->
### Contract

No action-specific fields. Supply optional `operation_id` for reliable recovery. This is the `action = "pause"` form of [session.command.apply](#sessioncommandapply), returning its transition receipt. It pauses the live task and suspends queued input admission. An empty or terminal task slot remains valid. It does not interrupt the current turn; explicit cancellation uses `session.turn.cancel`. It emits the canonical session/task events and no request progress.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [session_api_examples.lua](../../../../nvim/tests/forge/session_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.goal.pause",
  "params": {
    "_command": {
      "id": "example-pause"
    },
    "operation_id": "example-pause"
  },
  "id": 33,
  "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72"
}
```

```json
{
  "id": 33,
  "result": {
    "id": "example-pause",
    "action": "pause",
    "revision": 1,
    "error": null,
    "state": "completed"
  }
}
```

<!-- /captured-example -->

## `session.goal.resume`

Resume the session task through the retained goal alias.

- **Context:** Explicit existing session.
- **Input:** operation_id when supplied and optional message/text and existing resume metadata.
- **Result and effects:** Returns the command receipt. The controller prepares any required pending entry for later delivery.
- **Boundary:** Resume does not start a provider inline or turn configuration changes into task resumption. Preserve the shared task command contract and response variant.
- **Inspected source:** [task/command.rs](../../../rust/forge/crates/forge-harness/src/task/command.rs), [service/task_state.rs](../../../rust/forge/crates/forge-harness/src/service/task_state.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/tests/forge/harness_tasks_host.lua:80](../../../../nvim/tests/forge/harness_tasks_host.lua). Live integration-control use: Exercise raw host task normalization independently of the Lua task coordinator.

<!-- detailed-contract -->
### Contract

Accepts [ResumeRequest](native-schemas.md#resumerequest): optional `message` (`text` alias), `lifecycle`, and `continuation`, plus optional stable `operation_id`. This is the `action = "resume"` form of [session.command.apply](#sessioncommandapply). It validates the current paused/recoverable task, commits its resumed state, and lets the input coordinator deliver eligible work. The receipt precedes completion of provider work. It emits no request progress and does not silently select another task or session.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_tasks_host.lua](../../../../nvim/tests/forge/harness_tasks_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "params": {
    "operation_id": "raw-host-resume",
    "_command": {
      "runtime_epoch": "b5c5ed38-fc80-4db1-825d-f873247a6f8b",
      "id": "raw-host-resume"
    }
  },
  "method": "session.goal.resume",
  "session_id": "3f514535-7cfd-42bd-baa6-2731a62f3949",
  "id": 42
}
```

```json
{
  "result": {
    "action": "resume",
    "state": "completed",
    "revision": 1,
    "error": null,
    "id": "raw-host-resume"
  },
  "id": 42
}
```

<!-- /captured-example -->

## `session.goal.set`

Install a Goal task through the shared command boundary.

- **Context:** Explicit existing session.
- **Input:** objective, normalized to text, and operation_id when supplied.
- **Result and effects:** Returns the completed command receipt. Commits the Goal and its pending entry, selecting native support through shared preparation.
- **Boundary:** The coordinator delivers the entry. An active turn is preserved, with provider capabilities deciding steering versus a later turn. Preserve the shared task command contract and response variant.
- **Inspected source:** [task/command.rs](../../../rust/forge/crates/forge-harness/src/task/command.rs), [service/task_state.rs](../../../rust/forge/crates/forge-harness/src/service/task_state.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/tests/forge/harness_interrupt_host.lua:56](../../../../nvim/tests/forge/harness_interrupt_host.lua). Live integration-control use: Install the durable task used by the interrupted-host recovery scenario.

<!-- detailed-contract -->
### Contract

Requires nonblank `text` (string); `objective` is accepted when text is absent. Optional `operation_id` is a stable mutation identity, and optional `display_text` controls presentation. This is the `action = "goal"` form of [session.command.apply](#sessioncommandapply) and inherits its receipt, persistence, event ordering, and recovery contract. It selects the session-level task and queues its start prompt. During active work it promotes the task without cancelling the provider turn. When idle it begins a new exchange through input delivery. It emits no request-progress stream.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [session_api_examples.lua](../../../../nvim/tests/forge/session_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.goal.set",
  "params": {
    "text": "Verify explicit task admission",
    "_command": {
      "id": "example-goal"
    },
    "operation_id": "example-goal"
  },
  "id": 29,
  "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72"
}
```

```json
{
  "id": 29,
  "result": {
    "id": "example-goal",
    "action": "goal",
    "revision": 1,
    "error": null,
    "state": "completed"
  }
}
```

<!-- /captured-example -->

## `session.health.get`

Check whether the selected session coordinator is responsive.

- **Context:** Explicit existing session.
- **Input:** An empty parameter object.
- **Result and effects:** Returns session_id and responsive status.
- **Boundary:** Responsiveness is not proof that the provider is idle or available.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `control`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/health.lua:52](../../../../nvim/lua/forge/views/harness/health.lua).

<!-- detailed-contract -->
### Contract

No parameter fields. Returns `{ "session_id": "…", "responsive": true }` after resolving the explicit session and checking that it is not closing. This tests the session service, not provider/network health. It uses the control-response channel and emits no progress, provider prompt, or mutation.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [semantic_execution_host.lua](../../../../nvim/tests/forge/semantic_execution_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 55,
  "method": "session.health.get",
  "params": {},
  "session_id": "34d8be31-e867-4265-9fd8-9a0db4216c1d"
}
```

```json
{
  "id": 55,
  "result": {
    "session_id": "34d8be31-e867-4265-9fd8-9a0db4216c1d",
    "responsive": true
  }
}
```

<!-- /captured-example -->

## `session.initialize`

Initialize the shared runtime and explicitly create or resume the requested session.

- **Context:** Session lifecycle or explicit existing session.
- **Input:** Storage/configuration paths, workspace, client_id, backend launch configuration, model/effort options, and explicit session/create/lease choices.
- **Result and effects:** Returns the initial session snapshot, including session.id.
- **Boundary:** The cleanup must distinguish runtime readiness from implicit default-session creation.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `runtime`. Admission: `control`. Recovery: `none`.

**Parameter schema:** [InitializeRequest](native-schemas.md#initializerequest).

**Current caller:** [nvim/lua/forge/client.lua:561](../../../../nvim/lua/forge/client.lua).

<!-- detailed-contract -->
### Contract

Accepts [InitializeRequest](runtime-schemas.md#initializerequest): required `data_root`, `workspace`, `client_id`, and backend launch descriptor; optional stored session ID, new-session name, configuration/permission paths, background identity, provider launch map, and lease-conflict action. `model` defaults to `default`, `effort` to `medium`, and `goal_max_turns` to 20. The outer session envelope is forbidden because initialization establishes ownership. The additional boolean `services_only` defaults to false.

Normal initialization returns [BrokerSnapshot](runtime-schemas.md#brokersnapshot) for the created/resumed session, including its stable ID and runtime epoch. Services-only initialization returns `initialized: true` and optional startup timing without selecting or creating a session. Repeating services initialization requires the same workspace/data root. Runtime mismatch, provider/configuration failure, and session lease conflicts are explicit. Initialization does not run a user prompt. Do not use the old host-handshake name for this operation.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [github_api_examples.lua](../../../../nvim/tests/forge/github_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "params": {
    "effort": "medium",
    "backend_launch_by_provider": {
      "codex": {
        "kind": "codex",
        "command": [
          "codex",
          "app-server"
        ]
      },
      "mock": {
        "kind": "mock",
        "command": [
          "mock"
        ]
      },
      "claude": {
        "kind": "claude",
        "command": [
          "claude-agent-acp"
        ]
      },
      "copilot": {
        "kind": "copilot",
        "command": []
      },
      "antigravity": {
        "kind": "antigravity",
        "command": [
          "agy_acp_server"
        ]
      }
    },
    "data_root": "/fixture/nvim.0/9nPz8C/0/data/forge/harness",
    "permission_file": "/fixture/nvim.0/9nPz8C/0/data/forge/permissions.json",
    "backend": {
      "kind": "mock",
      "command": [
        "mock"
      ]
    },
    "goal_max_turns": 20,
    "config_dir": "/fixture/nvim.0\\9nPz8C\\0/data",
    "model": "default",
    "workspace": "/fixture/nvim.0\\9nPz8C\\0\\workspace",
    "services_only": true,
    "client_id": "nvim-28904-51103309871100"
  },
  "id": 2,
  "method": "session.initialize"
}
```

```json
{
  "result": {
    "initialized": true,
    "startup_timing": {
      "phases": {
        "settings_catalog_ms": 27.5453,
        "repository_ready_ms": 28.4843,
        "runtime_ready_ms": 29.9146,
        "registry_ready_ms": 0.0102
      }
    }
  },
  "id": 2
}
```

<!-- /captured-example -->

## `session.input.apply`

Apply an input or queue lifecycle operation.

- **Context:** Explicit existing session.
- **Input:** Operation id, action, control_revision, and optional after_control ordering identity.
- **Result and effects:** Ordinary input operations return input changes and acknowledgements. A submitted User message containing recognized task syntax normalizes to the corresponding command response before queue/provider admission.
- **Boundary:** Preserve operation identity through retry and honor entry/control revisions. Committed queue edits are parsed again before delivery, using the stable message/revision command identity. TaskEntry and TaskContinuation instructions remain literal internal input and cannot be forged through this public endpoint.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/lua/forge/command.lua:27](../../../../nvim/lua/forge/command.lua).

<!-- detailed-contract -->
### Contract

Accepts [InputOperation](runtime-schemas.md#inputoperation): nonempty `id`, integer `control_revision` defaulting to zero, optional string `after_control`, and a required tagged [InputAction](runtime-schemas.md#inputaction). The seven action variants are `submit`, `retry`, `resume`, `reserve`, `commit`, `release`, and `remove`. Their exact fields and nested message/edit types are in the linked declarations.

`submit` accepts user-purpose input only. It requires a new nonempty message identity and nonblank text unless model configuration is present. `intent` is `auto` or `queue`. Client configuration can select only `model`, `effort`, `service_tier`, and `context_window`. Harness alone generates task-entry and task-continuation purposes. `reserve` captures a message revision for editing, `commit` validates every edit before changing the queue, and `release` drops that reservation. `remove` and `retry` require the captured editable revision. `resume` requires the current control revision.

Returns [InputUpdate](runtime-schemas.md#inputupdate). Inspect its optional `error` even when the transport succeeded. Validation failures can be recorded as unsuccessful updates without changing messages. Successful admission publishes `input_changed` before the final response and wakes the input worker afterward. The worker may later publish delivering, accepted, failed, or unknown delivery state. Admission acknowledgement is not provider acceptance.

Reusing an operation ID with identical data returns its receipt and current snapshot. Different data with that ID fails. An uncertain submission is reconciled before retry. Confirmed acceptance removes it without resending, and confirmed non-delivery is reported before another explicit retry. Queue ownership ends when the session runtime is replaced. Retain the user draft and original operation/submission identities across a lost response.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_host.lua](../../../../nvim/tests/forge/harness_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 12,
  "method": "session.input.apply",
  "params": {
    "id": "46304:51091528187100:2",
    "action": {
      "message": {
        "id": "46304:51091528181800:1",
        "purpose": {
          "kind": "user"
        },
        "text": "literal ** prompt with native ownership",
        "attachments": [],
        "revision": 0,
        "intent": "auto",
        "delivery": "queued"
      },
      "kind": "submit"
    },
    "control_revision": 0,
    "_command": {
      "id": "46304:51091528187100:2",
      "runtime_epoch": "f163bf85-f136-46d7-aa14-8ce071f6f423"
    }
  },
  "session_id": "7950e589-be54-4804-9e68-585010a566a5"
}
```

```json
{
  "id": 12,
  "result": {
    "snapshot": {
      "control_operation_id": null,
      "messages": [
        {
          "id": "46304:51091528181800:1",
          "route": null,
          "reservation": null,
          "intent": "auto",
          "delivery": "queued",
          "text": "literal ** prompt with native ownership",
          "purpose": {
            "kind": "user"
          },
          "config": null,
          "attachments": [],
          "revision": 1,
          "error": null,
          "submission_id": null
        }
      ],
      "revision": 1,
      "control_revision": 0,
      "suspended": false
    },
    "suspended": false,
    "control_revision": 0,
    "removed": [],
    "upsert": [
      {
        "id": "46304:51091528181800:1",
        "route": null,
        "reservation": null,
        "intent": "auto",
        "delivery": "queued",
        "text": "literal ** prompt with native ownership",
        "purpose": {
          "kind": "user"
        },
        "config": null,
        "attachments": [],
        "revision": 1,
        "error": null,
        "submission_id": null
      }
    ],
    "operation_id": "46304:51091528187100:2",
    "retracted": null,
    "revision": 1,
    "error": null,
    "prepend": false
  }
}
```

<!-- /captured-example -->

## `session.input.get`

Read the authoritative composer/input state.

- **Context:** Explicit existing session.
- **Input:** An empty parameter object.
- **Result and effects:** Returns the current input snapshot and revisions.
- **Boundary:** The caller reconciles it with locally pending edits.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/input_state.lua:270](../../../../nvim/lua/forge/views/harness/input_state.lua).

<!-- detailed-contract -->
### Contract

No parameter fields. Returns [InputSnapshot](runtime-schemas.md#inputsnapshot) plus boolean `execution_busy`. An empty `messages` array means no queued input. `revision` orders queue snapshots, while `control_revision` and `control_operation_id` identify suspend/resume control. No provider invocation or mutation occurs. Subscription updates can follow immediately, so apply this snapshot only to its captured session and merge by revision.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [semantic_execution_host.lua](../../../../nvim/tests/forge/semantic_execution_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 6,
  "method": "session.input.get",
  "params": {},
  "session_id": "34d8be31-e867-4265-9fd8-9a0db4216c1d"
}
```

```json
{
  "id": 6,
  "result": {
    "execution_busy": false,
    "messages": [],
    "suspended": false,
    "control_revision": 0,
    "revision": 0,
    "control_operation_id": null
  }
}
```

<!-- /captured-example -->

## `session.input.operation.get`

Read the outcome of an identified input operation.

- **Context:** Explicit existing session.
- **Input:** The input operation id.
- **Result and effects:** Returns the retained operation outcome when available.
- **Boundary:** Absence is not proof that an external mutation succeeded.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/input_state.lua:206](../../../../nvim/lua/forge/views/harness/input_state.lua).

<!-- detailed-contract -->
### Contract

Requires `id` (string), the original input operation identity. Returns [InputUpdate](runtime-schemas.md#inputupdate) or JSON null when the current queue owner has no such operation. A found update includes a current snapshot so recovery can converge after a lost callback. This read emits no progress and must not be treated as permission to resend an unknown provider submission.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_host.lua](../../../../nvim/tests/forge/harness_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 131,
  "method": "session.input.operation.get",
  "params": {
    "id": "46304:51092222466300:20"
  },
  "session_id": "7950e589-be54-4804-9e68-585010a566a5"
}
```

```json
{
  "id": 131,
  "result": {
    "snapshot": {
      "control_operation_id": "46304:51092167364300:1",
      "messages": [
        {
          "id": "46304:51092222462300:19",
          "route": "turn",
          "reservation": null,
          "intent": "auto",
          "delivery": "accepted",
          "text": "Recover a lost admission acknowledgement",
          "purpose": {
            "kind": "user"
          },
          "config": null,
          "attachments": [],
          "revision": 4,
          "error": null,
          "submission_id": "b1755914-3e32-44e3-8ce3-7058f5d21ab0"
        }
      ],
      "revision": 38,
      "control_revision": 1,
      "suspended": false
    },
    "suspended": false,
    "control_revision": 1,
    "removed": [],
    "upsert": [
      {
        "id": "46304:51092222462300:19",
        "route": null,
        "reservation": null,
        "intent": "auto",
        "delivery": "queued",
        "text": "Recover a lost admission acknowledgement",
        "purpose": {
          "kind": "user"
        },
        "config": null,
        "attachments": [],
        "revision": 1,
        "error": null,
        "submission_id": null
      }
    ],
    "operation_id": "46304:51092222466300:20",
    "retracted": null,
    "revision": 38,
    "error": null,
    "prepend": true
  }
}
```

<!-- /captured-example -->

## `session.input.timeout.configure`

Update session input/approval timeout policy.

- **Context:** Explicit existing session.
- **Input:** The timeout settings accepted by the coordinator.
- **Result and effects:** Returns the resulting configuration/state.
- **Boundary:** This changes policy and does not imply an immediate timeout event.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/lua/forge/views/harness/controller.lua:2345](../../../../nvim/lua/forge/views/harness/controller.lua).

<!-- detailed-contract -->
### Contract

Accepts a nonempty object containing only `approval_timeout` and/or `answer_timeout`. Each supplied value is [InputTimeout](runtime-schemas.md#inputtimeout): integer `amount` from zero through the unsigned 32-bit range and `unit` equal to `"s"`, `"m"`, or `"h"`. Zero disables expiry. Omitted fields retain their saved setting. Null, unknown fields, and durations outside the platform timer range fail.

Returns both effective timeout values. This saves the supplied fields in global settings using the captured settings revision, then updates this session's permission coordinator. Concurrent settings changes can reject the write. It can run during provider work and does not send a prompt or emit request progress. Existing deadlines retain the semantics of their captured timer state.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_input_timeout_host.lua](../../../../nvim/tests/forge/harness_input_timeout_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.input.timeout.configure",
  "session_id": "7a4cb50e-dc73-4902-b3aa-58b4d3fbe642",
  "params": {
    "_command": {
      "runtime_epoch": "95d300de-cf83-404c-b3b4-7aa5b86af665",
      "id": "command:44104:47094078377200:2"
    },
    "answer_timeout": {
      "amount": 0,
      "unit": "h"
    },
    "approval_timeout": {
      "amount": 2,
      "unit": "s"
    }
  },
  "id": 11
}
```

```json
{
  "result": {
    "answer_timeout": {
      "amount": 0,
      "unit": "h"
    },
    "approval_timeout": {
      "amount": 2,
      "unit": "s"
    }
  },
  "id": 11
}
```

<!-- /captured-example -->

## `session.list`

List persisted sessions according to the requested selection.

- **Context:** Lifecycle: explicit target/source where applicable.
- **Input:** scope and workspace filters used by the session picker.
- **Result and effects:** Returns session records including their IDs.
- **Boundary:** Discovery must work without implicitly selecting a session.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Session ID forbidden. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/session_picker.lua:171](../../../../nvim/lua/forge/views/harness/session_picker.lua).

<!-- detailed-contract -->
### Contract

Accepts optional `scope` (`"repo"` by default or `"all"`) and optional `workspace` (path string, defaulting to the initialized workspace). Unknown scopes fail. The service canonicalizes the workspace and returns an array of [HarnessSession](runtime-schemas.md#harnesssession), excluding background conversations. An empty array is valid. Shared initialization suffices and no existing session, provider invocation, or lease acquisition is required. The result describes saved metadata, not exclusive ownership of the listed sessions.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_host.lua](../../../../nvim/tests/forge/harness_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 30,
  "params": {
    "scope": "all"
  },
  "method": "session.list"
}
```

```json
{
  "id": 30,
  "result": [
    {
      "id": "7950e589-be54-4804-9e68-585010a566a5",
      "native_fork": true,
      "background": null,
      "primary_agent_id": "7950e589-be54-4804-9e68-585010a566a5:agent:primary",
      "last_compaction_turn": null,
      "backend_session_id": "mock-session",
      "service_tier": "default",
      "plan_executor": {
        "model": null,
        "enabled": false,
        "effort": null
      },
      "plan_compact": false,
      "workspace": "/fixture/nvim.0\\v3g7w8\\0",
      "created_at_ms": 1793254195509,
      "context_window": null,
      "default_approval": "ask",
      "plan_approval": null,
      "lease_owner": "nvim-46304-51091320903500",
      "name": "literal ** prompt with native ",
      "approval_mode": "ask",
      "context_usage": null,
      "shell": "system",
      "backend": "mock",
      "model": "default",
      "effort": "medium",
      "auto_compaction": {
        "threshold_tokens": 128000,
        "enabled": false
      },
      "approval_timeout": {
        "unit": "m",
        "amount": 10
      },
      "access": {
        "readable_directory": [],
        "writable_directory": [],
        "windows_sandbox": "elevated",
        "sandbox": false,
        "read_access": "full",
        "write_access": "workspace"
      },
      "updated_at_ms": 1793254195719,
      "resolved_model": "default",
      "plan_auto_approve_revisions": true,
      "provider_label": "Mock backend",
      "purpose": "conversation",
      "answer_timeout": {
        "unit": "m",
        "amount": 10
      },
      "provider_fork_state": {
        "state": "ready"
      },
      "active_task": null,
      "native_compact": true,
      "provider_checkpoint_id": "mock-checkpoint"
    }
  ]
}
```

<!-- /captured-example -->

## `session.lsp.analyze`

Perform the authorized semantic analysis supported by the LSP service.

- **Context:** Explicit existing session.
- **Input:** id, path, source text, and optional source_offset/source_total supplied together.
- **Result and effects:** Returns analysis/transfer progress for that source part.
- **Boundary:** A partial source transfer requires both offset and total; malformed or missing fields fail explicitly.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/lsp/bridge.lua:50](../../../../nvim/lua/forge/lsp/bridge.lua).

<!-- detailed-contract -->
### Contract

Requires `id` (pending editor-request string), `path` (source path string), and `source` (UTF-8 text). Optional `source_offset` and `source_total` are nonnegative byte counts and must appear together. Without them, the call analyzes the whole text. Chunked upload starts at offset zero and each later chunk must match the same path/total and the current accumulated byte count. Empty chunks are valid only for a zero-byte source. Incomplete upload returns `{ "pending": true }`.

Completion returns `declarations` and boolean `complete`. Each declaration includes name, kind, signature, visibility, line/column, start/end lines, parent/depth, and precise range start/end positions. Structural analysis supports Rust, TypeScript, TSX, JavaScript, and Lua. Authorization runs before analysis. Missing or wrong-session requests, revoked scope, malformed upload offsets, unsupported language, and syntax-worker failures are explicit. The analysis has a ten-second worker deadline and periodically rechecks request cancellation. It reads submitted source without writing files, invoking a provider, or emitting request progress.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Request and terminal response captured from `lsp_callbacks_enforce_session_and_request_ownership` in [service.rs](../../../../nvim/rust/forge/crates/forge-harness/src/service.rs) through the native service dispatcher. The fixture creates the preceding request authority in memory and starts no external provider. Temporary paths use `/fixture`.

```json
{
  "id": 3,
  "method": "session.lsp.analyze",
  "params": {
    "id": "440ca92d-5d0e-4ae3-ae92-17550a56b5a2",
    "path": "/fixture/.tmpmHDj2s\\source.rs",
    "source": "pub fn callback_source() {}\n"
  },
  "session_id": "45cc80c3-ac84-4905-8210-ba8624029945"
}
```

```json
{
  "id": 3,
  "result": {
    "declarations": [
      {
        "name": "callback_source",
        "kind": "function_item",
        "signature": "pub fn callback_source()",
        "visibility": "public",
        "line": 1,
        "column": 8,
        "start_line": 1,
        "end_line": 1,
        "parent": null,
        "depth": 1,
        "range_start_line": 1,
        "range_start_column": 1,
        "range_end_line": 1,
        "range_end_column": 28
      }
    ],
    "complete": true
  }
}
```

<!-- /captured-example -->

## `session.lsp.authorize`

Authorize a language-server query for the selected workspace/session.

- **Context:** Explicit existing session.
- **Input:** id identifying the LSP bridge request, and paths to authorize.
- **Result and effects:** Returns authorization for the bridge request's selected source paths.
- **Boundary:** Authorization is distinct from performing a project mutation.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/lsp/bridge.lua:34](../../../../nvim/lua/forge/lsp/bridge.lua).

<!-- detailed-contract -->
### Contract

Requires `id` (string) for a pending editor LSP request and `paths` (array of path strings). Returns an object mapping each supplied path to a boolean authorization result. Relative paths resolve within the captured request workspace. The service evaluates the request's retained read policy before the editor reads source bytes. Denial of one path returns false for that path; missing/expired request, wrong session, cancelled scope, or changed LSP configuration fails the call. No provider prompt, write, or request progress occurs. Approval applies only to this active request scope.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Request and terminal response captured from `lsp_callbacks_enforce_session_and_request_ownership` in [service.rs](../../../../nvim/rust/forge/crates/forge-harness/src/service.rs) through the native service dispatcher. The fixture creates the preceding request authority in memory and starts no external provider. Temporary paths use `/fixture`.

```json
{
  "id": 2,
  "method": "session.lsp.authorize",
  "params": {
    "id": "440ca92d-5d0e-4ae3-ae92-17550a56b5a2",
    "paths": [
      "/fixture/.tmpmHDj2s\\source.rs"
    ]
  },
  "session_id": "45cc80c3-ac84-4905-8210-ba8624029945"
}
```

```json
{
  "id": 2,
  "result": {
    "C:\\Users\\mattm\\AppData\\Local\\Temp\\.tmpmHDj2s\\source.rs": true
  }
}
```

<!-- /captured-example -->

## `session.lsp.respond`

Return a language-server result requested by the Rust host.

- **Context:** Explicit existing session.
- **Input:** Request id, result, and optional failure.
- **Result and effects:** Acknowledges the returned result.
- **Boundary:** Use the LSP bridge request ID, not an unrelated wire request ID.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/lsp/bridge.lua:63](../../../../nvim/lua/forge/lsp/bridge.lua).

<!-- detailed-contract -->
### Contract

Requires `id` (string) for a pending editor request. Supply `result` in the request's typed LSP response shape, or `failure` (string) to settle it as failed. The service validates session/scope ownership, removes exactly that pending request, and returns `{ "accepted": true }`. A malformed result settles the waiting operation as an invalid editor response. A missing/previously settled request or wrong session fails explicitly. This is completion of existing editor work, not a new provider prompt, file write, or request-progress stream.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Request and terminal response captured from `lsp_callbacks_enforce_session_and_request_ownership` in [service.rs](../../../../nvim/rust/forge/crates/forge-harness/src/service.rs) through the native service dispatcher. The fixture creates the preceding request authority in memory and starts no external provider. Temporary paths use `/fixture`.

```json
{
  "id": 4,
  "method": "session.lsp.respond",
  "params": {
    "id": "440ca92d-5d0e-4ae3-ae92-17550a56b5a2",
    "result": {
      "rows": [
        {
          "path": "/fixture/.tmpmHDj2s\\source.rs",
          "line": 1,
          "column": 8,
          "text": "pub fn callback_source()",
          "modified": false
        }
      ],
      "source": "buffer"
    }
  },
  "session_id": "45cc80c3-ac84-4905-8210-ba8624029945"
}
```

```json
{
  "id": 4,
  "result": {
    "accepted": true
  }
}
```

<!-- /captured-example -->

## `session.new`

Create a new session using explicit creation/inheritance context.

- **Context:** Lifecycle: explicit target/source where applicable.
- **Input:** Creation fields such as name, with any source session provided explicitly.
- **Result and effects:** Returns a snapshot containing the new session and its ID.
- **Boundary:** Creation must not require the caller to already own the new ID.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Optional explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/client.lua:913](../../../../nvim/lua/forge/client.lua).

<!-- detailed-contract -->
### Contract

Accepts optional `name` and `source_session_id`, with optional explicit outer session context. When both source and context are supplied they must match. With a source it creates a provider-empty conversation using that source's workspace/backend and effective saved preferences. Without a source it uses the initialized shared runtime. Returns [BrokerSnapshot](runtime-schemas.md#brokersnapshot) for the new session, then publishes `session_created` with that session's identity. It does not inherit live task state or provider history. Subsequent requests must use the returned `session.id`; creation never retargets an existing callback. Missing source, incompatible configured backend, and storage/lease failure are errors.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_host.lua](../../../../nvim/tests/forge/harness_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 22,
  "params": {
    "name": "Idle cached deletion regression"
  },
  "method": "session.new"
}
```

```json
{
  "id": 22,
  "result": {
    "artifact": [],
    "lsp_tools": false,
    "subagent_tools": false,
    "session_tools": true,
    "ask_model": {},
    "active_elicitation": null,
    "access_abbreviation": "rxw",
    "checks_running": false,
    "agent": {
      "run": [],
      "summary": {}
    },
    "prompt_history": [
      {
        "text": "literal ** prompt with native ownership",
        "attachments": []
      }
    ],
    "capability": {
      "compaction": {
        "lifecycle": true,
        "background": false,
        "metrics": true,
        "summary": true
      },
      "ultrafast_mode": true,
      "native_turn_rollback": true,
      "native_goal": false,
      "native_fork": true,
      "native_steer": true,
      "background_terminals": true,
      "agent": {
        "interrupt": true,
        "spawn": true,
        "input": true,
        "observe": true,
        "parallel": true
      },
      "model_selection": true,
      "approval_mode_list": [
        "ask",
        "deny",
        "allow"
      ],
      "effort_selection": true,
      "catalog": {
        "skill": false,
        "mcp": false,
        "live_mcp_mutation": false
      },
      "permission_control": true,
      "fast_mode": true,
      "native_compact": true,
      "os_sandbox": false
    },
    "restore_recovery": null,
    "status_thoughts": false,
    "check_editing": false,
    "check_timing": "after_queue",
    "session": {
      "id": "251cbe68-448c-45af-bd75-fe41dafe2982",
      "native_fork": false,
      "background": null,
      "primary_agent_id": "251cbe68-448c-45af-bd75-fe41dafe2982:agent:primary",
      "last_compaction_turn": null,
      "backend_session_id": null,
      "service_tier": "default",
      "plan_executor": {
        "model": null,
        "enabled": false,
        "effort": null
      },
      "plan_compact": false,
      "workspace": "/fixture/nvim.0\\v3g7w8\\0",
      "created_at_ms": 1793254195767,
      "context_window": null,
      "default_approval": "ask",
      "plan_approval": null,
      "lease_owner": "nvim-46304-51091320903500",
      "name": "Idle cached deletion regression",
      "approval_mode": "ask",
      "context_usage": null,
      "shell": "system",
      "backend": "mock",
      "model": "default",
      "effort": "medium",
      "auto_compaction": {
        "threshold_tokens": 128000,
        "enabled": false
      },
      "approval_timeout": {
        "unit": "m",
        "amount": 10
      },
      "access": {
        "readable_directory": [],
        "writable_directory": [],
        "windows_sandbox": "elevated",
        "sandbox": false,
        "read_access": "full",
        "write_access": "workspace"
      },
      "updated_at_ms": 1793254195767,
      "resolved_model": "default",
      "plan_auto_approve_revisions": true,
      "provider_label": "Mock CLI",
      "purpose": "conversation",
      "answer_timeout": {
        "unit": "m",
        "amount": 10
      },
      "provider_fork_state": {
        "state": "ready"
      },
      "active_task": null,
      "native_compact": true,
      "provider_checkpoint_id": null
    },
    "active_check_run": null,
    "checks_deferred": false,
    "runtime_epoch": "e13d4e22-2a5d-4390-a016-21c5f7e8e023",
    "task_operation": null,
    "timeline_revision": 1,
    "active_plan": null,
    "no_checkpoint": true,
    "status": {
      "kind": "idle"
    },
    "active_wait": null,
    "approval": [],
    "automatic_checks": false,
    "snapshot_revision": 2
  }
}
```

<!-- /captured-example -->

## `session.permission.set`

Apply the selected session's permission/access change.

- **Context:** Explicit existing session.
- **Input:** Permission intent fields accepted by the configuration service, with operation identity where supplied.
- **Result and effects:** Returns the transition result.
- **Boundary:** The service routes this through the separate configuration command path. Permission changes must preserve the complete active task and cannot resume it implicitly.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/tests/forge/harness_acp_children_host.lua:76](../../../../nvim/tests/forge/harness_acp_children_host.lua). Live integration-control use: Configure the mock ACP parent policy before exercising child policy inheritance.

<!-- detailed-contract -->
### Contract

Accepts `change` ([PermissionChange](native-schemas.md#permissionchange)), optional `resume` (false by default), and a stable `operation_id` for reconciliation. It routes through the canonical `permission` action of [session.command.apply](#sessioncommandapply), returning that action's durable receipt. Omission generates an identity from the session and transport request ID, which cannot identify a later retry using a new request ID. The permission change is captured before admission, applied to this session, and optionally reserves resumed work. Invalid modes/paths, stale operation identity, or persistence failures are explicit. A completed receipt proves the state transition, not completion of any resumed provider work.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [session_api_examples.lua](../../../../nvim/tests/forge/session_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.permission.set",
  "params": {
    "change": {
      "kind": "set_approval",
      "mode": "ask"
    },
    "_command": {
      "id": "example-permission"
    },
    "operation_id": "example-permission"
  },
  "id": 27,
  "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72"
}
```

```json
{
  "id": 27,
  "result": {
    "id": "example-permission",
    "action": "permission",
    "revision": 1,
    "error": null,
    "state": "accepted"
  }
}
```

<!-- /captured-example -->

## `session.plan.accept`

Install Execute from the reviewed Plan artifact.

- **Context:** Explicit existing session.
- **Input:** plan_id, reviewed digest, saved_source_digest, approval_mode, plan_executor/executor, plan_compact/compact, comment, and operation_id as accepted by ExecuteSelection.
- **Result and effects:** Returns a session snapshot with task_operation. Commits the execution task and required artifact effects.
- **Boundary:** Validate reviewed identity and digests atomically. Do not start provider execution inside acceptance. Preserve the shared task command contract and response variant.
- **Inspected source:** [task/command.rs](../../../rust/forge/crates/forge-harness/src/task/command.rs), [service/task_state.rs](../../../rust/forge/crates/forge-harness/src/service/task_state.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/tests/forge/semantic_execution_host.lua:46](../../../../nvim/tests/forge/semantic_execution_host.lua). Live integration-control use: Start the reviewed execution used to verify semantic operation traces.

<!-- detailed-contract -->
### Contract

Accepts [ExecuteSelection](native-schemas.md#executeselection), including optional captured `plan_id`, `digest`, `saved_source_digest`, approval mode, executor/compaction controls, and comment. The special plan ID `last` is normalized to omitted selection. Supply a stable `operation_id` for retries. This is the `action = "execute"` form of [session.command.apply](#sessioncommandapply), defaulting to snapshot response with `task_operation`. Acceptance validates the selected artifact and current review state before scheduling execution. It does not report the execution finished. Stale digests, unavailable plans, invalid approval settings, and incompatible task transitions fail without selecting an unrelated plan.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [semantic_execution_host.lua](../../../../nvim/tests/forge/semantic_execution_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 25,
  "method": "session.plan.accept",
  "params": {
    "approval_mode": "ask",
    "plan_id": "6bc6bb92-5de0-46d8-aa46-06e223e6b543",
    "digest": "096875bb579e3d04ad228d5f5f36089e9e742004133eca80ed847bfcc1b054a2",
    "_command": {
      "id": "command:42040:51101179031700:2",
      "runtime_epoch": "81866b45-2336-4e0f-868f-584e6a16c8c7"
    }
  },
  "session_id": "34d8be31-e867-4265-9fd8-9a0db4216c1d"
}
```

```json
{
  "id": 25,
  "result": {
    "access_abbreviation": "rxw",
    "agent": {
      "run": [],
      "summary": {}
    },
    "session_tools": true,
    "ask_model": {},
    "lsp_tools": false,
    "subagent_tools": false,
    "artifact": [
      {
        "id": "6bc6bb92-5de0-46d8-aa46-06e223e6b543",
        "created_at_ms": 1793254204688,
        "working_path": "/fixture/nvim.0/S3Olz7/1/forge/harness\\plans\\34d8be31-e867-4265-9fd8-9a0db4216c1d\\6bc6bb92-5de0-46d8-aa46-06e223e6b543\\working.md",
        "state": "accepted",
        "title": "Design the requested change",
        "updated_at_ms": 1793254205316
      }
    ],
    "timeline_revision": 3,
    "approval": [],
    "session": {
      "id": "34d8be31-e867-4265-9fd8-9a0db4216c1d",
      "provider_fork_state": {
        "state": "ready"
      },
      "updated_at_ms": 1793254205316,
      "service_tier": "default",
      "approval_mode": "ask",
      "auto_compaction": {
        "enabled": false,
        "threshold_tokens": 128000
      },
      "purpose": "conversation",
      "native_compact": true,
      "workspace": "/fixture/nvim.0\\S3Olz7\\0",
      "native_fork": true,
      "name": "Active declaration design: ```",
      "shell": "system",
      "approval_timeout": {
        "unit": "m",
        "amount": 10
      },
      "answer_timeout": {
        "unit": "m",
        "amount": 10
      },
      "access": {
        "read_access": "full",
        "write_access": "workspace",
        "sandbox": false,
        "writable_directory": [],
        "windows_sandbox": "elevated",
        "readable_directory": []
      },
      "context_usage": null,
      "resolved_model": null,
      "provider_label": "Mock CLI",
      "background": null,
      "backend": "mock",
      "context_window": null,
      "model": "default",
      "effort": "medium",
      "active_task": {
        "created_at_ms": 1793254205316,
        "kind": "execute",
        "revision": "8fed1028-6d2d-4130-8478-e5806433a5e8",
        "execution": {
          "record": {
            "id": "56a5d3c2-712b-4f81-a9a7-64409205e410",
            "plan_id": "6bc6bb92-5de0-46d8-aa46-06e223e6b543",
            "verification": [],
            "approval_comments": [],
            "session_id": "34d8be31-e867-4265-9fd8-9a0db4216c1d",
            "review_paused": false,
            "observed_source": {},
            "observed_generation": 0,
            "state": "active",
            "completed_at_ms": null,
            "progress": {
              "matched": [],
              "different": [],
              "unverified": [],
              "source_digest": {},
              "declaration_changes": [],
              "missing": [],
              "warning": []
            },
            "phase": "implement",
            "observed_check": [],
            "original_revision": 1,
            "revision": 1,
            "pending_revision_reason": null,
            "revision_history": [],
            "lifecycle": [],
            "check_runs": [],
            "generation": 0,
            "created_at_ms": 1793254205316,
            "baseline_checkpoint": null,
            "implementation_report": null,
            "planning_backend_session_id": "mock-session",
            "execution_backend_session_id": "mock-session",
            "findings": []
          },
          "continuation": {
            "max_turn_count": 20,
            "consecutive_no_progress": 0,
            "max_consecutive_no_progress": 2,
            "turn_count": 0
          },
          "title": "Design the requested change",
          "native": false
        },
        "updated_at_ms": 1793254205316,
        "entry": {
          "id": "e0b6b5ac-0286-43fd-90aa-0cd33db5d82f",
          "delivery": "pending",
          "instruction": "Start the Execute task for this accepted plan. Inspect the current workspace before implementation.\nExecution ID: 56a5d3c2-712b-4f81-a9a7-64409205e410\nPlan ID: 6bc6bb92-5de0-46d8-aa46-06e223e6b543\nImplement the accepted design and tests. The accepted plan is frozen during Implement. Make justified implementation deviations when needed, including internal helpers, and explain them in your completion summary. Do not edit or submit the plan, reconcile declaration metadata, or run builds, compiler checks, tests, linters, formatters, or runtime checks. Harness runs validation and conformance assessment in Checking. When writing is finished, call plan_phase_done. Declarations and call/access differences do not gate Implement completion. Phase: Implement. Accepted revision: 1. Read the target with plan_read. Call plan_phase_done with this phase and revision when its work is finished, then end the turn after success. Harness commits the transition and supplies the next phase. Ending a turn alone does not end the phase. Do not call goal_complete for this execution.\nUnresolved findings: \nReviewer approval comments (context only, not plan edits):\n\nAccepted semantic design:\n{\n  \"plan_id\": \"6bc6bb92-5de0-46d8-aa46-06e223e6b543\",\n  \"version\": 2,\n  \"title\": \"Design the requested change\",\n  \"request\": \"build the feature\",\n  \"declaration_files\": {\n    \"paths\": [\n      \"plan.json\",\n      \"src/change.rs\"\n    ],\n    \"changed_paths\": [\n      \"src/change.rs\"\n    ]\n  },\n  \"document\": {\n    \"objective\": \"Revise the registry interface.\",\n    \"requirements\": [\n      \"Preserve the registry ownership boundary.\"\n    ],\n    \"background\": \"The registry owns published handles used by its callers.\",\n    \"decisions\": [],\n    \"design\": \"Revise the registry interface while preserving its ownership boundary.\",\n    \"flows\": [],\n    \"tests\": []\n  }\n}",
          "execution": {
            "approval_mode": "ask",
            "executor": {
              "enabled": false,
              "model": null,
              "effort": null
            },
            "compact": false
          }
        }
      },
      "plan_auto_approve_revisions": false,
      "plan_compact": false,
      "last_compaction_turn": null,
      "default_approval": "ask",
      "plan_approval": null,
      "created_at_ms": 1793254204592,
      "lease_owner": "nvim-42040-51100399821900",
      "backend_session_id": "mock-session",
      "provider_checkpoint_id": null,
      "primary_agent_id": "34d8be31-e867-4265-9fd8-9a0db4216c1d:agent:primary",
      "plan_executor": {
        "enabled": false,
        "model": null,
        "effort": null
      }
    },
    "no_checkpoint": false,
    "check_editing": false,
    "active_wait": null,
    "task_operation": {
      "id": "command:42040:51101179031700:2",
      "action": "execute",
      "revision": 1,
      "error": null,
      "state": "completed"
    },
    "check_timing": "after_queue",
    "active_elicitation": null,
    "active_plan": {
      "id": "6bc6bb92-5de0-46d8-aa46-06e223e6b543",
      "validation_warning": [],
      "generation": {
        "max_turn_count": 20,
        "consecutive_no_progress": 0,
        "max_consecutive_no_progress": 2,
        "canonical_revision": 1,
        "turn_count": 1
      },
      "review_digest": "096875bb579e3d04ad228d5f5f36089e9e742004133eca80ed847bfcc1b054a2",
      "session_id": "34d8be31-e867-4265-9fd8-9a0db4216c1d",
      "state": "accepted",
      "elicitation": null,
      "request": "build the feature",
      "working_path": "/fixture/nvim.0/S3Olz7/1/forge/harness\\plans\\34d8be31-e867-4265-9fd8-9a0db4216c1d\\6bc6bb92-5de0-46d8-aa46-06e223e6b543\\working.md",
      "updated_at_ms": 1793254205316,
      "document_version": 2,
      "model_revision": 1,
      "submitted_version": 2,
      "accepted_revision": 1,
      "user_revision": 1,
      "manual_changes": [],
      "accepted_digest": "096875bb579e3d04ad228d5f5f36089e9e742004133eca80ed847bfcc1b054a2",
      "acceptance": null,
      "question_ledger": {
        "resolution": []
      },
      "created_at_ms": 1793254204688,
      "title": "Design the requested change"
    },
    "capability": {
      "native_goal": false,
      "native_steer": true,
      "native_fork": true,
      "approval_mode_list": [
        "ask",
        "deny",
        "allow"
      ],
      "agent": {
        "spawn": true,
        "observe": true,
        "interrupt": true,
        "parallel": true,
        "input": true
      },
      "catalog": {
        "mcp": false,
        "live_mcp_mutation": false,
        "skill": false
      },
      "native_compact": true,
      "permission_control": true,
      "ultrafast_mode": true,
      "fast_mode": true,
      "effort_selection": true,
      "native_turn_rollback": true,
      "os_sandbox": false,
      "compaction": {
        "background": false,
        "metrics": true,
        "summary": true,
        "lifecycle": true
      },
      "background_terminals": true,
      "model_selection": true
    },
    "restore_recovery": null,
    "prompt_history": [
      {
        "attachments": [],
        "text": "/plan build the feature"
      }
    ],
    "checks_running": false,
    "runtime_epoch": "81866b45-2336-4e0f-868f-584e6a16c8c7",
    "active_check_run": null,
    "automatic_checks": false,
    "snapshot_revision": 11,
    "status": {
      "kind": "idle"
    },
    "checks_deferred": false,
    "status_thoughts": false
  }
}
```

<!-- /captured-example -->

## `session.plan.acceptance.begin`

Begin acceptance review through the shared review command.

- **Context:** Explicit existing session.
- **Input:** PlanReviewRequest fields: plan_id, digest, saved_source_digest, approval_mode, comment, annotations, and operation_id when supplied.
- **Result and effects:** Returns a session snapshot with task_operation and the acceptance-question state.
- **Boundary:** Preserve independent execution-revision review routing. Main-task review commits without acquiring provider execution. Preserve the shared task command contract and response variant.
- **Inspected source:** [task/command.rs](../../../rust/forge/crates/forge-harness/src/task/command.rs), [service/task_state.rs](../../../rust/forge/crates/forge-harness/src/service/task_state.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/lua/forge/views/plan_review/native_controller.lua:223](../../../../nvim/lua/forge/views/plan_review/native_controller.lua).

<!-- detailed-contract -->
### Contract

Accepts [PlanReviewRequest](native-schemas.md#planreviewrequest) and optional stable `operation_id`. This is `action = "begin_acceptance"` in [session.command.apply](#sessioncommandapply), defaulting to a snapshot response. It captures the reviewed plan/digests and starts the acceptance lifecycle, including required questions. Review capture may supply the plan fields from an owned document input. Invalid or stale review identity fails. The resulting question state and receipt persist together; follow-up acceptance must retain that captured identity.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [plan_api_examples.lua](../../../../nvim/tests/forge/plan_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.plan.acceptance.begin",
  "params": {
    "operation_id": "example-acceptance",
    "digest": "48e3cfe2f821383be1029c5468751bb59603daf4292062137fefa0e02ec4558f",
    "_command": {
      "id": "example-acceptance"
    },
    "plan_id": "0d9cb210-8737-41fb-a3dd-601aec6d70c4"
  },
  "session_id": "2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447",
  "id": 23
}
```

```json
{
  "result": {
    "ask_model": {},
    "access_abbreviation": "rxw",
    "agent": {
      "run": [],
      "summary": {}
    },
    "subagent_tools": false,
    "lsp_tools": false,
    "session_tools": true,
    "status_thoughts": false,
    "check_editing": false,
    "runtime_epoch": "c832b8be-b9d8-4a4e-a936-dccfbab52e11",
    "active_wait": null,
    "approval": [],
    "prompt_history": [],
    "status": {
      "exchange_id": null,
      "owner": "plan_acceptance",
      "kind": "awaiting_input",
      "plan_id": "0d9cb210-8737-41fb-a3dd-601aec6d70c4"
    },
    "artifact": [
      {
        "updated_at_ms": 1793254176557,
        "title": "Design the requested change",
        "working_path": "/fixture/nvim.0/Nr9VKJ/0/data/forge/harness\\plans\\2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447\\0d9cb210-8737-41fb-a3dd-601aec6d70c4\\working.md",
        "state": "awaiting_review",
        "created_at_ms": 1793254176371,
        "id": "0d9cb210-8737-41fb-a3dd-601aec6d70c4"
      }
    ],
    "active_elicitation": {
      "elicitation": {
        "answer_revision": 0,
        "answer": [],
        "answer_order": {},
        "clarification_active": false,
        "resolution_source": "user",
        "question_set": {
          "questions": [
            {
              "options": [
                {
                  "description": "Apply saved rules and ask for remaining requests.",
                  "recommended": false,
                  "label": "ask"
                },
                {
                  "description": "Reject requests that require approval.",
                  "recommended": false,
                  "label": "deny"
                },
                {
                  "description": "Approve remaining requests once, preserving explicit denies.",
                  "recommended": false,
                  "label": "allow"
                }
              ],
              "header": "Tool approval",
              "allow_freeform": false,
              "question": "How should execution handle tool approval?",
              "id": "acceptance-execution-mode"
            }
          ],
          "id": "plan-acceptance"
        },
        "deadline": null,
        "revision": 1,
        "current_index": 0
      },
      "exchange_id": null,
      "owner": "plan_acceptance",
      "plan_id": "0d9cb210-8737-41fb-a3dd-601aec6d70c4"
    },
    "timeline_revision": 3,
    "active_plan": {
      "updated_at_ms": 1793254176557,
      "session_id": "2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447",
      "document_version": 3,
      "state": "awaiting_review",
      "title": "Design the requested change",
      "working_path": "/fixture/nvim.0/Nr9VKJ/0/data/forge/harness\\plans\\2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447\\0d9cb210-8737-41fb-a3dd-601aec6d70c4\\working.md",
      "submitted_version": 2,
      "accepted_revision": null,
      "user_revision": 0,
      "model_revision": 1,
      "accepted_digest": null,
      "elicitation": null,
      "acceptance": {
        "approval_mode_list": [
          "ask",
          "deny",
          "allow"
        ],
        "review_digest": "48e3cfe2f821383be1029c5468751bb59603daf4292062137fefa0e02ec4558f",
        "overall_comment": null,
        "saved_source_digest": "e1ab12eb864c57cac89973d08a71b78816a5f6e481f498cc87d9be9d5e81a8ff",
        "elicitation": {
          "answer_revision": 0,
          "answer": [],
          "answer_order": {},
          "clarification_active": false,
          "resolution_source": "user",
          "question_set": {
            "questions": [
              {
                "options": [
                  {
                    "description": "Apply saved rules and ask for remaining requests.",
                    "recommended": false,
                    "label": "ask"
                  },
                  {
                    "description": "Reject requests that require approval.",
                    "recommended": false,
                    "label": "deny"
                  },
                  {
                    "description": "Approve remaining requests once, preserving explicit denies.",
                    "recommended": false,
                    "label": "allow"
                  }
                ],
                "header": "Tool approval",
                "allow_freeform": false,
                "question": "How should execution handle tool approval?",
                "id": "acceptance-execution-mode"
              }
            ],
            "id": "plan-acceptance"
          },
          "deadline": null,
          "revision": 1,
          "current_index": 0
        }
      },
      "question_ledger": {
        "resolution": []
      },
      "request": "Build the API fixture",
      "review_digest": "48e3cfe2f821383be1029c5468751bb59603daf4292062137fefa0e02ec4558f",
      "generation": {
        "consecutive_no_progress": 0,
        "max_consecutive_no_progress": 2,
        "canonical_revision": 1,
        "turn_count": 1,
        "max_turn_count": 20
      },
      "validation_warning": [],
      "id": "0d9cb210-8737-41fb-a3dd-601aec6d70c4",
      "manual_changes": [
        "Updated plan checks: verify enabled"
      ],
      "created_at_ms": 1793254176371
    },
    "restore_recovery": null,
    "session": {
      "last_compaction_turn": null,
      "primary_agent_id": "2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447:agent:primary",
      "backend_session_id": "mock-session",
      "provider_checkpoint_id": null,
      "model": "default",
      "provider_label": "Mock CLI",
      "resolved_model": null,
      "plan_executor": {
        "enabled": false,
        "model": null,
        "effort": null
      },
      "plan_compact": false,
      "plan_auto_approve_revisions": true,
      "approval_timeout": {
        "amount": 10,
        "unit": "m"
      },
      "effort": "medium",
      "answer_timeout": {
        "amount": 10,
        "unit": "m"
      },
      "background": null,
      "access": {
        "sandbox": false,
        "windows_sandbox": "elevated",
        "read_access": "full",
        "write_access": "workspace",
        "readable_directory": [],
        "writable_directory": []
      },
      "context_usage": null,
      "approval_mode": "ask",
      "active_task": {
        "updated_at_ms": 1793254176557,
        "plan": {
          "paused": false,
          "artifact": {
            "updated_at_ms": 1793254176557,
            "session_id": "2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447",
            "document_version": 3,
            "state": "awaiting_review",
            "title": "Design the requested change",
            "working_path": "/fixture/nvim.0/Nr9VKJ/0/data/forge/harness\\plans\\2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447\\0d9cb210-8737-41fb-a3dd-601aec6d70c4\\working.md",
            "submitted_version": 2,
            "accepted_revision": null,
            "user_revision": 0,
            "model_revision": 1,
            "accepted_digest": null,
            "elicitation": null,
            "acceptance": {
              "approval_mode_list": [
                "ask",
                "deny",
                "allow"
              ],
              "review_digest": "48e3cfe2f821383be1029c5468751bb59603daf4292062137fefa0e02ec4558f",
              "overall_comment": null,
              "saved_source_digest": "e1ab12eb864c57cac89973d08a71b78816a5f6e481f498cc87d9be9d5e81a8ff",
              "elicitation": {
                "answer_revision": 0,
                "answer": [],
                "answer_order": {},
                "clarification_active": false,
                "resolution_source": "user",
                "question_set": {
                  "questions": [
                    {
                      "options": [
                        {
                          "description": "Apply saved rules and ask for remaining requests.",
                          "recommended": false,
                          "label": "ask"
                        },
                        {
                          "description": "Reject requests that require approval.",
                          "recommended": false,
                          "label": "deny"
                        },
                        {
                          "description": "Approve remaining requests once, preserving explicit denies.",
                          "recommended": false,
                          "label": "allow"
                        }
                      ],
                      "header": "Tool approval",
                      "allow_freeform": false,
                      "question": "How should execution handle tool approval?",
                      "id": "acceptance-execution-mode"
                    }
                  ],
                  "id": "plan-acceptance"
                },
                "deadline": null,
                "revision": 1,
                "current_index": 0
              }
            },
            "question_ledger": {
              "resolution": []
            },
            "request": "Build the API fixture",
            "review_digest": "48e3cfe2f821383be1029c5468751bb59603daf4292062137fefa0e02ec4558f",
            "generation": {
              "consecutive_no_progress": 0,
              "max_consecutive_no_progress": 2,
              "canonical_revision": 1,
              "turn_count": 1,
              "max_turn_count": 20
            },
            "validation_warning": [],
            "id": "0d9cb210-8737-41fb-a3dd-601aec6d70c4",
            "manual_changes": [
              "Updated plan checks: verify enabled"
            ],
            "created_at_ms": 1793254176371
          }
        },
        "kind": "plan",
        "entry": {
          "instruction": "Active declaration design:\n```json\n{\n  \"plan_id\": \"0d9cb210-8737-41fb-a3dd-601aec6d70c4\",\n  \"version\": 1,\n  \"title\": \"Build the API fixture\",\n  \"request\": \"Build the API fixture\",\n  \"declaration_files\": {\n    \"paths\": [\n      \"plan.json\"\n    ],\n    \"changed_paths\": []\n  },\n  \"document\": {\n    \"objective\": \"\",\n    \"requirements\": [],\n    \"background\": \"\",\n    \"decisions\": [],\n    \"design\": \"\",\n    \"flows\": [],\n    \"verification\": [],\n    \"tests\": []\n  }\n}\n```\n\nYou are planning a software change in the Harness Plan task. The effective permission for this turn governs repository access. Planning may inspect source but must not modify project files.\n\n# Design a software change through declarations\n\nDesign the final interfaces and ownership structures required by the user's request.\nHarness starts with an empty declaration design and captures each existing file's immutable\nbaseline on its first successful edit. Edit virtual\noverview files, complete configuration files, and the virtual plan.json specification, then submit them for mandatory review. Do not implement the design or\nmodify project files.\n\n## Procedure\n\n1. Inspect the request and affected source. Read implementation source to understand behavior,\n   constraints, and ownership. Reuse existing interfaces unless the change requires a new boundary.\n   For every code-change plan, identify the affected languages and inspect their installed\n   compilers or interpreters and project-selected versions. Read version configuration and use\n   read-only version queries from the project directory. Base syntax, API, and dependency choices\n   on those observed versions, even when no dependencies change. Report a missing or unresolved\n   toolchain instead of assuming a version.\n2. Use the supplied plan identity, version, and feedback context. Call `plan_read` when\n   current text, version, or additional context is needed. Omit `path` for the inventory. Supply\n   `path` and optional 1-based inclusive `start_line` and `end_line` for the affected range.\n   Set `baseline: true` to read original declarations. For an uncaptured path, Harness extracts only\n   that workspace file and returns its source digest without adding it to the saved design.\n   Read existing files before their first edit. Optional `source_digests` on a patch maps paths to\n   the returned digests and preserves inspection identities across turns. Harness also retains\n   inspected identities within the current turn. Omit displayed line-number prefixes from patches.\n3. Resolve discoverable facts from source. Ask only about material decisions source cannot answer.\n   Respect requests not to ask questions. Use `question_ask` when needed and end the turn.\n4. Design complete declaration files and required configuration changes. Preserve unchanged declarations, private members, complete\n   types, generics, visibility, documentation, and associations. Add only useful requested work.\n   Give every declaration in each source overview you author or revise an attached explanatory code comment.\n   Record objective, requirements, background, decisions, design, flows, checks, verification, and tests in virtual `plan.json`. Add usage examples only when useful.\n   Do not author JSON entities, tasks, stages, prerequisites, or execution reports.\n5. Edit with `design_patch`, supplying `plan_id`, `expected_version`, and `patch`.\n   An optional `title` names the design. Multiple files and chunks can change atomically. Use the\n   returned version in the next call. The response includes a compact applied diff. Use it to confirm\n   a focused edit without rereading the file. Read additional ranges only for missing context,\n   unexpected results, or a truncated diff. Correct every validation failure.\n   Update, Delete, and Move capture an existing source baseline only when the whole patch succeeds.\n   Add requires an absent workspace destination. Later edits use the saved proposal, including\n   retained deletion and rename state, without extracting the source again.\n6. Keep `plan.json` consistent with the final declarations. Call `plan_submit` with the exact current `plan_id` and `expected_version`.\n   Submission requests review and never authorizes implementation. If further work or user input arrives\n   in the same turn, continue reading and editing the plan, then submit its latest version again.\n   Any edit after submission requires a new successful submission before the plan is ready for review.\n\n## Dependency selection\n\nInspect the project's selected compiler or runtime, package manager, declared support range,\nmanifest constraints, and lockfile before choosing dependencies. Resolve the versions actually\nselected in the project directory. For example, check Rust toolchain files and rust-version,\nNode version files and package.json engines/packageManager, or Python requires-python and the\nproject environment. Choose the newest stable release compatible with those constraints and\nthe required features, including relevant peer and transitive dependency requirements.\n\nAnother installed version does not change the project's compatibility target. Do not switch\nor upgrade compilers, runtimes, or package managers merely to use a newer dependency. If no\ncompatible release meets the task, report the conflict and make any required environment change\nan explicit design decision.\n\nUse API documentation for the selected dependency version when designing declarations and\nexamples. Record material compatibility constraints in Background. Documentation retrieval\nfailures alone do not justify downgrading. Planning selects and proposes dependencies but does\nnot install them or run compilation checks.\n\n## Plan metadata\n\nHarness creates a virtual `plan.json` containing `objective`, optional `usage`, `requirements`,\n`background`, `decisions`, `design`, `flows`, `checks`, `verification`, and `tests`. It is plan metadata, never a project file\nor declaration overview. Read and update it through the same tools as declaration files. Do not\nmove it, delete it, or add other fields. Drafts start with empty strings and lists, with `usage` absent.\n\nWrite for an implementer who can inspect the repository and proposed declarations but has no\nconversation history. Preserve settled user requirements and consequential decisions in this metadata.\nDo not depend on phrases such as \"as discussed\" or details available only in earlier messages.\nKeep every section consistent with the final declarations and update it when review changes the plan.\n\nConsult `technical-writing` and its Architectural Overview profile before drafting or revising plan\nprose. Apply its guidance within the structure and length limits below.\n\nThe review order is Objective, Usage when present, Requirements, Background, Decisions when nonempty,\nDesign, Flows when nonempty, proposed declaration changes, Tests, and Verification. Each section has a distinct role:\n\n### Objective\n\nWrite `objective` as a short statement of the requested outcome and scope. State what the user\nneeds to accomplish or which failure needs correction. For new functionality, state the task\nwithout inventing a defect. Do not substitute an inventory of files or implementation steps.\n\n### Usage\n\nUse the optional Markdown string `usage` for representative successful use. Omit the field when\nexamples add no useful information, such as an internal refactor. An empty string or null also\nhides this section. State the relevant starting conditions, input or action, and observable result.\nDistinguish illustrative values from exact required output.\n\n- For a CLI, show the command or stdin followed by stdout, stderr, and exit status.\n- For a UI, show the starting state, user action, and visible result.\n- For an API, show the request, response, and observable side effects.\n- For a library, show the call and preconditions, then its return value, mutation, or error.\n- For a background process, show the trigger and eventual observable result.\n- For a data migration, show representative data before and after the migration.\n\n### Requirements\n\nIdentify the 2–4 key requirements: requested outcomes and essential acceptance constraints.\nWrite them as a JSON array of nonempty strings. For bug fixes, this section can be omitted by\nleaving `requirements` empty when the objective and design already capture the acceptance criteria.\n\nStart each entry with a short, bold sentence stating an observable guarantee. Add one plain-text\nsentence only when it supplies necessary conditions or scope. Store the Markdown bold markers\ninside the requirement string. The bold sentence must communicate the guarantee on its own.\nUse direct behavior statements rather than repeating a feature name with \"Provide\", \"Support\",\nor \"Implement\". Do not repeat the objective or explain why a design choice was selected.\n\nThese examples show the format. Derive each guarantee from the current task rather than copying\nan unrelated example:\n\n- **Cancellation preserves saved data.** Cancelling before publication leaves previously saved\n  records unchanged.\n- **Failed imports leave stored records unchanged.** Report invalid rows without committing\n  any part of the import.\n- **Existing callers remain compatible.** Preserve public signatures and documented error\n  behavior.\n\nKeep entries at a consistent level of abstraction. Group closely related behaviors without\ncombining unrelated requirements merely to reduce the count. Put ownership and lifecycle mechanisms\nin Design, exact defaults and local behavioral contracts in declarations, consequential choices in\nDecisions, and individual test scenarios in Tests. Preserve an exact API, dependency, or mechanism\nhere when the user explicitly requires it. Retain acceptance-critical error, recovery, compatibility,\nand scope constraints.\n\nRequirements describe properties of the delivered change. Preserve task-specific constraints such\nas compatibility, offline operation, or protecting user configuration. Do not promote every design\nchoice into a mandatory requirement or create implementation milestones. Keep routine validation\nand implementation safeguards in the relevant Design or declaration unless they are explicitly\nrequested or essential to a task-specific acceptance constraint. Do not invent numeric limits or\nadditional product requirements. Ground any required limit in the user's request or a verified\nconstraint.\n\nWhen following an existing rule requires concrete work, plan the resulting change instead of\nrepeating the rule. Include supported file changes in the proposed files and put exact verification\ncommands and observable manual checks in Verification. Mention a generated artifact or unsupported\nfile change in Design only when its specific contents are essential to the requested change and are\nnot represented elsewhere, using a brief sentence alongside its owning component. A routine README\ncontents list does not explain the design and belongs outside this section. Do not invent\nplaceholder generated contents or add a separate file inventory.\n\nWhen the change introduces generated files, inspect the project's existing ignore rules and\npropose any needed .gitignore addition or update as a normal virtual file change. Preserve existing\ncomments, negations, and unrelated rules. Add appropriate generated paths, such as /target/ for a\nroot Rust package. Do not propose an empty file merely because .gitignore is absent. Keep source\nand required lockfiles tracked. Generate or update required lockfiles during implementation, not\nin Plan. Routine lockfile handling belongs to implementation and needs no Design prose. The model\ncreates or updates the actual file during implementation. Do not modify workspace files in Plan.\n\n### Background\n\nWrite `background` as Markdown explaining the existing system directly relevant to this task.\nIdentify inspected components using repository-relative paths and symbol names. Explain what each\nowns, how the affected operation currently flows between them, and the integration points for this\nchange. Include existing behavior that must be understood or preserved, such as persistence,\nerror propagation, and platform limits. Explain the current limitation when applicable.\n\nDescribe established starting conditions and explicitly identify material unresolved constraints.\nOmit discovery chronology, tool failures, and evidence-gathering details unless they leave an\nuncertainty that changes implementation or verification. Include only facts needed to understand\nor implement this change. Do not give a repository tour, summarize the conversation, repeat the\nrequested behavior, or describe the proposed solution. For a new project, briefly describe the\nstarting repository, available infrastructure, and relevant conventions.\n\n### Decisions\n\nWrite `decisions` as a JSON array of objects with nonempty `decision` and `rationale` strings.\n\nRecord consequential choices that resolve questions left open by the task. The answer may come\nfrom asking the user or from your own planning. Include choices where a different answer would\nmaterially change the behavior, scope, or integration contract.\n\nIn `decision`, state the selected answer directly. In `rationale`, explain why it fits this task\nand any consequence or tradeoff the implementer must preserve. Record the settled choice, not\nthe conversation or deliberation that produced it.\n\nA choice resolved through user clarification belongs here. Requirements already fixed by the\noriginal request do not need repeating. Omit solution summaries, component inventories, routine\nconventions, and dependency selections without a task-specific tradeoff. An empty list is valid.\n\nThese examples show rendered decisions. Store each bold sentence as `decision` without the bold\nmarkers and its explanation as `rationale`. Derive the actual choices from the current task rather\nthan copying these examples:\n\n- **Update matching records and create unmatched records.** Users can import revised CSV files\n  to maintain existing data instead of creating duplicates.\n- **Reject the entire import if any row is invalid.** Report row-specific errors and leave stored\n  data unchanged, so users can correct and retry the file without reconciling a partial import.\n- **Resume interrupted jobs from the last completed item.** Completed work remains recorded, and\n  restarting retries only unfinished items. This requires durable progress tracking.\n- **Pause synchronization when both copies of a file have changed.** Preserve both versions and\n  ask the user to resolve the conflict, preventing automatic synchronization from discarding an edit.\n- **Preserve the public API during the internal refactor.** Existing callers can adopt the change\n  without modifying their code. Public API cleanup remains outside this refactor.\n- **Generate reports as background jobs.** Large reports can finish after the user leaves the page.\n  Users receive a completion notification and download the result when it is ready.\n\n### Design\n\nWrite `design` as Markdown explaining how the proposed solution works and satisfies the requirements.\nUse an unheaded opening paragraph followed by `### Ownership`. Store flows separately in `flows`.\nUse this template, replacing the bracketed instructions with task-specific content:\n\n```markdown\n[One paragraph of 2–3 sentences: describe the concrete solution, explain the main structural choice and its reason, then introduce the main objects and their roles.]\n\n### Ownership\n[One paragraph of 1–2 sentences per main owner: its responsibility, the meaning of its owned state, and its boundary with other owners. Define supporting code names as they appear.]\n```\n\n**Opening.** Write one paragraph of 2–3 sentences explaining how the design solves the user's task.\nFollow this progression:\n\n1. Concrete solution: state what will be implemented or changed and its defining behavior.\n   Describe the proposed solution, not just the objective.\n2. Structural choice and reason: explain the main organizing choice and one concrete capability\n   or guarantee it provides.\n3. Main objects: introduce the principal components by their actual code names and briefly\n   explain their roles.\n\nCombine the structural reason and component introduction when two sentences suffice. Expand\nresponsibilities in Ownership and interactions in Flows. Keep configuration values, detailed\nalgorithms, and secondary components out of the opening.\n\nThe examples below demonstrate structure and level of detail. Derive the solution, reasoning,\nand component names from the current task and design. Do not copy the examples' implementation\nchoices. The generated opening is one paragraph without the example labels.\n\n**Feature: CSV import**\n\nSolution: Implement CSV import as a workflow that previews incoming records, reports validation\nerrors, and saves the import after confirmation.\n\nReason: Separate validation from persistence so users can correct invalid records before\nchanging stored data.\n\nObjects: `ImportSession` holds the pending records and validation results, while `ImportService`\nvalidates the data and commits the confirmed import.\n\n**Feature: log-search CLI**\n\nSolution: Implement log search as a command that filters files by time range and severity, then\nprints matching records.\n\nReason: Process files incrementally so memory use does not grow with the total size of the\ninput files.\n\nObjects: `SearchCommand` handles arguments, `LogScanner` reads and filters records, and\n`MatchWriter` formats the output.\n\n**Bugfix: cancellation leaves the interface stuck**\n\nSolution: Make cancelling a request stop its active work and return the interface to idle.\n\nReason: Associate callbacks with a request generation so results arriving after cancellation\ncannot restore stale progress.\n\nObjects: `RequestController` owns the active generation and cancellation handle, while `RunState`\nrecords the status displayed by the interface.\n\n**Refactor: duplicated authentication**\n\nSolution: Move authentication from individual route handlers into shared middleware while\npreserving existing responses.\n\nReason: Centralize credential validation so every protected route applies the same expiry and\nrevocation checks.\n\nObjects: `AuthMiddleware` gates protected requests, `TokenVerifier` validates credentials, and\n`RequestIdentity` carries the authenticated user to the handler.\n\n**Bugfix: interrupted saves corrupt settings**\n\nSolution: Preserve the last complete settings file when saving is interrupted or fails.\n\nReason: Write the replacement to a temporary file and publish it atomically so readers never\nobserve partially written settings.\n\nObjects: `SettingsStore` owns serialization and loading, while `AtomicFileWriter` manages the\ntemporary file and final replacement.\n\n**Ownership.** Expand the owners introduced in the opening, in the same order, using one paragraph\nof 1–2 sentences per owner. State what each owner creates, changes, or retains, and where its authority\nends. Introduce supporting types with their meaning, such as \"`ImportSession`, the pending records\nand validation results\", rather than listing names after \"owns\". Group supporting modules and data under that\nowner. Use connected prose, not bullets, file inventories, or paragraphs for every type.\nKeep thin composition wrappers in the opening unless they own a separate lifecycle. Begin Ownership\nwith the component that controls the central state, not its configuration or launcher. Configuration\nfields, numeric defaults, key bindings, visual styling, algorithms, and schedule-by-schedule details\nbelong in the relevant declarations. Include a detail here only when changing it changes the\nownership boundary. Do not add README contents, documentation chores, or repeated setup descriptions.\n\nDescribe the scope of the change, not the architecture of the entire system. For bugfixes, explain\nthe invariant being restored and the mechanism that restores it. Describe only the existing owners\nand interactions relevant to the fix. A single ownership paragraph and one flow are sufficient\nfor a localized change, and an existing function or module can be the owner. Do not invent new\ncomponents to fill the template.\n\nDescribe the target design in present tense. Preserve terminology across sections, remove repeated\nclaims, and demonstrate guarantees through concrete mechanisms. Keep detailed contracts in\ndeclarations, detailed tradeoffs in Decisions, and existing behavior in Background. Flows retain the ordering\nand failure rules needed to explain the result, without repeating local algorithms or configuration\nvalues. Execution instructions remain governed by the active system and repository instructions.\nThe complete plan must support implementation without the prior conversation.\n\nOnly the opening and Ownership belong in the `design` string. Omit a Files\nand artifacts section, repeated file inventories, routine housekeeping, and explanations of Harness\nformat limitations. Do not include executable implementation bodies, mutable execution progress,\nor claims that implementation or verification has finished.\n\nBefore submission, review the Design against this template. Confirm that the opening describes the\nsolution, explains its structure, and introduces the main objects. Define unexplained code names\nand move configuration or implementation detail out of Ownership. Confirm that every flow arrow\nrepresents production, transformation, storage, or consumption through the described objects.\nShorten any ownership paragraph exceeding two sentences instead of joining unrelated clauses.\nKeep acceptance-critical behavior in the plan, placing each detail in its designated section.\n\n### Flows\n\nWrite `flows` as an array of `{title, description, trigger, steps}` objects, or [] when no runtime\nflow adds useful information. Harness generates the diagram after Design. Do not put a Flows\nheading or hand-formatted arrows in `design` or flow labels.\n\nEach flow describes one execution started by one explicit event. Use a concise description for\nits purpose and ordering guarantees. Separate events into separate flows even when they share\nstate: collecting a shield stores charges, while a later enemy contact reads those charges.\nDo not connect these actions into one execution merely because one uses the other's output.\n\n- `trigger` names the event that starts this execution.\n- `steps` is an ordered array. Each step has a required `action` naming a function or concrete\n  operation, optional `effects`, and optional `branches`.\n- Each effect has exactly `target` and `change`. It is terminal data describing an observable\n  change, such as create, update, remove, or preserve. Effects cannot have steps, actions,\n  children, branches, or triggers. Put subsequent execution in the enclosing `steps` array.\n- Each branch has a required `condition` and a nonempty `steps` array. The condition is evaluated\n  during the current execution. A later event requires a new flow, not a conditional branch.\n  Each applicable branch runs its steps before the next enclosing step. When alternatives are\n  exclusive, write mutually exclusive conditions explicitly. Branches do not mean parallel work.\n\nUse descriptive labels and the exact declaration names where available. Harness normalizes label\nwhitespace for display while retaining the original content. Keep resulting\nstate in `effects`, rather than treating data as executable steps. Express conditions that govern\nexecution in branch conditions or action labels instead of adding separate input inventories.\nFlow titles must be unique. There are no total flow, step-count, or nesting-depth limits.\nKeep diagrams focused for readability without omitting meaningful execution or effects.\n\nFor example, collection and contact are independent triggers:\n\n```json\n[\n  {\n    \"title\": \"Collect a shield\",\n    \"description\": \"Collection stores protection for a later accepted hit.\",\n    \"trigger\": \"player overlaps a shield pickup\",\n    \"steps\": [{\n      \"action\": \"collect_shield_pickups\",\n      \"effects\": [{\"target\": \"PlayerAbilities\", \"change\": \"grant shield charges to\"}]\n    }]\n  },\n  {\n    \"title\": \"Absorb contact\",\n    \"description\": \"An accepted contact consumes available protection.\",\n    \"trigger\": \"accepted enemy contact\",\n    \"steps\": [{\n      \"action\": \"PlayerAbilities.take_hit\",\n      \"branches\": [{\n        \"condition\": \"shield charges remain\",\n        \"steps\": [{\"action\": \"absorb hit\", \"effects\": [\n          {\"target\": \"shield charge\", \"change\": \"consume one\"},\n          {\"target\": \"contact immunity\", \"change\": \"start\"}\n        ]}]\n      }]\n    }]\n  }\n]\n```\n\nThe renderer uses `→` for the first action, `↓` for subsequent execution, and `┄▶` for effects.\nConditions introduce branch steps. Effects never acquire execution arrows.\n\nUse Markdown inline code for code identifiers, concrete paths, and commands in prose fields.\nEncode paragraph breaks as `\\n\\n` inside JSON strings.\nThe required tests array inventories every test involved in the plan, grouped by project-relative\nfile. Each cases entry names the test (module-qualified when needed), its change (new, modified,\nremoved, or reused), and a description of the scenario and expected result or removal rationale.\nInclude inline test modules and separate test files consistently. List existing coverage being\nreused, omit unrelated repository tests, and use [] when no tests apply. Keep names unique within\na file and list each file once. Native Cargo case checks require the full runtime test name.\nUse the optional target field (lib, bin:<name>, or test:<name>) when a file belongs to multiple\nCargo test targets. Tests is rendered\nbefore Verification and records planned coverage. Verification retains execution commands and manual checks.\nObjective, Background, and Design must be nonempty before submission.\n\nA plan's metadata can use this shape:\n\n```json\n{\n  \"objective\": \"Support observable, cancellable texture replacement.\",\n  \"usage\": \"Start a replacement, cancel it before publication, and confirm the existing texture remains visible.\",\n  \"requirements\": [\n    \"**Cancellation preserves the current texture.** Cancelling before publication leaves the visible texture unchanged.\",\n    \"**Submitted frames retain valid textures.** Allocations remain valid until every submitted frame using them completes.\"\n  ],\n  \"background\": \"The existing renderer obtains published handles from `TextureRegistry`. Submitted frames can outlive the frame in which a replacement is requested.\",\n  \"decisions\": [\n    {\n      \"decision\": \"Publish replacements at frame boundaries.\",\n      \"rationale\": \"A single publication point keeps each submitted frame's texture selection consistent.\"\n    }\n  ],\n  \"design\": \"Support texture replacement that reports progress and can be cancelled while the current texture remains visible. Separate preparation from publication so pending work leaves the current texture usable. `TextureStreaming`, the preparation service, passes completed replacements to `TextureRegistry`, the owner of the handles used by the renderer.\\n\\n### Ownership\\n`TextureStreaming` creates and prepares each replacement, exposing its progress and cancellation through `TextureRequest`, the caller's handle to pending work. Preparation leaves the published texture unchanged.\\n\\n`TextureRegistry` controls which texture the renderer can use and retains replaced allocations while submitted frames still reference them. Only the registry publishes or retires a texture.\",\n  \"flows\": [\n    {\n      \"title\": \"Publish a replacement\",\n      \"description\": \"Publication at a frame boundary selects the prepared texture for rendering.\",\n      \"trigger\": \"prepared replacement is ready at a frame boundary\",\n      \"steps\": [\n        {\n          \"action\": \"TextureRegistry.publish\",\n          \"effects\": [\n            {\n              \"target\": \"published handle\",\n              \"change\": \"replace\"\n            }\n          ]\n        },\n        {\n          \"action\": \"Renderer\"\n        }\n      ]\n    },\n    {\n      \"title\": \"Retire an old allocation\",\n      \"description\": \"The replaced allocation remains alive until its last submitted frame completes.\",\n      \"trigger\": \"last submitted frame using the old allocation completes\",\n      \"steps\": [\n        {\n          \"action\": \"TextureRegistry.retire\",\n          \"effects\": [\n            {\n              \"target\": \"replaced allocation\",\n              \"change\": \"release\"\n            }\n          ]\n        }\n      ]\n    },\n    {\n      \"title\": \"Cancel preparation\",\n      \"description\": \"Cancellation releases only the unpublished replacement.\",\n      \"trigger\": \"cancellation requested\",\n      \"steps\": [\n        {\n          \"action\": \"TextureRequest.cancel\"\n        },\n        {\n          \"action\": \"TextureStreaming.discard\",\n          \"effects\": [\n            {\n              \"target\": \"unpublished allocation\",\n              \"change\": \"release\"\n            }\n          ]\n        }\n      ]\n    }\n  ],\n  \"verification\": [\"Cancel a pending replacement and confirm the current texture remains visible.\", \"Replace a texture with frames in flight and confirm rendering remains valid.\"],\n  \"tests\": [\n    {\n      \"file\": \"src/texture.rs\",\n      \"cases\": [\n        {\n          \"name\": \"tests::texture_replacement_cancellation_preserves_published_handle\",\n          \"change\": \"new\",\n          \"description\": \"Cancel before publication and verify the original texture handle remains published.\"\n        },\n        {\n          \"name\": \"tests::texture_replacement_retains_in_flight_allocation\",\n          \"change\": \"new\",\n          \"description\": \"Replace a texture with frames in flight and verify its allocation remains valid until their completion.\"\n        }\n      ]\n    }\n  ]\n}\n```\n\nThis example illustrates the schema. Use inspected project facts and actual supported commands\nfor the real plan. Read the saved text before patching when it is not already available. Metadata\nand declaration edits can share one atomic patch:\n\n```text\n*** Begin Patch\n*** Update File: plan.json\n@@\n-  \"objective\": \"\",\n+  \"objective\": \"Support observable, cancellable texture requests.\",\n+  \"usage\": \"Start a replacement, cancel it before publication, and confirm the existing texture remains visible.\",\n*** End Patch\n```\n\n### Checks and verification\n\nAutomated check definitions belong to the project's saved `.forge.json`. Harness configures missing or invalid checks before starting this planning turn. Read the saved configuration through `check_definition_list`. An explicitly empty configuration is valid. If the configuration becomes missing or invalid during planning, report the submission failure and use the harness-checks skill to repair it with check-editing permission. Test inventory describes intended coverage independently of commands. Applicable enabled checks with `auto:true` are included automatically. Use `plan_checks_add` with the current plan ID, version, and check IDs to add other enabled checks explicitly. The `checks` array in virtual `plan.json` stores those references, not commands. Never run validation directly through shell tools while planning or implementing. During implementation and resolution, finish the phase and let Harness Checking run validation.\n\nHarness executes these commands locally, outside the model provider sandbox, after Implement and Resolve. Harness records exit status, duration, and saved stdout/stderr and checks declaration conformance. The model must not execute automated checks during Implement, Resolve, or manual Verify.\n\nRecord manual actions in `verification`, an array of actions and expected observations. Identify actions requiring a person or unavailable hardware. Manual Verify starts only after declarations and automated checks pass. Planning records requirements, not results. Execution state and results remain Harness-owned.\n\n## Overview syntax\n\nPaths match project-relative source paths. Supported extensions are `.rs`, `.ts`, `.tsx`, `.lua`, and\n`.toml`, `.json`, `.jsonc`, `.yaml`, `.yml`, and XML configuration paths (including `.xml`,\n`.csproj`, `.fsproj`, `.vbproj`, `.props`, `.targets`, `.resx`, and `.plist`), plus `.gitignore` files whose contents are preserved verbatim. Source overviews retain native layouts but are not compilable implementation files. Never write\nexecutable function bodies, empty or placeholder bodies, or executable initializers. Academic\npseudocode belongs only in an indented `Algorithm` block, never in a native-language body. Preserve\nreference qualifiers, generic arguments, and complete return types. Do not infer missing types.\n\nPreserve syntactic nesting and attach documentation to its declaration. Harness owns presentation\nformatting, indentation style, blank-line spacing, column widths, and line wrapping. Do not manually\nwrap content or shorten names, comments, commands, or diagrams to satisfy a display width.\nHarness formats each requested workspace overview and captured baseline, then canonicalizes both\ndeclaration snapshots on submission. The repository line width is a formatting preference, not a\nsubmission constraint. Literals, code examples, and indivisible types remain intact. Read virtual\nfile ranges when their exact current text is unavailable and match that text, including indentation.\nDraft patches retain their layout until submission. Submission can reformat declarations, so read\naffected ranges before another revision unless current formatted text is already supplied in feedback.\n\n## Function algorithms, changes, and references\n\nCategorize callable behavior against the captured source baseline, including after resubmission:\n\n- A new function or method requires one nonempty `Algorithm` block describing its complete behavior\n  in academic pseudocode. A `Change` block does not satisfy this requirement.\n- A modified function or method uses one `Change` block, preferably one short sentence: state\n  the concrete problem or purpose, then the behavioral change that addresses it. Ground the reason\n  in the task or inspected baseline, without inventing a defect. Use a second short sentence only\n  when essential. Omit unchanged behavior and implementation detail that does not explain the delta.\n  Include internal changes even when its signature and reference lists remain identical. Do not\n  restate the complete algorithm or use `Algorithm` for an existing callable. Unmodified callables\n  do not need a `Change` block.\n\nPlace the heading at the signature's indentation and its content one level deeper, before `Calls`,\n`Constructs`, and `Accesses`, in that order. Never combine `Algorithm` and `Change` on one callable.\nPartial drafts may omit a new algorithm, but submission rejects that omission. Preserve an authored\nblock unless its intended behavior changes. Removing `Change` withdraws that delta. Configuration\nand non-callable declarations do not have behavior blocks. Review folds both categories with their\ncallable and never appends `[changed]` to a collapsed signature.\n\n### Academic pseudocode\n\nRead and apply the `academic-pseudocode` skill before authoring `Algorithm` blocks. Its local\n`SKILL.md` contains the complete writing rules. Keep algorithms as high-level and compact as\npossible while preserving the complete behavioral contract.\n\nFor this plan, keep domain identifiers searchable and preserve numeric bounds, tie-breaking,\nsaturation, ownership, and deferred effects explicitly. Function signatures already declare inputs\nand results, so do not repeat them as boilerplate in each `Algorithm` block.\n\nAlgorithm submission validates statement syntax and indentation with Forge's pseudocode parser.\nUse `target ← expression` for assignment, `if condition`, `otherwise if condition`, `otherwise`,\n`for each item in collection`, and `while condition`. Every control statement needs a nonempty\nbody indented with spaces. A trailing colon is optional. Keep each statement on its own line,\nalign siblings, and dedent to an existing enclosing level. Use `return` with an optional result.\nUse `break` and `continue` only inside loops. Expressions and operation statements may use concise\nnatural language, identifiers, numbers, quoted strings, parentheses, and brackets. Use `=` for\nequality and `≠`, `≤`, or `≥` for comparisons. Omit native-language braces and semicolons.\nIncomplete drafts remain editable. Repair the reported callable, line, and column before resubmitting.\n\nKeep explicit field references consistent with `Accesses`: `Projectile.previous_position` in\nan Algorithm requires `Projectile::previous_position` in Rust's `Accesses` section. Use signature\nbindings for instance receivers when available. Qualified fields must name real declared members.\n\nMake construction explicit as `Type(field = value, ...)`, or `Type()` for a zero-field marker.\nUse the exact qualified name from `Constructs`, including factory names such as\n`Store::new(config = config)`. Every target listed in `Constructs` for an Algorithm must have\na matching invocation with named arguments or no arguments. Construct values before describing\nthe operation that queues, spawns, inserts, or publishes them. Reused shared assets are not new\nconstructions. Ordinary helper calls may use positional arguments. Do not hide constructed values\nand their initial fields in prose such as \"queue a shot with damage\".\n\nSource inspection extracts function-body references alongside declarations in Rust, TypeScript,\nTSX, and Lua. Use these sections, with one qualified target per line and no arguments:\n\n- `Calls` records invocations other than construction and callable values, including callback\n  registration. A callable passed as a callback remains here even when it is a factory.\n- `Constructs` records types instantiated directly, such as `Weapon` for `Weapon { ... }` or\n  `new Weapon(...)`, and factory calls whose declared return is an owned named type. List the\n  callable itself, such as `Weapon::new` or `Weapon::default` returning `Self` or `Weapon`.\n  Use the return type, not a method-name convention. Borrowed, scalar, and unit returns stay in\n  `Calls`. Rust tuple and unit constructors belong here when their type is resolved.\n  Do not list a type here merely because it appears in a signature, type annotation, generic\n  argument, cast, or destructuring pattern.\n- `Accesses` records field reads, writes, references, and named value uses. A struct initializer\n  such as `Weapon { cooldown_remaining: 0.0 }` records `Weapon` under `Constructs` and\n  `Weapon::cooldown_remaining` under `Accesses`. Destructured fields also belong in `Accesses`.\n\nPreserve occurrence order and repetitions within each category. Parsing retains captured\ninterleaving and semantic kinds in saved data. Unknown or computed receivers remain unresolved\nand must not be matched to a same-named property without type evidence.\n\nEvery newly declared internal symbol needs a resolved incoming use before submission. Include each registered system or callback in the registering function's Calls, even when the framework invokes it later. Include new property and value uses in Accesses and new type uses in signatures or Constructs. Imports, declaration names, owner headers, prose, and self references alone do not count. Truly public APIs, recognized executable entry points, tests, and trait contract items need no in-plan caller. Public exposure includes module and member accessibility and public re-exports, so pub(crate) and pub items hidden in private modules remain internal. The rule applies to new symbols relative to the captured source baseline, including resubmission, while partial draft edits remain permitted.\nThe virtual file includes `Calls`, `Constructs`, and `Accesses` blocks after each captured callable signature.\nEdit it atomically with declarations through `design_patch`. Include names only, without\narguments, assignments, control flow, or other statements. Qualify methods with a receiver type only\nwhen source evidence identifies that type. Keep unresolved receiver names when evidence is absent.\n\nKeep call occurrences in extraction or authored order, including repetitions. Review deduplicates\ntargets while retaining parsed order by default. Preserve unchanged Calls blocks when editing\nsignatures. Update an owner's list when relationships change and include a block for a newly planned\ncallable when its relationships are known. An empty block states that the callable has no calls.\nAn absent block states that call information is unavailable. Do not manufacture missing information\nfor historical plans or configuration documents.\n\nThe structured list follows the signature at the same indentation level. Target names use one\nadditional indentation level:\n\n```text\npub fn dispatch(client: &Client);\nChange\n  Avoid futile retries of rejected credentials by stopping on authentication\n  failure and recording the final attempt.\nCalls\n  validate_policy\n  Client::send\n  record_metrics\n```\n\nA newly introduced callable instead has an algorithm, with reference evidence listed separately:\n\n```text\npub fn next_attempt(attempt: u32, limit: u32) -> Option<u32>;\nAlgorithm\n  if attempt ≥ limit\n    return none\n  return attempt + 1\n```\n\n## Declaration comments\n\nEvery declaration in a source overview you author or revise must have an attached explanatory\ncode comment, including private declarations, types, fields, enum variants, traits or interfaces,\nimplementation blocks, functions, methods, aliases, and module-level bindings. Preserve accurate\nexisting comments and add missing ones. Imports, attributes, parameters, and configuration entries\ndo not need separate declaration comments.\n\nStart newly added or edited comments with `Get` instead of `Returns`. Submission rejects comments\nwhose first word is `Returns`, ignoring case. The rule applies only to the start of the complete\ncomment, not later sentences or continuation lines. Unchanged captured comments remain valid.\n\nFollow the repository's code-comment instructions and, when available, the technical-writing\nskill's Code Comments and API Documentation profiles. Reuse instructions already loaded in context. This planning requirement makes declaration\ncomments mandatory even when general source-comment guidance permits omission.\n\nExplain the declaration's role, ownership boundary, or behavioral contract. For fields and\nvariants, explain their domain meaning or represented condition. For callables, explain the\nobservable result, mutation, or failure condition the signature cannot convey. Include relevant\nbounds and lifecycle or ordering constraints. Use only contracts supported by the inspected source\nor settled proposal. Do not invent behavior or narrate the signature, such as \"stores a value\"\nor \"gets the current texture.\" A comment on a container does not replace comments on its members.\n\nUse native comment syntax, such as Rust `///`, TypeScript JSDoc, and Lua `---` or `---@` annotations.\nKeep comments immediately attached to declarations and above their attributes. Before submission,\nread the edited overviews and check that every declaration has a useful comment consistent with\nthe final design. Comments describe the design without adding function bodies or pseudocode.\n\nRust retains structs, fields, enums, traits, aliases, imports, attributes, and `impl` blocks.\nTerminate callable signatures with a semicolon, including methods:\n\n```rust\n/// Publishes uploaded replacements while retaining textures still used by submitted frames.\npub struct TextureRegistry {\n  /// Published texture available to new frame submissions, if one has been installed.\n  current: Option<TextureHandle>,\n}\n\n/// Exposes the published texture and the frame-boundary replacement operation.\nimpl TextureRegistry {\n  /// Get the published handle without changing the active texture version.\n  pub fn current(&self) -> Option<TextureHandle>;\n\n  /// Installs a completed upload at a frame boundary and retains the previous allocation until its final GPU use.\n  pub fn publish(&mut self, replacement: TextureHandle);\n}\n```\n\nTypeScript retains classes, interfaces, aliases, fields, exports, and complete signatures:\n\n```typescript\n/** Provides asynchronous access to texture handles by their stable identifier. */\nexport interface TextureStore {\n  /** Resolves a usable handle or rejects when the texture cannot be loaded. */\n  get(id: string): Promise<TextureHandle>;\n}\n\n/** Resolves texture handles through the configured storage provider. */\nexport class TextureRegistry {\n  /** Provider used to resolve handles without exposing storage access to callers. */\n  private store: TextureStore;\n\n  /** Resolves the requested handle and propagates provider failures. */\n  get(id: string): Promise<TextureHandle>;\n}\n```\n\nArrow bindings retain their native header through `=>`, with no expression or body:\n\n```typescript\n/** Starts an observable texture request using the caller's identifier. */\nexport const request = <T>(id: T): TextureRequest =>;\n```\n\nKeep an absent return annotation absent. Lua table members appear as dotted bindings and named\nfunction headers beneath their module binding.\n\nLua retains bindings, named function headers, module returns, and declaration annotations.\nDo not add function-closing `end` to signature-only headers:\n\n```lua\n---Exposes access to the currently published texture handles.\nlocal M\n\n---Get the published handle for the requested texture.\n---@param id string\n---@return TextureHandle\nfunction M.get(id)\n\nreturn M\n```\n\n## Manifests and configuration\n\nJSON, JSONC, TOML, YAML, XML, and .gitignore files retain their complete contents, including values, comments\nwhere allowed, arrays, attributes, and nested structures. These are editable configuration proposals\nrather than abbreviated declarations. Include every affected manifest and configuration file when\nchanging dependencies, features, scripts, package settings, workspace members, or build targets.\nThis includes `Cargo.toml`, `package.json`, `tsconfig.json`, CI YAML, and XML project files as needed.\nKeep unaffected settings and include affected nested package manifests. Do not omit configuration\nchanges because the design focuses on source declarations.\n\nRead the existing virtual file and patch it with the same Add File, Update File, Delete File, and\nMove to operations. Use valid syntax and preserve exact string values. Source restrictions on\ninitializers do not apply to configuration. Harness validates configuration and preserves its text\nwithout applying declaration indentation or prose wrapping. Configuration remains visible in\npublic-only inspection. Do not write actual project files during planning.\n\nUse strict JSON for `package.json` and ordinary `.json` files. `.jsonc`, `tsconfig*.json`,\n`jsconfig*.json`, and JSON files beneath `.vscode/` allow comments and trailing commas, but require\nquoted property names and commas between values. YAML retains block scalars and multiple documents.\nXML retains elements, attributes, namespaces, comments, and CDATA. XML admission checks well-formedness,\nnot application schemas, and never fetches external DTDs. TOML admission rejects duplicate keys.\nThe root virtual `plan.json` remains reserved for plan metadata, even if the checkout contains a\nproject file with that name. Other nested `plan.json` paths are ordinary JSON proposals.\n\nFor example, a proposed dependency section retains its complete settings:\n\n```toml\n[dependencies]\nengine = { version = \"1.2\", default-features = false, features = [\"render\"] }\n```\n\n## Context-based file patches\n\nPatch paths are virtual proposed files, not paths beneath a real `design/` directory. Add File\ncreates an overview, Delete File proposes source removal, and Move to follows Update File to\npropose a rename. `@@` starts a chunk. Context lines start with a space, removals with `-`, and\nadditions with `+`:\n\n```text\n*** Begin Patch\n*** Update File: src/textures.rs\n@@\n-  pub fn request(texture: TextureId) -> TextureHandle;\n+  pub fn request(texture: TextureId) -> TextureRequest;\n@@\n   pub fn status(request: RequestId) -> RequestStatus;\n+  /// Cancels a pending request and reports whether cancellation was accepted.\n+  pub fn cancel(request: RequestId) -> bool;\n*** End Patch\n```\n\nInvalid patches change no files and do not increment the version. No-op patches retain the current\nversion. Changed patches increment it. Correct the reported syntax,\ncontext, or version and retry. Never bypass validation with ordinary filesystem tools.\n\n## Review revisions and boundaries\n\nReview feedback shows file excerpts with one line number per row. `-` marks removed baseline text,\n`+` marks proposed additions, and unmarked rows provide context. Removed rows use baseline line\nnumbers. Other rows use proposed line numbers. Comments follow each excerpt as `8: comment` or\n`8–12: comment`. Replacements can share a line number. These excerpts are review context, not patch\ninput. Resolve each comment against the saved design and preserve its intended context.\nRead current proposed files before revising them. Preserve useful decisions and submit the new version.\n\nUnsupported file formats cannot be edited through declaration controls. Preserve essential changes\nto those files briefly alongside their owning component in Design when not represented elsewhere,\nwithout explaining the format limitation or inventing a supported-file substitute. Planning never writes\nimplementation bodies or performs execution. For a function behavior change, retain the signature\nwhen it remains valid and add or update its Change summary in the virtual declaration. Explain the\nbehavior in Design and include its coverage in Tests. Never manufacture an interface change to make\na diff appear. Declaration conformance does not prove runtime behavior.\n\nIf affected workspace source changed since extraction, report the need for a fresh baseline.\nDo not silently replace the baseline or modify project source.\n\n\nUser request:\nBuild the API fixture",
          "delivery": "accepted",
          "id": "f13219b4-1ba5-43e5-9b30-f302ca79501c"
        },
        "revision": "fd32622e-0ffd-4f3a-8824-64988cac0c9f",
        "turn": {
          "continuation": null,
          "id": {
            "exchange_ordinal": 1,
            "turn_ordinal": 0
          }
        },
        "created_at_ms": 1793254176371
      },
      "default_approval": "ask",
      "plan_approval": null,
      "created_at_ms": 1793254176316,
      "updated_at_ms": 1793254176557,
      "shell": "system",
      "native_fork": true,
      "native_compact": true,
      "workspace": "/fixture/nvim.0\\Nr9VKJ\\0\\workspace",
      "service_tier": "default",
      "auto_compaction": {
        "threshold_tokens": 128000,
        "enabled": false
      },
      "name": "Active declaration design: ```",
      "context_window": null,
      "id": "2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447",
      "backend": "mock",
      "purpose": "conversation",
      "provider_fork_state": {
        "state": "ready"
      },
      "lease_owner": "nvim-43764-51072127019700"
    },
    "checks_running": false,
    "check_timing": "after_queue",
    "no_checkpoint": true,
    "automatic_checks": false,
    "checks_deferred": false,
    "active_check_run": null,
    "capability": {
      "background_terminals": true,
      "approval_mode_list": [
        "ask",
        "deny",
        "allow"
      ],
      "native_fork": true,
      "native_compact": true,
      "os_sandbox": false,
      "compaction": {
        "summary": true,
        "lifecycle": true,
        "background": false,
        "metrics": true
      },
      "agent": {
        "input": true,
        "spawn": true,
        "observe": true,
        "interrupt": true,
        "parallel": true
      },
      "catalog": {
        "skill": false,
        "mcp": false,
        "live_mcp_mutation": false
      },
      "native_steer": true,
      "native_turn_rollback": true,
      "native_goal": false,
      "model_selection": true,
      "effort_selection": true,
      "fast_mode": true,
      "ultrafast_mode": true,
      "permission_control": true
    },
    "task_operation": {
      "error": null,
      "state": "completed",
      "revision": 1,
      "action": "begin_acceptance",
      "id": "example-acceptance"
    },
    "snapshot_revision": 20
  },
  "id": 23
}
```

<!-- /captured-example -->

## `session.plan.acceptance.cancel`

Cancel pending acceptance review.

- **Context:** Explicit existing session.
- **Input:** PlanReviewRequest fields and operation_id when supplied.
- **Result and effects:** Returns a session snapshot with task_operation after updating the owned acceptance state.
- **Boundary:** Cancellation of review is separate from cancelling a provider turn or clearing the entire task. Preserve the shared task command contract and response variant.
- **Inspected source:** [task/command.rs](../../../rust/forge/crates/forge-harness/src/task/command.rs), [service/task_state.rs](../../../rust/forge/crates/forge-harness/src/service/task_state.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/lua/forge/views/harness/controller.lua:898](../../../../nvim/lua/forge/views/harness/controller.lua).

<!-- detailed-contract -->
### Contract

Accepts [PlanReviewRequest](native-schemas.md#planreviewrequest) and optional stable `operation_id`. This is `action = "cancel_acceptance"` in [session.command.apply](#sessioncommandapply), defaulting to a snapshot response. It cancels the identified acceptance lifecycle without deleting the plan or claiming that execution completed. Stale or mismatched acceptance state fails. Replay returns the original receipt rather than cancelling a later lifecycle.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [plan_api_examples.lua](../../../../nvim/tests/forge/plan_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.plan.acceptance.cancel",
  "params": {
    "operation_id": "example-cancel-acceptance",
    "_command": {
      "id": "example-cancel-acceptance"
    },
    "plan_id": "0d9cb210-8737-41fb-a3dd-601aec6d70c4"
  },
  "session_id": "2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447",
  "id": 24
}
```

```json
{
  "result": {
    "ask_model": {},
    "access_abbreviation": "rxw",
    "agent": {
      "run": [],
      "summary": {}
    },
    "subagent_tools": false,
    "lsp_tools": false,
    "session_tools": true,
    "status_thoughts": false,
    "check_editing": false,
    "runtime_epoch": "c832b8be-b9d8-4a4e-a936-dccfbab52e11",
    "active_wait": null,
    "approval": [],
    "prompt_history": [],
    "status": {
      "revision": 1,
      "kind": "awaiting_plan_review",
      "plan_id": "0d9cb210-8737-41fb-a3dd-601aec6d70c4"
    },
    "artifact": [
      {
        "updated_at_ms": 1793254176585,
        "title": "Design the requested change",
        "working_path": "/fixture/nvim.0/Nr9VKJ/0/data/forge/harness\\plans\\2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447\\0d9cb210-8737-41fb-a3dd-601aec6d70c4\\working.md",
        "state": "awaiting_review",
        "created_at_ms": 1793254176371,
        "id": "0d9cb210-8737-41fb-a3dd-601aec6d70c4"
      }
    ],
    "active_elicitation": null,
    "timeline_revision": 3,
    "active_plan": {
      "updated_at_ms": 1793254176585,
      "session_id": "2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447",
      "document_version": 3,
      "state": "awaiting_review",
      "title": "Design the requested change",
      "working_path": "/fixture/nvim.0/Nr9VKJ/0/data/forge/harness\\plans\\2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447\\0d9cb210-8737-41fb-a3dd-601aec6d70c4\\working.md",
      "submitted_version": 2,
      "accepted_revision": null,
      "user_revision": 0,
      "model_revision": 1,
      "accepted_digest": null,
      "elicitation": null,
      "acceptance": null,
      "question_ledger": {
        "resolution": []
      },
      "request": "Build the API fixture",
      "review_digest": "48e3cfe2f821383be1029c5468751bb59603daf4292062137fefa0e02ec4558f",
      "generation": {
        "consecutive_no_progress": 0,
        "max_consecutive_no_progress": 2,
        "canonical_revision": 1,
        "turn_count": 1,
        "max_turn_count": 20
      },
      "validation_warning": [],
      "id": "0d9cb210-8737-41fb-a3dd-601aec6d70c4",
      "manual_changes": [
        "Updated plan checks: verify enabled"
      ],
      "created_at_ms": 1793254176371
    },
    "restore_recovery": null,
    "session": {
      "last_compaction_turn": null,
      "primary_agent_id": "2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447:agent:primary",
      "backend_session_id": "mock-session",
      "provider_checkpoint_id": null,
      "model": "default",
      "provider_label": "Mock CLI",
      "resolved_model": null,
      "plan_executor": {
        "enabled": false,
        "model": null,
        "effort": null
      },
      "plan_compact": false,
      "plan_auto_approve_revisions": true,
      "approval_timeout": {
        "amount": 10,
        "unit": "m"
      },
      "effort": "medium",
      "answer_timeout": {
        "amount": 10,
        "unit": "m"
      },
      "background": null,
      "access": {
        "sandbox": false,
        "windows_sandbox": "elevated",
        "read_access": "full",
        "write_access": "workspace",
        "readable_directory": [],
        "writable_directory": []
      },
      "context_usage": null,
      "approval_mode": "ask",
      "active_task": {
        "updated_at_ms": 1793254176585,
        "plan": {
          "paused": false,
          "artifact": {
            "updated_at_ms": 1793254176585,
            "session_id": "2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447",
            "document_version": 3,
            "state": "awaiting_review",
            "title": "Design the requested change",
            "working_path": "/fixture/nvim.0/Nr9VKJ/0/data/forge/harness\\plans\\2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447\\0d9cb210-8737-41fb-a3dd-601aec6d70c4\\working.md",
            "submitted_version": 2,
            "accepted_revision": null,
            "user_revision": 0,
            "model_revision": 1,
            "accepted_digest": null,
            "elicitation": null,
            "acceptance": null,
            "question_ledger": {
              "resolution": []
            },
            "request": "Build the API fixture",
            "review_digest": "48e3cfe2f821383be1029c5468751bb59603daf4292062137fefa0e02ec4558f",
            "generation": {
              "consecutive_no_progress": 0,
              "max_consecutive_no_progress": 2,
              "canonical_revision": 1,
              "turn_count": 1,
              "max_turn_count": 20
            },
            "validation_warning": [],
            "id": "0d9cb210-8737-41fb-a3dd-601aec6d70c4",
            "manual_changes": [
              "Updated plan checks: verify enabled"
            ],
            "created_at_ms": 1793254176371
          }
        },
        "kind": "plan",
        "entry": {
          "instruction": "Active declaration design:\n```json\n{\n  \"plan_id\": \"0d9cb210-8737-41fb-a3dd-601aec6d70c4\",\n  \"version\": 1,\n  \"title\": \"Build the API fixture\",\n  \"request\": \"Build the API fixture\",\n  \"declaration_files\": {\n    \"paths\": [\n      \"plan.json\"\n    ],\n    \"changed_paths\": []\n  },\n  \"document\": {\n    \"objective\": \"\",\n    \"requirements\": [],\n    \"background\": \"\",\n    \"decisions\": [],\n    \"design\": \"\",\n    \"flows\": [],\n    \"verification\": [],\n    \"tests\": []\n  }\n}\n```\n\nYou are planning a software change in the Harness Plan task. The effective permission for this turn governs repository access. Planning may inspect source but must not modify project files.\n\n# Design a software change through declarations\n\nDesign the final interfaces and ownership structures required by the user's request.\nHarness starts with an empty declaration design and captures each existing file's immutable\nbaseline on its first successful edit. Edit virtual\noverview files, complete configuration files, and the virtual plan.json specification, then submit them for mandatory review. Do not implement the design or\nmodify project files.\n\n## Procedure\n\n1. Inspect the request and affected source. Read implementation source to understand behavior,\n   constraints, and ownership. Reuse existing interfaces unless the change requires a new boundary.\n   For every code-change plan, identify the affected languages and inspect their installed\n   compilers or interpreters and project-selected versions. Read version configuration and use\n   read-only version queries from the project directory. Base syntax, API, and dependency choices\n   on those observed versions, even when no dependencies change. Report a missing or unresolved\n   toolchain instead of assuming a version.\n2. Use the supplied plan identity, version, and feedback context. Call `plan_read` when\n   current text, version, or additional context is needed. Omit `path` for the inventory. Supply\n   `path` and optional 1-based inclusive `start_line` and `end_line` for the affected range.\n   Set `baseline: true` to read original declarations. For an uncaptured path, Harness extracts only\n   that workspace file and returns its source digest without adding it to the saved design.\n   Read existing files before their first edit. Optional `source_digests` on a patch maps paths to\n   the returned digests and preserves inspection identities across turns. Harness also retains\n   inspected identities within the current turn. Omit displayed line-number prefixes from patches.\n3. Resolve discoverable facts from source. Ask only about material decisions source cannot answer.\n   Respect requests not to ask questions. Use `question_ask` when needed and end the turn.\n4. Design complete declaration files and required configuration changes. Preserve unchanged declarations, private members, complete\n   types, generics, visibility, documentation, and associations. Add only useful requested work.\n   Give every declaration in each source overview you author or revise an attached explanatory code comment.\n   Record objective, requirements, background, decisions, design, flows, checks, verification, and tests in virtual `plan.json`. Add usage examples only when useful.\n   Do not author JSON entities, tasks, stages, prerequisites, or execution reports.\n5. Edit with `design_patch`, supplying `plan_id`, `expected_version`, and `patch`.\n   An optional `title` names the design. Multiple files and chunks can change atomically. Use the\n   returned version in the next call. The response includes a compact applied diff. Use it to confirm\n   a focused edit without rereading the file. Read additional ranges only for missing context,\n   unexpected results, or a truncated diff. Correct every validation failure.\n   Update, Delete, and Move capture an existing source baseline only when the whole patch succeeds.\n   Add requires an absent workspace destination. Later edits use the saved proposal, including\n   retained deletion and rename state, without extracting the source again.\n6. Keep `plan.json` consistent with the final declarations. Call `plan_submit` with the exact current `plan_id` and `expected_version`.\n   Submission requests review and never authorizes implementation. If further work or user input arrives\n   in the same turn, continue reading and editing the plan, then submit its latest version again.\n   Any edit after submission requires a new successful submission before the plan is ready for review.\n\n## Dependency selection\n\nInspect the project's selected compiler or runtime, package manager, declared support range,\nmanifest constraints, and lockfile before choosing dependencies. Resolve the versions actually\nselected in the project directory. For example, check Rust toolchain files and rust-version,\nNode version files and package.json engines/packageManager, or Python requires-python and the\nproject environment. Choose the newest stable release compatible with those constraints and\nthe required features, including relevant peer and transitive dependency requirements.\n\nAnother installed version does not change the project's compatibility target. Do not switch\nor upgrade compilers, runtimes, or package managers merely to use a newer dependency. If no\ncompatible release meets the task, report the conflict and make any required environment change\nan explicit design decision.\n\nUse API documentation for the selected dependency version when designing declarations and\nexamples. Record material compatibility constraints in Background. Documentation retrieval\nfailures alone do not justify downgrading. Planning selects and proposes dependencies but does\nnot install them or run compilation checks.\n\n## Plan metadata\n\nHarness creates a virtual `plan.json` containing `objective`, optional `usage`, `requirements`,\n`background`, `decisions`, `design`, `flows`, `checks`, `verification`, and `tests`. It is plan metadata, never a project file\nor declaration overview. Read and update it through the same tools as declaration files. Do not\nmove it, delete it, or add other fields. Drafts start with empty strings and lists, with `usage` absent.\n\nWrite for an implementer who can inspect the repository and proposed declarations but has no\nconversation history. Preserve settled user requirements and consequential decisions in this metadata.\nDo not depend on phrases such as \"as discussed\" or details available only in earlier messages.\nKeep every section consistent with the final declarations and update it when review changes the plan.\n\nConsult `technical-writing` and its Architectural Overview profile before drafting or revising plan\nprose. Apply its guidance within the structure and length limits below.\n\nThe review order is Objective, Usage when present, Requirements, Background, Decisions when nonempty,\nDesign, Flows when nonempty, proposed declaration changes, Tests, and Verification. Each section has a distinct role:\n\n### Objective\n\nWrite `objective` as a short statement of the requested outcome and scope. State what the user\nneeds to accomplish or which failure needs correction. For new functionality, state the task\nwithout inventing a defect. Do not substitute an inventory of files or implementation steps.\n\n### Usage\n\nUse the optional Markdown string `usage` for representative successful use. Omit the field when\nexamples add no useful information, such as an internal refactor. An empty string or null also\nhides this section. State the relevant starting conditions, input or action, and observable result.\nDistinguish illustrative values from exact required output.\n\n- For a CLI, show the command or stdin followed by stdout, stderr, and exit status.\n- For a UI, show the starting state, user action, and visible result.\n- For an API, show the request, response, and observable side effects.\n- For a library, show the call and preconditions, then its return value, mutation, or error.\n- For a background process, show the trigger and eventual observable result.\n- For a data migration, show representative data before and after the migration.\n\n### Requirements\n\nIdentify the 2–4 key requirements: requested outcomes and essential acceptance constraints.\nWrite them as a JSON array of nonempty strings. For bug fixes, this section can be omitted by\nleaving `requirements` empty when the objective and design already capture the acceptance criteria.\n\nStart each entry with a short, bold sentence stating an observable guarantee. Add one plain-text\nsentence only when it supplies necessary conditions or scope. Store the Markdown bold markers\ninside the requirement string. The bold sentence must communicate the guarantee on its own.\nUse direct behavior statements rather than repeating a feature name with \"Provide\", \"Support\",\nor \"Implement\". Do not repeat the objective or explain why a design choice was selected.\n\nThese examples show the format. Derive each guarantee from the current task rather than copying\nan unrelated example:\n\n- **Cancellation preserves saved data.** Cancelling before publication leaves previously saved\n  records unchanged.\n- **Failed imports leave stored records unchanged.** Report invalid rows without committing\n  any part of the import.\n- **Existing callers remain compatible.** Preserve public signatures and documented error\n  behavior.\n\nKeep entries at a consistent level of abstraction. Group closely related behaviors without\ncombining unrelated requirements merely to reduce the count. Put ownership and lifecycle mechanisms\nin Design, exact defaults and local behavioral contracts in declarations, consequential choices in\nDecisions, and individual test scenarios in Tests. Preserve an exact API, dependency, or mechanism\nhere when the user explicitly requires it. Retain acceptance-critical error, recovery, compatibility,\nand scope constraints.\n\nRequirements describe properties of the delivered change. Preserve task-specific constraints such\nas compatibility, offline operation, or protecting user configuration. Do not promote every design\nchoice into a mandatory requirement or create implementation milestones. Keep routine validation\nand implementation safeguards in the relevant Design or declaration unless they are explicitly\nrequested or essential to a task-specific acceptance constraint. Do not invent numeric limits or\nadditional product requirements. Ground any required limit in the user's request or a verified\nconstraint.\n\nWhen following an existing rule requires concrete work, plan the resulting change instead of\nrepeating the rule. Include supported file changes in the proposed files and put exact verification\ncommands and observable manual checks in Verification. Mention a generated artifact or unsupported\nfile change in Design only when its specific contents are essential to the requested change and are\nnot represented elsewhere, using a brief sentence alongside its owning component. A routine README\ncontents list does not explain the design and belongs outside this section. Do not invent\nplaceholder generated contents or add a separate file inventory.\n\nWhen the change introduces generated files, inspect the project's existing ignore rules and\npropose any needed .gitignore addition or update as a normal virtual file change. Preserve existing\ncomments, negations, and unrelated rules. Add appropriate generated paths, such as /target/ for a\nroot Rust package. Do not propose an empty file merely because .gitignore is absent. Keep source\nand required lockfiles tracked. Generate or update required lockfiles during implementation, not\nin Plan. Routine lockfile handling belongs to implementation and needs no Design prose. The model\ncreates or updates the actual file during implementation. Do not modify workspace files in Plan.\n\n### Background\n\nWrite `background` as Markdown explaining the existing system directly relevant to this task.\nIdentify inspected components using repository-relative paths and symbol names. Explain what each\nowns, how the affected operation currently flows between them, and the integration points for this\nchange. Include existing behavior that must be understood or preserved, such as persistence,\nerror propagation, and platform limits. Explain the current limitation when applicable.\n\nDescribe established starting conditions and explicitly identify material unresolved constraints.\nOmit discovery chronology, tool failures, and evidence-gathering details unless they leave an\nuncertainty that changes implementation or verification. Include only facts needed to understand\nor implement this change. Do not give a repository tour, summarize the conversation, repeat the\nrequested behavior, or describe the proposed solution. For a new project, briefly describe the\nstarting repository, available infrastructure, and relevant conventions.\n\n### Decisions\n\nWrite `decisions` as a JSON array of objects with nonempty `decision` and `rationale` strings.\n\nRecord consequential choices that resolve questions left open by the task. The answer may come\nfrom asking the user or from your own planning. Include choices where a different answer would\nmaterially change the behavior, scope, or integration contract.\n\nIn `decision`, state the selected answer directly. In `rationale`, explain why it fits this task\nand any consequence or tradeoff the implementer must preserve. Record the settled choice, not\nthe conversation or deliberation that produced it.\n\nA choice resolved through user clarification belongs here. Requirements already fixed by the\noriginal request do not need repeating. Omit solution summaries, component inventories, routine\nconventions, and dependency selections without a task-specific tradeoff. An empty list is valid.\n\nThese examples show rendered decisions. Store each bold sentence as `decision` without the bold\nmarkers and its explanation as `rationale`. Derive the actual choices from the current task rather\nthan copying these examples:\n\n- **Update matching records and create unmatched records.** Users can import revised CSV files\n  to maintain existing data instead of creating duplicates.\n- **Reject the entire import if any row is invalid.** Report row-specific errors and leave stored\n  data unchanged, so users can correct and retry the file without reconciling a partial import.\n- **Resume interrupted jobs from the last completed item.** Completed work remains recorded, and\n  restarting retries only unfinished items. This requires durable progress tracking.\n- **Pause synchronization when both copies of a file have changed.** Preserve both versions and\n  ask the user to resolve the conflict, preventing automatic synchronization from discarding an edit.\n- **Preserve the public API during the internal refactor.** Existing callers can adopt the change\n  without modifying their code. Public API cleanup remains outside this refactor.\n- **Generate reports as background jobs.** Large reports can finish after the user leaves the page.\n  Users receive a completion notification and download the result when it is ready.\n\n### Design\n\nWrite `design` as Markdown explaining how the proposed solution works and satisfies the requirements.\nUse an unheaded opening paragraph followed by `### Ownership`. Store flows separately in `flows`.\nUse this template, replacing the bracketed instructions with task-specific content:\n\n```markdown\n[One paragraph of 2–3 sentences: describe the concrete solution, explain the main structural choice and its reason, then introduce the main objects and their roles.]\n\n### Ownership\n[One paragraph of 1–2 sentences per main owner: its responsibility, the meaning of its owned state, and its boundary with other owners. Define supporting code names as they appear.]\n```\n\n**Opening.** Write one paragraph of 2–3 sentences explaining how the design solves the user's task.\nFollow this progression:\n\n1. Concrete solution: state what will be implemented or changed and its defining behavior.\n   Describe the proposed solution, not just the objective.\n2. Structural choice and reason: explain the main organizing choice and one concrete capability\n   or guarantee it provides.\n3. Main objects: introduce the principal components by their actual code names and briefly\n   explain their roles.\n\nCombine the structural reason and component introduction when two sentences suffice. Expand\nresponsibilities in Ownership and interactions in Flows. Keep configuration values, detailed\nalgorithms, and secondary components out of the opening.\n\nThe examples below demonstrate structure and level of detail. Derive the solution, reasoning,\nand component names from the current task and design. Do not copy the examples' implementation\nchoices. The generated opening is one paragraph without the example labels.\n\n**Feature: CSV import**\n\nSolution: Implement CSV import as a workflow that previews incoming records, reports validation\nerrors, and saves the import after confirmation.\n\nReason: Separate validation from persistence so users can correct invalid records before\nchanging stored data.\n\nObjects: `ImportSession` holds the pending records and validation results, while `ImportService`\nvalidates the data and commits the confirmed import.\n\n**Feature: log-search CLI**\n\nSolution: Implement log search as a command that filters files by time range and severity, then\nprints matching records.\n\nReason: Process files incrementally so memory use does not grow with the total size of the\ninput files.\n\nObjects: `SearchCommand` handles arguments, `LogScanner` reads and filters records, and\n`MatchWriter` formats the output.\n\n**Bugfix: cancellation leaves the interface stuck**\n\nSolution: Make cancelling a request stop its active work and return the interface to idle.\n\nReason: Associate callbacks with a request generation so results arriving after cancellation\ncannot restore stale progress.\n\nObjects: `RequestController` owns the active generation and cancellation handle, while `RunState`\nrecords the status displayed by the interface.\n\n**Refactor: duplicated authentication**\n\nSolution: Move authentication from individual route handlers into shared middleware while\npreserving existing responses.\n\nReason: Centralize credential validation so every protected route applies the same expiry and\nrevocation checks.\n\nObjects: `AuthMiddleware` gates protected requests, `TokenVerifier` validates credentials, and\n`RequestIdentity` carries the authenticated user to the handler.\n\n**Bugfix: interrupted saves corrupt settings**\n\nSolution: Preserve the last complete settings file when saving is interrupted or fails.\n\nReason: Write the replacement to a temporary file and publish it atomically so readers never\nobserve partially written settings.\n\nObjects: `SettingsStore` owns serialization and loading, while `AtomicFileWriter` manages the\ntemporary file and final replacement.\n\n**Ownership.** Expand the owners introduced in the opening, in the same order, using one paragraph\nof 1–2 sentences per owner. State what each owner creates, changes, or retains, and where its authority\nends. Introduce supporting types with their meaning, such as \"`ImportSession`, the pending records\nand validation results\", rather than listing names after \"owns\". Group supporting modules and data under that\nowner. Use connected prose, not bullets, file inventories, or paragraphs for every type.\nKeep thin composition wrappers in the opening unless they own a separate lifecycle. Begin Ownership\nwith the component that controls the central state, not its configuration or launcher. Configuration\nfields, numeric defaults, key bindings, visual styling, algorithms, and schedule-by-schedule details\nbelong in the relevant declarations. Include a detail here only when changing it changes the\nownership boundary. Do not add README contents, documentation chores, or repeated setup descriptions.\n\nDescribe the scope of the change, not the architecture of the entire system. For bugfixes, explain\nthe invariant being restored and the mechanism that restores it. Describe only the existing owners\nand interactions relevant to the fix. A single ownership paragraph and one flow are sufficient\nfor a localized change, and an existing function or module can be the owner. Do not invent new\ncomponents to fill the template.\n\nDescribe the target design in present tense. Preserve terminology across sections, remove repeated\nclaims, and demonstrate guarantees through concrete mechanisms. Keep detailed contracts in\ndeclarations, detailed tradeoffs in Decisions, and existing behavior in Background. Flows retain the ordering\nand failure rules needed to explain the result, without repeating local algorithms or configuration\nvalues. Execution instructions remain governed by the active system and repository instructions.\nThe complete plan must support implementation without the prior conversation.\n\nOnly the opening and Ownership belong in the `design` string. Omit a Files\nand artifacts section, repeated file inventories, routine housekeeping, and explanations of Harness\nformat limitations. Do not include executable implementation bodies, mutable execution progress,\nor claims that implementation or verification has finished.\n\nBefore submission, review the Design against this template. Confirm that the opening describes the\nsolution, explains its structure, and introduces the main objects. Define unexplained code names\nand move configuration or implementation detail out of Ownership. Confirm that every flow arrow\nrepresents production, transformation, storage, or consumption through the described objects.\nShorten any ownership paragraph exceeding two sentences instead of joining unrelated clauses.\nKeep acceptance-critical behavior in the plan, placing each detail in its designated section.\n\n### Flows\n\nWrite `flows` as an array of `{title, description, trigger, steps}` objects, or [] when no runtime\nflow adds useful information. Harness generates the diagram after Design. Do not put a Flows\nheading or hand-formatted arrows in `design` or flow labels.\n\nEach flow describes one execution started by one explicit event. Use a concise description for\nits purpose and ordering guarantees. Separate events into separate flows even when they share\nstate: collecting a shield stores charges, while a later enemy contact reads those charges.\nDo not connect these actions into one execution merely because one uses the other's output.\n\n- `trigger` names the event that starts this execution.\n- `steps` is an ordered array. Each step has a required `action` naming a function or concrete\n  operation, optional `effects`, and optional `branches`.\n- Each effect has exactly `target` and `change`. It is terminal data describing an observable\n  change, such as create, update, remove, or preserve. Effects cannot have steps, actions,\n  children, branches, or triggers. Put subsequent execution in the enclosing `steps` array.\n- Each branch has a required `condition` and a nonempty `steps` array. The condition is evaluated\n  during the current execution. A later event requires a new flow, not a conditional branch.\n  Each applicable branch runs its steps before the next enclosing step. When alternatives are\n  exclusive, write mutually exclusive conditions explicitly. Branches do not mean parallel work.\n\nUse descriptive labels and the exact declaration names where available. Harness normalizes label\nwhitespace for display while retaining the original content. Keep resulting\nstate in `effects`, rather than treating data as executable steps. Express conditions that govern\nexecution in branch conditions or action labels instead of adding separate input inventories.\nFlow titles must be unique. There are no total flow, step-count, or nesting-depth limits.\nKeep diagrams focused for readability without omitting meaningful execution or effects.\n\nFor example, collection and contact are independent triggers:\n\n```json\n[\n  {\n    \"title\": \"Collect a shield\",\n    \"description\": \"Collection stores protection for a later accepted hit.\",\n    \"trigger\": \"player overlaps a shield pickup\",\n    \"steps\": [{\n      \"action\": \"collect_shield_pickups\",\n      \"effects\": [{\"target\": \"PlayerAbilities\", \"change\": \"grant shield charges to\"}]\n    }]\n  },\n  {\n    \"title\": \"Absorb contact\",\n    \"description\": \"An accepted contact consumes available protection.\",\n    \"trigger\": \"accepted enemy contact\",\n    \"steps\": [{\n      \"action\": \"PlayerAbilities.take_hit\",\n      \"branches\": [{\n        \"condition\": \"shield charges remain\",\n        \"steps\": [{\"action\": \"absorb hit\", \"effects\": [\n          {\"target\": \"shield charge\", \"change\": \"consume one\"},\n          {\"target\": \"contact immunity\", \"change\": \"start\"}\n        ]}]\n      }]\n    }]\n  }\n]\n```\n\nThe renderer uses `→` for the first action, `↓` for subsequent execution, and `┄▶` for effects.\nConditions introduce branch steps. Effects never acquire execution arrows.\n\nUse Markdown inline code for code identifiers, concrete paths, and commands in prose fields.\nEncode paragraph breaks as `\\n\\n` inside JSON strings.\nThe required tests array inventories every test involved in the plan, grouped by project-relative\nfile. Each cases entry names the test (module-qualified when needed), its change (new, modified,\nremoved, or reused), and a description of the scenario and expected result or removal rationale.\nInclude inline test modules and separate test files consistently. List existing coverage being\nreused, omit unrelated repository tests, and use [] when no tests apply. Keep names unique within\na file and list each file once. Native Cargo case checks require the full runtime test name.\nUse the optional target field (lib, bin:<name>, or test:<name>) when a file belongs to multiple\nCargo test targets. Tests is rendered\nbefore Verification and records planned coverage. Verification retains execution commands and manual checks.\nObjective, Background, and Design must be nonempty before submission.\n\nA plan's metadata can use this shape:\n\n```json\n{\n  \"objective\": \"Support observable, cancellable texture replacement.\",\n  \"usage\": \"Start a replacement, cancel it before publication, and confirm the existing texture remains visible.\",\n  \"requirements\": [\n    \"**Cancellation preserves the current texture.** Cancelling before publication leaves the visible texture unchanged.\",\n    \"**Submitted frames retain valid textures.** Allocations remain valid until every submitted frame using them completes.\"\n  ],\n  \"background\": \"The existing renderer obtains published handles from `TextureRegistry`. Submitted frames can outlive the frame in which a replacement is requested.\",\n  \"decisions\": [\n    {\n      \"decision\": \"Publish replacements at frame boundaries.\",\n      \"rationale\": \"A single publication point keeps each submitted frame's texture selection consistent.\"\n    }\n  ],\n  \"design\": \"Support texture replacement that reports progress and can be cancelled while the current texture remains visible. Separate preparation from publication so pending work leaves the current texture usable. `TextureStreaming`, the preparation service, passes completed replacements to `TextureRegistry`, the owner of the handles used by the renderer.\\n\\n### Ownership\\n`TextureStreaming` creates and prepares each replacement, exposing its progress and cancellation through `TextureRequest`, the caller's handle to pending work. Preparation leaves the published texture unchanged.\\n\\n`TextureRegistry` controls which texture the renderer can use and retains replaced allocations while submitted frames still reference them. Only the registry publishes or retires a texture.\",\n  \"flows\": [\n    {\n      \"title\": \"Publish a replacement\",\n      \"description\": \"Publication at a frame boundary selects the prepared texture for rendering.\",\n      \"trigger\": \"prepared replacement is ready at a frame boundary\",\n      \"steps\": [\n        {\n          \"action\": \"TextureRegistry.publish\",\n          \"effects\": [\n            {\n              \"target\": \"published handle\",\n              \"change\": \"replace\"\n            }\n          ]\n        },\n        {\n          \"action\": \"Renderer\"\n        }\n      ]\n    },\n    {\n      \"title\": \"Retire an old allocation\",\n      \"description\": \"The replaced allocation remains alive until its last submitted frame completes.\",\n      \"trigger\": \"last submitted frame using the old allocation completes\",\n      \"steps\": [\n        {\n          \"action\": \"TextureRegistry.retire\",\n          \"effects\": [\n            {\n              \"target\": \"replaced allocation\",\n              \"change\": \"release\"\n            }\n          ]\n        }\n      ]\n    },\n    {\n      \"title\": \"Cancel preparation\",\n      \"description\": \"Cancellation releases only the unpublished replacement.\",\n      \"trigger\": \"cancellation requested\",\n      \"steps\": [\n        {\n          \"action\": \"TextureRequest.cancel\"\n        },\n        {\n          \"action\": \"TextureStreaming.discard\",\n          \"effects\": [\n            {\n              \"target\": \"unpublished allocation\",\n              \"change\": \"release\"\n            }\n          ]\n        }\n      ]\n    }\n  ],\n  \"verification\": [\"Cancel a pending replacement and confirm the current texture remains visible.\", \"Replace a texture with frames in flight and confirm rendering remains valid.\"],\n  \"tests\": [\n    {\n      \"file\": \"src/texture.rs\",\n      \"cases\": [\n        {\n          \"name\": \"tests::texture_replacement_cancellation_preserves_published_handle\",\n          \"change\": \"new\",\n          \"description\": \"Cancel before publication and verify the original texture handle remains published.\"\n        },\n        {\n          \"name\": \"tests::texture_replacement_retains_in_flight_allocation\",\n          \"change\": \"new\",\n          \"description\": \"Replace a texture with frames in flight and verify its allocation remains valid until their completion.\"\n        }\n      ]\n    }\n  ]\n}\n```\n\nThis example illustrates the schema. Use inspected project facts and actual supported commands\nfor the real plan. Read the saved text before patching when it is not already available. Metadata\nand declaration edits can share one atomic patch:\n\n```text\n*** Begin Patch\n*** Update File: plan.json\n@@\n-  \"objective\": \"\",\n+  \"objective\": \"Support observable, cancellable texture requests.\",\n+  \"usage\": \"Start a replacement, cancel it before publication, and confirm the existing texture remains visible.\",\n*** End Patch\n```\n\n### Checks and verification\n\nAutomated check definitions belong to the project's saved `.forge.json`. Harness configures missing or invalid checks before starting this planning turn. Read the saved configuration through `check_definition_list`. An explicitly empty configuration is valid. If the configuration becomes missing or invalid during planning, report the submission failure and use the harness-checks skill to repair it with check-editing permission. Test inventory describes intended coverage independently of commands. Applicable enabled checks with `auto:true` are included automatically. Use `plan_checks_add` with the current plan ID, version, and check IDs to add other enabled checks explicitly. The `checks` array in virtual `plan.json` stores those references, not commands. Never run validation directly through shell tools while planning or implementing. During implementation and resolution, finish the phase and let Harness Checking run validation.\n\nHarness executes these commands locally, outside the model provider sandbox, after Implement and Resolve. Harness records exit status, duration, and saved stdout/stderr and checks declaration conformance. The model must not execute automated checks during Implement, Resolve, or manual Verify.\n\nRecord manual actions in `verification`, an array of actions and expected observations. Identify actions requiring a person or unavailable hardware. Manual Verify starts only after declarations and automated checks pass. Planning records requirements, not results. Execution state and results remain Harness-owned.\n\n## Overview syntax\n\nPaths match project-relative source paths. Supported extensions are `.rs`, `.ts`, `.tsx`, `.lua`, and\n`.toml`, `.json`, `.jsonc`, `.yaml`, `.yml`, and XML configuration paths (including `.xml`,\n`.csproj`, `.fsproj`, `.vbproj`, `.props`, `.targets`, `.resx`, and `.plist`), plus `.gitignore` files whose contents are preserved verbatim. Source overviews retain native layouts but are not compilable implementation files. Never write\nexecutable function bodies, empty or placeholder bodies, or executable initializers. Academic\npseudocode belongs only in an indented `Algorithm` block, never in a native-language body. Preserve\nreference qualifiers, generic arguments, and complete return types. Do not infer missing types.\n\nPreserve syntactic nesting and attach documentation to its declaration. Harness owns presentation\nformatting, indentation style, blank-line spacing, column widths, and line wrapping. Do not manually\nwrap content or shorten names, comments, commands, or diagrams to satisfy a display width.\nHarness formats each requested workspace overview and captured baseline, then canonicalizes both\ndeclaration snapshots on submission. The repository line width is a formatting preference, not a\nsubmission constraint. Literals, code examples, and indivisible types remain intact. Read virtual\nfile ranges when their exact current text is unavailable and match that text, including indentation.\nDraft patches retain their layout until submission. Submission can reformat declarations, so read\naffected ranges before another revision unless current formatted text is already supplied in feedback.\n\n## Function algorithms, changes, and references\n\nCategorize callable behavior against the captured source baseline, including after resubmission:\n\n- A new function or method requires one nonempty `Algorithm` block describing its complete behavior\n  in academic pseudocode. A `Change` block does not satisfy this requirement.\n- A modified function or method uses one `Change` block, preferably one short sentence: state\n  the concrete problem or purpose, then the behavioral change that addresses it. Ground the reason\n  in the task or inspected baseline, without inventing a defect. Use a second short sentence only\n  when essential. Omit unchanged behavior and implementation detail that does not explain the delta.\n  Include internal changes even when its signature and reference lists remain identical. Do not\n  restate the complete algorithm or use `Algorithm` for an existing callable. Unmodified callables\n  do not need a `Change` block.\n\nPlace the heading at the signature's indentation and its content one level deeper, before `Calls`,\n`Constructs`, and `Accesses`, in that order. Never combine `Algorithm` and `Change` on one callable.\nPartial drafts may omit a new algorithm, but submission rejects that omission. Preserve an authored\nblock unless its intended behavior changes. Removing `Change` withdraws that delta. Configuration\nand non-callable declarations do not have behavior blocks. Review folds both categories with their\ncallable and never appends `[changed]` to a collapsed signature.\n\n### Academic pseudocode\n\nRead and apply the `academic-pseudocode` skill before authoring `Algorithm` blocks. Its local\n`SKILL.md` contains the complete writing rules. Keep algorithms as high-level and compact as\npossible while preserving the complete behavioral contract.\n\nFor this plan, keep domain identifiers searchable and preserve numeric bounds, tie-breaking,\nsaturation, ownership, and deferred effects explicitly. Function signatures already declare inputs\nand results, so do not repeat them as boilerplate in each `Algorithm` block.\n\nAlgorithm submission validates statement syntax and indentation with Forge's pseudocode parser.\nUse `target ← expression` for assignment, `if condition`, `otherwise if condition`, `otherwise`,\n`for each item in collection`, and `while condition`. Every control statement needs a nonempty\nbody indented with spaces. A trailing colon is optional. Keep each statement on its own line,\nalign siblings, and dedent to an existing enclosing level. Use `return` with an optional result.\nUse `break` and `continue` only inside loops. Expressions and operation statements may use concise\nnatural language, identifiers, numbers, quoted strings, parentheses, and brackets. Use `=` for\nequality and `≠`, `≤`, or `≥` for comparisons. Omit native-language braces and semicolons.\nIncomplete drafts remain editable. Repair the reported callable, line, and column before resubmitting.\n\nKeep explicit field references consistent with `Accesses`: `Projectile.previous_position` in\nan Algorithm requires `Projectile::previous_position` in Rust's `Accesses` section. Use signature\nbindings for instance receivers when available. Qualified fields must name real declared members.\n\nMake construction explicit as `Type(field = value, ...)`, or `Type()` for a zero-field marker.\nUse the exact qualified name from `Constructs`, including factory names such as\n`Store::new(config = config)`. Every target listed in `Constructs` for an Algorithm must have\na matching invocation with named arguments or no arguments. Construct values before describing\nthe operation that queues, spawns, inserts, or publishes them. Reused shared assets are not new\nconstructions. Ordinary helper calls may use positional arguments. Do not hide constructed values\nand their initial fields in prose such as \"queue a shot with damage\".\n\nSource inspection extracts function-body references alongside declarations in Rust, TypeScript,\nTSX, and Lua. Use these sections, with one qualified target per line and no arguments:\n\n- `Calls` records invocations other than construction and callable values, including callback\n  registration. A callable passed as a callback remains here even when it is a factory.\n- `Constructs` records types instantiated directly, such as `Weapon` for `Weapon { ... }` or\n  `new Weapon(...)`, and factory calls whose declared return is an owned named type. List the\n  callable itself, such as `Weapon::new` or `Weapon::default` returning `Self` or `Weapon`.\n  Use the return type, not a method-name convention. Borrowed, scalar, and unit returns stay in\n  `Calls`. Rust tuple and unit constructors belong here when their type is resolved.\n  Do not list a type here merely because it appears in a signature, type annotation, generic\n  argument, cast, or destructuring pattern.\n- `Accesses` records field reads, writes, references, and named value uses. A struct initializer\n  such as `Weapon { cooldown_remaining: 0.0 }` records `Weapon` under `Constructs` and\n  `Weapon::cooldown_remaining` under `Accesses`. Destructured fields also belong in `Accesses`.\n\nPreserve occurrence order and repetitions within each category. Parsing retains captured\ninterleaving and semantic kinds in saved data. Unknown or computed receivers remain unresolved\nand must not be matched to a same-named property without type evidence.\n\nEvery newly declared internal symbol needs a resolved incoming use before submission. Include each registered system or callback in the registering function's Calls, even when the framework invokes it later. Include new property and value uses in Accesses and new type uses in signatures or Constructs. Imports, declaration names, owner headers, prose, and self references alone do not count. Truly public APIs, recognized executable entry points, tests, and trait contract items need no in-plan caller. Public exposure includes module and member accessibility and public re-exports, so pub(crate) and pub items hidden in private modules remain internal. The rule applies to new symbols relative to the captured source baseline, including resubmission, while partial draft edits remain permitted.\nThe virtual file includes `Calls`, `Constructs`, and `Accesses` blocks after each captured callable signature.\nEdit it atomically with declarations through `design_patch`. Include names only, without\narguments, assignments, control flow, or other statements. Qualify methods with a receiver type only\nwhen source evidence identifies that type. Keep unresolved receiver names when evidence is absent.\n\nKeep call occurrences in extraction or authored order, including repetitions. Review deduplicates\ntargets while retaining parsed order by default. Preserve unchanged Calls blocks when editing\nsignatures. Update an owner's list when relationships change and include a block for a newly planned\ncallable when its relationships are known. An empty block states that the callable has no calls.\nAn absent block states that call information is unavailable. Do not manufacture missing information\nfor historical plans or configuration documents.\n\nThe structured list follows the signature at the same indentation level. Target names use one\nadditional indentation level:\n\n```text\npub fn dispatch(client: &Client);\nChange\n  Avoid futile retries of rejected credentials by stopping on authentication\n  failure and recording the final attempt.\nCalls\n  validate_policy\n  Client::send\n  record_metrics\n```\n\nA newly introduced callable instead has an algorithm, with reference evidence listed separately:\n\n```text\npub fn next_attempt(attempt: u32, limit: u32) -> Option<u32>;\nAlgorithm\n  if attempt ≥ limit\n    return none\n  return attempt + 1\n```\n\n## Declaration comments\n\nEvery declaration in a source overview you author or revise must have an attached explanatory\ncode comment, including private declarations, types, fields, enum variants, traits or interfaces,\nimplementation blocks, functions, methods, aliases, and module-level bindings. Preserve accurate\nexisting comments and add missing ones. Imports, attributes, parameters, and configuration entries\ndo not need separate declaration comments.\n\nStart newly added or edited comments with `Get` instead of `Returns`. Submission rejects comments\nwhose first word is `Returns`, ignoring case. The rule applies only to the start of the complete\ncomment, not later sentences or continuation lines. Unchanged captured comments remain valid.\n\nFollow the repository's code-comment instructions and, when available, the technical-writing\nskill's Code Comments and API Documentation profiles. Reuse instructions already loaded in context. This planning requirement makes declaration\ncomments mandatory even when general source-comment guidance permits omission.\n\nExplain the declaration's role, ownership boundary, or behavioral contract. For fields and\nvariants, explain their domain meaning or represented condition. For callables, explain the\nobservable result, mutation, or failure condition the signature cannot convey. Include relevant\nbounds and lifecycle or ordering constraints. Use only contracts supported by the inspected source\nor settled proposal. Do not invent behavior or narrate the signature, such as \"stores a value\"\nor \"gets the current texture.\" A comment on a container does not replace comments on its members.\n\nUse native comment syntax, such as Rust `///`, TypeScript JSDoc, and Lua `---` or `---@` annotations.\nKeep comments immediately attached to declarations and above their attributes. Before submission,\nread the edited overviews and check that every declaration has a useful comment consistent with\nthe final design. Comments describe the design without adding function bodies or pseudocode.\n\nRust retains structs, fields, enums, traits, aliases, imports, attributes, and `impl` blocks.\nTerminate callable signatures with a semicolon, including methods:\n\n```rust\n/// Publishes uploaded replacements while retaining textures still used by submitted frames.\npub struct TextureRegistry {\n  /// Published texture available to new frame submissions, if one has been installed.\n  current: Option<TextureHandle>,\n}\n\n/// Exposes the published texture and the frame-boundary replacement operation.\nimpl TextureRegistry {\n  /// Get the published handle without changing the active texture version.\n  pub fn current(&self) -> Option<TextureHandle>;\n\n  /// Installs a completed upload at a frame boundary and retains the previous allocation until its final GPU use.\n  pub fn publish(&mut self, replacement: TextureHandle);\n}\n```\n\nTypeScript retains classes, interfaces, aliases, fields, exports, and complete signatures:\n\n```typescript\n/** Provides asynchronous access to texture handles by their stable identifier. */\nexport interface TextureStore {\n  /** Resolves a usable handle or rejects when the texture cannot be loaded. */\n  get(id: string): Promise<TextureHandle>;\n}\n\n/** Resolves texture handles through the configured storage provider. */\nexport class TextureRegistry {\n  /** Provider used to resolve handles without exposing storage access to callers. */\n  private store: TextureStore;\n\n  /** Resolves the requested handle and propagates provider failures. */\n  get(id: string): Promise<TextureHandle>;\n}\n```\n\nArrow bindings retain their native header through `=>`, with no expression or body:\n\n```typescript\n/** Starts an observable texture request using the caller's identifier. */\nexport const request = <T>(id: T): TextureRequest =>;\n```\n\nKeep an absent return annotation absent. Lua table members appear as dotted bindings and named\nfunction headers beneath their module binding.\n\nLua retains bindings, named function headers, module returns, and declaration annotations.\nDo not add function-closing `end` to signature-only headers:\n\n```lua\n---Exposes access to the currently published texture handles.\nlocal M\n\n---Get the published handle for the requested texture.\n---@param id string\n---@return TextureHandle\nfunction M.get(id)\n\nreturn M\n```\n\n## Manifests and configuration\n\nJSON, JSONC, TOML, YAML, XML, and .gitignore files retain their complete contents, including values, comments\nwhere allowed, arrays, attributes, and nested structures. These are editable configuration proposals\nrather than abbreviated declarations. Include every affected manifest and configuration file when\nchanging dependencies, features, scripts, package settings, workspace members, or build targets.\nThis includes `Cargo.toml`, `package.json`, `tsconfig.json`, CI YAML, and XML project files as needed.\nKeep unaffected settings and include affected nested package manifests. Do not omit configuration\nchanges because the design focuses on source declarations.\n\nRead the existing virtual file and patch it with the same Add File, Update File, Delete File, and\nMove to operations. Use valid syntax and preserve exact string values. Source restrictions on\ninitializers do not apply to configuration. Harness validates configuration and preserves its text\nwithout applying declaration indentation or prose wrapping. Configuration remains visible in\npublic-only inspection. Do not write actual project files during planning.\n\nUse strict JSON for `package.json` and ordinary `.json` files. `.jsonc`, `tsconfig*.json`,\n`jsconfig*.json`, and JSON files beneath `.vscode/` allow comments and trailing commas, but require\nquoted property names and commas between values. YAML retains block scalars and multiple documents.\nXML retains elements, attributes, namespaces, comments, and CDATA. XML admission checks well-formedness,\nnot application schemas, and never fetches external DTDs. TOML admission rejects duplicate keys.\nThe root virtual `plan.json` remains reserved for plan metadata, even if the checkout contains a\nproject file with that name. Other nested `plan.json` paths are ordinary JSON proposals.\n\nFor example, a proposed dependency section retains its complete settings:\n\n```toml\n[dependencies]\nengine = { version = \"1.2\", default-features = false, features = [\"render\"] }\n```\n\n## Context-based file patches\n\nPatch paths are virtual proposed files, not paths beneath a real `design/` directory. Add File\ncreates an overview, Delete File proposes source removal, and Move to follows Update File to\npropose a rename. `@@` starts a chunk. Context lines start with a space, removals with `-`, and\nadditions with `+`:\n\n```text\n*** Begin Patch\n*** Update File: src/textures.rs\n@@\n-  pub fn request(texture: TextureId) -> TextureHandle;\n+  pub fn request(texture: TextureId) -> TextureRequest;\n@@\n   pub fn status(request: RequestId) -> RequestStatus;\n+  /// Cancels a pending request and reports whether cancellation was accepted.\n+  pub fn cancel(request: RequestId) -> bool;\n*** End Patch\n```\n\nInvalid patches change no files and do not increment the version. No-op patches retain the current\nversion. Changed patches increment it. Correct the reported syntax,\ncontext, or version and retry. Never bypass validation with ordinary filesystem tools.\n\n## Review revisions and boundaries\n\nReview feedback shows file excerpts with one line number per row. `-` marks removed baseline text,\n`+` marks proposed additions, and unmarked rows provide context. Removed rows use baseline line\nnumbers. Other rows use proposed line numbers. Comments follow each excerpt as `8: comment` or\n`8–12: comment`. Replacements can share a line number. These excerpts are review context, not patch\ninput. Resolve each comment against the saved design and preserve its intended context.\nRead current proposed files before revising them. Preserve useful decisions and submit the new version.\n\nUnsupported file formats cannot be edited through declaration controls. Preserve essential changes\nto those files briefly alongside their owning component in Design when not represented elsewhere,\nwithout explaining the format limitation or inventing a supported-file substitute. Planning never writes\nimplementation bodies or performs execution. For a function behavior change, retain the signature\nwhen it remains valid and add or update its Change summary in the virtual declaration. Explain the\nbehavior in Design and include its coverage in Tests. Never manufacture an interface change to make\na diff appear. Declaration conformance does not prove runtime behavior.\n\nIf affected workspace source changed since extraction, report the need for a fresh baseline.\nDo not silently replace the baseline or modify project source.\n\n\nUser request:\nBuild the API fixture",
          "delivery": "accepted",
          "id": "f13219b4-1ba5-43e5-9b30-f302ca79501c"
        },
        "revision": "fd32622e-0ffd-4f3a-8824-64988cac0c9f",
        "turn": {
          "continuation": null,
          "id": {
            "exchange_ordinal": 1,
            "turn_ordinal": 0
          }
        },
        "created_at_ms": 1793254176371
      },
      "default_approval": "ask",
      "plan_approval": null,
      "created_at_ms": 1793254176316,
      "updated_at_ms": 1793254176585,
      "shell": "system",
      "native_fork": true,
      "native_compact": true,
      "workspace": "/fixture/nvim.0\\Nr9VKJ\\0\\workspace",
      "service_tier": "default",
      "auto_compaction": {
        "threshold_tokens": 128000,
        "enabled": false
      },
      "name": "Active declaration design: ```",
      "context_window": null,
      "id": "2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447",
      "backend": "mock",
      "purpose": "conversation",
      "provider_fork_state": {
        "state": "ready"
      },
      "lease_owner": "nvim-43764-51072127019700"
    },
    "checks_running": false,
    "check_timing": "after_queue",
    "no_checkpoint": true,
    "automatic_checks": false,
    "checks_deferred": false,
    "active_check_run": null,
    "capability": {
      "background_terminals": true,
      "approval_mode_list": [
        "ask",
        "deny",
        "allow"
      ],
      "native_fork": true,
      "native_compact": true,
      "os_sandbox": false,
      "compaction": {
        "summary": true,
        "lifecycle": true,
        "background": false,
        "metrics": true
      },
      "agent": {
        "input": true,
        "spawn": true,
        "observe": true,
        "interrupt": true,
        "parallel": true
      },
      "catalog": {
        "skill": false,
        "mcp": false,
        "live_mcp_mutation": false
      },
      "native_steer": true,
      "native_turn_rollback": true,
      "native_goal": false,
      "model_selection": true,
      "effort_selection": true,
      "fast_mode": true,
      "ultrafast_mode": true,
      "permission_control": true
    },
    "task_operation": {
      "error": null,
      "state": "completed",
      "revision": 1,
      "action": "cancel_acceptance",
      "id": "example-cancel-acceptance"
    },
    "snapshot_revision": 21
  },
  "id": 24
}
```

<!-- /captured-example -->

## `session.plan.cancel`

Cancel the owned Plan workflow or its independent revision review.

- **Context:** Explicit existing session.
- **Input:** PlanReviewRequest fields and operation_id when supplied.
- **Result and effects:** Returns a session snapshot with task_operation for the main-task command. Finalizes an owned settled waiting exchange when applicable.
- **Boundary:** Do not finalize an active provider exchange. Preserve the independent revision-review branch and its response contract. Preserve the shared task command contract and response variant.
- **Inspected source:** [task/command.rs](../../../rust/forge/crates/forge-harness/src/task/command.rs), [service/task_state.rs](../../../rust/forge/crates/forge-harness/src/service/task_state.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/lua/forge/views/plan_review/native_controller.lua:330](../../../../nvim/lua/forge/views/plan_review/native_controller.lua).

<!-- detailed-contract -->
### Contract

Accepts [PlanReviewRequest](native-schemas.md#planreviewrequest) and optional stable `operation_id`. This is `action = "cancel_plan"` in [session.command.apply](#sessioncommandapply), defaulting to a snapshot response. It cancels the identified planning lifecycle while retaining its historical records. It inherits atomic state/receipt persistence and duplicate-ID recovery. Mismatched current plan or review identity fails rather than cancelling another plan.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [plan_api_examples.lua](../../../../nvim/tests/forge/plan_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.plan.cancel",
  "params": {
    "operation_id": "example-cancel-plan",
    "digest": "d308f128c13d79758b1b31604ea72984644f4ecd278ed4f229f19726b9057775",
    "_command": {
      "id": "example-cancel-plan"
    },
    "plan_id": "0d9cb210-8737-41fb-a3dd-601aec6d70c4"
  },
  "session_id": "2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447",
  "id": 39
}
```

```json
{
  "result": {
    "ask_model": {},
    "access_abbreviation": "rxw",
    "agent": {
      "run": [],
      "summary": {}
    },
    "subagent_tools": false,
    "lsp_tools": false,
    "session_tools": true,
    "status_thoughts": false,
    "check_editing": false,
    "runtime_epoch": "c832b8be-b9d8-4a4e-a936-dccfbab52e11",
    "active_wait": null,
    "approval": [],
    "prompt_history": [],
    "status": {
      "kind": "idle"
    },
    "artifact": [
      {
        "updated_at_ms": 1793254176770,
        "title": "Design the requested change",
        "working_path": "/fixture/nvim.0/Nr9VKJ/0/data/forge/harness\\plans\\2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447\\0d9cb210-8737-41fb-a3dd-601aec6d70c4\\working.md",
        "state": "cancelled",
        "created_at_ms": 1793254176371,
        "id": "0d9cb210-8737-41fb-a3dd-601aec6d70c4"
      }
    ],
    "active_elicitation": null,
    "timeline_revision": 6,
    "active_plan": null,
    "restore_recovery": null,
    "session": {
      "last_compaction_turn": null,
      "primary_agent_id": "2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447:agent:primary",
      "backend_session_id": "mock-session",
      "provider_checkpoint_id": null,
      "model": "default",
      "provider_label": "Mock CLI",
      "resolved_model": null,
      "plan_executor": {
        "enabled": false,
        "model": null,
        "effort": null
      },
      "plan_compact": false,
      "plan_auto_approve_revisions": true,
      "approval_timeout": {
        "amount": 10,
        "unit": "m"
      },
      "effort": "medium",
      "answer_timeout": {
        "amount": 10,
        "unit": "m"
      },
      "background": null,
      "access": {
        "sandbox": false,
        "windows_sandbox": "elevated",
        "read_access": "full",
        "write_access": "workspace",
        "readable_directory": [],
        "writable_directory": []
      },
      "context_usage": null,
      "approval_mode": "ask",
      "active_task": null,
      "default_approval": "ask",
      "plan_approval": null,
      "created_at_ms": 1793254176316,
      "updated_at_ms": 1793254176770,
      "shell": "system",
      "native_fork": true,
      "native_compact": true,
      "workspace": "/fixture/nvim.0\\Nr9VKJ\\0\\workspace",
      "service_tier": "default",
      "auto_compaction": {
        "threshold_tokens": 128000,
        "enabled": false
      },
      "name": "Active declaration design: ```",
      "context_window": null,
      "id": "2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447",
      "backend": "mock",
      "purpose": "conversation",
      "provider_fork_state": {
        "state": "ready"
      },
      "lease_owner": "nvim-43764-51072127019700"
    },
    "checks_running": false,
    "check_timing": "after_queue",
    "no_checkpoint": true,
    "automatic_checks": false,
    "checks_deferred": false,
    "active_check_run": null,
    "capability": {
      "background_terminals": true,
      "approval_mode_list": [
        "ask",
        "deny",
        "allow"
      ],
      "native_fork": true,
      "native_compact": true,
      "os_sandbox": false,
      "compaction": {
        "summary": true,
        "lifecycle": true,
        "background": false,
        "metrics": true
      },
      "agent": {
        "input": true,
        "spawn": true,
        "observe": true,
        "interrupt": true,
        "parallel": true
      },
      "catalog": {
        "skill": false,
        "mcp": false,
        "live_mcp_mutation": false
      },
      "native_steer": true,
      "native_turn_rollback": true,
      "native_goal": false,
      "model_selection": true,
      "effort_selection": true,
      "fast_mode": true,
      "ultrafast_mode": true,
      "permission_control": true
    },
    "task_operation": {
      "error": null,
      "state": "completed",
      "revision": 1,
      "action": "cancel_plan",
      "id": "example-cancel-plan"
    },
    "snapshot_revision": 36
  },
  "id": 39
}
```

<!-- /captured-example -->

## `session.plan.changes.request`

Commit requested Plan changes and any pending revision entry.

- **Context:** Explicit existing session.
- **Input:** PlanReviewRequest fields, including captured plan/digests, comment, annotations, and operation_id when supplied.
- **Result and effects:** Returns a session snapshot with task_operation for the main-task command. The coordinator later delivers required provider input.
- **Boundary:** Preserve captured review/version validation and independent revision-review routing. The command itself does not cancel or run the provider. Preserve the shared task command contract and response variant.
- **Inspected source:** [task/command.rs](../../../rust/forge/crates/forge-harness/src/task/command.rs), [service/task_state.rs](../../../rust/forge/crates/forge-harness/src/service/task_state.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/lua/forge/views/plan_review/native_controller.lua:333](../../../../nvim/lua/forge/views/plan_review/native_controller.lua).

<!-- detailed-contract -->
### Contract

Accepts [PlanReviewRequest](native-schemas.md#planreviewrequest): captured plan/digests, optional comment and annotations, approval mode, and optional stable `operation_id`. This is `action = "request_changes"` in [session.command.apply](#sessioncommandapply), defaulting to a snapshot response. It validates the captured review and commits requested revision work. Provider revision runs afterward through input delivery. Stale review data, unavailable artifacts, or incompatible task state fail. Preserve the operation identity across an uncertain response.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [plan_api_examples.lua](../../../../nvim/tests/forge/plan_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.plan.changes.request",
  "params": {
    "operation_id": "example-request-changes",
    "digest": "48e3cfe2f821383be1029c5468751bb59603daf4292062137fefa0e02ec4558f",
    "plan_id": "0d9cb210-8737-41fb-a3dd-601aec6d70c4",
    "_command": {
      "id": "example-request-changes"
    },
    "comment": "Clarify the fixture behavior"
  },
  "session_id": "2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447",
  "id": 26
}
```

```json
{
  "result": {
    "ask_model": {},
    "access_abbreviation": "rxw",
    "agent": {
      "run": [],
      "summary": {}
    },
    "subagent_tools": false,
    "lsp_tools": false,
    "session_tools": true,
    "status_thoughts": false,
    "check_editing": false,
    "runtime_epoch": "c832b8be-b9d8-4a4e-a936-dccfbab52e11",
    "active_wait": null,
    "approval": [],
    "prompt_history": [],
    "status": {
      "kind": "idle"
    },
    "artifact": [
      {
        "updated_at_ms": 1793254176610,
        "title": "Design the requested change",
        "working_path": "/fixture/nvim.0/Nr9VKJ/0/data/forge/harness\\plans\\2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447\\0d9cb210-8737-41fb-a3dd-601aec6d70c4\\working.md",
        "state": "revising",
        "created_at_ms": 1793254176371,
        "id": "0d9cb210-8737-41fb-a3dd-601aec6d70c4"
      }
    ],
    "active_elicitation": null,
    "timeline_revision": 3,
    "active_plan": {
      "updated_at_ms": 1793254176610,
      "session_id": "2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447",
      "document_version": 3,
      "state": "revising",
      "title": "Design the requested change",
      "working_path": "/fixture/nvim.0/Nr9VKJ/0/data/forge/harness\\plans\\2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447\\0d9cb210-8737-41fb-a3dd-601aec6d70c4\\working.md",
      "submitted_version": 3,
      "accepted_revision": null,
      "user_revision": 1,
      "model_revision": 2,
      "accepted_digest": null,
      "elicitation": null,
      "acceptance": null,
      "question_ledger": {
        "resolution": []
      },
      "request": "Build the API fixture",
      "review_digest": "48e3cfe2f821383be1029c5468751bb59603daf4292062137fefa0e02ec4558f",
      "generation": {
        "consecutive_no_progress": 0,
        "max_consecutive_no_progress": 2,
        "canonical_revision": 1,
        "turn_count": 0,
        "max_turn_count": 20
      },
      "validation_warning": [],
      "id": "0d9cb210-8737-41fb-a3dd-601aec6d70c4",
      "manual_changes": [
        "Updated plan checks: verify enabled"
      ],
      "created_at_ms": 1793254176371
    },
    "restore_recovery": null,
    "session": {
      "last_compaction_turn": null,
      "primary_agent_id": "2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447:agent:primary",
      "backend_session_id": "mock-session",
      "provider_checkpoint_id": null,
      "model": "default",
      "provider_label": "Mock CLI",
      "resolved_model": null,
      "plan_executor": {
        "enabled": false,
        "model": null,
        "effort": null
      },
      "plan_compact": false,
      "plan_auto_approve_revisions": true,
      "approval_timeout": {
        "amount": 10,
        "unit": "m"
      },
      "effort": "medium",
      "answer_timeout": {
        "amount": 10,
        "unit": "m"
      },
      "background": null,
      "access": {
        "sandbox": false,
        "windows_sandbox": "elevated",
        "read_access": "full",
        "write_access": "workspace",
        "readable_directory": [],
        "writable_directory": []
      },
      "context_usage": null,
      "approval_mode": "ask",
      "active_task": {
        "updated_at_ms": 1793254176610,
        "plan": {
          "paused": false,
          "artifact": {
            "updated_at_ms": 1793254176610,
            "session_id": "2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447",
            "document_version": 3,
            "state": "revising",
            "title": "Design the requested change",
            "working_path": "/fixture/nvim.0/Nr9VKJ/0/data/forge/harness\\plans\\2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447\\0d9cb210-8737-41fb-a3dd-601aec6d70c4\\working.md",
            "submitted_version": 3,
            "accepted_revision": null,
            "user_revision": 1,
            "model_revision": 2,
            "accepted_digest": null,
            "elicitation": null,
            "acceptance": null,
            "question_ledger": {
              "resolution": []
            },
            "request": "Build the API fixture",
            "review_digest": "48e3cfe2f821383be1029c5468751bb59603daf4292062137fefa0e02ec4558f",
            "generation": {
              "consecutive_no_progress": 0,
              "max_consecutive_no_progress": 2,
              "canonical_revision": 1,
              "turn_count": 0,
              "max_turn_count": 20
            },
            "validation_warning": [],
            "id": "0d9cb210-8737-41fb-a3dd-601aec6d70c4",
            "manual_changes": [
              "Updated plan checks: verify enabled"
            ],
            "created_at_ms": 1793254176371
          }
        },
        "kind": "plan",
        "entry": {
          "instruction": "Revise the saved canonical plan in the Harness Plan task. Address every annotation and overall comment. Apply requested design changes with design_patch. Explain when a comment needs no edit or conflicts with another requirement. Do not invent an edit merely to close a comment. Then call plan_submit with the exact resulting plan ID and version.\n\n# Design a software change through declarations\n\nDesign the final interfaces and ownership structures required by the user's request.\nHarness starts with an empty declaration design and captures each existing file's immutable\nbaseline on its first successful edit. Edit virtual\noverview files, complete configuration files, and the virtual plan.json specification, then submit them for mandatory review. Do not implement the design or\nmodify project files.\n\n## Procedure\n\n1. Inspect the request and affected source. Read implementation source to understand behavior,\n   constraints, and ownership. Reuse existing interfaces unless the change requires a new boundary.\n   For every code-change plan, identify the affected languages and inspect their installed\n   compilers or interpreters and project-selected versions. Read version configuration and use\n   read-only version queries from the project directory. Base syntax, API, and dependency choices\n   on those observed versions, even when no dependencies change. Report a missing or unresolved\n   toolchain instead of assuming a version.\n2. Use the supplied plan identity, version, and feedback context. Call `plan_read` when\n   current text, version, or additional context is needed. Omit `path` for the inventory. Supply\n   `path` and optional 1-based inclusive `start_line` and `end_line` for the affected range.\n   Set `baseline: true` to read original declarations. For an uncaptured path, Harness extracts only\n   that workspace file and returns its source digest without adding it to the saved design.\n   Read existing files before their first edit. Optional `source_digests` on a patch maps paths to\n   the returned digests and preserves inspection identities across turns. Harness also retains\n   inspected identities within the current turn. Omit displayed line-number prefixes from patches.\n3. Resolve discoverable facts from source. Ask only about material decisions source cannot answer.\n   Respect requests not to ask questions. Use `question_ask` when needed and end the turn.\n4. Design complete declaration files and required configuration changes. Preserve unchanged declarations, private members, complete\n   types, generics, visibility, documentation, and associations. Add only useful requested work.\n   Give every declaration in each source overview you author or revise an attached explanatory code comment.\n   Record objective, requirements, background, decisions, design, flows, checks, verification, and tests in virtual `plan.json`. Add usage examples only when useful.\n   Do not author JSON entities, tasks, stages, prerequisites, or execution reports.\n5. Edit with `design_patch`, supplying `plan_id`, `expected_version`, and `patch`.\n   An optional `title` names the design. Multiple files and chunks can change atomically. Use the\n   returned version in the next call. The response includes a compact applied diff. Use it to confirm\n   a focused edit without rereading the file. Read additional ranges only for missing context,\n   unexpected results, or a truncated diff. Correct every validation failure.\n   Update, Delete, and Move capture an existing source baseline only when the whole patch succeeds.\n   Add requires an absent workspace destination. Later edits use the saved proposal, including\n   retained deletion and rename state, without extracting the source again.\n6. Keep `plan.json` consistent with the final declarations. Call `plan_submit` with the exact current `plan_id` and `expected_version`.\n   Submission requests review and never authorizes implementation. If further work or user input arrives\n   in the same turn, continue reading and editing the plan, then submit its latest version again.\n   Any edit after submission requires a new successful submission before the plan is ready for review.\n\n## Dependency selection\n\nInspect the project's selected compiler or runtime, package manager, declared support range,\nmanifest constraints, and lockfile before choosing dependencies. Resolve the versions actually\nselected in the project directory. For example, check Rust toolchain files and rust-version,\nNode version files and package.json engines/packageManager, or Python requires-python and the\nproject environment. Choose the newest stable release compatible with those constraints and\nthe required features, including relevant peer and transitive dependency requirements.\n\nAnother installed version does not change the project's compatibility target. Do not switch\nor upgrade compilers, runtimes, or package managers merely to use a newer dependency. If no\ncompatible release meets the task, report the conflict and make any required environment change\nan explicit design decision.\n\nUse API documentation for the selected dependency version when designing declarations and\nexamples. Record material compatibility constraints in Background. Documentation retrieval\nfailures alone do not justify downgrading. Planning selects and proposes dependencies but does\nnot install them or run compilation checks.\n\n## Plan metadata\n\nHarness creates a virtual `plan.json` containing `objective`, optional `usage`, `requirements`,\n`background`, `decisions`, `design`, `flows`, `checks`, `verification`, and `tests`. It is plan metadata, never a project file\nor declaration overview. Read and update it through the same tools as declaration files. Do not\nmove it, delete it, or add other fields. Drafts start with empty strings and lists, with `usage` absent.\n\nWrite for an implementer who can inspect the repository and proposed declarations but has no\nconversation history. Preserve settled user requirements and consequential decisions in this metadata.\nDo not depend on phrases such as \"as discussed\" or details available only in earlier messages.\nKeep every section consistent with the final declarations and update it when review changes the plan.\n\nConsult `technical-writing` and its Architectural Overview profile before drafting or revising plan\nprose. Apply its guidance within the structure and length limits below.\n\nThe review order is Objective, Usage when present, Requirements, Background, Decisions when nonempty,\nDesign, Flows when nonempty, proposed declaration changes, Tests, and Verification. Each section has a distinct role:\n\n### Objective\n\nWrite `objective` as a short statement of the requested outcome and scope. State what the user\nneeds to accomplish or which failure needs correction. For new functionality, state the task\nwithout inventing a defect. Do not substitute an inventory of files or implementation steps.\n\n### Usage\n\nUse the optional Markdown string `usage` for representative successful use. Omit the field when\nexamples add no useful information, such as an internal refactor. An empty string or null also\nhides this section. State the relevant starting conditions, input or action, and observable result.\nDistinguish illustrative values from exact required output.\n\n- For a CLI, show the command or stdin followed by stdout, stderr, and exit status.\n- For a UI, show the starting state, user action, and visible result.\n- For an API, show the request, response, and observable side effects.\n- For a library, show the call and preconditions, then its return value, mutation, or error.\n- For a background process, show the trigger and eventual observable result.\n- For a data migration, show representative data before and after the migration.\n\n### Requirements\n\nIdentify the 2–4 key requirements: requested outcomes and essential acceptance constraints.\nWrite them as a JSON array of nonempty strings. For bug fixes, this section can be omitted by\nleaving `requirements` empty when the objective and design already capture the acceptance criteria.\n\nStart each entry with a short, bold sentence stating an observable guarantee. Add one plain-text\nsentence only when it supplies necessary conditions or scope. Store the Markdown bold markers\ninside the requirement string. The bold sentence must communicate the guarantee on its own.\nUse direct behavior statements rather than repeating a feature name with \"Provide\", \"Support\",\nor \"Implement\". Do not repeat the objective or explain why a design choice was selected.\n\nThese examples show the format. Derive each guarantee from the current task rather than copying\nan unrelated example:\n\n- **Cancellation preserves saved data.** Cancelling before publication leaves previously saved\n  records unchanged.\n- **Failed imports leave stored records unchanged.** Report invalid rows without committing\n  any part of the import.\n- **Existing callers remain compatible.** Preserve public signatures and documented error\n  behavior.\n\nKeep entries at a consistent level of abstraction. Group closely related behaviors without\ncombining unrelated requirements merely to reduce the count. Put ownership and lifecycle mechanisms\nin Design, exact defaults and local behavioral contracts in declarations, consequential choices in\nDecisions, and individual test scenarios in Tests. Preserve an exact API, dependency, or mechanism\nhere when the user explicitly requires it. Retain acceptance-critical error, recovery, compatibility,\nand scope constraints.\n\nRequirements describe properties of the delivered change. Preserve task-specific constraints such\nas compatibility, offline operation, or protecting user configuration. Do not promote every design\nchoice into a mandatory requirement or create implementation milestones. Keep routine validation\nand implementation safeguards in the relevant Design or declaration unless they are explicitly\nrequested or essential to a task-specific acceptance constraint. Do not invent numeric limits or\nadditional product requirements. Ground any required limit in the user's request or a verified\nconstraint.\n\nWhen following an existing rule requires concrete work, plan the resulting change instead of\nrepeating the rule. Include supported file changes in the proposed files and put exact verification\ncommands and observable manual checks in Verification. Mention a generated artifact or unsupported\nfile change in Design only when its specific contents are essential to the requested change and are\nnot represented elsewhere, using a brief sentence alongside its owning component. A routine README\ncontents list does not explain the design and belongs outside this section. Do not invent\nplaceholder generated contents or add a separate file inventory.\n\nWhen the change introduces generated files, inspect the project's existing ignore rules and\npropose any needed .gitignore addition or update as a normal virtual file change. Preserve existing\ncomments, negations, and unrelated rules. Add appropriate generated paths, such as /target/ for a\nroot Rust package. Do not propose an empty file merely because .gitignore is absent. Keep source\nand required lockfiles tracked. Generate or update required lockfiles during implementation, not\nin Plan. Routine lockfile handling belongs to implementation and needs no Design prose. The model\ncreates or updates the actual file during implementation. Do not modify workspace files in Plan.\n\n### Background\n\nWrite `background` as Markdown explaining the existing system directly relevant to this task.\nIdentify inspected components using repository-relative paths and symbol names. Explain what each\nowns, how the affected operation currently flows between them, and the integration points for this\nchange. Include existing behavior that must be understood or preserved, such as persistence,\nerror propagation, and platform limits. Explain the current limitation when applicable.\n\nDescribe established starting conditions and explicitly identify material unresolved constraints.\nOmit discovery chronology, tool failures, and evidence-gathering details unless they leave an\nuncertainty that changes implementation or verification. Include only facts needed to understand\nor implement this change. Do not give a repository tour, summarize the conversation, repeat the\nrequested behavior, or describe the proposed solution. For a new project, briefly describe the\nstarting repository, available infrastructure, and relevant conventions.\n\n### Decisions\n\nWrite `decisions` as a JSON array of objects with nonempty `decision` and `rationale` strings.\n\nRecord consequential choices that resolve questions left open by the task. The answer may come\nfrom asking the user or from your own planning. Include choices where a different answer would\nmaterially change the behavior, scope, or integration contract.\n\nIn `decision`, state the selected answer directly. In `rationale`, explain why it fits this task\nand any consequence or tradeoff the implementer must preserve. Record the settled choice, not\nthe conversation or deliberation that produced it.\n\nA choice resolved through user clarification belongs here. Requirements already fixed by the\noriginal request do not need repeating. Omit solution summaries, component inventories, routine\nconventions, and dependency selections without a task-specific tradeoff. An empty list is valid.\n\nThese examples show rendered decisions. Store each bold sentence as `decision` without the bold\nmarkers and its explanation as `rationale`. Derive the actual choices from the current task rather\nthan copying these examples:\n\n- **Update matching records and create unmatched records.** Users can import revised CSV files\n  to maintain existing data instead of creating duplicates.\n- **Reject the entire import if any row is invalid.** Report row-specific errors and leave stored\n  data unchanged, so users can correct and retry the file without reconciling a partial import.\n- **Resume interrupted jobs from the last completed item.** Completed work remains recorded, and\n  restarting retries only unfinished items. This requires durable progress tracking.\n- **Pause synchronization when both copies of a file have changed.** Preserve both versions and\n  ask the user to resolve the conflict, preventing automatic synchronization from discarding an edit.\n- **Preserve the public API during the internal refactor.** Existing callers can adopt the change\n  without modifying their code. Public API cleanup remains outside this refactor.\n- **Generate reports as background jobs.** Large reports can finish after the user leaves the page.\n  Users receive a completion notification and download the result when it is ready.\n\n### Design\n\nWrite `design` as Markdown explaining how the proposed solution works and satisfies the requirements.\nUse an unheaded opening paragraph followed by `### Ownership`. Store flows separately in `flows`.\nUse this template, replacing the bracketed instructions with task-specific content:\n\n```markdown\n[One paragraph of 2–3 sentences: describe the concrete solution, explain the main structural choice and its reason, then introduce the main objects and their roles.]\n\n### Ownership\n[One paragraph of 1–2 sentences per main owner: its responsibility, the meaning of its owned state, and its boundary with other owners. Define supporting code names as they appear.]\n```\n\n**Opening.** Write one paragraph of 2–3 sentences explaining how the design solves the user's task.\nFollow this progression:\n\n1. Concrete solution: state what will be implemented or changed and its defining behavior.\n   Describe the proposed solution, not just the objective.\n2. Structural choice and reason: explain the main organizing choice and one concrete capability\n   or guarantee it provides.\n3. Main objects: introduce the principal components by their actual code names and briefly\n   explain their roles.\n\nCombine the structural reason and component introduction when two sentences suffice. Expand\nresponsibilities in Ownership and interactions in Flows. Keep configuration values, detailed\nalgorithms, and secondary components out of the opening.\n\nThe examples below demonstrate structure and level of detail. Derive the solution, reasoning,\nand component names from the current task and design. Do not copy the examples' implementation\nchoices. The generated opening is one paragraph without the example labels.\n\n**Feature: CSV import**\n\nSolution: Implement CSV import as a workflow that previews incoming records, reports validation\nerrors, and saves the import after confirmation.\n\nReason: Separate validation from persistence so users can correct invalid records before\nchanging stored data.\n\nObjects: `ImportSession` holds the pending records and validation results, while `ImportService`\nvalidates the data and commits the confirmed import.\n\n**Feature: log-search CLI**\n\nSolution: Implement log search as a command that filters files by time range and severity, then\nprints matching records.\n\nReason: Process files incrementally so memory use does not grow with the total size of the\ninput files.\n\nObjects: `SearchCommand` handles arguments, `LogScanner` reads and filters records, and\n`MatchWriter` formats the output.\n\n**Bugfix: cancellation leaves the interface stuck**\n\nSolution: Make cancelling a request stop its active work and return the interface to idle.\n\nReason: Associate callbacks with a request generation so results arriving after cancellation\ncannot restore stale progress.\n\nObjects: `RequestController` owns the active generation and cancellation handle, while `RunState`\nrecords the status displayed by the interface.\n\n**Refactor: duplicated authentication**\n\nSolution: Move authentication from individual route handlers into shared middleware while\npreserving existing responses.\n\nReason: Centralize credential validation so every protected route applies the same expiry and\nrevocation checks.\n\nObjects: `AuthMiddleware` gates protected requests, `TokenVerifier` validates credentials, and\n`RequestIdentity` carries the authenticated user to the handler.\n\n**Bugfix: interrupted saves corrupt settings**\n\nSolution: Preserve the last complete settings file when saving is interrupted or fails.\n\nReason: Write the replacement to a temporary file and publish it atomically so readers never\nobserve partially written settings.\n\nObjects: `SettingsStore` owns serialization and loading, while `AtomicFileWriter` manages the\ntemporary file and final replacement.\n\n**Ownership.** Expand the owners introduced in the opening, in the same order, using one paragraph\nof 1–2 sentences per owner. State what each owner creates, changes, or retains, and where its authority\nends. Introduce supporting types with their meaning, such as \"`ImportSession`, the pending records\nand validation results\", rather than listing names after \"owns\". Group supporting modules and data under that\nowner. Use connected prose, not bullets, file inventories, or paragraphs for every type.\nKeep thin composition wrappers in the opening unless they own a separate lifecycle. Begin Ownership\nwith the component that controls the central state, not its configuration or launcher. Configuration\nfields, numeric defaults, key bindings, visual styling, algorithms, and schedule-by-schedule details\nbelong in the relevant declarations. Include a detail here only when changing it changes the\nownership boundary. Do not add README contents, documentation chores, or repeated setup descriptions.\n\nDescribe the scope of the change, not the architecture of the entire system. For bugfixes, explain\nthe invariant being restored and the mechanism that restores it. Describe only the existing owners\nand interactions relevant to the fix. A single ownership paragraph and one flow are sufficient\nfor a localized change, and an existing function or module can be the owner. Do not invent new\ncomponents to fill the template.\n\nDescribe the target design in present tense. Preserve terminology across sections, remove repeated\nclaims, and demonstrate guarantees through concrete mechanisms. Keep detailed contracts in\ndeclarations, detailed tradeoffs in Decisions, and existing behavior in Background. Flows retain the ordering\nand failure rules needed to explain the result, without repeating local algorithms or configuration\nvalues. Execution instructions remain governed by the active system and repository instructions.\nThe complete plan must support implementation without the prior conversation.\n\nOnly the opening and Ownership belong in the `design` string. Omit a Files\nand artifacts section, repeated file inventories, routine housekeeping, and explanations of Harness\nformat limitations. Do not include executable implementation bodies, mutable execution progress,\nor claims that implementation or verification has finished.\n\nBefore submission, review the Design against this template. Confirm that the opening describes the\nsolution, explains its structure, and introduces the main objects. Define unexplained code names\nand move configuration or implementation detail out of Ownership. Confirm that every flow arrow\nrepresents production, transformation, storage, or consumption through the described objects.\nShorten any ownership paragraph exceeding two sentences instead of joining unrelated clauses.\nKeep acceptance-critical behavior in the plan, placing each detail in its designated section.\n\n### Flows\n\nWrite `flows` as an array of `{title, description, trigger, steps}` objects, or [] when no runtime\nflow adds useful information. Harness generates the diagram after Design. Do not put a Flows\nheading or hand-formatted arrows in `design` or flow labels.\n\nEach flow describes one execution started by one explicit event. Use a concise description for\nits purpose and ordering guarantees. Separate events into separate flows even when they share\nstate: collecting a shield stores charges, while a later enemy contact reads those charges.\nDo not connect these actions into one execution merely because one uses the other's output.\n\n- `trigger` names the event that starts this execution.\n- `steps` is an ordered array. Each step has a required `action` naming a function or concrete\n  operation, optional `effects`, and optional `branches`.\n- Each effect has exactly `target` and `change`. It is terminal data describing an observable\n  change, such as create, update, remove, or preserve. Effects cannot have steps, actions,\n  children, branches, or triggers. Put subsequent execution in the enclosing `steps` array.\n- Each branch has a required `condition` and a nonempty `steps` array. The condition is evaluated\n  during the current execution. A later event requires a new flow, not a conditional branch.\n  Each applicable branch runs its steps before the next enclosing step. When alternatives are\n  exclusive, write mutually exclusive conditions explicitly. Branches do not mean parallel work.\n\nUse descriptive labels and the exact declaration names where available. Harness normalizes label\nwhitespace for display while retaining the original content. Keep resulting\nstate in `effects`, rather than treating data as executable steps. Express conditions that govern\nexecution in branch conditions or action labels instead of adding separate input inventories.\nFlow titles must be unique. There are no total flow, step-count, or nesting-depth limits.\nKeep diagrams focused for readability without omitting meaningful execution or effects.\n\nFor example, collection and contact are independent triggers:\n\n```json\n[\n  {\n    \"title\": \"Collect a shield\",\n    \"description\": \"Collection stores protection for a later accepted hit.\",\n    \"trigger\": \"player overlaps a shield pickup\",\n    \"steps\": [{\n      \"action\": \"collect_shield_pickups\",\n      \"effects\": [{\"target\": \"PlayerAbilities\", \"change\": \"grant shield charges to\"}]\n    }]\n  },\n  {\n    \"title\": \"Absorb contact\",\n    \"description\": \"An accepted contact consumes available protection.\",\n    \"trigger\": \"accepted enemy contact\",\n    \"steps\": [{\n      \"action\": \"PlayerAbilities.take_hit\",\n      \"branches\": [{\n        \"condition\": \"shield charges remain\",\n        \"steps\": [{\"action\": \"absorb hit\", \"effects\": [\n          {\"target\": \"shield charge\", \"change\": \"consume one\"},\n          {\"target\": \"contact immunity\", \"change\": \"start\"}\n        ]}]\n      }]\n    }]\n  }\n]\n```\n\nThe renderer uses `→` for the first action, `↓` for subsequent execution, and `┄▶` for effects.\nConditions introduce branch steps. Effects never acquire execution arrows.\n\nUse Markdown inline code for code identifiers, concrete paths, and commands in prose fields.\nEncode paragraph breaks as `\\n\\n` inside JSON strings.\nThe required tests array inventories every test involved in the plan, grouped by project-relative\nfile. Each cases entry names the test (module-qualified when needed), its change (new, modified,\nremoved, or reused), and a description of the scenario and expected result or removal rationale.\nInclude inline test modules and separate test files consistently. List existing coverage being\nreused, omit unrelated repository tests, and use [] when no tests apply. Keep names unique within\na file and list each file once. Native Cargo case checks require the full runtime test name.\nUse the optional target field (lib, bin:<name>, or test:<name>) when a file belongs to multiple\nCargo test targets. Tests is rendered\nbefore Verification and records planned coverage. Verification retains execution commands and manual checks.\nObjective, Background, and Design must be nonempty before submission.\n\nA plan's metadata can use this shape:\n\n```json\n{\n  \"objective\": \"Support observable, cancellable texture replacement.\",\n  \"usage\": \"Start a replacement, cancel it before publication, and confirm the existing texture remains visible.\",\n  \"requirements\": [\n    \"**Cancellation preserves the current texture.** Cancelling before publication leaves the visible texture unchanged.\",\n    \"**Submitted frames retain valid textures.** Allocations remain valid until every submitted frame using them completes.\"\n  ],\n  \"background\": \"The existing renderer obtains published handles from `TextureRegistry`. Submitted frames can outlive the frame in which a replacement is requested.\",\n  \"decisions\": [\n    {\n      \"decision\": \"Publish replacements at frame boundaries.\",\n      \"rationale\": \"A single publication point keeps each submitted frame's texture selection consistent.\"\n    }\n  ],\n  \"design\": \"Support texture replacement that reports progress and can be cancelled while the current texture remains visible. Separate preparation from publication so pending work leaves the current texture usable. `TextureStreaming`, the preparation service, passes completed replacements to `TextureRegistry`, the owner of the handles used by the renderer.\\n\\n### Ownership\\n`TextureStreaming` creates and prepares each replacement, exposing its progress and cancellation through `TextureRequest`, the caller's handle to pending work. Preparation leaves the published texture unchanged.\\n\\n`TextureRegistry` controls which texture the renderer can use and retains replaced allocations while submitted frames still reference them. Only the registry publishes or retires a texture.\",\n  \"flows\": [\n    {\n      \"title\": \"Publish a replacement\",\n      \"description\": \"Publication at a frame boundary selects the prepared texture for rendering.\",\n      \"trigger\": \"prepared replacement is ready at a frame boundary\",\n      \"steps\": [\n        {\n          \"action\": \"TextureRegistry.publish\",\n          \"effects\": [\n            {\n              \"target\": \"published handle\",\n              \"change\": \"replace\"\n            }\n          ]\n        },\n        {\n          \"action\": \"Renderer\"\n        }\n      ]\n    },\n    {\n      \"title\": \"Retire an old allocation\",\n      \"description\": \"The replaced allocation remains alive until its last submitted frame completes.\",\n      \"trigger\": \"last submitted frame using the old allocation completes\",\n      \"steps\": [\n        {\n          \"action\": \"TextureRegistry.retire\",\n          \"effects\": [\n            {\n              \"target\": \"replaced allocation\",\n              \"change\": \"release\"\n            }\n          ]\n        }\n      ]\n    },\n    {\n      \"title\": \"Cancel preparation\",\n      \"description\": \"Cancellation releases only the unpublished replacement.\",\n      \"trigger\": \"cancellation requested\",\n      \"steps\": [\n        {\n          \"action\": \"TextureRequest.cancel\"\n        },\n        {\n          \"action\": \"TextureStreaming.discard\",\n          \"effects\": [\n            {\n              \"target\": \"unpublished allocation\",\n              \"change\": \"release\"\n            }\n          ]\n        }\n      ]\n    }\n  ],\n  \"verification\": [\"Cancel a pending replacement and confirm the current texture remains visible.\", \"Replace a texture with frames in flight and confirm rendering remains valid.\"],\n  \"tests\": [\n    {\n      \"file\": \"src/texture.rs\",\n      \"cases\": [\n        {\n          \"name\": \"tests::texture_replacement_cancellation_preserves_published_handle\",\n          \"change\": \"new\",\n          \"description\": \"Cancel before publication and verify the original texture handle remains published.\"\n        },\n        {\n          \"name\": \"tests::texture_replacement_retains_in_flight_allocation\",\n          \"change\": \"new\",\n          \"description\": \"Replace a texture with frames in flight and verify its allocation remains valid until their completion.\"\n        }\n      ]\n    }\n  ]\n}\n```\n\nThis example illustrates the schema. Use inspected project facts and actual supported commands\nfor the real plan. Read the saved text before patching when it is not already available. Metadata\nand declaration edits can share one atomic patch:\n\n```text\n*** Begin Patch\n*** Update File: plan.json\n@@\n-  \"objective\": \"\",\n+  \"objective\": \"Support observable, cancellable texture requests.\",\n+  \"usage\": \"Start a replacement, cancel it before publication, and confirm the existing texture remains visible.\",\n*** End Patch\n```\n\n### Checks and verification\n\nAutomated check definitions belong to the project's saved `.forge.json`. Harness configures missing or invalid checks before starting this planning turn. Read the saved configuration through `check_definition_list`. An explicitly empty configuration is valid. If the configuration becomes missing or invalid during planning, report the submission failure and use the harness-checks skill to repair it with check-editing permission. Test inventory describes intended coverage independently of commands. Applicable enabled checks with `auto:true` are included automatically. Use `plan_checks_add` with the current plan ID, version, and check IDs to add other enabled checks explicitly. The `checks` array in virtual `plan.json` stores those references, not commands. Never run validation directly through shell tools while planning or implementing. During implementation and resolution, finish the phase and let Harness Checking run validation.\n\nHarness executes these commands locally, outside the model provider sandbox, after Implement and Resolve. Harness records exit status, duration, and saved stdout/stderr and checks declaration conformance. The model must not execute automated checks during Implement, Resolve, or manual Verify.\n\nRecord manual actions in `verification`, an array of actions and expected observations. Identify actions requiring a person or unavailable hardware. Manual Verify starts only after declarations and automated checks pass. Planning records requirements, not results. Execution state and results remain Harness-owned.\n\n## Overview syntax\n\nPaths match project-relative source paths. Supported extensions are `.rs`, `.ts`, `.tsx`, `.lua`, and\n`.toml`, `.json`, `.jsonc`, `.yaml`, `.yml`, and XML configuration paths (including `.xml`,\n`.csproj`, `.fsproj`, `.vbproj`, `.props`, `.targets`, `.resx`, and `.plist`), plus `.gitignore` files whose contents are preserved verbatim. Source overviews retain native layouts but are not compilable implementation files. Never write\nexecutable function bodies, empty or placeholder bodies, or executable initializers. Academic\npseudocode belongs only in an indented `Algorithm` block, never in a native-language body. Preserve\nreference qualifiers, generic arguments, and complete return types. Do not infer missing types.\n\nPreserve syntactic nesting and attach documentation to its declaration. Harness owns presentation\nformatting, indentation style, blank-line spacing, column widths, and line wrapping. Do not manually\nwrap content or shorten names, comments, commands, or diagrams to satisfy a display width.\nHarness formats each requested workspace overview and captured baseline, then canonicalizes both\ndeclaration snapshots on submission. The repository line width is a formatting preference, not a\nsubmission constraint. Literals, code examples, and indivisible types remain intact. Read virtual\nfile ranges when their exact current text is unavailable and match that text, including indentation.\nDraft patches retain their layout until submission. Submission can reformat declarations, so read\naffected ranges before another revision unless current formatted text is already supplied in feedback.\n\n## Function algorithms, changes, and references\n\nCategorize callable behavior against the captured source baseline, including after resubmission:\n\n- A new function or method requires one nonempty `Algorithm` block describing its complete behavior\n  in academic pseudocode. A `Change` block does not satisfy this requirement.\n- A modified function or method uses one `Change` block, preferably one short sentence: state\n  the concrete problem or purpose, then the behavioral change that addresses it. Ground the reason\n  in the task or inspected baseline, without inventing a defect. Use a second short sentence only\n  when essential. Omit unchanged behavior and implementation detail that does not explain the delta.\n  Include internal changes even when its signature and reference lists remain identical. Do not\n  restate the complete algorithm or use `Algorithm` for an existing callable. Unmodified callables\n  do not need a `Change` block.\n\nPlace the heading at the signature's indentation and its content one level deeper, before `Calls`,\n`Constructs`, and `Accesses`, in that order. Never combine `Algorithm` and `Change` on one callable.\nPartial drafts may omit a new algorithm, but submission rejects that omission. Preserve an authored\nblock unless its intended behavior changes. Removing `Change` withdraws that delta. Configuration\nand non-callable declarations do not have behavior blocks. Review folds both categories with their\ncallable and never appends `[changed]` to a collapsed signature.\n\n### Academic pseudocode\n\nRead and apply the `academic-pseudocode` skill before authoring `Algorithm` blocks. Its local\n`SKILL.md` contains the complete writing rules. Keep algorithms as high-level and compact as\npossible while preserving the complete behavioral contract.\n\nFor this plan, keep domain identifiers searchable and preserve numeric bounds, tie-breaking,\nsaturation, ownership, and deferred effects explicitly. Function signatures already declare inputs\nand results, so do not repeat them as boilerplate in each `Algorithm` block.\n\nAlgorithm submission validates statement syntax and indentation with Forge's pseudocode parser.\nUse `target ← expression` for assignment, `if condition`, `otherwise if condition`, `otherwise`,\n`for each item in collection`, and `while condition`. Every control statement needs a nonempty\nbody indented with spaces. A trailing colon is optional. Keep each statement on its own line,\nalign siblings, and dedent to an existing enclosing level. Use `return` with an optional result.\nUse `break` and `continue` only inside loops. Expressions and operation statements may use concise\nnatural language, identifiers, numbers, quoted strings, parentheses, and brackets. Use `=` for\nequality and `≠`, `≤`, or `≥` for comparisons. Omit native-language braces and semicolons.\nIncomplete drafts remain editable. Repair the reported callable, line, and column before resubmitting.\n\nKeep explicit field references consistent with `Accesses`: `Projectile.previous_position` in\nan Algorithm requires `Projectile::previous_position` in Rust's `Accesses` section. Use signature\nbindings for instance receivers when available. Qualified fields must name real declared members.\n\nMake construction explicit as `Type(field = value, ...)`, or `Type()` for a zero-field marker.\nUse the exact qualified name from `Constructs`, including factory names such as\n`Store::new(config = config)`. Every target listed in `Constructs` for an Algorithm must have\na matching invocation with named arguments or no arguments. Construct values before describing\nthe operation that queues, spawns, inserts, or publishes them. Reused shared assets are not new\nconstructions. Ordinary helper calls may use positional arguments. Do not hide constructed values\nand their initial fields in prose such as \"queue a shot with damage\".\n\nSource inspection extracts function-body references alongside declarations in Rust, TypeScript,\nTSX, and Lua. Use these sections, with one qualified target per line and no arguments:\n\n- `Calls` records invocations other than construction and callable values, including callback\n  registration. A callable passed as a callback remains here even when it is a factory.\n- `Constructs` records types instantiated directly, such as `Weapon` for `Weapon { ... }` or\n  `new Weapon(...)`, and factory calls whose declared return is an owned named type. List the\n  callable itself, such as `Weapon::new` or `Weapon::default` returning `Self` or `Weapon`.\n  Use the return type, not a method-name convention. Borrowed, scalar, and unit returns stay in\n  `Calls`. Rust tuple and unit constructors belong here when their type is resolved.\n  Do not list a type here merely because it appears in a signature, type annotation, generic\n  argument, cast, or destructuring pattern.\n- `Accesses` records field reads, writes, references, and named value uses. A struct initializer\n  such as `Weapon { cooldown_remaining: 0.0 }` records `Weapon` under `Constructs` and\n  `Weapon::cooldown_remaining` under `Accesses`. Destructured fields also belong in `Accesses`.\n\nPreserve occurrence order and repetitions within each category. Parsing retains captured\ninterleaving and semantic kinds in saved data. Unknown or computed receivers remain unresolved\nand must not be matched to a same-named property without type evidence.\n\nEvery newly declared internal symbol needs a resolved incoming use before submission. Include each registered system or callback in the registering function's Calls, even when the framework invokes it later. Include new property and value uses in Accesses and new type uses in signatures or Constructs. Imports, declaration names, owner headers, prose, and self references alone do not count. Truly public APIs, recognized executable entry points, tests, and trait contract items need no in-plan caller. Public exposure includes module and member accessibility and public re-exports, so pub(crate) and pub items hidden in private modules remain internal. The rule applies to new symbols relative to the captured source baseline, including resubmission, while partial draft edits remain permitted.\nThe virtual file includes `Calls`, `Constructs`, and `Accesses` blocks after each captured callable signature.\nEdit it atomically with declarations through `design_patch`. Include names only, without\narguments, assignments, control flow, or other statements. Qualify methods with a receiver type only\nwhen source evidence identifies that type. Keep unresolved receiver names when evidence is absent.\n\nKeep call occurrences in extraction or authored order, including repetitions. Review deduplicates\ntargets while retaining parsed order by default. Preserve unchanged Calls blocks when editing\nsignatures. Update an owner's list when relationships change and include a block for a newly planned\ncallable when its relationships are known. An empty block states that the callable has no calls.\nAn absent block states that call information is unavailable. Do not manufacture missing information\nfor historical plans or configuration documents.\n\nThe structured list follows the signature at the same indentation level. Target names use one\nadditional indentation level:\n\n```text\npub fn dispatch(client: &Client);\nChange\n  Avoid futile retries of rejected credentials by stopping on authentication\n  failure and recording the final attempt.\nCalls\n  validate_policy\n  Client::send\n  record_metrics\n```\n\nA newly introduced callable instead has an algorithm, with reference evidence listed separately:\n\n```text\npub fn next_attempt(attempt: u32, limit: u32) -> Option<u32>;\nAlgorithm\n  if attempt ≥ limit\n    return none\n  return attempt + 1\n```\n\n## Declaration comments\n\nEvery declaration in a source overview you author or revise must have an attached explanatory\ncode comment, including private declarations, types, fields, enum variants, traits or interfaces,\nimplementation blocks, functions, methods, aliases, and module-level bindings. Preserve accurate\nexisting comments and add missing ones. Imports, attributes, parameters, and configuration entries\ndo not need separate declaration comments.\n\nStart newly added or edited comments with `Get` instead of `Returns`. Submission rejects comments\nwhose first word is `Returns`, ignoring case. The rule applies only to the start of the complete\ncomment, not later sentences or continuation lines. Unchanged captured comments remain valid.\n\nFollow the repository's code-comment instructions and, when available, the technical-writing\nskill's Code Comments and API Documentation profiles. Reuse instructions already loaded in context. This planning requirement makes declaration\ncomments mandatory even when general source-comment guidance permits omission.\n\nExplain the declaration's role, ownership boundary, or behavioral contract. For fields and\nvariants, explain their domain meaning or represented condition. For callables, explain the\nobservable result, mutation, or failure condition the signature cannot convey. Include relevant\nbounds and lifecycle or ordering constraints. Use only contracts supported by the inspected source\nor settled proposal. Do not invent behavior or narrate the signature, such as \"stores a value\"\nor \"gets the current texture.\" A comment on a container does not replace comments on its members.\n\nUse native comment syntax, such as Rust `///`, TypeScript JSDoc, and Lua `---` or `---@` annotations.\nKeep comments immediately attached to declarations and above their attributes. Before submission,\nread the edited overviews and check that every declaration has a useful comment consistent with\nthe final design. Comments describe the design without adding function bodies or pseudocode.\n\nRust retains structs, fields, enums, traits, aliases, imports, attributes, and `impl` blocks.\nTerminate callable signatures with a semicolon, including methods:\n\n```rust\n/// Publishes uploaded replacements while retaining textures still used by submitted frames.\npub struct TextureRegistry {\n  /// Published texture available to new frame submissions, if one has been installed.\n  current: Option<TextureHandle>,\n}\n\n/// Exposes the published texture and the frame-boundary replacement operation.\nimpl TextureRegistry {\n  /// Get the published handle without changing the active texture version.\n  pub fn current(&self) -> Option<TextureHandle>;\n\n  /// Installs a completed upload at a frame boundary and retains the previous allocation until its final GPU use.\n  pub fn publish(&mut self, replacement: TextureHandle);\n}\n```\n\nTypeScript retains classes, interfaces, aliases, fields, exports, and complete signatures:\n\n```typescript\n/** Provides asynchronous access to texture handles by their stable identifier. */\nexport interface TextureStore {\n  /** Resolves a usable handle or rejects when the texture cannot be loaded. */\n  get(id: string): Promise<TextureHandle>;\n}\n\n/** Resolves texture handles through the configured storage provider. */\nexport class TextureRegistry {\n  /** Provider used to resolve handles without exposing storage access to callers. */\n  private store: TextureStore;\n\n  /** Resolves the requested handle and propagates provider failures. */\n  get(id: string): Promise<TextureHandle>;\n}\n```\n\nArrow bindings retain their native header through `=>`, with no expression or body:\n\n```typescript\n/** Starts an observable texture request using the caller's identifier. */\nexport const request = <T>(id: T): TextureRequest =>;\n```\n\nKeep an absent return annotation absent. Lua table members appear as dotted bindings and named\nfunction headers beneath their module binding.\n\nLua retains bindings, named function headers, module returns, and declaration annotations.\nDo not add function-closing `end` to signature-only headers:\n\n```lua\n---Exposes access to the currently published texture handles.\nlocal M\n\n---Get the published handle for the requested texture.\n---@param id string\n---@return TextureHandle\nfunction M.get(id)\n\nreturn M\n```\n\n## Manifests and configuration\n\nJSON, JSONC, TOML, YAML, XML, and .gitignore files retain their complete contents, including values, comments\nwhere allowed, arrays, attributes, and nested structures. These are editable configuration proposals\nrather than abbreviated declarations. Include every affected manifest and configuration file when\nchanging dependencies, features, scripts, package settings, workspace members, or build targets.\nThis includes `Cargo.toml`, `package.json`, `tsconfig.json`, CI YAML, and XML project files as needed.\nKeep unaffected settings and include affected nested package manifests. Do not omit configuration\nchanges because the design focuses on source declarations.\n\nRead the existing virtual file and patch it with the same Add File, Update File, Delete File, and\nMove to operations. Use valid syntax and preserve exact string values. Source restrictions on\ninitializers do not apply to configuration. Harness validates configuration and preserves its text\nwithout applying declaration indentation or prose wrapping. Configuration remains visible in\npublic-only inspection. Do not write actual project files during planning.\n\nUse strict JSON for `package.json` and ordinary `.json` files. `.jsonc`, `tsconfig*.json`,\n`jsconfig*.json`, and JSON files beneath `.vscode/` allow comments and trailing commas, but require\nquoted property names and commas between values. YAML retains block scalars and multiple documents.\nXML retains elements, attributes, namespaces, comments, and CDATA. XML admission checks well-formedness,\nnot application schemas, and never fetches external DTDs. TOML admission rejects duplicate keys.\nThe root virtual `plan.json` remains reserved for plan metadata, even if the checkout contains a\nproject file with that name. Other nested `plan.json` paths are ordinary JSON proposals.\n\nFor example, a proposed dependency section retains its complete settings:\n\n```toml\n[dependencies]\nengine = { version = \"1.2\", default-features = false, features = [\"render\"] }\n```\n\n## Context-based file patches\n\nPatch paths are virtual proposed files, not paths beneath a real `design/` directory. Add File\ncreates an overview, Delete File proposes source removal, and Move to follows Update File to\npropose a rename. `@@` starts a chunk. Context lines start with a space, removals with `-`, and\nadditions with `+`:\n\n```text\n*** Begin Patch\n*** Update File: src/textures.rs\n@@\n-  pub fn request(texture: TextureId) -> TextureHandle;\n+  pub fn request(texture: TextureId) -> TextureRequest;\n@@\n   pub fn status(request: RequestId) -> RequestStatus;\n+  /// Cancels a pending request and reports whether cancellation was accepted.\n+  pub fn cancel(request: RequestId) -> bool;\n*** End Patch\n```\n\nInvalid patches change no files and do not increment the version. No-op patches retain the current\nversion. Changed patches increment it. Correct the reported syntax,\ncontext, or version and retry. Never bypass validation with ordinary filesystem tools.\n\n## Review revisions and boundaries\n\nReview feedback shows file excerpts with one line number per row. `-` marks removed baseline text,\n`+` marks proposed additions, and unmarked rows provide context. Removed rows use baseline line\nnumbers. Other rows use proposed line numbers. Comments follow each excerpt as `8: comment` or\n`8–12: comment`. Replacements can share a line number. These excerpts are review context, not patch\ninput. Resolve each comment against the saved design and preserve its intended context.\nRead current proposed files before revising them. Preserve useful decisions and submit the new version.\n\nUnsupported file formats cannot be edited through declaration controls. Preserve essential changes\nto those files briefly alongside their owning component in Design when not represented elsewhere,\nwithout explaining the format limitation or inventing a supported-file substitute. Planning never writes\nimplementation bodies or performs execution. For a function behavior change, retain the signature\nwhen it remains valid and add or update its Change summary in the virtual declaration. Explain the\nbehavior in Design and include its coverage in Tests. Never manufacture an interface change to make\na diff appear. Declaration conformance does not prove runtime behavior.\n\nIf affected workspace source changed since extraction, report the need for a fresh baseline.\nDo not silently replace the baseline or modify project source.\n\n\nOverall review comment:\nClarify the fixture behavior\n\nCurrent declaration design:\n```json\n{\n  \"plan_id\": \"0d9cb210-8737-41fb-a3dd-601aec6d70c4\",\n  \"version\": 3,\n  \"title\": \"Design the requested change\",\n  \"request\": \"Build the API fixture\",\n  \"declaration_files\": {\n    \"paths\": [\n      \"plan.json\",\n      \"src/change.rs\"\n    ],\n    \"changed_paths\": [\n      \"src/change.rs\"\n    ]\n  },\n  \"document\": {\n    \"objective\": \"Revise the registry interface.\",\n    \"requirements\": [\n      \"Preserve the registry ownership boundary.\"\n    ],\n    \"background\": \"The registry owns published handles used by its callers.\",\n    \"decisions\": [],\n    \"design\": \"Revise the registry interface while preserving its ownership boundary.\",\n    \"flows\": [],\n    \"verification\": [\n      \"Confirm the requested interface behaves as designed.\"\n    ],\n    \"tests\": [],\n    \"checks\": [\n      \"verify\"\n    ]\n  }\n}\n```\n\nReview comments:\nNone\n\nManual plan edits recorded during review:\n- Updated plan checks: verify enabled\nThe current canonical plan determines which edits remain after the review decision. When acknowledging the review decision, state these manual changes.",
          "delivery": "pending",
          "id": "d963b213-9858-4d7b-a751-0aae6406e06b"
        },
        "revision": "e12ec645-4d04-4484-8acd-6a7a39a21b09",
        "turn": {
          "continuation": null,
          "id": {
            "exchange_ordinal": 1,
            "turn_ordinal": 0
          }
        },
        "created_at_ms": 1793254176371
      },
      "default_approval": "ask",
      "plan_approval": null,
      "created_at_ms": 1793254176316,
      "updated_at_ms": 1793254176610,
      "shell": "system",
      "native_fork": true,
      "native_compact": true,
      "workspace": "/fixture/nvim.0\\Nr9VKJ\\0\\workspace",
      "service_tier": "default",
      "auto_compaction": {
        "threshold_tokens": 128000,
        "enabled": false
      },
      "name": "Active declaration design: ```",
      "context_window": null,
      "id": "2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447",
      "backend": "mock",
      "purpose": "conversation",
      "provider_fork_state": {
        "state": "ready"
      },
      "lease_owner": "nvim-43764-51072127019700"
    },
    "checks_running": false,
    "check_timing": "after_queue",
    "no_checkpoint": true,
    "automatic_checks": false,
    "checks_deferred": false,
    "active_check_run": null,
    "capability": {
      "background_terminals": true,
      "approval_mode_list": [
        "ask",
        "deny",
        "allow"
      ],
      "native_fork": true,
      "native_compact": true,
      "os_sandbox": false,
      "compaction": {
        "summary": true,
        "lifecycle": true,
        "background": false,
        "metrics": true
      },
      "agent": {
        "input": true,
        "spawn": true,
        "observe": true,
        "interrupt": true,
        "parallel": true
      },
      "catalog": {
        "skill": false,
        "mcp": false,
        "live_mcp_mutation": false
      },
      "native_steer": true,
      "native_turn_rollback": true,
      "native_goal": false,
      "model_selection": true,
      "effort_selection": true,
      "fast_mode": true,
      "ultrafast_mode": true,
      "permission_control": true
    },
    "task_operation": {
      "error": null,
      "state": "completed",
      "revision": 1,
      "action": "request_changes",
      "id": "example-request-changes"
    },
    "snapshot_revision": 23
  },
  "id": 26
}
```

<!-- /captured-example -->

## `session.plan.check.update`

Update the reviewed plan's selected checks.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The plan check-selection update.
- **Result and effects:** Returns the updated plan and selection state.
- **Boundary:** This does not execute the checks.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/lua/forge/views/plan_review/document.lua:234](../../../../nvim/lua/forge/views/plan_review/document.lua).

<!-- detailed-contract -->
### Contract

Requires `plan_id`, current review `digest`, `expected_version`, and `enabled` (check-ID to boolean map). Optional `tests` is an array of [DesignTestSelection](runtime-schemas.md#designtestselection) to remove. Only a plan awaiting review in this session can change. Returns `{ "plan": ..., "content": ..., "document": ... }`, updates the saved version/digest, clears pending acceptance, appends a manual-change record, and publishes `plan_checks_updated`. Stale digest/version, invalid selections, or persistence/validation failure rejects the edit. It does not run the selected checks.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [plan_api_examples.lua](../../../../nvim/tests/forge/plan_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.plan.check.update",
  "params": {
    "_command": {
      "id": "command:43764:51072399404700:2"
    },
    "plan_id": "0d9cb210-8737-41fb-a3dd-601aec6d70c4",
    "enabled": {
      "verify": true
    },
    "expected_version": 2,
    "digest": "50bf52ad239f9f3874282a84dc7ef749893162c6bd1fd91492a2149e0f81bf9f"
  },
  "session_id": "2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447",
  "id": 22
}
```

```json
{
  "result": {
    "plan": {
      "updated_at_ms": 1793254176545,
      "session_id": "2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447",
      "document_version": 3,
      "state": "awaiting_review",
      "title": "Design the requested change",
      "working_path": "/fixture/nvim.0/Nr9VKJ/0/data/forge/harness\\plans\\2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447\\0d9cb210-8737-41fb-a3dd-601aec6d70c4\\working.md",
      "submitted_version": 2,
      "accepted_revision": null,
      "user_revision": 0,
      "model_revision": 1,
      "accepted_digest": null,
      "elicitation": null,
      "acceptance": null,
      "question_ledger": {
        "resolution": []
      },
      "request": "Build the API fixture",
      "review_digest": "48e3cfe2f821383be1029c5468751bb59603daf4292062137fefa0e02ec4558f",
      "generation": {
        "consecutive_no_progress": 0,
        "max_consecutive_no_progress": 2,
        "canonical_revision": 1,
        "turn_count": 1,
        "max_turn_count": 20
      },
      "validation_warning": [],
      "id": "0d9cb210-8737-41fb-a3dd-601aec6d70c4",
      "manual_changes": [
        "Updated plan checks: verify enabled"
      ],
      "created_at_ms": 1793254176371
    },
    "content": " Objective \nRevise the registry interface.\n\n Requirements \n- Preserve the registry ownership boundary.\n\n Background \nThe registry owns published handles used by its callers.\n\n Design \nRevise the registry interface while preserving its ownership boundary.\n\n Changes \nNew      src/change.rs +4 -0\n@@ +4 -0\npub fn requested_change() {\n  Algorithm\n    return\n}\n\n Tests · None planned \n\n Checks \nStage │ Command │ Scope │ Why                  │ Status\n──────┼─────────┼───────┼──────────────────────┼─────────\nTest  │ exit 0  │ All   │ Explicitly requested │ Will run\n\n Verification \n- Confirm the requested interface behaves as designed.\n\n",
    "document": {
      "assumptions": [],
      "schema_version": 15,
      "usage": null,
      "design": {
        "moved": {},
        "document": {
          "background": "The registry owns published handles used by its callers.",
          "objective": "Revise the registry interface.",
          "requirements": [
            "Preserve the registry ownership boundary."
          ],
          "design": "Revise the registry interface while preserving its ownership boundary.",
          "decisions": [],
          "flows": [],
          "verification": [
            "Confirm the requested interface behaves as designed."
          ],
          "tests": [],
          "checks": [
            "verify"
          ]
        },
        "check_preview": [
          {
            "category": "test",
            "selected": true,
            "pending": false,
            "reason": "Explicitly requested",
            "command": "exit 0",
            "scope": "all",
            "id": "verify"
          }
        ],
        "proposed_calls": {
          "src/change.rs": [
            {
              "owner": "requested_change",
              "behavior": {
                "kind": "algorithm",
                "text": "return"
              }
            }
          ]
        },
        "check_preview_source": "4e042592668edcd0adeb820385bd36afffa44d4f13cda5c8e2551d00cdd78f3f",
        "validation": {
          "fingerprint": "4c4e944135261402e989dc95a871880278ec2b695c01b26b77b7306d022a0155",
          "checked": 0,
          "diagnostic": []
        },
        "check_configuration": {
          "digest": "9af5a60012360f2ccc558e499c2a9c86460298f65fbf0b5c93ff6b8766c5a066",
          "helper_digest": {},
          "config": {
            "verify": {
              "trigger": {},
              "action": {
                "category": "test",
                "command": "exit 0"
              }
            }
          }
        },
        "baseline": {},
        "line_width": 80,
        "proposed": {
          "src/change.rs": "pub fn requested_change();\n"
        }
      },
      "dependencies": [],
      "stages": [],
      "version": 3,
      "title": "Design the requested change",
      "plan_id": "0d9cb210-8737-41fb-a3dd-601aec6d70c4",
      "prompt": "Build the API fixture",
      "flows": [],
      "entity_changes": [],
      "overview": "Build the API fixture"
    }
  },
  "id": 22
}
```

<!-- /captured-example -->

## `session.plan.declaration.get`

Resolve a source declaration referenced by a plan.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The plan/declaration navigation target.
- **Result and effects:** Returns declaration/navigation data.
- **Boundary:** Generated plan coordinates must resolve against the recorded source identity.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/controller.lua:1076](../../../../nvim/lua/forge/views/harness/controller.lua).

<!-- detailed-contract -->
### Contract

Requires `plan_id`, `path`, and submitted `revision` (unsigned 32-bit integer). `baseline` defaults to false; `document` defaults to false. For declarations, selects the baseline or proposed source at that revision. With `document: true`, path selects an overview section by title. Returns `{ "text": ..., "path": ..., "revision": ..., "baseline": ... }`. This is read-only and does not select/resume the plan task. Wrong session, missing revision/design/path, or unknown overview section fails.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [plan_api_examples.lua](../../../../nvim/tests/forge/plan_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.plan.declaration.get",
  "params": {
    "revision": 1,
    "path": "src/change.rs",
    "plan_id": "0d9cb210-8737-41fb-a3dd-601aec6d70c4"
  },
  "session_id": "2e7ca9e2-7fbe-4fa3-8229-42b8c40cb447",
  "id": 21
}
```

```json
{
  "result": {
    "path": "src/change.rs",
    "revision": 1,
    "text": "pub fn requested_change() {\n  Algorithm\n    return\n}\n",
    "baseline": false
  },
  "id": 21
}
```

<!-- /captured-example -->

## `session.plan.entity.rename`

Rename an entity in the reviewed plan model.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The captured entity and replacement-name payload.
- **Result and effects:** Returns the updated plan projection and revision.
- **Boundary:** Validate the current plan/source identity and preserve unrelated draft content.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/lua/forge/views/plan_review/document.lua:234](../../../../nvim/lua/forge/views/plan_review/document.lua).

<!-- detailed-contract -->
### Contract

Requires `plan_id` and nonempty new `name`. Supply either `symbol` for a declaration identity or `entity_name` for a planned added entity. A symbol rename requires current `expected_version`; optional `digest` validates the reviewed and working content. Returns `{ "plan": ..., "content": ..., "document": ..., "previous_name": ..., "name": ... }`, updates review identity, clears acceptance, and publishes `plan_entity_renamed`. Only a plan awaiting review can change. Missing entity/symbol, stale version/digest, invalid rename, and persistence failure reject the edit. This edits the planned declaration, not workspace source.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [plan_review_host.lua](../../../../nvim/tests/forge/plan_review_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "params": {
    "draft_annotation": [],
    "review": {
      "action": "session.plan.entity.rename",
      "revision": 0,
      "block": "plan:design:file:93e2f06f7a58b8c1fe2bffdadb338fc76bfc9d6acf090c7d22495734b917c7a5:hunk:0:row:0",
      "view": "view:51068319127200:2",
      "document": "plan:review:51068318955800",
      "target": "plan:declaration:plan:design:file:93e2f06f7a58b8c1fe2bffdadb338fc76bfc9d6acf090c7d22495734b917c7a5:hunk:0:row:0",
      "position": {
        "row": 0,
        "column": 7
      },
      "sequence": 2
    },
    "draft_source_digest": "84360b8ac6f3e1b28c0a22d557405d8791ea221bee71c8ef3ced58c2685de704",
    "operation_id": "plan-edit:51068398311100",
    "symbol": "/fixture/nvim.0/BqzBeR/0/src/change.rs:1:7",
    "expected_version": 2,
    "name": "reviewed_change",
    "_command": {
      "runtime_epoch": "29882e1e-eb41-4caa-a657-59139b500c42",
      "id": "plan-edit:51068398311100"
    }
  },
  "id": 29,
  "method": "session.plan.entity.rename",
  "session_id": "d1a54c49-ee08-4e0f-b449-8b737b347168"
}
```

```json
{
  "result": {
    "plan": {
      "request": "build the feature",
      "working_path": "/fixture/nvim.0/BqzBeR/1/forge/harness\\plans\\d1a54c49-ee08-4e0f-b449-8b737b347168\\8fc2d99f-10eb-4b8a-8dbd-983a665fe49e\\working.md",
      "review_digest": "0682487c882916d75d8f6afb1e513225db35a47c713117c61ae29fb3972adafc",
      "document_version": 3,
      "elicitation": null,
      "accepted_revision": null,
      "user_revision": 0,
      "accepted_digest": null,
      "question_ledger": {
        "resolution": []
      },
      "manual_changes": [
        "Renamed requested_change to reviewed_change"
      ],
      "id": "8fc2d99f-10eb-4b8a-8dbd-983a665fe49e",
      "generation": {
        "consecutive_no_progress": 0,
        "max_consecutive_no_progress": 2,
        "canonical_revision": 1,
        "turn_count": 1,
        "max_turn_count": 20
      },
      "validation_warning": [],
      "state": "awaiting_review",
      "created_at_ms": 1793254171808,
      "acceptance": null,
      "session_id": "d1a54c49-ee08-4e0f-b449-8b737b347168",
      "title": "Design the requested change",
      "updated_at_ms": 1793254172572,
      "model_revision": 1,
      "submitted_version": 2
    },
    "content": " Objective \nRevise the registry interface.\n\n Requirements \n- Preserve the registry ownership boundary.\n\n Background \nThe registry owns published handles used by its callers.\n\n Design \nRevise the registry interface while preserving its ownership boundary.\n\n Changes \nNew      src/change.rs +4 -0\n@@ +4 -0\npub fn reviewed_change() {\n  Algorithm\n    return\n}\n\n Tests · None planned \n\n Checks \nNo automated checks configured.\n\n Verification \n- Confirm the requested interface behaves as designed.\n\n",
    "document": {
      "assumptions": [],
      "prompt": "build the feature",
      "flows": [],
      "design": {
        "proposed": {
          "src/change.rs": "pub fn reviewed_change();\n"
        },
        "moved": {},
        "proposed_calls": {
          "src/change.rs": [
            {
              "behavior": {
                "kind": "algorithm",
                "text": "return"
              },
              "owner": "reviewed_change"
            }
          ]
        },
        "baseline": {},
        "check_preview_source": "96576c0ab069cfbb4bee5e4d7dfe1bff972e5d740476f8b7649dc8594cae0cb2",
        "document": {
          "tests": [],
          "requirements": [
            "Preserve the registry ownership boundary."
          ],
          "design": "Revise the registry interface while preserving its ownership boundary.",
          "decisions": [],
          "flows": [],
          "verification": [
            "Confirm the requested interface behaves as designed."
          ],
          "objective": "Revise the registry interface.",
          "background": "The registry owns published handles used by its callers."
        },
        "check_configuration": {
          "helper_digest": {},
          "digest": "d827049039f82a8b65a9b0f52e637cf3bc5d1e0dec7d6198edf901a5e3dcae7d",
          "config": {}
        },
        "line_width": 80
      },
      "plan_id": "8fc2d99f-10eb-4b8a-8dbd-983a665fe49e",
      "version": 3,
      "schema_version": 15,
      "overview": "build the feature",
      "title": "Design the requested change",
      "usage": null,
      "entity_changes": [],
      "dependencies": [],
      "stages": []
    },
    "review": {
      "source_edit": {
        "remove": [],
        "upsert": []
      },
      "checks": [],
      "patch": {
        "base_rows": 24,
        "base_blocks": 24,
        "base": 0,
        "next_rows": 24,
        "document": "plan:review:51068318955800",
        "metadata_edit": [
          {
            "metadata": {
              "decoration": [
                {
                  "priority": 90,
                  "range": {
                    "end": {
                      "row": 1,
                      "column": 0
                    },
                    "start": {
                      "row": 0,
                      "column": 0
                    }
                  },
                  "capture": "ForgeAddBg"
                }
              ],
              "visible_decoration": [
                {
                  "priority": 100,
                  "range": {
                    "end": {
                      "row": 0,
                      "column": 22
                    },
                    "start": {
                      "row": 0,
                      "column": 7
                    }
                  },
                  "capture": "@variable.rust"
                },
                {
                  "priority": 100,
                  "range": {
                    "end": {
                      "row": 0,
                      "column": 22
                    },
                    "start": {
                      "row": 0,
                      "column": 7
                    }
                  },
                  "capture": "@function.rust"
                },
                {
                  "priority": 100,
                  "range": {
                    "end": {
                      "row": 0,
                      "column": 3
                    },
                    "start": {
                      "row": 0,
                      "column": 0
                    }
                  },
                  "capture": "@keyword.modifier.rust"
                },
                {
                  "priority": 100,
                  "range": {
                    "end": {
                      "row": 0,
                      "column": 6
                    },
                    "start": {
                      "row": 0,
                      "column": 4
                    }
                  },
                  "capture": "@keyword.function.rust"
                },
                {
                  "priority": 100,
                  "range": {
                    "end": {
                      "row": 0,
                      "column": 23
                    },
                    "start": {
                      "row": 0,
                      "column": 22
                    }
                  },
                  "capture": "@punctuation.bracket.rust"
                },
                {
                  "priority": 100,
                  "range": {
                    "end": {
                      "row": 0,
                      "column": 24
                    },
                    "start": {
                      "row": 0,
                      "column": 23
                    }
                  },
                  "capture": "@punctuation.bracket.rust"
                },
                {
                  "priority": 100,
                  "range": {
                    "end": {
                      "row": 0,
                      "column": 25
                    },
                    "start": {
                      "row": 0,
                      "column": 24
                    }
                  },
                  "capture": "@punctuation.delimiter.rust"
                }
              ],
              "target": [
                {
                  "range": {
                    "end": {
                      "row": 1,
                      "column": 0
                    },
                    "start": {
                      "row": 0,
                      "column": 0
                    }
                  },
                  "id": "plan:declaration:plan:design:file:93e2f06f7a58b8c1fe2bffdadb338fc76bfc9d6acf090c7d22495734b917c7a5:hunk:0:row:0"
                }
              ],
              "collapse": [
                {
                  "closed": true,
                  "id": "plan:calls:93e2f06f7a58b8c1fe2bffdadb338fc76bfc9d6acf090c7d22495734b917c7a5:proposed:93c62e2681f0bedd18542d1ae659b8020eded775637164bf61d9e915ee0c1899",
                  "opening": {
                    "block": "plan:design:file:93e2f06f7a58b8c1fe2bffdadb338fc76bfc9d6acf090c7d22495734b917c7a5:hunk:0:row:0",
                    "position": {
                      "row": 0,
                      "column": 0
                    }
                  }
                }
              ],
              "gutter": [
                {
                  "placement": "inline",
                  "chunk": [
                    {
                      "capture": "ForgeAddBg",
                      "text": "   "
                    },
                    {
                      "capture": "ForgeAddBg",
                      "text": "  "
                    },
                    {
                      "capture": "ForgeAddLineNr",
                      "text": "  1"
                    },
                    {
                      "capture": "ForgeAddBg",
                      "text": "  "
                    },
                    {
                      "capture": "ForgeAddLineNr",
                      "text": "+"
                    },
                    {
                      "capture": "ForgeAddBg",
                      "text": " "
                    }
                  ],
                  "position": {
                    "row": 0,
                    "column": 0
                  },
                  "priority": 100
                }
              ],
              "editable_region": []
            },
            "row_count": 1,
            "block": "plan:design:file:93e2f06f7a58b8c1fe2bffdadb338fc76bfc9d6acf090c7d22495734b917c7a5:hunk:0:row:0"
          }
        ],
        "removed_block": [],
        "next": 1,
        "text_edit": [
          {
            "text": [
              "pub fn reviewed_change() {...}"
            ],
            "removed_rows": 1,
            "start_row": 15
          }
        ],
        "block_edit": [],
        "next_blocks": 24
      },
      "saved_source_digest": "646de0e859c13c520e06dd562549fda165e9772d360fb3d02ea146926ab45724",
      "version": 3,
      "annotation": [],
      "public_only": true
    },
    "name": "reviewed_change",
    "previous_name": "requested_change"
  },
  "id": 29
}
```

<!-- /captured-example -->

## `session.plan.get`

Read a selected saved plan.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** plan_id and optional revision.
- **Result and effects:** Returns the requested plan representation.
- **Boundary:** Selecting an old revision does not make it the current editable plan.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/controller.lua:1105](../../../../nvim/lua/forge/views/harness/controller.lua).

<!-- detailed-contract -->
### Contract

Requires `plan_id` (string). Optional `revision` is a positive integer identifying a historical model revision. Returns [PlanRecord](runtime-schemas.md#planrecord). A historical result substitutes the captured title, working path, document version, model revision, and review digest, and adds `historical_revision`. Missing artifacts, plans owned by another session, plans without submitted reviews, and unavailable revisions fail. Reading does not select a task or resume execution and emits no progress.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [semantic_execution_host.lua](../../../../nvim/tests/forge/semantic_execution_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 48,
  "method": "session.plan.get",
  "params": {
    "revision": 1,
    "plan_id": "6bc6bb92-5de0-46d8-aa46-06e223e6b543"
  },
  "session_id": "34d8be31-e867-4265-9fd8-9a0db4216c1d"
}
```

```json
{
  "id": 48,
  "result": {
    "id": "6bc6bb92-5de0-46d8-aa46-06e223e6b543",
    "validation_warning": [],
    "generation": {
      "max_turn_count": 20,
      "consecutive_no_progress": 0,
      "max_consecutive_no_progress": 2,
      "canonical_revision": 1,
      "turn_count": 1
    },
    "review_digest": "096875bb579e3d04ad228d5f5f36089e9e742004133eca80ed847bfcc1b054a2",
    "session_id": "34d8be31-e867-4265-9fd8-9a0db4216c1d",
    "state": "accepted",
    "elicitation": null,
    "request": "build the feature",
    "title": "Design the requested change",
    "working_path": "/fixture/nvim.0/S3Olz7/1/forge/harness\\plans\\34d8be31-e867-4265-9fd8-9a0db4216c1d\\6bc6bb92-5de0-46d8-aa46-06e223e6b543\\revisions\\submitted-0001.md",
    "updated_at_ms": 1793254205316,
    "document_version": 2,
    "model_revision": 1,
    "submitted_version": 2,
    "accepted_revision": 1,
    "user_revision": 1,
    "manual_changes": [],
    "accepted_digest": "096875bb579e3d04ad228d5f5f36089e9e742004133eca80ed847bfcc1b054a2",
    "acceptance": null,
    "question_ledger": {
      "resolution": []
    },
    "created_at_ms": 1793254204688,
    "historical_revision": 1
  }
}
```

<!-- /captured-example -->

## `session.plan.list`

List plans available for replanning/review.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** An empty parameter object.
- **Result and effects:** Returns the plan-choice inventory.
- **Boundary:** Listing does not accept a plan.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/task.lua:271](../../../../nvim/lua/forge/views/harness/task.lua).

<!-- detailed-contract -->
### Contract

No parameter fields. Returns an array of [PlanChoice](runtime-schemas.md#planchoice) for this session. Each choice identifies the plan, submitted digest if present, title, creation time, model revision count, current plan state, session name, and whether a completed execution exists. Empty results are valid. It does not select, approve, execute, or mutate a plan and emits no progress.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_tasks_host.lua](../../../../nvim/tests/forge/harness_tasks_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "params": {},
  "method": "session.plan.list",
  "session_id": "3f514535-7cfd-42bd-baa6-2731a62f3949",
  "id": 118
}
```

```json
{
  "result": [
    {
      "implemented": false,
      "digest": "3f4a1c1b5d71f3ce273736c289675ea4b46c9296c293192801cd8968d1017550",
      "created_at_ms": 1793250181249,
      "state": "cancelled",
      "revision_count": 1,
      "id": "167903dc-cda0-4314-afd7-94784869298d",
      "session_name": "Inspect the canonical command ",
      "title": "Design the requested change"
    }
  ],
  "id": 118
}
```

<!-- /captured-example -->

## `session.plan.question.answer`

Answer a question about a reviewed plan.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The captured plan-question target and answer.
- **Result and effects:** Returns the coordinator's answer/result updates.
- **Boundary:** The active question/plan revision must still match the captured target.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/lua/forge/views/plan_review/document.lua:213](../../../../nvim/lua/forge/views/plan_review/document.lua).

<!-- detailed-contract -->
### Contract

Requires `review` ([DocumentInput](repository-schemas.md#documentinput)), `draft_source_digest`, and `draft_annotation` (captured review annotations). It first saves annotations against that source digest, then collects questions owned by the captured review. Returns `{ "annotation": [...] }` after all selected questions have been answered. Nonempty question work creates a same-provider native fork, waits for fork readiness, and uses the provider-local Ask model selection or inherited planning model. The answer session has write access disabled and must submit through the dedicated answer tool. Stale review/source identity, unsupported native fork, unavailable model, provider failure, or invalid answer fails. The source session does not receive a new chat exchange for these questions.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [plan_review_host.lua](../../../../nvim/tests/forge/plan_review_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "params": {
    "draft_source_digest": "646de0e859c13c520e06dd562549fda165e9772d360fb3d02ea146926ab45724",
    "review": {
      "action": "session.plan.question.answer",
      "revision": 0,
      "block": "plan:section:objective",
      "view": "view:51068523823900:3",
      "document": "plan:review:51068523671100",
      "target": "plan:declaration:plan:section:objective",
      "position": {
        "row": 0,
        "column": 0
      },
      "sequence": 1
    },
    "draft_annotation": [
      {
        "source": {
          "body": "Sp literal ** note\nsecond line\n",
          "start_line": 2,
          "end_line": 2
        },
        "id": "1"
      },
      {
        "source": {
          "body": "Why does this change preserve ownership?",
          "start_line": 1,
          "end_line": 1
        },
        "id": "2",
        "kind": "question"
      }
    ],
    "_command": {
      "runtime_epoch": "29882e1e-eb41-4caa-a657-59139b500c42",
      "id": "command:8332:51068551062900:1"
    }
  },
  "id": 37,
  "method": "session.plan.question.answer",
  "session_id": "d1a54c49-ee08-4e0f-b449-8b737b347168"
}
```

```json
{
  "result": {
    "annotation": [
      {
        "source": {
          "body": "Sp literal ** note\nsecond line\n",
          "start_line": 2,
          "end_line": 2
        },
        "id": "1",
        "anchor": {
          "end": {
            "target_type": "section",
            "section": "objective"
          },
          "start": {
            "target_type": "section",
            "section": "objective"
          }
        }
      },
      {
        "id": "2",
        "kind": "question",
        "reply": {
          "body": "This declaration defines the proposed interface and separates its responsibility from the surrounding code. Implementation details remain outside the declaration plan.",
          "duration_ms": 216,
          "question_body": "Why does this change preserve ownership?"
        },
        "source": {
          "body": "Why does this change preserve ownership?",
          "start_line": 1,
          "end_line": 1
        },
        "anchor": {
          "end": {
            "target_type": "section",
            "section": "objective"
          },
          "start": {
            "target_type": "section",
            "section": "objective"
          }
        }
      }
    ]
  },
  "id": 37
}
```

<!-- /captured-example -->

## `session.plan.test.delete`

Remove selected tests from the reviewed plan inventory.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The captured test identities and plan edit context.
- **Result and effects:** Returns the updated plan projection/revision.
- **Boundary:** This edits the plan inventory, not arbitrary source files.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/lua/forge/views/plan_review/document.lua:234](../../../../nvim/lua/forge/views/plan_review/document.lua).

<!-- detailed-contract -->
### Contract

Requires `plan_id`, current review `digest`, `expected_version`, and a nonempty `tests` array of [DesignTestSelection](runtime-schemas.md#designtestselection). Each selected test must exist in the captured plan inventory. Removes those tests from a plan awaiting review and returns `{ "plan": ..., "content": ..., "document": ... }`. It updates version/digest, clears pending acceptance, retains a manual-change record, and publishes `plan_tests_deleted`. This changes planned test metadata, not source test files. Wrong owner/state, stale review, unknown tests, or persistence failure is explicit.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Request and terminal response captured from `deleting_selected_plan_tests_preserves_other_cases_and_rejects_stale_confirmation` in [mod.rs](../../../../nvim/rust/forge/crates/forge-harness/src/broker/mod.rs) through the native service dispatcher. The fixture creates the preceding request authority in memory and starts no external provider. Temporary paths use `/fixture`.

```json
{
  "id": 2,
  "method": "session.plan.test.delete",
  "params": {
    "plan_id": "c71528fd-b6b0-4bef-8cf9-2afb2f5552d2",
    "digest": "7f7574189d744af076ac171f116e9aac5edcd6dd69ec587bd7ad0b8930022e4c",
    "expected_version": 2,
    "tests": [
      {
        "file": "src/game.rs",
        "name": "first"
      },
      {
        "file": "src/game.rs",
        "name": "second"
      },
      {
        "file": "tests/game.rs",
        "name": "first"
      },
      {
        "file": "tests/game.rs",
        "name": "fourth"
      }
    ]
  },
  "session_id": "11526b68-16e0-468c-b217-9657a2021496"
}
```

```json
{
  "id": 2,
  "result": {
    "plan": {
      "id": "c71528fd-b6b0-4bef-8cf9-2afb2f5552d2",
      "session_id": "11526b68-16e0-468c-b217-9657a2021496",
      "request": "add test coverage",
      "title": "Migration plan",
      "state": "awaiting_review",
      "working_path": "/fixture/.tmpgpm8gE\\plans\\11526b68-16e0-468c-b217-9657a2021496\\c71528fd-b6b0-4bef-8cf9-2afb2f5552d2\\working.md",
      "document_version": 3,
      "model_revision": 1,
      "submitted_version": 2,
      "accepted_revision": null,
      "user_revision": 0,
      "manual_changes": [
        "Removed planned tests: src/game.rs::first, src/game.rs::second, tests/game.rs::first, tests/game.rs::fourth"
      ],
      "review_digest": "3acf1285383993512c44841cd64df3b1ab3656caa4308c9da2a430b4e6176efa",
      "accepted_digest": null,
      "elicitation": null,
      "acceptance": null,
      "question_ledger": {
        "resolution": []
      },
      "generation": {
        "turn_count": 2,
        "max_turn_count": 20,
        "consecutive_no_progress": 0,
        "max_consecutive_no_progress": 2,
        "canonical_revision": 1
      },
      "validation_warning": [],
      "created_at_ms": 100,
      "updated_at_ms": 100
    },
    "content": " Objective \nImplement the migration strategy.\n\n Requirements \n- Preserve the migration contract.\n\n Background \nThe fixture contains the original implementation.\n\n Design \nUse the selected migration strategy.\n\n Changes \nNew      src/migration.rs +5 -0\n@@ +5 -0\n/// Use the selected migration strategy.\npub fn migrate() {\n  Algorithm\n    return\n}\n\n Tests · 1 reused \nUnchanged tests/game.rs +0 -0\nkeep\n  Existing coverage\n\n Checks \nNo automated checks configured.\n\n Verification \nNone specified.\n\n",
    "document": {
      "schema_version": 15,
      "version": 3,
      "plan_id": "c71528fd-b6b0-4bef-8cf9-2afb2f5552d2",
      "title": "Migration plan",
      "prompt": "add test coverage",
      "overview": "add test coverage",
      "design": {
        "check_preview_source": "325b1990fc536861b0aa9e6e8e6961cece59d24d74a9c9bda706a9a40b3ddb09",
        "check_configuration": {
          "digest": "d827049039f82a8b65a9b0f52e637cf3bc5d1e0dec7d6198edf901a5e3dcae7d",
          "config": {},
          "helper_digest": {}
        },
        "validation": {
          "fingerprint": "2774951e62e8b602ff18911a8aa7f8e0e297f3949dade9ca5efb506cb35c8342",
          "checked": 0,
          "diagnostic": []
        },
        "document": {
          "objective": "Implement the migration strategy.",
          "requirements": [
            "Preserve the migration contract."
          ],
          "background": "The fixture contains the original implementation.",
          "decisions": [],
          "design": "Use the selected migration strategy.",
          "flows": [],
          "verification": [],
          "tests": [
            {
              "file": "tests/game.rs",
              "cases": [
                {
                  "name": "keep",
                  "change": "reused",
                  "description": "Existing coverage"
                }
              ]
            }
          ]
        },
        "line_width": 80,
        "baseline": {},
        "proposed": {
          "src/migration.rs": "/// Use the selected migration strategy.\npub fn migrate();\n"
        },
        "moved": {},
        "proposed_calls": {
          "src/migration.rs": [
            {
              "owner": "migrate",
              "behavior": {
                "kind": "algorithm",
                "text": "return"
              }
            }
          ]
        }
      },
      "usage": null,
      "entity_changes": [],
      "dependencies": [],
      "flows": [],
      "stages": [],
      "assumptions": []
    }
  }
}
```

<!-- /captured-example -->

## `session.preview`

Read a saved session preview.

- **Context:** Lifecycle: explicit target/source where applicable.
- **Input:** The selected session_id in the method payload.
- **Result and effects:** Returns preview data for that saved session.
- **Boundary:** Previewing does not by itself resume execution.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/session_picker.lua:43](../../../../nvim/lua/forge/views/harness/session_picker.lua).

<!-- detailed-contract -->
### Contract

Requires parameter `session_id` (string) identifying the saved session to preview. This target is distinct from the required calling-session envelope. Returns [SessionPreview](runtime-schemas.md#sessionpreview): rendered `lines`, saved `session`, and child-agent state. Its skipped native exchange/timeline collections are not serialized. Missing saved sessions fail. Reading the preview does not acquire the target's lease, select it, resume it, or invoke a provider. It emits no progress.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [session_api_examples.lua](../../../../nvim/tests/forge/session_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.preview",
  "params": {
    "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72"
  },
  "id": 8,
  "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72"
}
```

```json
{
  "id": 8,
  "result": {
    "agent": {
      "summary": {},
      "run": []
    },
    "session": {
      "name": "",
      "service_tier": "default",
      "shell": "system",
      "workspace": "/fixture/nvim.0\\XIjMZp\\0\\workspace",
      "purpose": "conversation",
      "last_compaction_turn": null,
      "primary_agent_id": "b14c3cec-edd9-49ca-a61f-467677607b72:agent:primary",
      "backend_session_id": null,
      "provider_checkpoint_id": null,
      "provider_fork_state": {
        "state": "ready"
      },
      "provider_label": "Mock CLI",
      "resolved_model": null,
      "plan_executor": {
        "enabled": false,
        "effort": null,
        "model": null
      },
      "plan_compact": false,
      "plan_auto_approve_revisions": true,
      "approval_timeout": {
        "amount": 10,
        "unit": "m"
      },
      "answer_timeout": {
        "amount": 10,
        "unit": "m"
      },
      "context_window": null,
      "access": {
        "readable_directory": [],
        "writable_directory": [],
        "windows_sandbox": "elevated",
        "sandbox": false,
        "read_access": "full",
        "write_access": "workspace"
      },
      "id": "b14c3cec-edd9-49ca-a61f-467677607b72",
      "approval_mode": "ask",
      "active_task": null,
      "default_approval": "ask",
      "plan_approval": null,
      "created_at_ms": 1793254175750,
      "updated_at_ms": 1793254175750,
      "lease_owner": "nvim-24460-51071561567100",
      "native_fork": false,
      "native_compact": true,
      "effort": "medium",
      "background": null,
      "backend": "mock",
      "model": "default",
      "auto_compaction": {
        "enabled": false,
        "threshold_tokens": 128000
      },
      "context_usage": null
    },
    "lines": []
  }
}
```

<!-- /captured-example -->

## `session.prompt.history.record`

Record a prompt-history entry.

- **Context:** Explicit existing session.
- **Input:** The prompt-history draft and its associated attachment information.
- **Result and effects:** Returns the updated history representation.
- **Boundary:** This records history and does not itself submit a provider turn.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/client.lua:485](../../../../nvim/lua/forge/client.lua).

<!-- detailed-contract -->
### Contract

Accepts [PromptDraft](runtime-schemas.md#promptdraft): text plus its attachment metadata. Returns the saved prompt-history array after recording server time. This writes local reusable draft history, not a session input submission, provider turn, or steering message. It emits no progress and does not require waiting for unrelated provider completion before its callback establishes persistence.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [semantic_execution_host.lua](../../../../nvim/tests/forge/semantic_execution_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 10,
  "method": "session.prompt.history.record",
  "params": {
    "attachments": [],
    "text": "/plan build the feature"
  },
  "session_id": "34d8be31-e867-4265-9fd8-9a0db4216c1d"
}
```

```json
{
  "id": 10,
  "result": [
    {
      "attachments": [],
      "text": "/plan build the feature"
    }
  ]
}
```

<!-- /captured-example -->

## `session.prompt.submit`

Submit a prompt through the retained direct-prompt service.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The prompt input and supported submission/configuration fields.
- **Result and effects:** Returns the submission/session result and provider events.
- **Boundary:** Ordinary Lua submission uses input.apply. Recognized task text is normalized before provider admission and returns its command response, while ordinary text retains prompt submission behavior. Retain this endpoint only if the usage audit demonstrates a caller.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/rust/forge/crates/forge-harness/src/service/input.rs:1108](../../../../nvim/rust/forge/crates/forge-harness/src/service/input.rs).

<!-- detailed-contract -->
### Contract

Requires nonempty `text` and accepts attachment selections in [PromptDraft](runtime-schemas.md#promptdraft). `operation_id` supplies stable identity for command normalization. User task commands such as `/goal` and `/plan` normalize to canonical session actions before provider admission; their response acknowledges the task transition, and the input coordinator delivers its pending entry separately. Ordinary text starts an exchange and returns [BrokerSnapshot](runtime-schemas.md#brokersnapshot) when that interaction settles. Paused or not-yet-entered tasks do not turn ordinary chat into task continuation.

The native coordinator also accepts `queued: true` for owned queued delivery and internal prompt-admission metadata. UI callers should use [session.input.apply](#sessioninputapply) for queued text, preserving message/document identity rather than constructing that internal metadata. Provider events and document updates can precede the terminal response. Attachment, ownership, admission, provider, and finalization errors remain explicit. A response failure is not evidence that no durable input or partial provider output exists.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [request_independence_host.lua](../../../../nvim/tests/forge/request_independence_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 35,
  "method": "session.prompt.submit",
  "session_id": "44880111-ea5e-4ba6-b9c7-bf431690ec3c",
  "params": {
    "text": "Complete after the fork is ready",
    "_command": {
      "id": "command:36464:51095409167400:1"
    }
  }
}
```

```json
{
  "id": 35,
  "result": {
    "session": {
      "answer_timeout": {
        "unit": "m",
        "amount": 10
      },
      "context_window": null,
      "access": {
        "read_access": "full",
        "sandbox": false,
        "write_access": "workspace",
        "readable_directory": [],
        "writable_directory": [],
        "windows_sandbox": "elevated"
      },
      "workspace": "/fixture/nvim.0\\q8FFKB\\0",
      "backend": "mock",
      "name": "held fork",
      "active_task": null,
      "default_approval": "ask",
      "plan_approval": null,
      "created_at_ms": 1793254199526,
      "updated_at_ms": 1793254202728,
      "lease_owner": "nvim-36464-51095217204200",
      "native_fork": true,
      "native_compact": true,
      "purpose": "conversation",
      "shell": "system",
      "model": "default",
      "background": null,
      "service_tier": "default",
      "effort": "medium",
      "context_usage": null,
      "primary_agent_id": "44880111-ea5e-4ba6-b9c7-bf431690ec3c:agent:primary",
      "auto_compaction": {
        "enabled": false,
        "threshold_tokens": 128000
      },
      "backend_session_id": "mock-session-fork",
      "provider_checkpoint_id": "mock-checkpoint",
      "provider_fork_state": {
        "state": "ready"
      },
      "provider_label": "Mock backend",
      "plan_compact": false,
      "resolved_model": "default",
      "plan_executor": {
        "enabled": false,
        "model": null,
        "effort": null
      },
      "plan_auto_approve_revisions": true,
      "approval_timeout": {
        "unit": "m",
        "amount": 10
      },
      "last_compaction_turn": null,
      "id": "44880111-ea5e-4ba6-b9c7-bf431690ec3c",
      "approval_mode": "ask"
    },
    "capability": {
      "approval_mode_list": [
        "ask",
        "deny",
        "allow"
      ],
      "native_fork": true,
      "native_compact": true,
      "os_sandbox": false,
      "compaction": {
        "background": false,
        "summary": true,
        "metrics": true,
        "lifecycle": true
      },
      "agent": {
        "input": true,
        "observe": true,
        "spawn": true,
        "parallel": true,
        "interrupt": true
      },
      "catalog": {
        "mcp": false,
        "live_mcp_mutation": false,
        "skill": false
      },
      "native_steer": true,
      "native_turn_rollback": true,
      "native_goal": false,
      "model_selection": true,
      "effort_selection": true,
      "fast_mode": true,
      "ultrafast_mode": true,
      "permission_control": true,
      "background_terminals": true
    },
    "exchange": {
      "id": "3a39ad12-41a6-4af1-9bcb-e9bce954f464",
      "state": "complete"
    }
  }
}
```

<!-- /captured-example -->

## `session.provider.authenticate`

Run the chosen provider authentication method.

- **Context:** Explicit existing session/provider.
- **Input:** method identifying an offered authentication action.
- **Result and effects:** Returns authentication completion status.
- **Boundary:** Authentication can require external interaction and is admitted separately from active work.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/provider_setup.lua:135](../../../../nvim/lua/forge/views/harness/provider_setup.lua).

<!-- detailed-contract -->
### Contract

Requires `method` (provider authentication method string). Returns `{ "authenticated": true }` after the selected provider's authentication flow completes. It uses this session's captured catalog request, may launch interactive/provider authentication work, and invalidates cached MCP discovery afterward. Unsupported methods, cancellation, provider failures, and missing credentials are errors. It does not submit a user prompt or grant unrelated command permissions.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [session_api_examples.lua](../../../../nvim/tests/forge/session_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.provider.authenticate",
  "params": {
    "method": "fixture"
  },
  "id": 17,
  "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72"
}
```

```json
{
  "id": 17,
  "error": {
    "code": "request_failed",
    "message": "unsupported provider capability: authentication"
  }
}
```

<!-- /captured-example -->

## `session.provider.capabilities.get`

Read the selected provider's capability manifest.

- **Context:** Explicit existing session/provider.
- **Input:** The explicit session context.
- **Result and effects:** Returns supported provider capabilities.
- **Boundary:** A capability listing is not permission to execute a tool.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/capability_picker.lua:9](../../../../nvim/lua/forge/views/harness/capability_picker.lua).

<!-- detailed-contract -->
### Contract

No parameter fields. Returns [CapabilityManifest](runtime-schemas.md#capabilitymanifest), containing backend identity/label, schema revision, optional provider version, runtime availability/reason, notices, and capability entries. Each capability reports support independently of current provider availability. Discovery has a 30-second service deadline and may contact the provider. It does not run a user prompt or emit request progress. Unsupported operations must remain visible as unsupported rather than being inferred from a missing menu item.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [session_api_examples.lua](../../../../nvim/tests/forge/session_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.provider.capabilities.get",
  "params": {},
  "id": 10,
  "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72"
}
```

```json
{
  "id": 10,
  "result": {
    "availability_reason": null,
    "notice": [],
    "capability": [
      {
        "required": true,
        "group": "Conversation",
        "id": "prompt",
        "implementation": "provider",
        "support": "supported",
        "label": "Streaming prompts",
        "application": "immediate",
        "reason": "Required provider contract"
      },
      {
        "required": true,
        "group": "Conversation",
        "id": "cancel",
        "implementation": "provider",
        "support": "supported",
        "label": "Cancel active work",
        "application": "immediate",
        "reason": "Required provider contract"
      },
      {
        "required": true,
        "group": "Conversation",
        "id": "resume",
        "implementation": "provider",
        "support": "supported",
        "label": "Resume conversation",
        "application": "immediate",
        "reason": "Required provider contract"
      },
      {
        "required": true,
        "group": "Conversation",
        "id": "cleanup",
        "implementation": "provider",
        "support": "supported",
        "label": "Collect provider execution",
        "application": "immediate",
        "reason": "Required provider contract"
      },
      {
        "required": true,
        "group": "Conversation",
        "id": "control_tools",
        "implementation": "provider",
        "support": "supported",
        "label": "Harness control tools",
        "application": "immediate",
        "reason": "Required provider contract"
      },
      {
        "required": false,
        "group": "Harness",
        "id": "goals",
        "implementation": "forge",
        "support": "supported",
        "label": "Set, pause, resume, and clear goals",
        "application": "immediate",
        "reason": "Forge owns this workflow"
      },
      {
        "required": false,
        "group": "Harness",
        "id": "queue",
        "implementation": "forge",
        "support": "supported",
        "label": "Queue prompts",
        "application": "immediate",
        "reason": "Forge owns this workflow"
      },
      {
        "required": false,
        "group": "Harness",
        "id": "undo",
        "implementation": "forge",
        "support": "supported",
        "label": "Restore workspace checkpoint",
        "application": "immediate",
        "reason": "Forge owns this workflow"
      },
      {
        "required": false,
        "group": "Harness",
        "id": "plans",
        "implementation": "forge",
        "support": "supported",
        "label": "Plan review and execution",
        "application": "immediate",
        "reason": "Forge owns this workflow"
      },
      {
        "required": false,
        "group": "Harness",
        "id": "questions",
        "implementation": "forge",
        "support": "supported",
        "label": "Harness question forms",
        "application": "immediate",
        "reason": "Forge owns this workflow"
      },
      {
        "required": false,
        "group": "Harness",
        "id": "session_delete",
        "implementation": "forge",
        "support": "supported",
        "label": "Delete Forge history",
        "application": "immediate",
        "reason": "Forge owns this workflow"
      },
      {
        "required": false,
        "group": "Provider",
        "id": "native_fork",
        "implementation": "provider",
        "support": "supported",
        "label": "Fork provider conversation",
        "application": "immediate",
        "reason": "Requires native provider history fork"
      },
      {
        "required": false,
        "group": "Provider",
        "id": "native_compact",
        "implementation": "provider",
        "support": "supported",
        "label": "Compact context manually",
        "application": "immediate",
        "reason": "Requires an explicit provider compaction operation"
      },
      {
        "required": false,
        "group": "Provider",
        "id": "native_steer",
        "implementation": "provider",
        "support": "supported",
        "label": "Steer active work",
        "application": "immediate",
        "reason": "Queued prompts remain available when steering is unsupported"
      },
      {
        "required": false,
        "group": "Provider",
        "id": "native_turn_rollback",
        "implementation": "provider",
        "support": "supported",
        "label": "Retract output-free cancelled prompt",
        "application": "immediate",
        "reason": "Cancellation still stops work without history retraction"
      },
      {
        "required": false,
        "group": "Provider",
        "id": "compaction_lifecycle",
        "implementation": "provider",
        "support": "supported",
        "label": "Compaction timeline events",
        "application": "immediate",
        "reason": "Shows provider evidence only"
      },
      {
        "required": false,
        "group": "Provider",
        "id": "background_compaction",
        "implementation": "provider",
        "support": "unsupported",
        "label": "Observe background compaction",
        "application": "immediate",
        "reason": "Requires compaction events outside an active prompt"
      },
      {
        "required": false,
        "group": "Provider",
        "id": "compaction_summary",
        "implementation": "provider",
        "support": "supported",
        "label": "Compaction summary",
        "application": "immediate",
        "reason": "Requires a provider-exposed summary"
      },
      {
        "required": false,
        "group": "Provider",
        "id": "compaction_metrics",
        "implementation": "provider",
        "support": "supported",
        "label": "Compaction metrics",
        "application": "immediate",
        "reason": "Requires provider-exposed usage evidence"
      },
      {
        "required": false,
        "group": "Provider",
        "id": "model_selection",
        "implementation": "provider",
        "support": "supported",
        "label": "Select model",
        "application": "immediate",
        "reason": "Model preferences remain specific to each provider"
      },
      {
        "required": false,
        "group": "Provider",
        "id": "effort_selection",
        "implementation": "provider",
        "support": "supported",
        "label": "Select reasoning effort",
        "application": "immediate",
        "reason": "Choices come from the selected provider"
      },
      {
        "required": false,
        "group": "Provider",
        "id": "fast_mode",
        "implementation": "provider",
        "support": "supported",
        "label": "Fast service tier",
        "application": "immediate",
        "reason": "Requires provider service-tier support"
      },
      {
        "required": false,
        "group": "Provider",
        "id": "ultrafast_mode",
        "implementation": "provider",
        "support": "supported",
        "label": "Ultrafast service tier",
        "application": "immediate",
        "reason": "Requires provider service-tier support"
      },
      {
        "required": false,
        "group": "Provider",
        "id": "skill_catalog",
        "implementation": "provider",
        "support": "unsupported",
        "label": "Select skills",
        "application": "immediate",
        "reason": "Requires an enforceable skill selection"
      },
      {
        "required": false,
        "group": "Provider",
        "id": "mcp_catalog",
        "implementation": "provider",
        "support": "unsupported",
        "label": "Select MCP servers",
        "application": "immediate",
        "reason": "Requires enforceable server selection"
      },
      {
        "required": true,
        "group": "Security",
        "id": "approval_rules",
        "implementation": "forge",
        "support": "supported",
        "label": "Saved approval rules",
        "application": "immediate",
        "reason": "Forge persists rules and answers individual provider requests"
      },
      {
        "required": false,
        "group": "Security",
        "id": "os_sandbox",
        "implementation": "provider",
        "support": "unsupported",
        "label": "Enforce OS sandbox",
        "application": "session_restart",
        "reason": "Only allowlisted providers enable OS isolation. Other providers resolve sandbox preferences to disabled"
      },
      {
        "required": false,
        "group": "Provider",
        "id": "background_terminals",
        "implementation": "provider",
        "support": "supported",
        "label": "Inspect background terminals",
        "application": "immediate",
        "reason": "Requires an explicit provider terminal inventory"
      },
      {
        "required": false,
        "group": "Provider",
        "id": "file_diff",
        "implementation": "provider",
        "support": "supported",
        "label": "Provider file diffs",
        "application": "immediate",
        "reason": "Projects structured provider changes into tool history and the exchange timeline"
      },
      {
        "required": false,
        "group": "Provider",
        "id": "background_termination",
        "implementation": "provider",
        "support": "supported",
        "label": "Terminate one background terminal",
        "application": "immediate",
        "reason": "Root cancellation remains available without independent terminal controls"
      },
      {
        "required": false,
        "group": "Harness",
        "id": "recap",
        "implementation": "provider",
        "support": "supported",
        "label": "Generate isolated recap and session name",
        "application": "immediate",
        "reason": "Generation must not append to the main provider conversation"
      },
      {
        "required": false,
        "group": "Setup",
        "id": "runtime_setup",
        "implementation": "forge",
        "support": "unsupported",
        "label": "Install supported provider runtime",
        "application": "idle",
        "reason": "ACP setup uses provider-advertised sign-in methods and preserves native credentials"
      },
      {
        "required": false,
        "group": "Setup",
        "id": "authentication",
        "implementation": "forge",
        "support": "unsupported",
        "label": "Sign in from Forge",
        "application": "idle",
        "reason": "ACP setup uses provider-advertised sign-in methods and preserves native credentials"
      },
      {
        "required": false,
        "group": "Harness",
        "id": "agent_observe",
        "implementation": "forge",
        "support": "supported",
        "label": "Inspect child timelines",
        "application": "immediate",
        "reason": "Harness owns child identity, fresh history, task queues, cancellation, and durable outcomes"
      },
      {
        "required": false,
        "group": "Harness",
        "id": "agent_spawn",
        "implementation": "forge",
        "support": "supported",
        "label": "Spawn independent child conversations",
        "application": "immediate",
        "reason": "Harness owns child identity, fresh history, task queues, cancellation, and durable outcomes"
      },
      {
        "required": false,
        "group": "Harness",
        "id": "agent_followup",
        "implementation": "forge",
        "support": "supported",
        "label": "Queue child follow-up tasks",
        "application": "immediate",
        "reason": "Harness owns child identity, fresh history, task queues, cancellation, and durable outcomes"
      },
      {
        "required": false,
        "group": "Harness",
        "id": "agent_message",
        "implementation": "forge",
        "support": "supported",
        "label": "Persist child informational messages",
        "application": "immediate",
        "reason": "Harness owns child identity, fresh history, task queues, cancellation, and durable outcomes"
      },
      {
        "required": false,
        "group": "Harness",
        "id": "agent_interrupt",
        "implementation": "forge",
        "support": "supported",
        "label": "Interrupt one child or all children",
        "application": "immediate",
        "reason": "Harness owns child identity, fresh history, task queues, cancellation, and durable outcomes"
      },
      {
        "required": false,
        "group": "Harness",
        "id": "agent_cross_provider",
        "implementation": "forge",
        "support": "supported",
        "label": "Select each child's provider and model",
        "application": "immediate",
        "reason": "Harness owns child identity, fresh history, task queues, cancellation, and durable outcomes"
      },
      {
        "required": false,
        "group": "Settings",
        "id": "lsp_tools",
        "implementation": "forge",
        "support": "supported",
        "label": "Query running editor language servers",
        "application": "server_restart",
        "reason": "Off by default. Read-only agent queries; interactive plan navigation stays available"
      },
      {
        "required": false,
        "group": "Settings",
        "id": "subagent_tools",
        "implementation": "forge",
        "support": "supported",
        "label": "Enable root model delegation tools",
        "application": "server_restart",
        "reason": "Off by default. Manual child spawning remains available and children cannot delegate"
      },
      {
        "required": false,
        "group": "Settings",
        "id": "session_tools",
        "implementation": "forge",
        "support": "supported",
        "label": "Inspect and search Harness sessions",
        "application": "server_restart",
        "reason": "On by default. Disabling hides history tools but retains configuration inspection and the Harness skill"
      },
      {
        "required": false,
        "group": "Provider",
        "id": "native_agent_suppression",
        "implementation": "provider",
        "support": "supported",
        "label": "Disable provider-native agent tools",
        "application": "session_restart",
        "reason": "Provider-native agent tools are disabled for session creation and resumption"
      },
      {
        "required": false,
        "group": "Settings",
        "id": "skill_mutation",
        "implementation": "provider",
        "support": "unsupported",
        "label": "Apply skill selection",
        "application": "session_restart",
        "reason": "Forge persists desired selection and verifies the provider result"
      },
      {
        "required": false,
        "group": "Settings",
        "id": "native_import",
        "implementation": "forge",
        "support": "supported",
        "label": "Import native skills and MCP definitions",
        "application": "server_restart",
        "reason": "Explicit source preview and selection. Imported entries start disabled and preserve native value references"
      },
      {
        "required": false,
        "group": "Settings",
        "id": "mcp_mutation",
        "implementation": "provider",
        "support": "unsupported",
        "label": "Apply MCP selection",
        "application": "server_restart",
        "reason": "Forge persists desired selection independently of native provider configuration"
      },
      {
        "required": false,
        "group": "Provider",
        "id": "mcp_runtime_status",
        "implementation": "provider",
        "support": "unsupported",
        "label": "Inspect MCP connection status",
        "application": "immediate",
        "reason": "ACP accepts configured servers but does not expose their readiness or tool inventory"
      },
      {
        "required": false,
        "group": "Provider",
        "id": "mcp_tool_catalog",
        "implementation": "provider",
        "support": "unsupported",
        "label": "Browse MCP tool definitions",
        "application": "immediate",
        "reason": "ACP accepts configured servers but does not expose their readiness or tool inventory"
      },
      {
        "required": false,
        "group": "Security",
        "id": "filesystem_access",
        "implementation": "forge",
        "support": "limited",
        "label": "Filesystem access scopes",
        "application": "execution_restart",
        "reason": "Scopes and write-off apply to reported file operations. Shell commands, MCP internals, and operations without permission callbacks retain provider-native restrictions. Codex does not enforce the selected read scope as OS read confinement"
      }
    ],
    "backend": "mock",
    "revision": 6,
    "label": "Mock CLI",
    "provider_version": null,
    "availability": "ready"
  }
}
```

<!-- /captured-example -->

## `session.provider.mcp.list`

Read MCP configuration/capabilities in the selected provider context.

- **Context:** Explicit existing session/provider.
- **Input:** The explicit session/provider context.
- **Result and effects:** Returns the MCP catalog/status.
- **Boundary:** Listing is distinct from changing settings or calling a server tool.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/provider_catalog.lua:65](../../../../nvim/lua/forge/views/harness/provider_catalog.lua).

<!-- detailed-contract -->
### Contract

No parameter fields. Returns an array of [McpDefinition](runtime-schemas.md#mcpdefinition), including each server's normalized state and available tool metadata. The session caches discovery under an MCP revision, which setup/authentication and resource changes invalidate. Server configuration can appear before handshake completion. Disabled, loading, connected, authentication-required, failed, and unavailable states are distinct. It may contact the provider or its servers but does not invoke a tool, mutate configuration, or emit request progress. Apply the result only to the originating session/provider generation.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_native_import_host.lua](../../../../nvim/tests/forge/harness_native_import_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 20,
  "method": "session.provider.mcp.list",
  "params": {},
  "session_id": "8fe7fcf9-64c4-49a0-b80f-813a4cd60e87"
}
```

```json
{
  "result": [
    {
      "source_path": "/fixture/nvim.0\\o3tBDD\\1\\native.json",
      "status": "disabled",
      "transport": "stdio",
      "name": "portable",
      "token_count": null,
      "token_estimated": false,
      "scope": "workspace",
      "status_detail": null,
      "enabled": false,
      "tool_error": null,
      "tools": [],
      "resource_id": "import:82151f6a5ad9948d7b3cb04babc95caa2c63e3ebefa5330917ef03556e6a8276"
    }
  ],
  "id": 20
}
```

<!-- /captured-example -->

## `session.provider.mock_response`

Control a test provider's response barrier.

- **Context:** Explicit existing session/provider.
- **Input:** A mock-response command such as hold, wait, release, or cancel, with its matching identity.
- **Result and effects:** Returns the command-specific mock-control result.
- **Boundary:** Only the mock provider supports this test interface.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/tests/forge/support/mock_provider.lua:18](../../../../nvim/tests/forge/support/mock_provider.lua). Live integration-control use: Hold, observe, release, and cancel mock responses deterministically in live host fixtures.

<!-- detailed-contract -->
### Contract

A live integration-control endpoint available only for the mock provider. `operation: "hold"` accepts optional prompt `mode` and `any_session` (false by default), then returns a new hold `id`. `wait` requires that ID and waits until a matching provider prompt arrives, returning `id`, provider `session_id`, `mode`, and full `prompt`. `release` requires an arrived hold; `cancel` can discard an armed or arrived hold. Both return `id`, `released`, and `cancelled`. IDs belong to the selected session even when matching allows any provider session. Unknown, released, cancelled, or unarrived holds fail according to the operation. These barriers affect test provider responses, not production providers.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_host.lua](../../../../nvim/tests/forge/harness_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 10,
  "method": "session.provider.mock_response",
  "params": {
    "operation": "hold",
    "any_session": false,
    "mode": "chat"
  },
  "session_id": "7950e589-be54-4804-9e68-585010a566a5"
}
```

```json
{
  "id": 10,
  "result": {
    "id": "b7a0f676-c883-4df3-a82a-80d8625bcb2e"
  }
}
```

<!-- /captured-example -->

## `session.provider.model.list`

List models available through the selected session's provider.

- **Context:** Explicit existing session/provider.
- **Input:** The explicit session context.
- **Result and effects:** Returns the provider model catalog.
- **Boundary:** Provider discovery can fail or exceed its discovery deadline.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/background_configuration.lua:72](../../../../nvim/lua/forge/views/harness/background_configuration.lua).

<!-- detailed-contract -->
### Contract

No parameter fields. Returns an array of [BackendModel](runtime-schemas.md#backendmodel), with provider-advertised reasoning/context controls and saved workspace preferences applied. Discovery may contact or launch the configured provider and has a 30-second service deadline. Resolving a default selection can emit session events before the response. No prompt or request-progress stream is produced. Catalog failure is explicit, not an empty-success fallback.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [semantic_execution_host.lua](../../../../nvim/tests/forge/semantic_execution_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 5,
  "method": "session.provider.model.list",
  "params": {},
  "session_id": "34d8be31-e867-4265-9fd8-9a0db4216c1d"
}
```

```json
{
  "id": 5,
  "result": [
    {
      "id": "mock-model",
      "reasoning": [
        "low",
        "medium",
        "high"
      ],
      "service_tiers": [
        "default",
        "fast",
        "ultrafast"
      ],
      "default_context_window": "default",
      "default_reasoning": "medium",
      "context_window": [
        {
          "id": "default",
          "token_limit": 100000
        }
      ],
      "vision": false,
      "is_default": true,
      "selected_reasoning": "medium",
      "description": "Deterministic Harness test model.",
      "selected_context_window": "default"
    }
  ]
}
```

<!-- /captured-example -->

## `session.provider.setup`

Inspect or install the selected provider integration.

- **Context:** Explicit existing session/provider.
- **Input:** install boolean and session/provider context.
- **Result and effects:** Returns setup outcome.
- **Boundary:** Installation and provider I/O are effects of this explicit operation.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/provider_setup.lua:153](../../../../nvim/lua/forge/views/harness/provider_setup.lua).

<!-- detailed-contract -->
### Contract

Accepts optional `install` (boolean, default false). Calls the selected provider's setup operation and returns its provider-specific result object. `install: false` inspects setup; `true` authorizes the configured provider's installation/setup path. The operation requires idle source execution and no pending cleanup, and invalidates MCP discovery after success. Unsupported providers, unavailable installer/runtime, active work, and setup failures are errors. It does not send a chat prompt.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [session_api_examples.lua](../../../../nvim/tests/forge/session_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.provider.setup",
  "params": {
    "install": false
  },
  "id": 16,
  "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72"
}
```

```json
{
  "id": 16,
  "error": {
    "code": "request_failed",
    "message": "unsupported provider capability: runtime_setup"
  }
}
```

<!-- /captured-example -->

## `session.provider.skill.list`

Read skills available to the selected provider context.

- **Context:** Explicit existing session/provider.
- **Input:** The explicit session/provider context.
- **Result and effects:** Returns the skill catalog.
- **Boundary:** Listing does not toggle skills or run them.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/provider_catalog.lua:59](../../../../nvim/lua/forge/views/harness/provider_catalog.lua).

<!-- detailed-contract -->
### Contract

No parameter fields. Returns an array of [SkillDefinition](runtime-schemas.md#skilldefinition): name, description, enabled/user-invocable flags, optional resource binding/scope, path, source, and argument hint. Empty supported catalogs return `[]`. Unsupported skill discovery fails explicitly with `unsupported provider capability: skill_catalog`. The discovery deadline is 30 seconds. It may contact the selected provider but does not invoke a skill, change its enabled state, or emit request progress.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [session_api_examples.lua](../../../../nvim/tests/forge/session_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.provider.skill.list",
  "params": {},
  "id": 11,
  "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72"
}
```

```json
{
  "id": 11,
  "error": {
    "code": "request_failed",
    "message": "unsupported provider capability: skill_catalog"
  }
}
```

<!-- /captured-example -->

## `session.question.answer`

Commit an answer to a pending question.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The question target, answer payload, and ordering/operation identity.
- **Result and effects:** Returns the updated question/session state.
- **Boundary:** Reject a target that no longer names the active question revision.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/lua/forge/command.lua:30](../../../../nvim/lua/forge/command.lua).

<!-- detailed-contract -->
### Contract

Requires `question_id` and a typed [PlanQuestionResponse](runtime-schemas.md#planquestionresponse). For durable UI answers, also supply `operation_id` and `target` ([QuestionTarget](runtime-schemas.md#questiontarget)); optional `answer_order` is a `[writer, sequence]` pair. The target captures owner kind, owner ID, question-set ID, and revision. Returns `{ "operation_id": ..., "active_elicitation": ... }` after the answer transaction commits, then publishes `question_updated`. Exact replay returns the retained receipt; identity reuse with different content fails. Older answers from the same writer do not overwrite a newer sequence. The legacy no-target path returns a full session snapshot after answering the current elicitation. It does not continue provider work. Missing/stale owners, invalid choices, and persistence failures remain errors.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_admission_host.lua](../../../../nvim/tests/forge/harness_admission_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 47,
  "session_id": "f9ead3a6-c5dc-4c34-b8d8-bb8193be9459",
  "params": {
    "question_id": "328087d38a78dba742351417aef632fb08147fafb65005920d46977b9ffecd15",
    "_command": {
      "id": "command:46292:51080682894500:6",
      "runtime_epoch": "53a51083-be7e-41ba-bee2-66727b27ff53"
    },
    "answer_order": [
      "46292:51074248819500",
      7
    ],
    "response": {
      "text": "Use the local fixture",
      "kind": "other"
    },
    "operation_id": "command:46292:51080682894500:6"
  },
  "method": "session.question.answer"
}
```

```json
{
  "id": 47,
  "result": {
    "agent": {
      "run": [],
      "summary": {}
    },
    "session": {
      "model": "default",
      "effort": "medium",
      "access": {
        "read_access": "full",
        "readable_directory": [],
        "writable_directory": [],
        "windows_sandbox": "elevated",
        "sandbox": false,
        "write_access": "off"
      },
      "background": null,
      "id": "f9ead3a6-c5dc-4c34-b8d8-bb8193be9459",
      "workspace": "/fixture/nvim.0\\qjVJBC\\0",
      "last_compaction_turn": null,
      "backend_session_id": "mock-session",
      "provider_checkpoint_id": "mock-checkpoint",
      "provider_label": "Mock backend",
      "resolved_model": "default",
      "provider_fork_state": {
        "state": "ready"
      },
      "updated_at_ms": 1793254184819,
      "shell": "system",
      "default_approval": "ask",
      "plan_approval": null,
      "created_at_ms": 1793254178327,
      "auto_compaction": {
        "enabled": false,
        "threshold_tokens": 128000
      },
      "primary_agent_id": "f9ead3a6-c5dc-4c34-b8d8-bb8193be9459:agent:primary",
      "purpose": "conversation",
      "backend": "mock",
      "active_task": null,
      "name": "Ask a fixture question.",
      "context_usage": null,
      "approval_mode": "allow",
      "native_compact": true,
      "plan_executor": {
        "enabled": false,
        "model": null,
        "effort": null
      },
      "service_tier": "default",
      "answer_timeout": {
        "unit": "m",
        "amount": 10
      },
      "plan_auto_approve_revisions": true,
      "native_fork": true,
      "plan_compact": false,
      "approval_timeout": {
        "unit": "m",
        "amount": 10
      },
      "context_window": null,
      "lease_owner": "nvim-46292-51074136430900"
    },
    "restore_recovery": null,
    "prompt_history": [],
    "approval": [],
    "access_abbreviation": "rx",
    "status": {
      "exchange_id": "dd0a4098-4ebb-44c6-b990-1bab081f2934",
      "kind": "awaiting_input",
      "plan_id": null,
      "owner": "interaction"
    },
    "checks_running": false,
    "active_wait": null,
    "active_check_run": null,
    "check_editing": false,
    "active_plan": null,
    "artifact": [],
    "automatic_checks": false,
    "timeline_revision": 8,
    "no_checkpoint": true,
    "session_tools": true,
    "ask_model": {},
    "capability": {
      "catalog": {
        "mcp": false,
        "skill": false,
        "live_mcp_mutation": false
      },
      "os_sandbox": false,
      "compaction": {
        "lifecycle": true,
        "metrics": true,
        "summary": true,
        "background": false
      },
      "native_compact": true,
      "agent": {
        "interrupt": true,
        "spawn": true,
        "input": true,
        "parallel": true,
        "observe": true
      },
      "native_turn_rollback": true,
      "native_goal": false,
      "native_steer": true,
      "approval_mode_list": [
        "ask",
        "deny",
        "allow"
      ],
      "background_terminals": true,
      "permission_control": true,
      "native_fork": true,
      "model_selection": true,
      "ultrafast_mode": true,
      "effort_selection": true,
      "fast_mode": true
    },
    "lsp_tools": false,
    "checks_deferred": false,
    "status_thoughts": false,
    "runtime_epoch": "53a51083-be7e-41ba-bee2-66727b27ff53",
    "subagent_tools": false,
    "snapshot_revision": 26,
    "task_operation": {
      "id": "46292:51080607241400:3",
      "action": "permission",
      "revision": 4,
      "error": null,
      "state": "completed"
    },
    "check_timing": "after_queue",
    "active_elicitation": {
      "elicitation": {
        "clarification_active": false,
        "question_set": {
          "id": "0794c3c88e22f560985e80b55660736b810e66f316f51e74a86aea617e6fd551",
          "questions": [
            {
              "id": "328087d38a78dba742351417aef632fb08147fafb65005920d46977b9ffecd15",
              "question": "Which fixture should the operation use?",
              "options": [],
              "allow_freeform": true,
              "header": "Planning feedback"
            }
          ]
        },
        "deadline": {
          "expires_at_ms": 1793254781425
        },
        "resolution_source": "user",
        "answer_order": {},
        "answer_revision": 2,
        "revision": 2,
        "answer": [
          {
            "question_id": "328087d38a78dba742351417aef632fb08147fafb65005920d46977b9ffecd15",
            "source": "user",
            "response": {
              "text": "Use the local fixture",
              "kind": "other"
            }
          }
        ],
        "current_index": 1
      },
      "exchange_id": "dd0a4098-4ebb-44c6-b990-1bab081f2934",
      "plan_id": null,
      "owner": "interaction"
    }
  }
}
```

<!-- /captured-example -->

## `session.question.ask`

Submit a question through the session's question workflow.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The question text and supported context.
- **Result and effects:** Returns the resulting question/workflow state.
- **Boundary:** This can involve provider work; it is not a local string lookup.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/tests/forge/harness_admission_host.lua:44](../../../../nvim/tests/forge/harness_admission_host.lua). Live integration-control use: Exercise overlapping clarification admission without replacing the pending question.

<!-- detailed-contract -->
### Contract

Requires `question_id` and nonempty `text`. The ID must identify the pending question or its containing set. This records clarification input under that elicitation and runs a provider clarification turn, returning [BrokerSnapshot](runtime-schemas.md#brokersnapshot) after settlement. It can update or withdraw questions and, when no decision remains, consume the clarification and continue. It does not accept clarification against plan-acceptance choices. Missing ownership, mismatched questions, active-work admission, and provider failures remain explicit.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_admission_host.lua](../../../../nvim/tests/forge/harness_admission_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 14,
  "session_id": "f9ead3a6-c5dc-4c34-b8d8-bb8193be9459",
  "params": {
    "question_id": "328087d38a78dba742351417aef632fb08147fafb65005920d46977b9ffecd15",
    "_command": {
      "id": "command:46292:51077321651500:2",
      "runtime_epoch": "53a51083-be7e-41ba-bee2-66727b27ff53"
    },
    "text": "Explain the available choices."
  },
  "method": "session.question.ask"
}
```

```json
{
  "id": 14,
  "result": {
    "capability": {
      "catalog": {
        "mcp": false,
        "skill": false,
        "live_mcp_mutation": false
      },
      "os_sandbox": false,
      "compaction": {
        "lifecycle": true,
        "metrics": true,
        "summary": true,
        "background": false
      },
      "native_compact": true,
      "agent": {
        "interrupt": true,
        "spawn": true,
        "input": true,
        "parallel": true,
        "observe": true
      },
      "native_turn_rollback": true,
      "native_goal": false,
      "native_steer": true,
      "approval_mode_list": [
        "ask",
        "deny",
        "allow"
      ],
      "background_terminals": true,
      "permission_control": true,
      "native_fork": true,
      "model_selection": true,
      "ultrafast_mode": true,
      "effort_selection": true,
      "fast_mode": true
    },
    "exchange": {
      "id": "dd0a4098-4ebb-44c6-b990-1bab081f2934",
      "state": "running"
    },
    "session": {
      "model": "default",
      "effort": "medium",
      "access": {
        "read_access": "full",
        "readable_directory": [],
        "writable_directory": [],
        "windows_sandbox": "elevated",
        "sandbox": false,
        "write_access": "workspace"
      },
      "background": null,
      "id": "f9ead3a6-c5dc-4c34-b8d8-bb8193be9459",
      "workspace": "/fixture/nvim.0\\qjVJBC\\0",
      "last_compaction_turn": null,
      "backend_session_id": "mock-session",
      "provider_checkpoint_id": "mock-checkpoint",
      "provider_label": "Mock backend",
      "resolved_model": "default",
      "provider_fork_state": {
        "state": "ready"
      },
      "updated_at_ms": 1793254184503,
      "shell": "system",
      "default_approval": "ask",
      "plan_approval": null,
      "created_at_ms": 1793254178327,
      "auto_compaction": {
        "enabled": false,
        "threshold_tokens": 128000
      },
      "primary_agent_id": "f9ead3a6-c5dc-4c34-b8d8-bb8193be9459:agent:primary",
      "purpose": "conversation",
      "backend": "mock",
      "active_task": null,
      "name": "Ask a fixture question.",
      "context_usage": null,
      "approval_mode": "ask",
      "native_compact": true,
      "plan_executor": {
        "enabled": false,
        "model": null,
        "effort": null
      },
      "service_tier": "default",
      "answer_timeout": {
        "unit": "m",
        "amount": 10
      },
      "plan_auto_approve_revisions": true,
      "native_fork": true,
      "plan_compact": false,
      "approval_timeout": {
        "unit": "m",
        "amount": 10
      },
      "context_window": null,
      "lease_owner": "nvim-46292-51074136430900"
    }
  }
}
```

<!-- /captured-example -->

## `session.question.continue`

Continue after required answers have committed.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** Optional captured question target with owner, id, set_id, and revision.
- **Result and effects:** Returns continuation/session state.
- **Boundary:** The handler rejects changed targets and incomplete required answers. Acceptance completion first resolves a prior receipt or captures the Plan ID and review digest, then commits CompleteAcceptance without provider admission. Other question continuations retain their provider-execution requirements.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/lua/forge/views/harness/controller.lua:878](../../../../nvim/lua/forge/views/harness/controller.lua).

<!-- detailed-contract -->
### Contract

Accepts optional captured `target` ([QuestionTarget](runtime-schemas.md#questiontarget)) and stable `operation_id`. The target must still identify the active elicitation and all required answers must have committed. For plan acceptance, normalization routes through the canonical acceptance command and returns its durable receipt. For an ordinary plan/interaction question set, it consumes the committed feedback and runs the corresponding provider continuation, returning the resulting snapshot. Continuation is a separate action from saving an answer. Missing answers, stale identity/revision, failed admission, and provider errors fail explicitly; already consumed feedback is not made pending again merely because a later provider turn fails.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_admission_host.lua](../../../../nvim/tests/forge/harness_admission_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 48,
  "session_id": "f9ead3a6-c5dc-4c34-b8d8-bb8193be9459",
  "params": {
    "_command": {
      "id": "command:46292:51080698317200:8",
      "runtime_epoch": "53a51083-be7e-41ba-bee2-66727b27ff53"
    }
  },
  "method": "session.question.continue"
}
```

```json
{
  "id": 48,
  "result": {
    "capability": {
      "catalog": {
        "mcp": false,
        "skill": false,
        "live_mcp_mutation": false
      },
      "os_sandbox": false,
      "compaction": {
        "lifecycle": true,
        "metrics": true,
        "summary": true,
        "background": false
      },
      "native_compact": true,
      "agent": {
        "interrupt": true,
        "spawn": true,
        "input": true,
        "parallel": true,
        "observe": true
      },
      "native_turn_rollback": true,
      "native_goal": false,
      "native_steer": true,
      "approval_mode_list": [
        "ask",
        "deny",
        "allow"
      ],
      "background_terminals": true,
      "permission_control": true,
      "native_fork": true,
      "model_selection": true,
      "ultrafast_mode": true,
      "effort_selection": true,
      "fast_mode": true
    },
    "exchange": {
      "id": "dd0a4098-4ebb-44c6-b990-1bab081f2934",
      "state": "complete"
    },
    "session": {
      "model": "default",
      "effort": "medium",
      "access": {
        "read_access": "full",
        "readable_directory": [],
        "writable_directory": [],
        "windows_sandbox": "elevated",
        "sandbox": false,
        "write_access": "off"
      },
      "background": null,
      "id": "f9ead3a6-c5dc-4c34-b8d8-bb8193be9459",
      "workspace": "/fixture/nvim.0\\qjVJBC\\0",
      "last_compaction_turn": null,
      "backend_session_id": "mock-session",
      "provider_checkpoint_id": "mock-checkpoint",
      "provider_label": "Mock backend",
      "resolved_model": "default",
      "provider_fork_state": {
        "state": "ready"
      },
      "updated_at_ms": 1793254187899,
      "shell": "system",
      "default_approval": "ask",
      "plan_approval": null,
      "created_at_ms": 1793254178327,
      "auto_compaction": {
        "enabled": false,
        "threshold_tokens": 128000
      },
      "primary_agent_id": "f9ead3a6-c5dc-4c34-b8d8-bb8193be9459:agent:primary",
      "purpose": "conversation",
      "backend": "mock",
      "active_task": null,
      "name": "Ask a fixture question.",
      "context_usage": null,
      "approval_mode": "allow",
      "native_compact": true,
      "plan_executor": {
        "enabled": false,
        "model": null,
        "effort": null
      },
      "service_tier": "default",
      "answer_timeout": {
        "unit": "m",
        "amount": 10
      },
      "plan_auto_approve_revisions": true,
      "native_fork": true,
      "plan_compact": false,
      "approval_timeout": {
        "unit": "m",
        "amount": 10
      },
      "context_window": null,
      "lease_owner": "nvim-46292-51074136430900"
    }
  }
}
```

<!-- /captured-example -->

## `session.question.skip`

Skip an explicitly targeted pending question.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The question target and ordering/operation identity.
- **Result and effects:** Returns the updated question/session state.
- **Boundary:** Skipping is an explicit answer-state mutation.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/lua/forge/command.lua:30](../../../../nvim/lua/forge/command.lua).

<!-- detailed-contract -->
### Contract

Requires `question_id`; the durable form also supplies `operation_id` and the captured [QuestionTarget](runtime-schemas.md#questiontarget). It follows [session.question.answer](#sessionquestionanswer) with response `{ "kind": "skipped" }`, returning the same receipt/elicitation shape and event. Skipping commits an explicit answer state; it does not implicitly continue the provider. A question that disallows skipping or a stale owner/revision fails.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_admission_host.lua](../../../../nvim/tests/forge/harness_admission_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 45,
  "session_id": "f9ead3a6-c5dc-4c34-b8d8-bb8193be9459",
  "params": {
    "question_id": "328087d38a78dba742351417aef632fb08147fafb65005920d46977b9ffecd15",
    "operation_id": "command:46292:51080666930800:4",
    "_command": {
      "id": "command:46292:51080666930800:4",
      "runtime_epoch": "53a51083-be7e-41ba-bee2-66727b27ff53"
    },
    "answer_order": [
      "46292:51074248819500",
      5
    ]
  },
  "method": "session.question.skip"
}
```

```json
{
  "id": 45,
  "result": {
    "agent": {
      "run": [],
      "summary": {}
    },
    "session": {
      "model": "default",
      "effort": "medium",
      "access": {
        "read_access": "full",
        "readable_directory": [],
        "writable_directory": [],
        "windows_sandbox": "elevated",
        "sandbox": false,
        "write_access": "off"
      },
      "background": null,
      "id": "f9ead3a6-c5dc-4c34-b8d8-bb8193be9459",
      "workspace": "/fixture/nvim.0\\qjVJBC\\0",
      "last_compaction_turn": null,
      "backend_session_id": "mock-session",
      "provider_checkpoint_id": "mock-checkpoint",
      "provider_label": "Mock backend",
      "resolved_model": "default",
      "provider_fork_state": {
        "state": "ready"
      },
      "updated_at_ms": 1793254184803,
      "shell": "system",
      "default_approval": "ask",
      "plan_approval": null,
      "created_at_ms": 1793254178327,
      "auto_compaction": {
        "enabled": false,
        "threshold_tokens": 128000
      },
      "primary_agent_id": "f9ead3a6-c5dc-4c34-b8d8-bb8193be9459:agent:primary",
      "purpose": "conversation",
      "backend": "mock",
      "active_task": null,
      "name": "Ask a fixture question.",
      "context_usage": null,
      "approval_mode": "allow",
      "native_compact": true,
      "plan_executor": {
        "enabled": false,
        "model": null,
        "effort": null
      },
      "service_tier": "default",
      "answer_timeout": {
        "unit": "m",
        "amount": 10
      },
      "plan_auto_approve_revisions": true,
      "native_fork": true,
      "plan_compact": false,
      "approval_timeout": {
        "unit": "m",
        "amount": 10
      },
      "context_window": null,
      "lease_owner": "nvim-46292-51074136430900"
    },
    "restore_recovery": null,
    "prompt_history": [],
    "approval": [],
    "access_abbreviation": "rx",
    "status": {
      "exchange_id": "dd0a4098-4ebb-44c6-b990-1bab081f2934",
      "kind": "awaiting_input",
      "plan_id": null,
      "owner": "interaction"
    },
    "checks_running": false,
    "active_wait": null,
    "active_check_run": null,
    "check_editing": false,
    "active_plan": null,
    "artifact": [],
    "automatic_checks": false,
    "timeline_revision": 7,
    "no_checkpoint": true,
    "session_tools": true,
    "ask_model": {},
    "capability": {
      "catalog": {
        "mcp": false,
        "skill": false,
        "live_mcp_mutation": false
      },
      "os_sandbox": false,
      "compaction": {
        "lifecycle": true,
        "metrics": true,
        "summary": true,
        "background": false
      },
      "native_compact": true,
      "agent": {
        "interrupt": true,
        "spawn": true,
        "input": true,
        "parallel": true,
        "observe": true
      },
      "native_turn_rollback": true,
      "native_goal": false,
      "native_steer": true,
      "approval_mode_list": [
        "ask",
        "deny",
        "allow"
      ],
      "background_terminals": true,
      "permission_control": true,
      "native_fork": true,
      "model_selection": true,
      "ultrafast_mode": true,
      "effort_selection": true,
      "fast_mode": true
    },
    "lsp_tools": false,
    "checks_deferred": false,
    "status_thoughts": false,
    "runtime_epoch": "53a51083-be7e-41ba-bee2-66727b27ff53",
    "subagent_tools": false,
    "snapshot_revision": 24,
    "task_operation": {
      "id": "46292:51080607241400:3",
      "action": "permission",
      "revision": 4,
      "error": null,
      "state": "completed"
    },
    "check_timing": "after_queue",
    "active_elicitation": {
      "elicitation": {
        "clarification_active": false,
        "question_set": {
          "id": "0794c3c88e22f560985e80b55660736b810e66f316f51e74a86aea617e6fd551",
          "questions": [
            {
              "id": "328087d38a78dba742351417aef632fb08147fafb65005920d46977b9ffecd15",
              "question": "Which fixture should the operation use?",
              "options": [],
              "allow_freeform": true,
              "header": "Planning feedback"
            }
          ]
        },
        "deadline": {
          "expires_at_ms": 1793254781425
        },
        "resolution_source": "user",
        "answer_order": {},
        "answer_revision": 1,
        "revision": 2,
        "answer": [
          {
            "question_id": "328087d38a78dba742351417aef632fb08147fafb65005920d46977b9ffecd15",
            "source": "user",
            "response": {
              "kind": "skipped"
            }
          }
        ],
        "current_index": 1
      },
      "exchange_id": "dd0a4098-4ebb-44c6-b990-1bab081f2934",
      "plan_id": null,
      "owner": "interaction"
    }
  }
}
```

<!-- /captured-example -->

## `session.rename`

Change a session's saved display name.

- **Context:** Lifecycle: explicit target/source where applicable.
- **Input:** The target session identity and new name.
- **Result and effects:** Returns the renamed session information.
- **Boundary:** Renaming does not change the session's stable ID.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/lua/forge/views/harness/session_name.lua:32](../../../../nvim/lua/forge/views/harness/session_name.lua).

<!-- detailed-contract -->
### Contract

Requires `session_id` (string) equal to the calling-session envelope and `name` (string). The service trims the name. Optional `expected_name` (string) fences an edit against a concurrent rename. Returns the updated [HarnessSession](runtime-schemas.md#harnesssession), including its new update timestamp. The metadata write validates the owning lease and fails on a mismatched target, lost owner, or changed expected name. It emits no provider prompt or progress. A retry without `expected_name` is a new unconditional metadata write.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_host.lua](../../../../nvim/tests/forge/harness_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 32,
  "method": "session.rename",
  "params": {
    "name": "Renamed while the provider is active",
    "_command": {
      "id": "command:46304:51091706160300:1",
      "runtime_epoch": "f163bf85-f136-46d7-aa14-8ce071f6f423"
    },
    "session_id": "7950e589-be54-4804-9e68-585010a566a5"
  },
  "session_id": "7950e589-be54-4804-9e68-585010a566a5"
}
```

```json
{
  "id": 32,
  "result": {
    "id": "7950e589-be54-4804-9e68-585010a566a5",
    "native_fork": true,
    "background": null,
    "primary_agent_id": "7950e589-be54-4804-9e68-585010a566a5:agent:primary",
    "last_compaction_turn": null,
    "backend_session_id": "mock-session",
    "service_tier": "default",
    "plan_executor": {
      "model": null,
      "enabled": false,
      "effort": null
    },
    "plan_compact": false,
    "workspace": "/fixture/nvim.0\\v3g7w8\\0",
    "created_at_ms": 1793254195509,
    "context_window": null,
    "default_approval": "ask",
    "plan_approval": null,
    "lease_owner": "nvim-46304-51091320903500",
    "name": "Renamed while the provider is active",
    "approval_mode": "ask",
    "context_usage": null,
    "shell": "system",
    "backend": "mock",
    "model": "default",
    "effort": "medium",
    "auto_compaction": {
      "threshold_tokens": 128000,
      "enabled": false
    },
    "approval_timeout": {
      "unit": "m",
      "amount": 10
    },
    "access": {
      "readable_directory": [],
      "writable_directory": [],
      "windows_sandbox": "elevated",
      "sandbox": false,
      "read_access": "full",
      "write_access": "workspace"
    },
    "updated_at_ms": 1793254195842,
    "resolved_model": "default",
    "plan_auto_approve_revisions": true,
    "provider_label": "Mock backend",
    "purpose": "conversation",
    "answer_timeout": {
      "unit": "m",
      "amount": 10
    },
    "provider_fork_state": {
      "state": "ready"
    },
    "active_task": null,
    "native_compact": true,
    "provider_checkpoint_id": "mock-checkpoint"
  }
}
```

<!-- /captured-example -->

## `session.resume`

Open an explicitly identified saved session.

- **Context:** Lifecycle: explicit target/source where applicable.
- **Input:** The target session_id and supported resume options.
- **Result and effects:** Returns the resumed session snapshot.
- **Boundary:** Unknown sessions and ownership/lease conflicts are explicit failures.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/client.lua:818](../../../../nvim/lua/forge/client.lua).

<!-- detailed-contract -->
### Contract

Requires the saved target `session_id` in parameters and explicit outer context for routing. Returns [BrokerSnapshot](runtime-schemas.md#brokersnapshot) from the target controller, resolving it from durable storage when necessary. It does not issue a provider prompt or change unrelated controllers. Unknown sessions, conflicting ownership/leases, and incompatible runtime configuration fail. The caller adopts the returned identity/epoch only if it still owns the original UI request.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_host.lua](../../../../nvim/tests/forge/harness_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 27,
  "method": "session.resume",
  "params": {
    "session_id": "7950e589-be54-4804-9e68-585010a566a5"
  },
  "session_id": "7950e589-be54-4804-9e68-585010a566a5"
}
```

```json
{
  "id": 27,
  "result": {
    "artifact": [],
    "lsp_tools": false,
    "subagent_tools": false,
    "session_tools": true,
    "ask_model": {},
    "active_elicitation": null,
    "access_abbreviation": "rxw",
    "checks_running": false,
    "agent": {
      "run": [],
      "summary": {}
    },
    "prompt_history": [
      {
        "text": "Keep this response active while sending follow-ups",
        "attachments": []
      },
      {
        "text": "literal ** prompt with native ownership",
        "attachments": []
      }
    ],
    "capability": {
      "compaction": {
        "lifecycle": true,
        "background": false,
        "metrics": true,
        "summary": true
      },
      "ultrafast_mode": true,
      "native_turn_rollback": true,
      "native_goal": false,
      "native_fork": true,
      "native_steer": true,
      "background_terminals": true,
      "agent": {
        "interrupt": true,
        "spawn": true,
        "input": true,
        "observe": true,
        "parallel": true
      },
      "model_selection": true,
      "approval_mode_list": [
        "ask",
        "deny",
        "allow"
      ],
      "effort_selection": true,
      "catalog": {
        "skill": false,
        "mcp": false,
        "live_mcp_mutation": false
      },
      "permission_control": true,
      "fast_mode": true,
      "native_compact": true,
      "os_sandbox": false
    },
    "restore_recovery": null,
    "status_thoughts": false,
    "check_editing": false,
    "check_timing": "after_queue",
    "session": {
      "id": "7950e589-be54-4804-9e68-585010a566a5",
      "native_fork": true,
      "background": null,
      "primary_agent_id": "7950e589-be54-4804-9e68-585010a566a5:agent:primary",
      "last_compaction_turn": null,
      "backend_session_id": "mock-session",
      "service_tier": "default",
      "plan_executor": {
        "model": null,
        "enabled": false,
        "effort": null
      },
      "plan_compact": false,
      "workspace": "/fixture/nvim.0\\v3g7w8\\0",
      "created_at_ms": 1793254195509,
      "context_window": null,
      "default_approval": "ask",
      "plan_approval": null,
      "lease_owner": "nvim-46304-51091320903500",
      "name": "literal ** prompt with native ",
      "approval_mode": "ask",
      "context_usage": null,
      "shell": "system",
      "backend": "mock",
      "model": "default",
      "effort": "medium",
      "auto_compaction": {
        "threshold_tokens": 128000,
        "enabled": false
      },
      "approval_timeout": {
        "unit": "m",
        "amount": 10
      },
      "access": {
        "readable_directory": [],
        "writable_directory": [],
        "windows_sandbox": "elevated",
        "sandbox": false,
        "read_access": "full",
        "write_access": "workspace"
      },
      "updated_at_ms": 1793254195719,
      "resolved_model": "default",
      "plan_auto_approve_revisions": true,
      "provider_label": "Mock backend",
      "purpose": "conversation",
      "answer_timeout": {
        "unit": "m",
        "amount": 10
      },
      "provider_fork_state": {
        "state": "ready"
      },
      "active_task": null,
      "native_compact": true,
      "provider_checkpoint_id": "mock-checkpoint"
    },
    "active_check_run": null,
    "checks_deferred": false,
    "runtime_epoch": "f163bf85-f136-46d7-aa14-8ce071f6f423",
    "task_operation": null,
    "timeline_revision": 5,
    "active_plan": null,
    "no_checkpoint": true,
    "status": {
      "started_at_ms": 1793254195790,
      "duration_ms": 0,
      "execution": null,
      "kind": "working",
      "reasoning_summary": null,
      "activity": "working"
    },
    "active_wait": null,
    "approval": [],
    "automatic_checks": false,
    "snapshot_revision": 5
  }
}
```

<!-- /captured-example -->

## `session.state.get`

Read the current session snapshot.

- **Context:** Explicit existing session.
- **Input:** An empty parameter object.
- **Result and effects:** Returns session state for reconciliation and presentation.
- **Boundary:** Do not apply a stale snapshot over newer local input.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/controller.lua:471](../../../../nvim/lua/forge/views/harness/controller.lua).

<!-- detailed-contract -->
### Contract

No parameter fields. Returns [BrokerSnapshot](runtime-schemas.md#brokersnapshot), including the session, current task/plan, capabilities, approval/question state, runtime epoch, and monotonic snapshot/timeline revisions. The skipped native `exchange` and `timeline` fields are not part of this result. Reads current in-memory admission state without starting a provider turn or taking another session's lease. No request progress is emitted. Reject snapshots from an older epoch or lower revision before updating the buffer.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [semantic_execution_host.lua](../../../../nvim/tests/forge/semantic_execution_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 13,
  "method": "session.state.get",
  "params": {},
  "session_id": "34d8be31-e867-4265-9fd8-9a0db4216c1d"
}
```

```json
{
  "id": 13,
  "result": {
    "access_abbreviation": "rxw",
    "agent": {
      "run": [],
      "summary": {}
    },
    "session_tools": true,
    "ask_model": {},
    "lsp_tools": false,
    "subagent_tools": false,
    "artifact": [
      {
        "id": "6bc6bb92-5de0-46d8-aa46-06e223e6b543",
        "created_at_ms": 1793254204688,
        "working_path": "/fixture/nvim.0/S3Olz7/1/forge/harness\\plans\\34d8be31-e867-4265-9fd8-9a0db4216c1d\\6bc6bb92-5de0-46d8-aa46-06e223e6b543\\working.md",
        "state": "generating",
        "title": "build the feature",
        "updated_at_ms": 1793254204688
      }
    ],
    "timeline_revision": 1,
    "approval": [],
    "session": {
      "id": "34d8be31-e867-4265-9fd8-9a0db4216c1d",
      "provider_fork_state": {
        "state": "ready"
      },
      "updated_at_ms": 1793254204688,
      "service_tier": "default",
      "approval_mode": "ask",
      "auto_compaction": {
        "enabled": false,
        "threshold_tokens": 128000
      },
      "purpose": "conversation",
      "native_compact": true,
      "workspace": "/fixture/nvim.0\\S3Olz7\\0",
      "native_fork": false,
      "name": "",
      "shell": "system",
      "approval_timeout": {
        "unit": "m",
        "amount": 10
      },
      "answer_timeout": {
        "unit": "m",
        "amount": 10
      },
      "access": {
        "read_access": "full",
        "write_access": "workspace",
        "sandbox": false,
        "writable_directory": [],
        "windows_sandbox": "elevated",
        "readable_directory": []
      },
      "context_usage": null,
      "resolved_model": null,
      "provider_label": "Mock CLI",
      "background": null,
      "backend": "mock",
      "context_window": null,
      "model": "default",
      "effort": "medium",
      "active_task": {
        "created_at_ms": 1793254204688,
        "kind": "plan",
        "revision": "f8fba592-5a57-45e5-b012-8da7d7a32c61",
        "plan": {
          "paused": false,
          "artifact": {
            "id": "6bc6bb92-5de0-46d8-aa46-06e223e6b543",
            "validation_warning": [],
            "generation": {
              "max_turn_count": 20,
              "consecutive_no_progress": 0,
              "max_consecutive_no_progress": 2,
              "canonical_revision": 0,
              "turn_count": 0
            },
            "review_digest": null,
            "session_id": "34d8be31-e867-4265-9fd8-9a0db4216c1d",
            "state": "generating",
            "elicitation": null,
            "request": "build the feature",
            "working_path": "/fixture/nvim.0/S3Olz7/1/forge/harness\\plans\\34d8be31-e867-4265-9fd8-9a0db4216c1d\\6bc6bb92-5de0-46d8-aa46-06e223e6b543\\working.md",
            "updated_at_ms": 1793254204688,
            "document_version": 1,
            "model_revision": 0,
            "submitted_version": null,
            "accepted_revision": null,
            "user_revision": 0,
            "manual_changes": [],
            "accepted_digest": null,
            "acceptance": null,
            "question_ledger": {
              "resolution": []
            },
            "created_at_ms": 1793254204688,
            "title": "build the feature"
          }
        },
        "updated_at_ms": 1793254204688,
        "entry": {
          "id": "7c0d16e9-ed6a-4efc-b888-ea1fce0f34b4",
          "delivery": "pending",
          "instruction": "Active declaration design:\n```json\n{\n  \"plan_id\": \"6bc6bb92-5de0-46d8-aa46-06e223e6b543\",\n  \"version\": 1,\n  \"title\": \"build the feature\",\n  \"request\": \"build the feature\",\n  \"declaration_files\": {\n    \"paths\": [\n      \"plan.json\"\n    ],\n    \"changed_paths\": []\n  },\n  \"document\": {\n    \"objective\": \"\",\n    \"requirements\": [],\n    \"background\": \"\",\n    \"decisions\": [],\n    \"design\": \"\",\n    \"flows\": [],\n    \"verification\": [],\n    \"tests\": []\n  }\n}\n```\n\nYou are planning a software change in the Harness Plan task. The effective permission for this turn governs repository access. Planning may inspect source but must not modify project files.\n\n# Design a software change through declarations\n\nDesign the final interfaces and ownership structures required by the user's request.\nHarness starts with an empty declaration design and captures each existing file's immutable\nbaseline on its first successful edit. Edit virtual\noverview files, complete configuration files, and the virtual plan.json specification, then submit them for mandatory review. Do not implement the design or\nmodify project files.\n\n## Procedure\n\n1. Inspect the request and affected source. Read implementation source to understand behavior,\n   constraints, and ownership. Reuse existing interfaces unless the change requires a new boundary.\n   For every code-change plan, identify the affected languages and inspect their installed\n   compilers or interpreters and project-selected versions. Read version configuration and use\n   read-only version queries from the project directory. Base syntax, API, and dependency choices\n   on those observed versions, even when no dependencies change. Report a missing or unresolved\n   toolchain instead of assuming a version.\n2. Use the supplied plan identity, version, and feedback context. Call `plan_read` when\n   current text, version, or additional context is needed. Omit `path` for the inventory. Supply\n   `path` and optional 1-based inclusive `start_line` and `end_line` for the affected range.\n   Set `baseline: true` to read original declarations. For an uncaptured path, Harness extracts only\n   that workspace file and returns its source digest without adding it to the saved design.\n   Read existing files before their first edit. Optional `source_digests` on a patch maps paths to\n   the returned digests and preserves inspection identities across turns. Harness also retains\n   inspected identities within the current turn. Omit displayed line-number prefixes from patches.\n3. Resolve discoverable facts from source. Ask only about material decisions source cannot answer.\n   Respect requests not to ask questions. Use `question_ask` when needed and end the turn.\n4. Design complete declaration files and required configuration changes. Preserve unchanged declarations, private members, complete\n   types, generics, visibility, documentation, and associations. Add only useful requested work.\n   Give every declaration in each source overview you author or revise an attached explanatory code comment.\n   Record objective, requirements, background, decisions, design, flows, checks, verification, and tests in virtual `plan.json`. Add usage examples only when useful.\n   Do not author JSON entities, tasks, stages, prerequisites, or execution reports.\n5. Edit with `design_patch`, supplying `plan_id`, `expected_version`, and `patch`.\n   An optional `title` names the design. Multiple files and chunks can change atomically. Use the\n   returned version in the next call. The response includes a compact applied diff. Use it to confirm\n   a focused edit without rereading the file. Read additional ranges only for missing context,\n   unexpected results, or a truncated diff. Correct every validation failure.\n   Update, Delete, and Move capture an existing source baseline only when the whole patch succeeds.\n   Add requires an absent workspace destination. Later edits use the saved proposal, including\n   retained deletion and rename state, without extracting the source again.\n6. Keep `plan.json` consistent with the final declarations. Call `plan_submit` with the exact current `plan_id` and `expected_version`.\n   Submission requests review and never authorizes implementation. If further work or user input arrives\n   in the same turn, continue reading and editing the plan, then submit its latest version again.\n   Any edit after submission requires a new successful submission before the plan is ready for review.\n\n## Dependency selection\n\nInspect the project's selected compiler or runtime, package manager, declared support range,\nmanifest constraints, and lockfile before choosing dependencies. Resolve the versions actually\nselected in the project directory. For example, check Rust toolchain files and rust-version,\nNode version files and package.json engines/packageManager, or Python requires-python and the\nproject environment. Choose the newest stable release compatible with those constraints and\nthe required features, including relevant peer and transitive dependency requirements.\n\nAnother installed version does not change the project's compatibility target. Do not switch\nor upgrade compilers, runtimes, or package managers merely to use a newer dependency. If no\ncompatible release meets the task, report the conflict and make any required environment change\nan explicit design decision.\n\nUse API documentation for the selected dependency version when designing declarations and\nexamples. Record material compatibility constraints in Background. Documentation retrieval\nfailures alone do not justify downgrading. Planning selects and proposes dependencies but does\nnot install them or run compilation checks.\n\n## Plan metadata\n\nHarness creates a virtual `plan.json` containing `objective`, optional `usage`, `requirements`,\n`background`, `decisions`, `design`, `flows`, `checks`, `verification`, and `tests`. It is plan metadata, never a project file\nor declaration overview. Read and update it through the same tools as declaration files. Do not\nmove it, delete it, or add other fields. Drafts start with empty strings and lists, with `usage` absent.\n\nWrite for an implementer who can inspect the repository and proposed declarations but has no\nconversation history. Preserve settled user requirements and consequential decisions in this metadata.\nDo not depend on phrases such as \"as discussed\" or details available only in earlier messages.\nKeep every section consistent with the final declarations and update it when review changes the plan.\n\nConsult `technical-writing` and its Architectural Overview profile before drafting or revising plan\nprose. Apply its guidance within the structure and length limits below.\n\nThe review order is Objective, Usage when present, Requirements, Background, Decisions when nonempty,\nDesign, Flows when nonempty, proposed declaration changes, Tests, and Verification. Each section has a distinct role:\n\n### Objective\n\nWrite `objective` as a short statement of the requested outcome and scope. State what the user\nneeds to accomplish or which failure needs correction. For new functionality, state the task\nwithout inventing a defect. Do not substitute an inventory of files or implementation steps.\n\n### Usage\n\nUse the optional Markdown string `usage` for representative successful use. Omit the field when\nexamples add no useful information, such as an internal refactor. An empty string or null also\nhides this section. State the relevant starting conditions, input or action, and observable result.\nDistinguish illustrative values from exact required output.\n\n- For a CLI, show the command or stdin followed by stdout, stderr, and exit status.\n- For a UI, show the starting state, user action, and visible result.\n- For an API, show the request, response, and observable side effects.\n- For a library, show the call and preconditions, then its return value, mutation, or error.\n- For a background process, show the trigger and eventual observable result.\n- For a data migration, show representative data before and after the migration.\n\n### Requirements\n\nIdentify the 2–4 key requirements: requested outcomes and essential acceptance constraints.\nWrite them as a JSON array of nonempty strings. For bug fixes, this section can be omitted by\nleaving `requirements` empty when the objective and design already capture the acceptance criteria.\n\nStart each entry with a short, bold sentence stating an observable guarantee. Add one plain-text\nsentence only when it supplies necessary conditions or scope. Store the Markdown bold markers\ninside the requirement string. The bold sentence must communicate the guarantee on its own.\nUse direct behavior statements rather than repeating a feature name with \"Provide\", \"Support\",\nor \"Implement\". Do not repeat the objective or explain why a design choice was selected.\n\nThese examples show the format. Derive each guarantee from the current task rather than copying\nan unrelated example:\n\n- **Cancellation preserves saved data.** Cancelling before publication leaves previously saved\n  records unchanged.\n- **Failed imports leave stored records unchanged.** Report invalid rows without committing\n  any part of the import.\n- **Existing callers remain compatible.** Preserve public signatures and documented error\n  behavior.\n\nKeep entries at a consistent level of abstraction. Group closely related behaviors without\ncombining unrelated requirements merely to reduce the count. Put ownership and lifecycle mechanisms\nin Design, exact defaults and local behavioral contracts in declarations, consequential choices in\nDecisions, and individual test scenarios in Tests. Preserve an exact API, dependency, or mechanism\nhere when the user explicitly requires it. Retain acceptance-critical error, recovery, compatibility,\nand scope constraints.\n\nRequirements describe properties of the delivered change. Preserve task-specific constraints such\nas compatibility, offline operation, or protecting user configuration. Do not promote every design\nchoice into a mandatory requirement or create implementation milestones. Keep routine validation\nand implementation safeguards in the relevant Design or declaration unless they are explicitly\nrequested or essential to a task-specific acceptance constraint. Do not invent numeric limits or\nadditional product requirements. Ground any required limit in the user's request or a verified\nconstraint.\n\nWhen following an existing rule requires concrete work, plan the resulting change instead of\nrepeating the rule. Include supported file changes in the proposed files and put exact verification\ncommands and observable manual checks in Verification. Mention a generated artifact or unsupported\nfile change in Design only when its specific contents are essential to the requested change and are\nnot represented elsewhere, using a brief sentence alongside its owning component. A routine README\ncontents list does not explain the design and belongs outside this section. Do not invent\nplaceholder generated contents or add a separate file inventory.\n\nWhen the change introduces generated files, inspect the project's existing ignore rules and\npropose any needed .gitignore addition or update as a normal virtual file change. Preserve existing\ncomments, negations, and unrelated rules. Add appropriate generated paths, such as /target/ for a\nroot Rust package. Do not propose an empty file merely because .gitignore is absent. Keep source\nand required lockfiles tracked. Generate or update required lockfiles during implementation, not\nin Plan. Routine lockfile handling belongs to implementation and needs no Design prose. The model\ncreates or updates the actual file during implementation. Do not modify workspace files in Plan.\n\n### Background\n\nWrite `background` as Markdown explaining the existing system directly relevant to this task.\nIdentify inspected components using repository-relative paths and symbol names. Explain what each\nowns, how the affected operation currently flows between them, and the integration points for this\nchange. Include existing behavior that must be understood or preserved, such as persistence,\nerror propagation, and platform limits. Explain the current limitation when applicable.\n\nDescribe established starting conditions and explicitly identify material unresolved constraints.\nOmit discovery chronology, tool failures, and evidence-gathering details unless they leave an\nuncertainty that changes implementation or verification. Include only facts needed to understand\nor implement this change. Do not give a repository tour, summarize the conversation, repeat the\nrequested behavior, or describe the proposed solution. For a new project, briefly describe the\nstarting repository, available infrastructure, and relevant conventions.\n\n### Decisions\n\nWrite `decisions` as a JSON array of objects with nonempty `decision` and `rationale` strings.\n\nRecord consequential choices that resolve questions left open by the task. The answer may come\nfrom asking the user or from your own planning. Include choices where a different answer would\nmaterially change the behavior, scope, or integration contract.\n\nIn `decision`, state the selected answer directly. In `rationale`, explain why it fits this task\nand any consequence or tradeoff the implementer must preserve. Record the settled choice, not\nthe conversation or deliberation that produced it.\n\nA choice resolved through user clarification belongs here. Requirements already fixed by the\noriginal request do not need repeating. Omit solution summaries, component inventories, routine\nconventions, and dependency selections without a task-specific tradeoff. An empty list is valid.\n\nThese examples show rendered decisions. Store each bold sentence as `decision` without the bold\nmarkers and its explanation as `rationale`. Derive the actual choices from the current task rather\nthan copying these examples:\n\n- **Update matching records and create unmatched records.** Users can import revised CSV files\n  to maintain existing data instead of creating duplicates.\n- **Reject the entire import if any row is invalid.** Report row-specific errors and leave stored\n  data unchanged, so users can correct and retry the file without reconciling a partial import.\n- **Resume interrupted jobs from the last completed item.** Completed work remains recorded, and\n  restarting retries only unfinished items. This requires durable progress tracking.\n- **Pause synchronization when both copies of a file have changed.** Preserve both versions and\n  ask the user to resolve the conflict, preventing automatic synchronization from discarding an edit.\n- **Preserve the public API during the internal refactor.** Existing callers can adopt the change\n  without modifying their code. Public API cleanup remains outside this refactor.\n- **Generate reports as background jobs.** Large reports can finish after the user leaves the page.\n  Users receive a completion notification and download the result when it is ready.\n\n### Design\n\nWrite `design` as Markdown explaining how the proposed solution works and satisfies the requirements.\nUse an unheaded opening paragraph followed by `### Ownership`. Store flows separately in `flows`.\nUse this template, replacing the bracketed instructions with task-specific content:\n\n```markdown\n[One paragraph of 2–3 sentences: describe the concrete solution, explain the main structural choice and its reason, then introduce the main objects and their roles.]\n\n### Ownership\n[One paragraph of 1–2 sentences per main owner: its responsibility, the meaning of its owned state, and its boundary with other owners. Define supporting code names as they appear.]\n```\n\n**Opening.** Write one paragraph of 2–3 sentences explaining how the design solves the user's task.\nFollow this progression:\n\n1. Concrete solution: state what will be implemented or changed and its defining behavior.\n   Describe the proposed solution, not just the objective.\n2. Structural choice and reason: explain the main organizing choice and one concrete capability\n   or guarantee it provides.\n3. Main objects: introduce the principal components by their actual code names and briefly\n   explain their roles.\n\nCombine the structural reason and component introduction when two sentences suffice. Expand\nresponsibilities in Ownership and interactions in Flows. Keep configuration values, detailed\nalgorithms, and secondary components out of the opening.\n\nThe examples below demonstrate structure and level of detail. Derive the solution, reasoning,\nand component names from the current task and design. Do not copy the examples' implementation\nchoices. The generated opening is one paragraph without the example labels.\n\n**Feature: CSV import**\n\nSolution: Implement CSV import as a workflow that previews incoming records, reports validation\nerrors, and saves the import after confirmation.\n\nReason: Separate validation from persistence so users can correct invalid records before\nchanging stored data.\n\nObjects: `ImportSession` holds the pending records and validation results, while `ImportService`\nvalidates the data and commits the confirmed import.\n\n**Feature: log-search CLI**\n\nSolution: Implement log search as a command that filters files by time range and severity, then\nprints matching records.\n\nReason: Process files incrementally so memory use does not grow with the total size of the\ninput files.\n\nObjects: `SearchCommand` handles arguments, `LogScanner` reads and filters records, and\n`MatchWriter` formats the output.\n\n**Bugfix: cancellation leaves the interface stuck**\n\nSolution: Make cancelling a request stop its active work and return the interface to idle.\n\nReason: Associate callbacks with a request generation so results arriving after cancellation\ncannot restore stale progress.\n\nObjects: `RequestController` owns the active generation and cancellation handle, while `RunState`\nrecords the status displayed by the interface.\n\n**Refactor: duplicated authentication**\n\nSolution: Move authentication from individual route handlers into shared middleware while\npreserving existing responses.\n\nReason: Centralize credential validation so every protected route applies the same expiry and\nrevocation checks.\n\nObjects: `AuthMiddleware` gates protected requests, `TokenVerifier` validates credentials, and\n`RequestIdentity` carries the authenticated user to the handler.\n\n**Bugfix: interrupted saves corrupt settings**\n\nSolution: Preserve the last complete settings file when saving is interrupted or fails.\n\nReason: Write the replacement to a temporary file and publish it atomically so readers never\nobserve partially written settings.\n\nObjects: `SettingsStore` owns serialization and loading, while `AtomicFileWriter` manages the\ntemporary file and final replacement.\n\n**Ownership.** Expand the owners introduced in the opening, in the same order, using one paragraph\nof 1–2 sentences per owner. State what each owner creates, changes, or retains, and where its authority\nends. Introduce supporting types with their meaning, such as \"`ImportSession`, the pending records\nand validation results\", rather than listing names after \"owns\". Group supporting modules and data under that\nowner. Use connected prose, not bullets, file inventories, or paragraphs for every type.\nKeep thin composition wrappers in the opening unless they own a separate lifecycle. Begin Ownership\nwith the component that controls the central state, not its configuration or launcher. Configuration\nfields, numeric defaults, key bindings, visual styling, algorithms, and schedule-by-schedule details\nbelong in the relevant declarations. Include a detail here only when changing it changes the\nownership boundary. Do not add README contents, documentation chores, or repeated setup descriptions.\n\nDescribe the scope of the change, not the architecture of the entire system. For bugfixes, explain\nthe invariant being restored and the mechanism that restores it. Describe only the existing owners\nand interactions relevant to the fix. A single ownership paragraph and one flow are sufficient\nfor a localized change, and an existing function or module can be the owner. Do not invent new\ncomponents to fill the template.\n\nDescribe the target design in present tense. Preserve terminology across sections, remove repeated\nclaims, and demonstrate guarantees through concrete mechanisms. Keep detailed contracts in\ndeclarations, detailed tradeoffs in Decisions, and existing behavior in Background. Flows retain the ordering\nand failure rules needed to explain the result, without repeating local algorithms or configuration\nvalues. Execution instructions remain governed by the active system and repository instructions.\nThe complete plan must support implementation without the prior conversation.\n\nOnly the opening and Ownership belong in the `design` string. Omit a Files\nand artifacts section, repeated file inventories, routine housekeeping, and explanations of Harness\nformat limitations. Do not include executable implementation bodies, mutable execution progress,\nor claims that implementation or verification has finished.\n\nBefore submission, review the Design against this template. Confirm that the opening describes the\nsolution, explains its structure, and introduces the main objects. Define unexplained code names\nand move configuration or implementation detail out of Ownership. Confirm that every flow arrow\nrepresents production, transformation, storage, or consumption through the described objects.\nShorten any ownership paragraph exceeding two sentences instead of joining unrelated clauses.\nKeep acceptance-critical behavior in the plan, placing each detail in its designated section.\n\n### Flows\n\nWrite `flows` as an array of `{title, description, trigger, steps}` objects, or [] when no runtime\nflow adds useful information. Harness generates the diagram after Design. Do not put a Flows\nheading or hand-formatted arrows in `design` or flow labels.\n\nEach flow describes one execution started by one explicit event. Use a concise description for\nits purpose and ordering guarantees. Separate events into separate flows even when they share\nstate: collecting a shield stores charges, while a later enemy contact reads those charges.\nDo not connect these actions into one execution merely because one uses the other's output.\n\n- `trigger` names the event that starts this execution.\n- `steps` is an ordered array. Each step has a required `action` naming a function or concrete\n  operation, optional `effects`, and optional `branches`.\n- Each effect has exactly `target` and `change`. It is terminal data describing an observable\n  change, such as create, update, remove, or preserve. Effects cannot have steps, actions,\n  children, branches, or triggers. Put subsequent execution in the enclosing `steps` array.\n- Each branch has a required `condition` and a nonempty `steps` array. The condition is evaluated\n  during the current execution. A later event requires a new flow, not a conditional branch.\n  Each applicable branch runs its steps before the next enclosing step. When alternatives are\n  exclusive, write mutually exclusive conditions explicitly. Branches do not mean parallel work.\n\nUse descriptive labels and the exact declaration names where available. Harness normalizes label\nwhitespace for display while retaining the original content. Keep resulting\nstate in `effects`, rather than treating data as executable steps. Express conditions that govern\nexecution in branch conditions or action labels instead of adding separate input inventories.\nFlow titles must be unique. There are no total flow, step-count, or nesting-depth limits.\nKeep diagrams focused for readability without omitting meaningful execution or effects.\n\nFor example, collection and contact are independent triggers:\n\n```json\n[\n  {\n    \"title\": \"Collect a shield\",\n    \"description\": \"Collection stores protection for a later accepted hit.\",\n    \"trigger\": \"player overlaps a shield pickup\",\n    \"steps\": [{\n      \"action\": \"collect_shield_pickups\",\n      \"effects\": [{\"target\": \"PlayerAbilities\", \"change\": \"grant shield charges to\"}]\n    }]\n  },\n  {\n    \"title\": \"Absorb contact\",\n    \"description\": \"An accepted contact consumes available protection.\",\n    \"trigger\": \"accepted enemy contact\",\n    \"steps\": [{\n      \"action\": \"PlayerAbilities.take_hit\",\n      \"branches\": [{\n        \"condition\": \"shield charges remain\",\n        \"steps\": [{\"action\": \"absorb hit\", \"effects\": [\n          {\"target\": \"shield charge\", \"change\": \"consume one\"},\n          {\"target\": \"contact immunity\", \"change\": \"start\"}\n        ]}]\n      }]\n    }]\n  }\n]\n```\n\nThe renderer uses `→` for the first action, `↓` for subsequent execution, and `┄▶` for effects.\nConditions introduce branch steps. Effects never acquire execution arrows.\n\nUse Markdown inline code for code identifiers, concrete paths, and commands in prose fields.\nEncode paragraph breaks as `\\n\\n` inside JSON strings.\nThe required tests array inventories every test involved in the plan, grouped by project-relative\nfile. Each cases entry names the test (module-qualified when needed), its change (new, modified,\nremoved, or reused), and a description of the scenario and expected result or removal rationale.\nInclude inline test modules and separate test files consistently. List existing coverage being\nreused, omit unrelated repository tests, and use [] when no tests apply. Keep names unique within\na file and list each file once. Native Cargo case checks require the full runtime test name.\nUse the optional target field (lib, bin:<name>, or test:<name>) when a file belongs to multiple\nCargo test targets. Tests is rendered\nbefore Verification and records planned coverage. Verification retains execution commands and manual checks.\nObjective, Background, and Design must be nonempty before submission.\n\nA plan's metadata can use this shape:\n\n```json\n{\n  \"objective\": \"Support observable, cancellable texture replacement.\",\n  \"usage\": \"Start a replacement, cancel it before publication, and confirm the existing texture remains visible.\",\n  \"requirements\": [\n    \"**Cancellation preserves the current texture.** Cancelling before publication leaves the visible texture unchanged.\",\n    \"**Submitted frames retain valid textures.** Allocations remain valid until every submitted frame using them completes.\"\n  ],\n  \"background\": \"The existing renderer obtains published handles from `TextureRegistry`. Submitted frames can outlive the frame in which a replacement is requested.\",\n  \"decisions\": [\n    {\n      \"decision\": \"Publish replacements at frame boundaries.\",\n      \"rationale\": \"A single publication point keeps each submitted frame's texture selection consistent.\"\n    }\n  ],\n  \"design\": \"Support texture replacement that reports progress and can be cancelled while the current texture remains visible. Separate preparation from publication so pending work leaves the current texture usable. `TextureStreaming`, the preparation service, passes completed replacements to `TextureRegistry`, the owner of the handles used by the renderer.\\n\\n### Ownership\\n`TextureStreaming` creates and prepares each replacement, exposing its progress and cancellation through `TextureRequest`, the caller's handle to pending work. Preparation leaves the published texture unchanged.\\n\\n`TextureRegistry` controls which texture the renderer can use and retains replaced allocations while submitted frames still reference them. Only the registry publishes or retires a texture.\",\n  \"flows\": [\n    {\n      \"title\": \"Publish a replacement\",\n      \"description\": \"Publication at a frame boundary selects the prepared texture for rendering.\",\n      \"trigger\": \"prepared replacement is ready at a frame boundary\",\n      \"steps\": [\n        {\n          \"action\": \"TextureRegistry.publish\",\n          \"effects\": [\n            {\n              \"target\": \"published handle\",\n              \"change\": \"replace\"\n            }\n          ]\n        },\n        {\n          \"action\": \"Renderer\"\n        }\n      ]\n    },\n    {\n      \"title\": \"Retire an old allocation\",\n      \"description\": \"The replaced allocation remains alive until its last submitted frame completes.\",\n      \"trigger\": \"last submitted frame using the old allocation completes\",\n      \"steps\": [\n        {\n          \"action\": \"TextureRegistry.retire\",\n          \"effects\": [\n            {\n              \"target\": \"replaced allocation\",\n              \"change\": \"release\"\n            }\n          ]\n        }\n      ]\n    },\n    {\n      \"title\": \"Cancel preparation\",\n      \"description\": \"Cancellation releases only the unpublished replacement.\",\n      \"trigger\": \"cancellation requested\",\n      \"steps\": [\n        {\n          \"action\": \"TextureRequest.cancel\"\n        },\n        {\n          \"action\": \"TextureStreaming.discard\",\n          \"effects\": [\n            {\n              \"target\": \"unpublished allocation\",\n              \"change\": \"release\"\n            }\n          ]\n        }\n      ]\n    }\n  ],\n  \"verification\": [\"Cancel a pending replacement and confirm the current texture remains visible.\", \"Replace a texture with frames in flight and confirm rendering remains valid.\"],\n  \"tests\": [\n    {\n      \"file\": \"src/texture.rs\",\n      \"cases\": [\n        {\n          \"name\": \"tests::texture_replacement_cancellation_preserves_published_handle\",\n          \"change\": \"new\",\n          \"description\": \"Cancel before publication and verify the original texture handle remains published.\"\n        },\n        {\n          \"name\": \"tests::texture_replacement_retains_in_flight_allocation\",\n          \"change\": \"new\",\n          \"description\": \"Replace a texture with frames in flight and verify its allocation remains valid until their completion.\"\n        }\n      ]\n    }\n  ]\n}\n```\n\nThis example illustrates the schema. Use inspected project facts and actual supported commands\nfor the real plan. Read the saved text before patching when it is not already available. Metadata\nand declaration edits can share one atomic patch:\n\n```text\n*** Begin Patch\n*** Update File: plan.json\n@@\n-  \"objective\": \"\",\n+  \"objective\": \"Support observable, cancellable texture requests.\",\n+  \"usage\": \"Start a replacement, cancel it before publication, and confirm the existing texture remains visible.\",\n*** End Patch\n```\n\n### Checks and verification\n\nAutomated check definitions belong to the project's saved `.forge.json`. Harness configures missing or invalid checks before starting this planning turn. Read the saved configuration through `check_definition_list`. An explicitly empty configuration is valid. If the configuration becomes missing or invalid during planning, report the submission failure and use the harness-checks skill to repair it with check-editing permission. Test inventory describes intended coverage independently of commands. Applicable enabled checks with `auto:true` are included automatically. Use `plan_checks_add` with the current plan ID, version, and check IDs to add other enabled checks explicitly. The `checks` array in virtual `plan.json` stores those references, not commands. Never run validation directly through shell tools while planning or implementing. During implementation and resolution, finish the phase and let Harness Checking run validation.\n\nHarness executes these commands locally, outside the model provider sandbox, after Implement and Resolve. Harness records exit status, duration, and saved stdout/stderr and checks declaration conformance. The model must not execute automated checks during Implement, Resolve, or manual Verify.\n\nRecord manual actions in `verification`, an array of actions and expected observations. Identify actions requiring a person or unavailable hardware. Manual Verify starts only after declarations and automated checks pass. Planning records requirements, not results. Execution state and results remain Harness-owned.\n\n## Overview syntax\n\nPaths match project-relative source paths. Supported extensions are `.rs`, `.ts`, `.tsx`, `.lua`, and\n`.toml`, `.json`, `.jsonc`, `.yaml`, `.yml`, and XML configuration paths (including `.xml`,\n`.csproj`, `.fsproj`, `.vbproj`, `.props`, `.targets`, `.resx`, and `.plist`), plus `.gitignore` files whose contents are preserved verbatim. Source overviews retain native layouts but are not compilable implementation files. Never write\nexecutable function bodies, empty or placeholder bodies, or executable initializers. Academic\npseudocode belongs only in an indented `Algorithm` block, never in a native-language body. Preserve\nreference qualifiers, generic arguments, and complete return types. Do not infer missing types.\n\nPreserve syntactic nesting and attach documentation to its declaration. Harness owns presentation\nformatting, indentation style, blank-line spacing, column widths, and line wrapping. Do not manually\nwrap content or shorten names, comments, commands, or diagrams to satisfy a display width.\nHarness formats each requested workspace overview and captured baseline, then canonicalizes both\ndeclaration snapshots on submission. The repository line width is a formatting preference, not a\nsubmission constraint. Literals, code examples, and indivisible types remain intact. Read virtual\nfile ranges when their exact current text is unavailable and match that text, including indentation.\nDraft patches retain their layout until submission. Submission can reformat declarations, so read\naffected ranges before another revision unless current formatted text is already supplied in feedback.\n\n## Function algorithms, changes, and references\n\nCategorize callable behavior against the captured source baseline, including after resubmission:\n\n- A new function or method requires one nonempty `Algorithm` block describing its complete behavior\n  in academic pseudocode. A `Change` block does not satisfy this requirement.\n- A modified function or method uses one `Change` block, preferably one short sentence: state\n  the concrete problem or purpose, then the behavioral change that addresses it. Ground the reason\n  in the task or inspected baseline, without inventing a defect. Use a second short sentence only\n  when essential. Omit unchanged behavior and implementation detail that does not explain the delta.\n  Include internal changes even when its signature and reference lists remain identical. Do not\n  restate the complete algorithm or use `Algorithm` for an existing callable. Unmodified callables\n  do not need a `Change` block.\n\nPlace the heading at the signature's indentation and its content one level deeper, before `Calls`,\n`Constructs`, and `Accesses`, in that order. Never combine `Algorithm` and `Change` on one callable.\nPartial drafts may omit a new algorithm, but submission rejects that omission. Preserve an authored\nblock unless its intended behavior changes. Removing `Change` withdraws that delta. Configuration\nand non-callable declarations do not have behavior blocks. Review folds both categories with their\ncallable and never appends `[changed]` to a collapsed signature.\n\n### Academic pseudocode\n\nRead and apply the `academic-pseudocode` skill before authoring `Algorithm` blocks. Its local\n`SKILL.md` contains the complete writing rules. Keep algorithms as high-level and compact as\npossible while preserving the complete behavioral contract.\n\nFor this plan, keep domain identifiers searchable and preserve numeric bounds, tie-breaking,\nsaturation, ownership, and deferred effects explicitly. Function signatures already declare inputs\nand results, so do not repeat them as boilerplate in each `Algorithm` block.\n\nAlgorithm submission validates statement syntax and indentation with Forge's pseudocode parser.\nUse `target ← expression` for assignment, `if condition`, `otherwise if condition`, `otherwise`,\n`for each item in collection`, and `while condition`. Every control statement needs a nonempty\nbody indented with spaces. A trailing colon is optional. Keep each statement on its own line,\nalign siblings, and dedent to an existing enclosing level. Use `return` with an optional result.\nUse `break` and `continue` only inside loops. Expressions and operation statements may use concise\nnatural language, identifiers, numbers, quoted strings, parentheses, and brackets. Use `=` for\nequality and `≠`, `≤`, or `≥` for comparisons. Omit native-language braces and semicolons.\nIncomplete drafts remain editable. Repair the reported callable, line, and column before resubmitting.\n\nKeep explicit field references consistent with `Accesses`: `Projectile.previous_position` in\nan Algorithm requires `Projectile::previous_position` in Rust's `Accesses` section. Use signature\nbindings for instance receivers when available. Qualified fields must name real declared members.\n\nMake construction explicit as `Type(field = value, ...)`, or `Type()` for a zero-field marker.\nUse the exact qualified name from `Constructs`, including factory names such as\n`Store::new(config = config)`. Every target listed in `Constructs` for an Algorithm must have\na matching invocation with named arguments or no arguments. Construct values before describing\nthe operation that queues, spawns, inserts, or publishes them. Reused shared assets are not new\nconstructions. Ordinary helper calls may use positional arguments. Do not hide constructed values\nand their initial fields in prose such as \"queue a shot with damage\".\n\nSource inspection extracts function-body references alongside declarations in Rust, TypeScript,\nTSX, and Lua. Use these sections, with one qualified target per line and no arguments:\n\n- `Calls` records invocations other than construction and callable values, including callback\n  registration. A callable passed as a callback remains here even when it is a factory.\n- `Constructs` records types instantiated directly, such as `Weapon` for `Weapon { ... }` or\n  `new Weapon(...)`, and factory calls whose declared return is an owned named type. List the\n  callable itself, such as `Weapon::new` or `Weapon::default` returning `Self` or `Weapon`.\n  Use the return type, not a method-name convention. Borrowed, scalar, and unit returns stay in\n  `Calls`. Rust tuple and unit constructors belong here when their type is resolved.\n  Do not list a type here merely because it appears in a signature, type annotation, generic\n  argument, cast, or destructuring pattern.\n- `Accesses` records field reads, writes, references, and named value uses. A struct initializer\n  such as `Weapon { cooldown_remaining: 0.0 }` records `Weapon` under `Constructs` and\n  `Weapon::cooldown_remaining` under `Accesses`. Destructured fields also belong in `Accesses`.\n\nPreserve occurrence order and repetitions within each category. Parsing retains captured\ninterleaving and semantic kinds in saved data. Unknown or computed receivers remain unresolved\nand must not be matched to a same-named property without type evidence.\n\nEvery newly declared internal symbol needs a resolved incoming use before submission. Include each registered system or callback in the registering function's Calls, even when the framework invokes it later. Include new property and value uses in Accesses and new type uses in signatures or Constructs. Imports, declaration names, owner headers, prose, and self references alone do not count. Truly public APIs, recognized executable entry points, tests, and trait contract items need no in-plan caller. Public exposure includes module and member accessibility and public re-exports, so pub(crate) and pub items hidden in private modules remain internal. The rule applies to new symbols relative to the captured source baseline, including resubmission, while partial draft edits remain permitted.\nThe virtual file includes `Calls`, `Constructs`, and `Accesses` blocks after each captured callable signature.\nEdit it atomically with declarations through `design_patch`. Include names only, without\narguments, assignments, control flow, or other statements. Qualify methods with a receiver type only\nwhen source evidence identifies that type. Keep unresolved receiver names when evidence is absent.\n\nKeep call occurrences in extraction or authored order, including repetitions. Review deduplicates\ntargets while retaining parsed order by default. Preserve unchanged Calls blocks when editing\nsignatures. Update an owner's list when relationships change and include a block for a newly planned\ncallable when its relationships are known. An empty block states that the callable has no calls.\nAn absent block states that call information is unavailable. Do not manufacture missing information\nfor historical plans or configuration documents.\n\nThe structured list follows the signature at the same indentation level. Target names use one\nadditional indentation level:\n\n```text\npub fn dispatch(client: &Client);\nChange\n  Avoid futile retries of rejected credentials by stopping on authentication\n  failure and recording the final attempt.\nCalls\n  validate_policy\n  Client::send\n  record_metrics\n```\n\nA newly introduced callable instead has an algorithm, with reference evidence listed separately:\n\n```text\npub fn next_attempt(attempt: u32, limit: u32) -> Option<u32>;\nAlgorithm\n  if attempt ≥ limit\n    return none\n  return attempt + 1\n```\n\n## Declaration comments\n\nEvery declaration in a source overview you author or revise must have an attached explanatory\ncode comment, including private declarations, types, fields, enum variants, traits or interfaces,\nimplementation blocks, functions, methods, aliases, and module-level bindings. Preserve accurate\nexisting comments and add missing ones. Imports, attributes, parameters, and configuration entries\ndo not need separate declaration comments.\n\nStart newly added or edited comments with `Get` instead of `Returns`. Submission rejects comments\nwhose first word is `Returns`, ignoring case. The rule applies only to the start of the complete\ncomment, not later sentences or continuation lines. Unchanged captured comments remain valid.\n\nFollow the repository's code-comment instructions and, when available, the technical-writing\nskill's Code Comments and API Documentation profiles. Reuse instructions already loaded in context. This planning requirement makes declaration\ncomments mandatory even when general source-comment guidance permits omission.\n\nExplain the declaration's role, ownership boundary, or behavioral contract. For fields and\nvariants, explain their domain meaning or represented condition. For callables, explain the\nobservable result, mutation, or failure condition the signature cannot convey. Include relevant\nbounds and lifecycle or ordering constraints. Use only contracts supported by the inspected source\nor settled proposal. Do not invent behavior or narrate the signature, such as \"stores a value\"\nor \"gets the current texture.\" A comment on a container does not replace comments on its members.\n\nUse native comment syntax, such as Rust `///`, TypeScript JSDoc, and Lua `---` or `---@` annotations.\nKeep comments immediately attached to declarations and above their attributes. Before submission,\nread the edited overviews and check that every declaration has a useful comment consistent with\nthe final design. Comments describe the design without adding function bodies or pseudocode.\n\nRust retains structs, fields, enums, traits, aliases, imports, attributes, and `impl` blocks.\nTerminate callable signatures with a semicolon, including methods:\n\n```rust\n/// Publishes uploaded replacements while retaining textures still used by submitted frames.\npub struct TextureRegistry {\n  /// Published texture available to new frame submissions, if one has been installed.\n  current: Option<TextureHandle>,\n}\n\n/// Exposes the published texture and the frame-boundary replacement operation.\nimpl TextureRegistry {\n  /// Get the published handle without changing the active texture version.\n  pub fn current(&self) -> Option<TextureHandle>;\n\n  /// Installs a completed upload at a frame boundary and retains the previous allocation until its final GPU use.\n  pub fn publish(&mut self, replacement: TextureHandle);\n}\n```\n\nTypeScript retains classes, interfaces, aliases, fields, exports, and complete signatures:\n\n```typescript\n/** Provides asynchronous access to texture handles by their stable identifier. */\nexport interface TextureStore {\n  /** Resolves a usable handle or rejects when the texture cannot be loaded. */\n  get(id: string): Promise<TextureHandle>;\n}\n\n/** Resolves texture handles through the configured storage provider. */\nexport class TextureRegistry {\n  /** Provider used to resolve handles without exposing storage access to callers. */\n  private store: TextureStore;\n\n  /** Resolves the requested handle and propagates provider failures. */\n  get(id: string): Promise<TextureHandle>;\n}\n```\n\nArrow bindings retain their native header through `=>`, with no expression or body:\n\n```typescript\n/** Starts an observable texture request using the caller's identifier. */\nexport const request = <T>(id: T): TextureRequest =>;\n```\n\nKeep an absent return annotation absent. Lua table members appear as dotted bindings and named\nfunction headers beneath their module binding.\n\nLua retains bindings, named function headers, module returns, and declaration annotations.\nDo not add function-closing `end` to signature-only headers:\n\n```lua\n---Exposes access to the currently published texture handles.\nlocal M\n\n---Get the published handle for the requested texture.\n---@param id string\n---@return TextureHandle\nfunction M.get(id)\n\nreturn M\n```\n\n## Manifests and configuration\n\nJSON, JSONC, TOML, YAML, XML, and .gitignore files retain their complete contents, including values, comments\nwhere allowed, arrays, attributes, and nested structures. These are editable configuration proposals\nrather than abbreviated declarations. Include every affected manifest and configuration file when\nchanging dependencies, features, scripts, package settings, workspace members, or build targets.\nThis includes `Cargo.toml`, `package.json`, `tsconfig.json`, CI YAML, and XML project files as needed.\nKeep unaffected settings and include affected nested package manifests. Do not omit configuration\nchanges because the design focuses on source declarations.\n\nRead the existing virtual file and patch it with the same Add File, Update File, Delete File, and\nMove to operations. Use valid syntax and preserve exact string values. Source restrictions on\ninitializers do not apply to configuration. Harness validates configuration and preserves its text\nwithout applying declaration indentation or prose wrapping. Configuration remains visible in\npublic-only inspection. Do not write actual project files during planning.\n\nUse strict JSON for `package.json` and ordinary `.json` files. `.jsonc`, `tsconfig*.json`,\n`jsconfig*.json`, and JSON files beneath `.vscode/` allow comments and trailing commas, but require\nquoted property names and commas between values. YAML retains block scalars and multiple documents.\nXML retains elements, attributes, namespaces, comments, and CDATA. XML admission checks well-formedness,\nnot application schemas, and never fetches external DTDs. TOML admission rejects duplicate keys.\nThe root virtual `plan.json` remains reserved for plan metadata, even if the checkout contains a\nproject file with that name. Other nested `plan.json` paths are ordinary JSON proposals.\n\nFor example, a proposed dependency section retains its complete settings:\n\n```toml\n[dependencies]\nengine = { version = \"1.2\", default-features = false, features = [\"render\"] }\n```\n\n## Context-based file patches\n\nPatch paths are virtual proposed files, not paths beneath a real `design/` directory. Add File\ncreates an overview, Delete File proposes source removal, and Move to follows Update File to\npropose a rename. `@@` starts a chunk. Context lines start with a space, removals with `-`, and\nadditions with `+`:\n\n```text\n*** Begin Patch\n*** Update File: src/textures.rs\n@@\n-  pub fn request(texture: TextureId) -> TextureHandle;\n+  pub fn request(texture: TextureId) -> TextureRequest;\n@@\n   pub fn status(request: RequestId) -> RequestStatus;\n+  /// Cancels a pending request and reports whether cancellation was accepted.\n+  pub fn cancel(request: RequestId) -> bool;\n*** End Patch\n```\n\nInvalid patches change no files and do not increment the version. No-op patches retain the current\nversion. Changed patches increment it. Correct the reported syntax,\ncontext, or version and retry. Never bypass validation with ordinary filesystem tools.\n\n## Review revisions and boundaries\n\nReview feedback shows file excerpts with one line number per row. `-` marks removed baseline text,\n`+` marks proposed additions, and unmarked rows provide context. Removed rows use baseline line\nnumbers. Other rows use proposed line numbers. Comments follow each excerpt as `8: comment` or\n`8–12: comment`. Replacements can share a line number. These excerpts are review context, not patch\ninput. Resolve each comment against the saved design and preserve its intended context.\nRead current proposed files before revising them. Preserve useful decisions and submit the new version.\n\nUnsupported file formats cannot be edited through declaration controls. Preserve essential changes\nto those files briefly alongside their owning component in Design when not represented elsewhere,\nwithout explaining the format limitation or inventing a supported-file substitute. Planning never writes\nimplementation bodies or performs execution. For a function behavior change, retain the signature\nwhen it remains valid and add or update its Change summary in the virtual declaration. Explain the\nbehavior in Design and include its coverage in Tests. Never manufacture an interface change to make\na diff appear. Declaration conformance does not prove runtime behavior.\n\nIf affected workspace source changed since extraction, report the need for a fresh baseline.\nDo not silently replace the baseline or modify project source.\n\n\nUser request:\nbuild the feature"
        }
      },
      "plan_auto_approve_revisions": false,
      "plan_compact": false,
      "last_compaction_turn": null,
      "default_approval": "ask",
      "plan_approval": null,
      "created_at_ms": 1793254204592,
      "lease_owner": "nvim-42040-51100399821900",
      "backend_session_id": null,
      "provider_checkpoint_id": null,
      "primary_agent_id": "34d8be31-e867-4265-9fd8-9a0db4216c1d:agent:primary",
      "plan_executor": {
        "enabled": false,
        "model": null,
        "effort": null
      }
    },
    "no_checkpoint": false,
    "check_editing": false,
    "active_wait": null,
    "task_operation": {
      "id": "42040:51100545358200:1",
      "action": "plan",
      "revision": 1,
      "error": null,
      "state": "completed"
    },
    "check_timing": "after_queue",
    "active_elicitation": null,
    "active_plan": {
      "id": "6bc6bb92-5de0-46d8-aa46-06e223e6b543",
      "validation_warning": [],
      "generation": {
        "max_turn_count": 20,
        "consecutive_no_progress": 0,
        "max_consecutive_no_progress": 2,
        "canonical_revision": 0,
        "turn_count": 0
      },
      "review_digest": null,
      "session_id": "34d8be31-e867-4265-9fd8-9a0db4216c1d",
      "state": "generating",
      "elicitation": null,
      "request": "build the feature",
      "working_path": "/fixture/nvim.0/S3Olz7/1/forge/harness\\plans\\34d8be31-e867-4265-9fd8-9a0db4216c1d\\6bc6bb92-5de0-46d8-aa46-06e223e6b543\\working.md",
      "updated_at_ms": 1793254204688,
      "document_version": 1,
      "model_revision": 0,
      "submitted_version": null,
      "accepted_revision": null,
      "user_revision": 0,
      "manual_changes": [],
      "accepted_digest": null,
      "acceptance": null,
      "question_ledger": {
        "resolution": []
      },
      "created_at_ms": 1793254204688,
      "title": "build the feature"
    },
    "capability": {
      "native_goal": false,
      "native_steer": true,
      "native_fork": true,
      "approval_mode_list": [
        "ask",
        "deny",
        "allow"
      ],
      "agent": {
        "spawn": true,
        "observe": true,
        "interrupt": true,
        "parallel": true,
        "input": true
      },
      "catalog": {
        "mcp": false,
        "live_mcp_mutation": false,
        "skill": false
      },
      "native_compact": true,
      "permission_control": true,
      "ultrafast_mode": true,
      "fast_mode": true,
      "effort_selection": true,
      "native_turn_rollback": true,
      "os_sandbox": false,
      "compaction": {
        "background": false,
        "metrics": true,
        "summary": true,
        "lifecycle": true
      },
      "background_terminals": true,
      "model_selection": true
    },
    "restore_recovery": null,
    "prompt_history": [
      {
        "attachments": [],
        "text": "/plan build the feature"
      }
    ],
    "checks_running": false,
    "runtime_epoch": "81866b45-2336-4e0f-868f-584e6a16c8c7",
    "active_check_run": null,
    "automatic_checks": false,
    "snapshot_revision": 4,
    "status": {
      "kind": "idle"
    },
    "checks_deferred": false,
    "status_thoughts": false
  }
}
```

<!-- /captured-example -->

## `session.task.get`

Read the session's optional active task.

- **Context:** Explicit existing session.
- **Input:** An empty parameter object.
- **Result and effects:** Returns one serialized ActiveTask or null, never a list of historical task records.
- **Boundary:** This reads HarnessSession.active_task without selecting or resuming anything. Exchanges do not supply task authority.
- **Inspected source:** [broker/observation.rs](../../../rust/forge/crates/forge-harness/src/broker/observation.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/tests/forge/harness_access_host.lua:92](../../../../nvim/tests/forge/harness_access_host.lua). Live integration-control use: Verify that permission/configuration mutations create no task or provider execution.

<!-- detailed-contract -->
### Contract

No parameter fields. Returns the selected session's [ActiveTask](runtime-schemas.md#activetask), or JSON null when no task is selected. Completed and paused states remain distinguishable in the task value. This is a read, not task resumption. It emits no progress and does not invoke the provider.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [session_api_examples.lua](../../../../nvim/tests/forge/session_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.task.get",
  "params": {},
  "id": 4,
  "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72"
}
```

```json
{
  "id": 4,
  "result": null
}
```

<!-- /captured-example -->

## `session.trace.clear`

Clear the selected session's trace records.

- **Context:** Explicit existing session.
- **Input:** The explicit session context.
- **Result and effects:** Clears the selected session log and its three rotated segments, then returns its trace status.
- **Boundary:** Other session records retain their own ownership.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/settings.lua:167](../../../../nvim/lua/forge/views/harness/settings.lua).

<!-- detailed-contract -->
### Contract

No parameter fields. Clears the selected session's trace file and returns [TraceStatus](runtime-schemas.md#tracestatus): effective enabled state, path, and whether tracing was configured. This changes diagnostic storage only, not the session transcript or other sessions' logs. Filesystem failure is explicit. Use the returned path rather than inferring it from the current directory.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_host.lua](../../../../nvim/tests/forge/harness_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 260,
  "method": "session.trace.clear",
  "params": {},
  "session_id": "7950e589-be54-4804-9e68-585010a566a5"
}
```

```json
{
  "id": 260,
  "result": {
    "enabled": true,
    "configured": true,
    "path": "/fixture/nvim.0/v3g7w8/1/forge/harness\\logs\\7950e589-be54-4804-9e68-585010a566a5.jsonl"
  }
}
```

<!-- /captured-example -->

## `session.turn.cancel`

Cancel the selected session's current execution.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The explicit session context and accepted control fields.
- **Result and effects:** Returns cancellation/control state.
- **Boundary:** Cancellation must not be replayed as a new prompt.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `control`. Recovery: `session_command`.

**Current caller:** [nvim/tests/forge/harness_interrupt_host.lua:38](../../../../nvim/tests/forge/harness_interrupt_host.lua). Live integration-control use: Cancel provider work at admission and active-execution boundaries.

<!-- detailed-contract -->
### Contract

Accepts optional root `target` and `restore_prompt_if_no_output` (boolean, default false). It validates root control ownership, quiesces managed children, cancels root approvals, and collects provider cleanup before acknowledging cancellation. Returns `{ "cancel_requested": true }` for an active provider turn, or the settled broker cancellation result when already idle. Prompt restoration only applies when the turn can be retracted before output. Cleanup failure remains an error and prevents pretending the provider stopped. Pausing the selected task/input queue is a separate state action used by the UI before cancellation.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_host.lua](../../../../nvim/tests/forge/harness_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 116,
  "method": "session.turn.cancel",
  "params": {
    "restore_prompt_if_no_output": true,
    "_command": {
      "id": "command:46304:51092182234300:3",
      "runtime_epoch": "f163bf85-f136-46d7-aa14-8ce071f6f423"
    }
  },
  "session_id": "7950e589-be54-4804-9e68-585010a566a5"
}
```

```json
{
  "id": 116,
  "result": {
    "cancel_requested": true
  }
}
```

<!-- /captured-example -->

## `session.turn.restart`

Restart work through the session coordinator.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The restart/control payload accepted by the coordinator.
- **Result and effects:** Returns the new execution/control state.
- **Boundary:** Preserve task ownership and operation identity across the restart boundary.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `control`. Recovery: `session_command`.

**Current caller:** [nvim/tests/forge/harness_interrupt_host.lua:60](../../../../nvim/tests/forge/harness_interrupt_host.lua). Live integration-control use: Restart a cancelled provider while checking durable stop preservation.

<!-- detailed-contract -->
### Contract

Accepts optional `mode`, which is echoed in `{ "restart_requested": true, "mode": ... }`. It quiesces children, cancels root approvals, and requests restart after provider cleanup. A cleanup failure is returned and retained instead of admitting overlapping provider work. This acknowledges restart scheduling, not a completed replacement turn. The session and provider generation remain the ownership boundary for later events.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [session_api_examples.lua](../../../../nvim/tests/forge/session_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.turn.restart",
  "params": {
    "_command": {
      "id": "command:24460:51071676301700:1"
    }
  },
  "id": 5,
  "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72"
}
```

```json
{
  "id": 5,
  "result": {
    "mode": null,
    "restart_requested": true
  }
}
```

<!-- /captured-example -->

## `session.turn.steer`

Steer active execution through the coordinator.

- **Context:** Explicit existing session; coordinator handles control methods.
- **Input:** The steering text and accepted control fields.
- **Result and effects:** Returns steering admission/result state.
- **Boundary:** Reject child targets before interpreting task syntax. Recognized task text uses shared command normalization before provider steering and returns the command result. Ordinary steering retains its accepted/unaccepted cancellation semantics.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

**Transport:** Required explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/rust/forge/tests/harness_broker_stdio.rs:1361](../../../../nvim/rust/forge/tests/harness_broker_stdio.rs). Live integration-control use: Exercise native steering while an independent request owns provider execution.

<!-- detailed-contract -->
### Contract

Requires nonempty `text`, optional root `target`, and optional attachment input in the same shape as [session.prompt.submit](#sessionpromptsubmit). The service resolves attachments and persists a steering submission before delivering through the active parent-input boundary or provider-native steering. Returns `{ "steered": true, "delivery": "direct" }` when delivery is accepted. This is not a new completed exchange. Wrong target, missing attachment, unavailable steering, or provider rejection fails. A submitted goal/plan command should use the canonical task action rather than being treated as literal steering text.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [session_api_examples.lua](../../../../nvim/tests/forge/session_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "session.turn.steer",
  "params": {
    "text": "Keep working on the fixture",
    "_command": {
      "id": "example-steering"
    },
    "operation_id": "example-steering"
  },
  "id": 31,
  "session_id": "b14c3cec-edd9-49ca-a61f-467677607b72"
}
```

```json
{
  "id": 31,
  "result": {
    "steered": true,
    "delivery": "direct"
  }
}
```

<!-- /captured-example -->


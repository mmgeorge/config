---
name: fn-walk
description: >-
  Generates a detailed, section-by-section code walkthrough with code blocks and
  in-line invariant comments. Use when asked "/fn-walk", "walk through this code",
  "detailed walkthrough", "step by step walkthrough", or when a deep, section-by-section
  code breakdown is requested.
targets:
  - '*'
---

# Detailed Function Walkthrough

Deliver a thorough, section-by-section code walkthrough that breaks down an implementation phase-by-phase with annotated code snippets, invariants, and consequence explanations.

## Pre-Investigation Workflow

1. **Locate Entity:** Inspect the target entity with `sem_entities` or `sem_context` to read the complete implementation source.
2. **Inspect Callers:** Pass the exact `filePath` returned by `sem_context` directly to `sem_impact` without guessing paths or stripping directory prefixes.
3. **Trace Lifecycles:** Map the function's internal execution into 3–5 discrete architectural phases (such as Preflight Validation, Preparation/Snapshots, Atomic Mutation, Stale Entity Cleanup).

## Response Structure

Structure the walkthrough using this exact sequence:

1. **Problem-First Summary (2–3 sentences):**
   - Sentence 1: Open with the problem, hazard, or controlling contract, naming
     the specific target function or method name as the active subject (e.g., `publish_loaded`
     or `SessionStore::reconcile`). Natural opening patterns include:
     - Purpose infinitive: `To prevent / ensure / isolate / preserve [outcome], [fn_name] [action]...`
     - Direct action: `[fn_name] [action] to [outcome]...`
     - Operational condition: `When [condition], [fn_name] [action]...`
     - Never substitute broad struct or class names alone, and never open with dummy subjects
       (`"This function..."`, `"This method..."`), process narration, or preambles.
   - Sentence 2: State the pre-validation or ordering invariant that protects system state.
   - Sentence 3: State the preserved consumer contract (such as handle stability, temporal consistency, or error boundaries).

2. **Sequential Architectural Sections (`### 1. ...`, `### 2. ...`):**
   For each numbered phase:
   - **Context Sentence:** State the phase invariant, trigger, or safety guarantee *before* presenting any code.
   - **Annotated Code Snippet:** Show the relevant slice of code, adding concise in-line comments (`// ...`) that explain *non-obvious invariants, guarantees, and error paths* rather than restating syntax.
   - **Consequence Sentence:** State what state transition or invariant is established for subsequent phases or external consumers.

3. **Atomicity and Error Isolation (`### Atomicity and Error Isolation`):**
   - Detail the error recovery paths, preflight validation safety, partial mutation reachability, and rollback guarantees across the entire function.

## Example

To prevent message loss and dangling connection handles during cluster rebalancing, `SessionStore::reconcile` synchronizes active worker allocations under a single monotonic epoch. Pre-validating candidate partitions upfront ensures that invalid routing topologies cannot corrupt routing state. Reusing active connection handles ensures caller clients experience zero reconnect churn.

### 1. Preflight Partition Validation

The coordinator verifies topology completeness and node reachability upfront to guarantee that malformed partition maps fail before modifying cluster routing state:

```rust
let mut seen_keys = HashSet::new();
for assignment in &plan.assignments {
    // 1. Enforce unique primary partitions across assignments
    if !seen_keys.insert(assignment.partition_id) {
        return Err(ClusterError::duplicate_partition(assignment.partition_id));
    }
    // 2. Enforce node capability contract
    if !assignment.node.supports(assignment.protocol) {
        return Err(ClusterError::protocol_mismatch(&assignment.node));
    }
}

// 3. Ensure candidate topology contains all required primary partitions
if !seen_keys.is_superset(&self.required_partitions) {
    return Err(ClusterError::incomplete_topology());
}
```

Preflight checks ensure that subsequent worker allocations and routing table mutations operate on a valid partition graph.

---

### 2. Snapshot Active Allocations and Resolve Handles

To distinguish retained assignments from dropped nodes, the store snapshots live connections and reuses established socket handles:

```rust
// Snapshot active workers allocated in the prior epoch
let previous_nodes = self.active_workers_for_cluster(&plan.cluster_id);

// Resolve handles: reuse live sockets to eliminate reconnect latency
let mut staged_workers = Vec::with_capacity(plan.assignments.len());
for assignment in &plan.assignments {
    let handle = match self.workers.get_mut(&assignment.node_id) {
        Some(live) => live.clone(),          // Preserves active TCP handle
        None => self.open_worker(assignment)?, // Connects newly attached node
    };
    staged_workers.push((assignment, handle));
}
```

Reusing active socket handles guarantees that client streams observe zero disconnect churn during rebalancing.

---

### 3. Atomic Epoch Transition and Routing Table Update

The store increments the cluster epoch and atomically applies all staged assignments:

```rust
let new_epoch = self.allocate_epoch();
let mut events = Vec::new();

for (assignment, handle) in staged_workers {
    let event = handle.apply_epoch(
        assignment.weights,
        new_epoch,
        &assignment.backup_targets,
    );
    events.push(event);
    self.workers.insert(assignment.node_id, handle);
}
```

All worker targets transition to the new routing weights simultaneously, preventing traffic split-brain across epochs.

---

### 4. Drain Dropped Nodes

The store identifies workers present in the previous epoch but omitted from the new topology, initiating graceful drain:

```rust
// Nodes present in prior epoch but omitted from new assignment plan
for dropped_node in previous_nodes.difference(&plan.active_node_ids()) {
    if let Some(worker) = self.workers.get_mut(dropped_node) {
        worker.mark_draining(new_epoch);
        events.push(SessionEvent::Detached { node_id: *dropped_node });
    }
}
```

Dropped workers finish active in-flight requests before their socket connections are reclaimed.

---

### Atomicity and Error Isolation

All topology validation runs prior to epoch allocation and socket mutation, isolating active routing tables from orchestrator errors. Because handle downcasting is pre-validated during preflight checks, mid-loop failures during `apply_epoch` are unreachable under the cluster invariant.
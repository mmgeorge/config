---
name: fn-why
description: >-
  Explains why a function, method, or subsystem exists, its controlling invariant,
  caller contract, and lifecycle flow. Use when asked "/fn-why", "why does this function exist",
  "explain this function", "what does this function do", or when walking through a function's
  caller contract and failure boundaries.
targets:
  - '*'
---

# Function Why & Lifecycle Walkthrough

Ground code explanations in the controlling problem and consumer contract rather than narrating local line-by-line mechanics.

## Pre-Investigation Workflow

1. **Locate Entity:** Inspect the target entity with `sem_entities` or `sem_context` to read its implementation.
2. **Inspect Callers:** Pass the exact `filePath` returned by `sem_context` directly to `sem_impact` without guessing paths or stripping directory prefixes.
3. **Determine Contract:** Identify what callers rely on (handle stability, atomic revision allocations, cache coherence, or error boundaries) before drafting.

## Response Structure

Structure the explanation using this exact sequence:

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
2. **Lifecycle Flow (2–3 architectural phases):**
   - Format each phase title with bold asterisks immediately after the number (`1. **Phase Title:** ...`). Never omit the `**` bold markers.
   - Limit each numbered phase to **1 concise sentence** stating the invariant and action.
   - Group execution strictly into 2–3 high-level architectural phases (such as Validation and Reconciliation). Never create separate numbered steps for intermediate variable setup.
   - Prefer a flat numbered list with **no sub-bullets**. Use sub-bullets only when strictly necessary to distinguish 2–3 multi-state diffs or branch fates (`- **State:** Invariant and event`).
   - Do not recite method parameter lists, intermediate variable assignments (`It first builds...`), or local helper calls.
   - Do not nest beyond one level of sub-bullets or emit loose arrow chains.
3. **Boundary and Failure Isolation (1 sentence):**
   - End with exactly one sentence evaluating atomicity boundaries, failure isolation, or rollback guarantees.
   - Do not append an artificial `### Conclusion` heading, conversational ergonomic commentary (*"The function feels long because..."*), or a redundant step recap.

## Example

To prevent message loss and dangling connection handles during cluster rebalancing, `SessionStore::reconcile` synchronizes active worker allocations under a single monotonic epoch. Pre-validating candidate partitions upfront ensures that invalid routing topologies cannot corrupt routing state. Reusing active connection handles ensures caller clients experience zero reconnect churn.

1. **Preflight Partition Validation:** Verifies node reachability, checks key uniqueness, and rejects routing maps with missing primary partitions before mutating cluster state.
2. **Reconcile Worker Allocations:** Applies the new partition topology under the shared epoch, preserving socket handles for unchanged assignments:
   - **Retained nodes:** Updated in-place with new routing weights and epoch counters (`SessionEvent::Rebalanced`).
   - **New nodes:** Initialized and registered as active partition targets (`SessionEvent::Attached`).
   - **Dropped nodes (`previous \ new`):** Marked draining and tombstoned for graceful shutdown (`SessionEvent::Detached`).

State mutation begins only after topology verification passes, isolating routing tables from partial orchestrator failures.
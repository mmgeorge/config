# Proposal Profile

*Read when writing an RFC or proposing a major design change, a new subsystem, an API redesign, or an engineering strategy.*

## Goal

Demonstrate that a proposed design satisfies measurable goals better than credible alternatives, while stating its costs, new failure modes, migration plan, and open questions honestly so reviewers can make an informed accept/reject decision.

## Sections

Write these sections in order.

### Problem

- Describe the concrete user pain, bottleneck, or architectural limitation and what it costs to leave the current system unchanged.

### Goals, Non-Goals, and Criteria

- State measurable goals the solution must hit and explicit non-goals so scope stays bounded.
- Name the criteria used to compare options (such as p99 latency, memory ceiling, rollout risk, or backward compatibility).

### Proposal

- Show the caller-facing contract first (API, CLI, config, or workflow), then explain internal ownership, state transitions, concurrency, and failure recovery.

### Alternatives

- Compare each serious alternative against the same criteria—using a table when comparing three or more dimensions—and explain where each falls short.

### Risks and Mitigations

- List new failure modes, operational overhead, performance costs, or security exposure introduced by the design, paired with concrete mitigations.

### Migration and Rollback

- Explain backward compatibility, data or schema transitions, cutover sequencing, and how to roll back safely if production metrics regress.

### Open Questions

- List unresolved questions reviewers must settle before approval and the prototype or measurement needed to answer each uncertain claim.

## Leave Out

- Sprint checklists or task breakdowns (those belong in an implementation plan after approval).
- Unverifiable claims ("this approach is much cleaner and faster").

## Example

````markdown
# RFC: Replace Document Polling with Push Webhooks

## Problem

Currently, the editor polls the document API on a fixed interval to check for edits from other collaborators. That polling loop leaves concurrent editors seeing stale state between intervals and keeps the API serving repeated no-op reads even when documents sit idle.

## Goals and Scope

Deliver edit notifications as soon as commits land while eliminating steady-state no-op polls from active editors. Legacy third-party integrations remain on polling and stay outside this change. Options are evaluated on delivery latency, server memory per connected client, and behavior behind restrictive firewalls.

## Proposal

When a client opens a document, it registers a callback URL with the notification service. After each committed edit, the notification service signs the event payload and pushes it to every subscribed URL with bounded retries on transient failures.

## Alternatives

Long polling delivers updates without inbound callbacks, but holding an open HTTP connection per active client increases server memory proportional to concurrency and complicates rolling restarts when connections drain.

## Risks and Open Questions

Clients behind restrictive corporate firewalls cannot receive inbound webhook calls and fall back to polling when registration health checks fail. Before rollout, we need telemetry showing what share of active clients sit behind unreachable NATs or firewalls.
````

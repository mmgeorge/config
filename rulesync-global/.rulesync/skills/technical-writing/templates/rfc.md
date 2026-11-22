# RFC and Proposal Profile

*Read when proposing a major design change, a new subsystem, an API redesign, or an engineering strategy.*

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

> ### Replace 30-Second Document Polling with Push Webhooks
>
> Collaborators currently discover document changes by polling the server every 30 seconds. That polling delay leaves concurrent editors up to half a minute out of sync and accounts for 60% of total API request volume.
>
> **Goals and scope:** Deliver document change notifications within 1 second while cutting notification request volume by at least 40%. Third-party legacy integrations remain on polling and are out of scope. Options are compared on delivery latency, server memory per active client, and firewall resilience.
>
> **Proposal:** When a client opens a document, it registers a callback URL with the notification service. On every committed edit, the service pushes a signed event payload to subscribed URLs with three retries over 10 seconds. In a 200-client load test, median delivery latency dropped to 400 ms and total server requests fell by 52%.
>
> **Alternatives:** Long polling met the 1-second delivery goal in the same 200-client benchmark, but holding an open HTTP connection per client doubled server memory usage and complicates rolling restarts.
>
> **Risks and open questions:** Clients behind restrictive corporate firewalls cannot receive inbound webhook calls and must fall back to polling automatically when registration health checks fail. Before approving rollout, we need telemetry on what fraction of active clients sit behind unreachable NATs or firewalls.

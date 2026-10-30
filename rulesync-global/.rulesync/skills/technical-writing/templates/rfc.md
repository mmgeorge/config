# RFC and Proposal Profile

*Read when proposing a major design change, a new technology, an API redesign, or an engineering strategy.*

## Controlling Claim

The proposal shows that its design meets the stated goals better than the alternatives, and records its costs, its risks, and the evidence still needed.

## Sections

Write these sections in order.

### 1. Problem

- Describe the use cases, bottlenecks, or user problems behind the proposal.
- Describe what it costs to keep things as they are.

### 2. Goals and criteria

- List the measurable goals the proposal must meet.
- List the non-goals, so reviewers know what is out of scope.
- Name the criteria used to compare options, such as latency, durability, or backward compatibility.

### 3. Proposal

- Show what users will see first, such as commands, API calls, or configuration.
- Then describe the design: which part owns what, the states data passes through, the data formats, and how work runs in parallel.
- State what stays true when something fails and the system recovers.

### 4. Alternatives

- Compare each alternative against the same criteria, in a table when that helps, and say why it falls short.

### 5. Risks

- Describe new failure modes, added cost, operational work, and security exposure, and how each one is reduced.

### 6. Migration

- Explain what stays compatible, how data formats change, and when old behavior is removed.
- Describe how data moves without downtime and how to roll back.

### 7. Open questions

- List the questions reviewers must settle before accepting the proposal.
- Describe the prototype, benchmark, or experiment that will answer any uncertain claim.

## Leave out

- Do not include task tickets or checklists. Those belong in an implementation plan after the proposal is accepted.
- Do not make claims the criteria cannot check, such as "this approach is far better."

## Example

A shortened proposal:

> Clients learn about document changes by polling the server every 30 seconds, so collaborators see each other's edits up to 30 seconds late, and polling makes up 60% of server requests. This proposal replaces polling with webhooks. The goal is to show changes within one second without adding server load. Moving existing third-party integrations off polling is out of scope.
>
> Clients register a callback URL when they open a document, and the server calls it with each change. In a test with 200 clients, changes arrived in 400 ms on average and server requests fell by half. Long polling also met the one-second goal, but it holds one open connection per client and doubled memory use in the same test.
>
> Webhooks add a new way to fail: a client behind a firewall cannot receive callbacks. Those clients keep polling until they can register a reachable URL. Before this proposal is accepted, we need to measure how many current clients sit behind such firewalls.

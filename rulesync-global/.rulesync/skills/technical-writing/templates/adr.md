# Architecture Decision Record Profile

*Read when recording a significant design decision for future maintainers.*

## Controlling Claim

The record shows that one decision answers a stated problem under stated constraints, and what the decision costs.

## Sections

Write these sections in order.

### 1. Title and status

- Give the decision a short title, such as "ADR-004: Keep drafts in a local cache for offline sync."
- Record its status: Proposed, Accepted, Rejected, Superseded, or Deprecated.
- Record the date, the authors, the reviewers, and any earlier record this one replaces.

### 2. Context

- Describe how the system works today and the problem the decision solves.
- State the constraints and requirements fairly, without slanting them toward the chosen option.

### 3. Decision

- State what was decided, in the present tense.
- Say which parts of the system change, which part owns what, and how the parts communicate.

### 4. Alternatives

- List the other options considered, judged against the same requirements.
- Say why each one was rejected.

### 5. Consequences

- Describe what the decision makes possible, what it costs, and what it requires from other parts of the system, including any new ways the system can fail.

### 6. Verification

- Say how the team will check that the decision works, such as with tests, load tests, or lint rules.

## Leave out

- Do not turn the record into a system overview, an API manual, or a task list. Link those documents instead.
- Do not reopen a settled decision unless new constraints or measured failures justify it.

## Example

A shortened decision record:

> ADR-004: Keep drafts in a local cache for offline sync. Accepted in March 2026.
>
> The editor sends each edit straight to the document server. When the network drops, the editor shows an error and the edit is lost. Any fix must work without a network connection and must not slow down typing.
>
> We will write each edit to a cache on the user's disk before it reaches the server, and a background worker will send cached edits whenever the server is reachable. The editor owns writing to the cache, and the worker owns removing entries once the server confirms them.
>
> We also considered keeping unsent edits in memory, but they would be lost if the editor crashed. Retrying the request inside the editor would block typing while the network is down.
>
> The cache adds a small disk write to every edit and a background worker to maintain. A full disk now stops edits from being saved, so the editor must report that error. An integration test that disconnects the network during an edit checks that no edit is lost.

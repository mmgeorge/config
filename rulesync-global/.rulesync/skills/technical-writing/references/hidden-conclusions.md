# Hidden Conclusions

*Read when planning the evaluative takeaway of a document, section, or paragraph, or deciding which evidence can support it.*

A hidden conclusion is a private organizing target rather than a sentence to insert into the document. It identifies the qualitative judgment that the complete evidence should earn and helps the writer select relevant mechanisms, examples, consequences, and failure behavior.

Use hidden conclusions at three levels:

- A **document-level conclusion** shapes the qualities the complete operating model must earn.
- A **section-level conclusion** keeps one architectural subject focused on a meaningful result.
- A **paragraph-level conclusion** selects the mechanics and consequences that belong together.

Keep the explicit argument intact. State goals, required capabilities, architectural decisions, warrants, concrete consequences, and contractual requirements such as latency limits, compatibility promises, and security boundaries directly. A hidden conclusion may shape the argument, but it cannot carry a logical step the reader must evaluate.

Evidence and warrants serve different roles. Evidence shows what the design does. A warrant explains why that behavior supports the point. The hidden conclusion names the qualitative judgment that may emerge after the reader follows that reasoning.

## Conclusion Catalog

| Quality | Intrinsic hidden conclusion | Comparative hidden conclusion | Evidence to expose |
| --- | --- | --- | --- |
| Maintainability | Changes remain localized and preserve one source of truth. | This design requires fewer coordinated changes than the alternative. | Owners, change sites, generated outputs, and migration steps |
| Clarity | Readers can trace ownership, inputs, transformations, and outputs. | This design exposes responsibilities more clearly. | Named actors, stable terms, and explicit data flow |
| Simplicity | The design minimizes states, representations, handoffs, and special cases. | This design removes machinery required by the alternative. | State eliminated, steps removed, and exceptions avoided |
| Modularity | Subsystems change independently through narrow contracts. | This design couples fewer subsystems. | Interface boundaries, dependency direction, and replacement scope |
| Extensibility | New capabilities enter through established extension points. | This design accepts new cases without modifying central logic. | Registration points, interfaces, and unchanged core paths |
| Reusability | One primitive serves multiple contexts without duplication. | This design duplicates less behavior or data. | Shared definitions and distinct consumers |
| Coherence | Subsystems follow one ownership and lifecycle model. | This design has fewer exceptions and competing conventions. | Repeated patterns and eliminated special paths |
| Evolvability | Versions and migrations preserve intent as structures change. | This design accommodates change with less breakage. | Version fields, migration paths, and compatibility boundaries |
| Correctness | Invalid states cannot cross the validation boundary. | This design detects errors earlier. | Constraints, rejection points, and accepted-state invariants |
| Safety | Ownership and validation prevent partial or invalid mutations. | This design exposes less mutable state. | Mutation boundaries, transactions, and rollback behavior |
| Security | Trust boundaries constrain untrusted input and authority. | This design presents a narrower attack surface. | Authentication, authorization, validation, and least privilege |
| Reliability | Failures remain isolated and valid state survives unsuccessful work. | This design isolates more failure modes. | Failure boundaries, retries, idempotency, and retained state |
| Recoverability | Authoritative inputs can regenerate derived outputs. | This design loses less work after failure. | Backups, last-known-good state, replay, and regeneration |
| Performance | Critical paths perform bounded, necessary work. | This design performs less work on the critical path. | Measurements, complexity, allocations, batching, and caching |
| Responsiveness | Expensive work does not block interactive operations. | This design returns control sooner. | Async boundaries, background work, and latency budgets |
| Scalability | Work partitions without global coordination. | This design removes a bottleneck or coordination point. | Sharding keys, independent workers, and bounded shared state |
| Efficiency | The design avoids repeated computation, storage, or transfer. | This design consumes fewer resources or incurs lower cost. | Cache reuse, incremental work, compression, and measured resource use |
| Determinism | Explicit inputs and versions reproduce the same output. | This design depends less on ordering or hidden state. | Seeds, versions, stable ordering, and isolated random streams |
| Debuggability | A failure traces back to its source, transformation, and owner. | This design preserves more diagnostic context. | Provenance, stable identities, errors, and source locations |
| Observability | Runtime behavior exposes meaningful operational signals. | This design reveals causes the alternative hides. | Logs, metrics, traces, correlation IDs, and state inspection |
| Testability | Deterministic inputs and narrow contracts produce inspectable outputs. | This design requires fewer integrated dependencies to test. | Test seams, fixtures, deterministic clocks, and pure transformations |
| Reviewability | Changes produce focused diffs with stable identities. | This design creates less generated noise. | Textual sources, minimal patches, and stable ordering |
| Portability | Platform-independent sources produce target-specific outputs. | This design avoids separate authoring paths per platform. | Logical identities, platform adapters, and target selection |
| Usability | Users complete work without avoidable transitions or duplicated input. | This design requires fewer manual operations. | Concrete steps, shared sources, immediate feedback, and undo behavior |
| Automation | Tools derive repeatable work from authoritative inputs. | This design requires less manual synchronization. | Watchers, generators, validation, and reproducible commands |
| Automation readiness | External tools can inspect, modify, validate, and review the authoritative inputs through established interfaces. | This design requires no separate representation or privileged editing path for automated contributors. | Versioned text, explicit schemas, deterministic diagnostics, narrow patches, and ordinary review artifacts |
| Auditability | History and provenance explain how state reached runtime. | This design retains evidence the alternative discards. | Revisions, actor identity, provenance, and immutable records |

## Automation Readiness

Treat AI-assisted development as one consumer of an automation-ready architecture. In the document, expose the representations and interfaces that let external tools participate, then let the reader infer the capability rather than calling the design AI-ready, automation-friendly, or easy to automate.

Prefer evidence that supports the complete edit cycle. Authoritative inputs remain structured, versioned text. Explicit schemas and deterministic validators define accepted changes. Tools submit narrow source patches, receive precise diagnostics, and produce the same review artifacts as other contributors.

Weak:

> The deployment workflow is AI-friendly and easy to automate.

Better:

> Deployment policies remain versioned text. The editor and command-line validator apply the same schema, and every accepted change enters review as an ordinary source patch.

## Intrinsic Conclusions

Use an intrinsic conclusion when the document explains one design. Select only conclusions that matter to the document, section, or paragraph purpose, then expose the mechanics listed in the catalog. Do not add a comparison.

## Comparative Conclusions

Use a comparative conclusion only when the document already needs to evaluate alternatives, replace an existing design, or record a tradeoff.

1. State the shared requirement.
2. Describe the existing or alternative mechanism.
3. Show the concrete consequence of that mechanism.
4. Describe the proposed mechanism at the same level of detail.
5. Show the corresponding consequence.

Do not write “the old design is unmaintainable” or “our design is simpler.” Show the number of synchronized representations, coordination points, runtime operations, failure paths, or migration steps. Let the reader make the comparison.

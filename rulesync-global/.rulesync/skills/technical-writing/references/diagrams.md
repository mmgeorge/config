# Diagrams

*Read before adding or reviewing an architecture diagram, sequence chart, state diagram, or data-flow diagram.*

## When to use a diagram

Use a diagram when layout explains something faster than prose, such as how parts nest, which parts run in parallel, how state changes, or how data moves through a pipeline.

- Put the diagram right next to the text that explains it.
- State the point the reader should take from the diagram in the paragraph before it.
- Never put a rule or contract only in a diagram. Repeat it in prose, alt text, or a table.
- Keep anything the reader may copy, such as names, commands, paths, and error codes, in text instead of in an image.

## One question per diagram

Draw each diagram to answer one question, such as:

- How does a payload move from input, through each change, to storage?
- What path does a request take from the client through the gateway and service to the database?
- What states can an item be in, and what moves it from one state to another?
- Which part owns which data, and which parts depend on which?

When a diagram tries to answer two unrelated questions, such as where things are deployed and how data flows at runtime, split it into two diagrams.

## Size

- Keep the longest path to six nodes or fewer. Count the longest chain of arrows, not the total number of nodes.
- When a path is longer, split the diagram where data crosses an ownership boundary, a network hop, or a write to storage.
- Merge internal steps into one named node when they do not change which part owns the data or what must stay true.

## Nodes, arrows, and groups

- Name components, services, stores, and workers with nouns, such as `IngestWorker` or `DocumentStore`.
- Name a processing step with a verb only when the step itself is the subject, such as `ValidateSignature`.
- Name states with adjectives or past participles, such as `Draft`, `PendingSync`, or `Failed`.
- Label every arrow with what crosses it, such as `SyncRequest` or `HTTP POST /v1/events`. Avoid vague labels such as "sends," "calls," or "uses."
- Use solid arrows for direct, blocking calls and dashed arrows for asynchronous events and queues.
- Use two labeled arrows instead of one double-headed arrow when each direction carries something different.
- Draw a box around parts that are truly separate, such as different processes, trust zones, thread pools, or services, and label the box with what separates them.

## Accessibility

- Do not use color alone to carry meaning. Pair it with a shape, a line style, or a label.
- Lay out hierarchy from top to bottom, and pipelines and request flows from left to right.
- Give a rendered diagram alt text that describes the starting state, each step, and the result.

## Example

The paragraph before the diagram states its point: a draft reaches the server only through the sync worker, and the worker deletes it only after the server confirms the save.

```text
Editor ──Draft──▶ DraftCache ──Draft──▶ SyncWorker ──SaveRequest──▶ DocumentServer
                                            ▲                             │
                                            └────────SaveConfirmed────────┘
```

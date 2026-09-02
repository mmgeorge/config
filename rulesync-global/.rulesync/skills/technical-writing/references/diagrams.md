# Engineering Diagrams Guide

*Read when designing, reviewing, or labeling architecture diagrams, sequence charts, state machines, and dataflow visuals.*

## Purpose and Text Supplementation

Use diagrams when spatial arrangement, component hierarchy, concurrency boundaries, state machines, or data pipelines clarify architecture faster than prose alone.

- Place the diagram immediately adjacent to the text that explains the flow.
- State the architectural conclusion or invariant the reader must take away in the preceding paragraph.
- Never make a diagram the sole carrier of an engineering constraint or contract. Provide equivalent prose, descriptive alt text, or a structured lookup table.
- Keep copyable identifiers, configuration blocks, commands, file paths, and error codes in markdown text rather than in image graphics.

## Focus on One Architectural Flow

Design each diagram around a single architectural question:

- **Data pipelines:** Trace a payload from ingest through transformation to durable storage.
- **Request lifecycles:** Trace an RPC or HTTP call across client, gateway, service, and persistence layers.
- **State machines:** Map valid states, guard conditions, transition events, and terminal states.
- **Component ownership:** Map subsystem boundaries, managed storage, and dependency directions.

Split diagrams when a visual attempts to depict multiple orthogonal concerns (such as deployment infrastructure and runtime dataflow) on one canvas.

## Path Depth and Complexity Limits

- **Maximum Path Depth:** Limit the longest directed path to six nodes or fewer. Measure the longest linear sequence through the graph rather than total node count.
- **Boundary Splitting:** When a path exceeds six nodes, split the visual at an ownership boundary, network hop, or storage persistence step into sequential diagrams.
- **Transformation Collapsing:** Collapse low-level internal processing steps into a single named transformation node when the intermediate steps do not alter component boundaries or state invariants.

## Node, Edge, and Cluster Semantics

### Node Semantics
- Use noun phrases for components, services, stores, workers, and state buffers (such as `IngestWorker`, `DocumentStore`, `DraftBuffer`).
- Use active verb phrases for standalone computational stages only when the transformation itself is the primary architectural subject (such as `TokenizeInput`, `ValidateSignature`).
- Use state adjectives or past participles for state machine nodes (such as `Draft`, `PendingSync`, `Committed`, `Failed`).

### Edge Semantics
- Label every edge with the specific data artifact, message payload, event type, or protocol method crossing the boundary (such as `SyncRequest`, `JSON Patch`, `HTTP POST /v1/events`).
- Avoid generic edge labels such as `sends`, `calls`, `uses`, or `processes`.
- Use solid arrows for synchronous, blocking calls or direct dataflow.
- Use dashed arrows for asynchronous events, message queues, or background synchronization.
- Treat unlabeled bidirectional arrows as ambiguous. Use two oppositely directed arrows with distinct payload labels when each direction performs separate work.

### Subgraphs and Clusters
- Use visual subgraphs or bounding boxes to represent hard isolation boundaries: process spaces, network trust zones, thread pools, or distinct services.
- Label each cluster with its boundary type (such as `Worker Process`, `VPC Subnet`, `Client Runtime`).

## Accessibility and Encodings

- Do not rely on color alone to convey status, state, or meaning. Pair color with distinct shapes, line styles (solid, dashed, dotted), or text labels.
- For flowchart or sequence directions, default to top-to-bottom (`TD`) for containment and hierarchy, and left-to-right (`LR`) for pipelines, lifecycles, and sequential requests.
- Provide descriptive alt text for rendered images that explains the starting state, intermediate transformations, and final result.

## Diagram Review Checklist

1. Does the diagram illustrate exactly one flow or boundary relationship?
2. Does the preceding prose state the rule or invariant shown in the diagram?
3. Is the longest directed path six nodes or fewer?
4. Are node labels concrete noun phrases and edge labels specific payloads or protocols?
5. Are asynchronous or decoupled interactions visually distinct from synchronous calls?
6. Are ownership domains, process spaces, or network zones grouped into explicit subgraphs?
7. Is all critical copyable text (commands, paths, error strings) present in markdown prose?

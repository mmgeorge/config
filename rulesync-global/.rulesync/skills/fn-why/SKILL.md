---
name: fn-why
description: >-
  Explains why a function, method, or subsystem exists, its controlling invariant,
  caller contract, and failure boundaries without narrating code line by line.
  Use when asked "/fn-why", "why does this function exist", "explain this function",
  or "what does this function do".
targets:
  - '*'
---

# Function Why

Explain why a function exists, the operating constraint or problem it addresses, what callers rely on, and where its guarantees stop—without walking through local variables or line-by-line syntax. Ground every claim in the actual implementation and its callers.

## Investigation

1. **Read the implementation:** Inspect the target function and any helper it delegates state changes to.
2. **Inspect callers:** Trace callers across the workspace to see what invariants downstream code actually depends on—such as handle stability, idempotency, ordering, or error recovery.
3. **Check failure paths honestly:** Note every early return, `?` propagation, lock boundary, or side effect that runs before the function completes so you never claim atomicity the code does not enforce.

## How to Write the Explanation

Keep the explanation concise, causal, and readable: prefer natural prose over rigid checklists, use active voice and natural articles with type names (`the AssetCache`, `an AssetHandle`), state operating constraints positively (contrasting a simpler approach only when it is genuinely tempting), and omit meta signposting or `### Conclusion` recaps.

- **Lead with the constraint and contract:** Name the function directly as the active subject (never `"This function..."` or `"This method..."`). Open with the operating constraint, coordination problem, or failure it addresses, then state the core invariant and what callers can rely on.
- **Summarize the major phases:** Group execution into the 2–3 meaningful stages that actually move state forward (for example, checking the candidate plan before changing live state, then updating active assignments). Use connected prose—or a short ordered list only when strict sequential stages read clearer—and skip local variable setup, parameter recitations, and helper plumbing.
- **End on failure boundaries:** Close by stating what stays intact on error and where partial state or rollback limits exist, verified directly against the code's error paths.

## Examples

### `AssetCache::reload`

`AssetCache::reload` updates a modified asset while letting existing `AssetHandle` holders keep reading the last valid revision until the replacement finishes decoding. Reloading in place could break rendering if the new file fails to parse or contains invalid schema fields.

`reload` reads and decodes the updated payload into an isolated staging entry, verifies all subasset dependencies, and swaps the entry's revision pointer only after every dependency resolves. Callers polling their existing handles observe the new revision atomically on the next frame without re-fetching handles.

I/O errors, decode failures, and missing dependencies discard the staged entry and emit a failure event while keeping the previous revision live.

### `WebhookSigner::verify`

`WebhookSigner::verify` authenticates incoming webhook payloads and rejects replayed deliveries. Checking the HMAC digest with ordinary string equality could leak signature bytes through timing differences, and skipping timestamp checks would let an attacker resend a captured request indefinitely.

`verify` parses the signed timestamp header, confirms it falls within a five-minute window of the current clock, and recomputes the HMAC-SHA256 digest over the timestamp and raw request body. It compares the expected and received digests in constant time before returning the verified payload.

Missing headers, expired timestamps, and signature mismatches all return a generic `InvalidSignature` error without exposing which check failed. Replay protection stops at the five-minute window: callers that need strict once-only handling must still deduplicate by delivery ID.

# Technical Explanation Profile

*Read when writing a deep dive, explaining a non-obvious mechanism, or analyzing why a system behaves the way it does.*

## Goal

Give the reader a causal model sharp enough to predict what happens when inputs, load, settings, or failure conditions change. Unlike a how-to guide or reference, an explanation answers *why* rather than *how to click* or *what every flag does*.

## Sections

Write these sections in order.

### The Behavior or Question

- Open with the observable behavior, failure mode, or design tension being explained—what the reader actually sees in logs, traces, frames, or profiler output.

### Mental Model and Concepts

- Define each concept in dependency order before using it in the causal chain.
- Anchor unfamiliar mechanisms to familiar engineering models when helpful (for example, comparing a custom lease loop to Raft heartbeats or a solver step to an implicit optimization problem).

### Causal Mechanism

- Walk step by step through what changes at each stage, connecting each internal state transition to its observable consequence.
- Name the exact component, thread, or pass responsible for each step—avoid passive voice that hides the owner.

### Evidence and Counterexample

- Ground the explanation in a concrete trace, log pattern, formula, or measurement.
- Include a counterexample showing what happens when one underlying assumption breaks (and how changing that variable removes the behavior).

### Scope and Limits

- State clearly where the model stops applying—for example, under a different failure mode, above a memory cap, or once a fallback takes over.

## Leave Out

- Step-by-step operator instructions (link a how-to guide).
- Exhaustive API listings (link a reference).
- Proposals, roadmaps, or open-ended brainstorming (link an RFC).

## Example

A causal explanation with an observable hook, concrete trace, counterexample, and explicit boundary:

> ### Why API Latency Spikes at the Top of Every Hour
>
> Read latency on the catalog API jumps from 5 ms to 900 ms during the first two seconds of every hour, even though request volume stays flat. The spike happens because the response cache aligns every entry's expiration to the clock hour, causing hundreds of unrelated endpoints to miss the cache simultaneously and stampede the database.
>
> Each cache entry stores a serialized response paired with an expiration timestamp computed as `floor_to_hour(now) + 1h`. When a request hits an expired entry, the request handler queries PostgreSQL directly and writes the fresh response back to Redis. Because the handler does not coalesce concurrent misses for the same key, every in-flight request for that endpoint issues its own database query until the first write completes.
>
> At `XX:00:00`, every cached key crosses its expiration timestamp in the same millisecond. Request logs confirm the mechanism: cache-miss bursts cluster tightly between `XX:00:00.000` and `XX:00:01.800`, and PostgreSQL active connections hit the pool ceiling of 64 during the exact same window before dropping back to idle.
>
> Adding ±5 minutes of per-key jitter breaks the lockstep expiry: misses spread evenly across a ten-minute window, database concurrency stays below 4, and p99 latency remains flat across the hour boundary. This explanation accounts only for periodic hourly spikes; it does not cover cold-start latency after a full Redis flush during deployments, which invalidates all keys regardless of TTL jitter.

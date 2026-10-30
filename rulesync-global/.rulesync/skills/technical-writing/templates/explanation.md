# Technical Explanation Profile

*Read when writing a deep dive, a concept explanation, or an analysis of how a mechanism works.*

## Main Claim

The explanation gives the reader a model of why the system behaves as it does, clear enough to predict what happens when inputs, settings, or failures change.

### Example main claims

These examples connect an observed behavior to its cause. Use them as alternatives to adapt, not a checklist.

- "Hourly latency spikes occur because cache entries expire together and trigger concurrent database reads."
- "When retries lack deduplication, a timeout after commit can cause the same operation to run twice."
- "Queue latency grows when jobs arrive faster than workers can finish them."

## Sections

Write these sections in order.

### 1. The question

- State the behavior or concept being explained, and what the reader can observe.

### 2. Concepts first

- Define each concept before the explanation uses it, in the order the concepts depend on each other.
- Relate unfamiliar ideas to familiar ones, such as comparing a custom consensus protocol to Raft.

### 3. How it works

- Walk through causes and effects step by step, including the data that changes along the way.
- Connect each intermediate step to the result the reader sees.

### 4. Evidence

- Show short code traces, logs, or measurements that confirm the explanation.
- Include a counterexample that shows what happens when an assumption breaks.

### 5. Limits

- Say where the explanation stops applying, such as above a certain load or when a fallback takes over.

## Leave out

- Do not turn the explanation into step-by-step instructions. Link a how-to guide.
- Do not list every API member. Link the reference.
- Do not argue for a proposal or plan a roadmap.

## Example

A shortened explanation:

> The API slows down at the start of every hour because the response cache expires all of its entries at the same moment, and the next requests all miss the cache at once.
>
> Each cache entry holds a response and the time it expires. When a request misses the cache, its handler builds the response from the database and writes it back. The handler cannot tell that another request is already rebuilding the same entry.
>
> At the top of the hour, hundreds of entries expire together. Every request for those entries reaches the database in the same second, and database latency rises from 5 ms to 900 ms until the cache fills again. The request logs show the misses grouped within two seconds of each hour.
>
> If entries expired at random times within the hour, the misses would spread out and the database would never see the spike. This explanation holds only while entries share an expiry time, so it does not cover the slowdowns during deploys, which clear the whole cache.

---
name: fn-walk
description: >-
  Walks through a function or method phase by phase with focused, annotated code
  slices, causal explanations, and verified failure boundaries. Use when asked
  "/fn-walk", "walk through this code", "detailed walkthrough", or "step by step
  walkthrough".
targets:
  - '*'
---

# Function Walkthrough

Walk a reader through how a function actually works from start to finish, pairing focused slices of its real implementation with the invariants and failure behavior each slice enforces. Ground every statement in the source code and its callers.

## Investigation

1. **Read the complete implementation:** Inspect the target function and any helpers that own state transitions.
2. **Inspect callers:** Trace callers across the workspace to confirm what contracts downstream code relies on.
3. **Group by meaningful phases:** Divide the body into 2–5 logical phases where ownership, validation, or state transitions actually shift—never split trivial variable declarations into their own sections.

## How to Structure the Walkthrough

Write in active voice with natural articles for type names (`the WriteAheadLog`, `a Checkpoint`), compare against realistic failure modes rather than strawmen, use unnumbered domain-named headings, and omit meta signposting or `### Conclusion` recaps.

- **Opening summary:** Name the target function directly as the active subject (never `"This function..."`). State the problem it solves, the overall sequence of stages in plain words, and the contract callers rely on. When a function coordinates three or more interacting components or locks, a concise Mermaid `sequenceDiagram` (without `accTitle` or `accDescr`) may accompany the summary.
- **Phase sections (unnumbered, domain-named headings):** Name each `###` heading after the point of that phase—for example, `### Copying Live Records to a Temporary File` or `### Atomically Replacing the Active Log`—never numbered buckets like `### 1. Preflight Validation`. For each phase:
  - Open with a short paragraph stating what state this phase reads, stages, or mutates without re-arguing high-level motives already established in the opening.
  - Show only the relevant code slice, adding concise inline comments (`// ...`) solely for non-obvious invariants, ordering constraints, or failure paths—never narrate obvious syntax or number comments (`// 1.`, `// 2.`).
  - Close with what the phase guarantees for the next stage or for callers when it succeeds or fails.
- **Failure and recovery boundary:** End with a concrete domain-named section (for example, `### Crashes Before and After the Rename`) grounded strictly in the code's real `?` returns, mutations, and cleanup paths. State honestly if a mid-function failure can leave side effects behind.

## Example

Rewriting a write-ahead log in place risks corrupting persisted state if power drops halfway through stripping obsolete entries. `WriteAheadLog::compact` reclaims disk space while guaranteeing that recovery sees either the complete pre-compaction log or the complete compacted file.

### Copying Live Records to a Temporary File

`compact` opens a sibling temporary file beside the active log and streams only records newer than the latest durable checkpoint into it. Because the live log path stays untouched during the copy, concurrent readers and crash recovery continue seeing the original file if anything fails here.

```rust
let temp_path = self.dir.join("wal.compact.tmp");
let mut temp = File::create(&temp_path)?;

for record in self.read_since(checkpoint)? {
  record.encode_into(&mut temp)?;
}
// Flush data to storage before renaming so a crash cannot leave an empty file at the live path.
temp.sync_all()?;
```

Once `sync_all` returns, every surviving record is durable on disk inside `wal.compact.tmp`.

### Atomically Replacing the Active Log

With the temporary file flushed, `compact` renames it over the active log path and syncs the parent directory so the directory entry survives power loss.

```rust
let live_path = self.dir.join("wal.log");
fs::rename(&temp_path, &live_path)?;
self.sync_dir()?;
```

After `rename`, any new reader opening `wal.log` observes only the compacted snapshot.

### Crashes Before and After the Rename

Any I/O error or crash before `fs::rename` leaves `wal.log` untouched; startup deletes any leftover `wal.compact.tmp` before replaying records. If power fails between `fs::rename` and `sync_dir`, the file contents remain valid, though the filesystem may revert the directory entry back to the pre-compaction `wal.log`.

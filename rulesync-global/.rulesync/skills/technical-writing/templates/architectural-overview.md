# Architectural Overview Profile

*Read when writing or revising an overview of a system's architecture, a subsystem design, or a feature's design.*

## Main Claim

The overview shows how the system meets its requirements: what each part is responsible for, what data it owns, and how that data changes as the system runs.

Describe the architecture in the present tense, as it works. Avoid words such as "intended," "proposed," or "future," except for a migration that is still in progress and changes how callers use the system today.

### Example main claims

These examples connect architecture to a system requirement. Use them as alternatives to adapt, not a checklist.

- "Draft sync preserves locally saved edits across application restarts and sends them when the server becomes reachable."
- "Per-document queues preserve edit order while allowing unrelated documents to sync independently."
- "Versioned cache keys prevent responses from an earlier deployment from being served after a new version activates."

## Sections

Write these sections in order.

### 1. Scope

- Say what the system does for its users or callers.
- List its main parts and the external services it depends on.
- Define each unfamiliar term where it first appears.

### 2. Data and ownership

- Describe the main kinds of data, how each one is identified, and the rules that must always hold for it.
- For each kind of data, say which part creates it, changes it, and deletes it.
- Describe how the data is stored and sent: its format, where it lives, and whether it survives a restart.

### 3. Components and storage

- For each part, describe what it does, how other parts call it, and what it stores.
- Name the databases, caches, and schemas, and explain how parts are kept from interfering with each other.
- Explain why the main storage and boundary decisions were made.

### 4. How data moves

- Follow a request or input from where it enters, through each change, to where it is stored and how the response returns.
- Describe the states each piece of data passes through, the events that move it between them, and any background jobs.
- Explain how copies stay in sync: when caches refresh and what consistency callers can rely on.

### 5. Failures and concurrency

- Describe how work runs in parallel, such as separate workers that pass messages, thread pools, or locks.
- Describe what happens when part of the system fails, including timeouts, retries, and recovery.
- State the limits: how much work each queue holds and what happens when it is full.

### 6. How the parts work together

- Show how the parts combine to deliver what the system promises its callers.

## Leave out

- Do not include proposal history, rejected options, feasibility discussion, roadmaps, rollout checklists, or backlog items. These belong in an RFC or an implementation plan.
- Do not include function signatures or variable names unless they define a boundary between parts.

## Example

A shortened overview of offline draft sync in a text editor:

> Draft sync keeps a user's unsaved edits when the editor closes or the network drops, and sends them to the document server later. Three parts share this work: the editor, a local draft cache, and a sync worker that talks to the server's save API.
>
> Each pending edit is stored as a draft, identified by its document ID and a sequence number. The editor creates drafts, and the sync worker deletes them once the server confirms the save. No other part changes them. Drafts live as JSON files in the user's data directory, so they survive a restart.
>
> When the user types, the editor writes the edit to the draft cache before it changes the open document. The sync worker reads the cache in sequence order and sends each draft to the server. Because the cache keeps one file per document, a slow save on one document never delays another.
>
> A draft stays pending until the server confirms it. If a save fails, the worker leaves the draft in place and tries again after a longer delay each time. Only the worker sends saves, so two saves for the same document never run at once.
>
> Together, these rules mean an edit is never lost on its way to the server: it reaches the cache before the document changes, and it leaves the cache only after the server has it.

---
name: technical-writing
description: >-
  Author and revise technical documents, architectural overviews, RFCs,
  reference docs, READMEs, API docstrings, and rustdoc. Use when creating or
  editing standalone documentation files or docstrings. Source code comments
  follow the active language skill.
---
# Technical Writing

Write for technically proficient readers. When audience context is unstated, address a developer who understands general software engineering but does not know this repository. Ground audience assumptions in the prompt, existing documentation, or repository context, and state unverified assumptions explicitly.

## Objective and claims

Every document makes claims, including documentation that reads as neutral description. A reference page claims its options are complete and correct for a stated version. A how-to guide claims its steps produce the stated result. API documentation claims callers can rely on the described behavior. A document that never states its claims leaves the reader unable to tell what to trust or act on.

A **claim** is a statement the reader could doubt, so the document must support it. "The cache uses Redis" is a fact the reader can check directly. "The cache never serves stale data after a deploy" is a claim, because it needs evidence and reasoning.

Claims are explicit or implicit. An **explicit claim** appears in the text, such as:

- "These steps rotate the credentials without downtime."
- "Retrying a failed upload never creates a duplicate record."
- "The parser rejects input with an unknown version field."
- "Webhooks cut sync delay from 30 seconds to under 1 second."

An **implicit claim** is a point the writer wants the reader to conclude without stating it.

Leave implicit claims unstated, and show the evidence that supports them instead. Calling an implementation "clean" or "robust" asserts its quality without evidence. The reader should reach the conclusion from what the document shows:

| Implicit claim | Show instead |
| --- | --- |
| "This implementation is simpler than the one it replaces." | The change removes two locks and one background thread. |
| "The new API is a drop-in replacement for the old one." | Each old call maps to a new call with the same arguments, and the existing tests pass unchanged. |
| "We considered the obvious alternatives." | Each alternative is compared against the same criteria as the chosen design. |
| "You should use this library because its authors know what they are doing." | Benchmark results, production users, and documented failure handling. |

An omission can also create an implicit claim that the evidence does not support. A change description that lists only benefits implies the change has no costs. Leaving out a failure path implies the system cannot fail there. State the costs and failure paths the reader needs.

The **reader objective** is what the reader came to do. It selects the [document profile](#document-profiles) and decides which claims the document must make.

**Controlling claims** are what the reader must believe to meet the objective. Each profile template's Controlling Claim section gives the first one. A claim belongs only if the reader cannot meet the objective without accepting it.

**Supporting claims** are why the reader should believe a controlling claim. Each one proves one part of it and needs:

- **Evidence** the reader can check: facts, runtime behavior, schemas, code examples, or measurements.
- **A warrant** when the link is not obvious: the reasoning that explains why the evidence proves the claim.
- **A qualification** when the claim has limits: where it stops holding, attached to that claim instead of a generic disclaimer.

For a reader who must rotate production credentials:

- Objective: rotate the credentials.
- Controlling claim: "These steps rotate the credentials without downtime."
- Supporting claim: "The new credential works before the old one is revoked."
- Evidence: the step 3 command and its expected output.
- Warrant: the command authenticates with only the new credential, so its success proves the new credential works alone.
- Qualification: this holds only for clients that reread the secret on each connection.

Include only content that supports a controlling claim. A paragraph on why the team chose its secret manager supports no claim in the credential guide, so it belongs in an ADR.

## Workflow

When the user asks for review or advice, report findings and leave files unchanged.

1. **Survey existing documentation.** Search for pages that already cover the topic or depend on it. Update every affected page instead of duplicating its content. When no page covers the topic, add one and link it from the nearest index or parent page.
2. **Select a document profile.** Match the reader's objective to a row in the [document profiles](#document-profiles) table below, and read that profile's template.
3. **State the controlling claims.** Adapt the selected template's Controlling Claim section, such as the [architectural overview's](templates/architectural-overview.md#controlling-claim), to the subject, and add any claims the objective still needs, as [objective and claims](#objective-and-claims) shows. Write each claim as one sentence.
4. **Verify against sources.** Check behavior against source code, schemas, tests, traces, or runtime output before asserting it. For API documentation, inspect implementations, callers, and tests, and document only the caller contract.
5. **Outline and draft.** Order the content as the [reading path](#reading-path) describes, and write it under the [composition](#composition) rules.
6. **Check claims and links.** Confirm that every controlling and supporting claim has evidence, and that the document shows evidence for every implicit claim or drops the wording that implies it. Check terms, identifiers, examples, links, cross-references, and other pages that describe the same behavior. After structural edits, recheck scope statements, transitions, and prerequisite order.
7. **Proofread.** Reread the document against the global communication rules and the [composition](#composition) and [terminology](#terminology) sections. Repair sentence emphasis, modifier attachment, pronoun reference, tense, and needless words. Preserve the author's tone and the repository's house style.

## Document profiles

Each profile template defines its primary controlling claim, the sections to write, and what to leave out.

| Reader Objective | Document Profile |
| --- | --- |
| Understand a system or subsystem architecture | [Architectural overview](templates/architectural-overview.md) |
| Propose or evaluate a technical change or strategy | [RFC and proposal](templates/rfc.md) |
| Record an architecturally significant decision | [Architecture decision record](templates/adr.md) |
| Understand causal mechanics, mental models, or concepts | [Technical explanation](templates/explanation.md) |
| Execute an operational procedure or runbook | [How-to guide](templates/how-to.md) |
| Look up an exhaustive interface, CLI, schema, or error catalog | [Reference](templates/reference.md) |
| Learn a technology by building an end-to-end project | [Tutorial](templates/tutorial.md) |
| Predict how a public API, docstring, or endpoint behaves | [API documentation](templates/api-documentation.md) |

## Composition

- **Paragraph structure:** Open each expository paragraph with its point, support it in the body, and end on its strongest consequence or required transition. Connect paragraphs with explicit continuation, qualification, contrast, or causal transitions.
- **Sentence structure:** Keep subjects near their verbs, modifiers beside what they modify, and pronouns near clear antecedents. Put new or emphatic information at the end of the sentence or paragraph.
- **Tense and punctuation:** Keep each summary in one governing tense. Use the serial comma.
- **No decoration:** Omit emoji in headings and checklists.
- **No difficulty tiers:** Replace `basic`, `advanced`, `trivial`, or `complex` with the background the reader needs, phrased as "assumes a background in X" or "assumes familiarity with X".
- **Grounded nouns:** Replace `things`, `details`, and importance words such as `important`, `critical`, or `major` with exact domain entities and attributes.
- **Mapping rules over slogans:** State explicit conditions and consequences instead of aphorisms or circular qualifiers.
- **Plain wording:** Write literal statements instead of punchy phrasing. Avoid catchphrases such as "show, don't tell", figurative verbs such as "earn" or "unlock", and contrasts written for rhythm rather than meaning.


| Category | Before | After |
| --- | --- | --- |
| **Difficulty tiers** | *"This advanced tutorial is not for trivial tasks."* | *"This tutorial assumes a background in async Rust and Redis."* |
| **Generic nouns** | *"Show interactions between two important things."* | *"Show interactions between two components or stores."* |
| **Aphorisms** | *"Fail fast."* | *"Check the config file when the app starts, so a typo fails at launch instead of during a user's request."* |
| **Circular qualifiers** | *"Retry the requests that can be retried."* | *"Retry requests that fail with a timeout or a 503 status."* |
| **Punchy phrasing** | *"Keep implicit claims implicit and earn them: show, don't tell."* | *"Leave implicit claims unstated, and show the evidence that supports them."* |

## Terminology

- **Specialized terms:** Use one only when the reader needs it. Take it from the code, an API, or an established industry concept instead of inventing one. Define it where it first appears, before any claim that depends on it: say what kind of thing it is, what it does, and how it differs from similar terms.
- **Restricted terms:** Reserve `projection` for coordinate spaces. Replace `materialization` and `actor` with the exact role, such as `thread`, `task`, `worker`, `system`, `process`, or `pipeline`.
- **Identifiers:** For every public or persistent ID, key, hash, handle, URI, or path, state what it identifies, who creates and resolves it, where it stays valid, and what replaces it.
- **Durability words:** Treat `stable`, `persistent`, `durable`, and `permanent` as guarantees, and state when each one stops holding.

## Reading path

The claims set what the document must prove. The reading path sets the order in which the reader meets them.

- Put each definition, prerequisite, and supporting claim before the first claim that depends on it.
- Open every page with its scope, prerequisites, and result. Readers often arrive from a search or a link, not from the previous page.
- Within a section, state a component's purpose before its implementation, inputs before transformations, initial state before mutation, and each mechanism beside its failure behavior.
- Keep a heading only when readers navigate directly to its point. Use heading depth for containment, keep sibling headings at one level of abstraction, and name domain concepts instead of writing stages, such as "Cache invalidation" rather than "Step 2: Details".
- Give each topic one primary page and link to it from the others. Split a page when readers look up, update, or link its parts separately.

## Examples

Use minimal code or data examples that demonstrate one constraint or interaction. Include the required setup and omit non-functional boilerplate.

Read the [tables](references/tables.md) reference before adding or reviewing a table, and the [diagrams](references/diagrams.md) reference before adding or reviewing a diagram.

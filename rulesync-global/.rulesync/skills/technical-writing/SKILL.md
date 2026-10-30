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

## Claims

Technical writing asks readers to accept conclusions about a system: how it behaves, why a design satisfies its requirements, or whether a procedure achieves its goal. Treat those conclusions as **claims** that need support.

A **claim** states what the reader should accept as true; its support explains why. Support can come from a directly verifiable fact, a measurement, or reasoning that connects several pieces of evidence. The broader the assertion, the more conditions the document must account for.

Claims can be explicit or implied. An **explicit claim** appears in the text, such as:

- "These steps rotate the credentials without downtime."
- "Retrying a failed upload never creates a duplicate record."
- "The parser rejects input with an unknown version field."
- "Webhooks cut sync delay from 30 seconds to under 1 second."

Some claims are better conveyed through evidence than stated outright, especially when an explicit assertion would sound promotional. These **implied claims** express judgments the writer intends readers to reach for themselves.

| Implied claim | What to show instead |
| --- | --- |
| "This change improves performance." | Compare measurements before and after the change, and state the test conditions. |
| "This change reduces complexity." | Identify the dependencies, coordination steps, or background tasks it removes. |
| "The system is robust." | Show recovery behavior, its verification, and the conditions it does not cover. |
| "This design best fits the requirements." | Compare other alternatives, including its costs and limitations. |

When supporting an implied claim, account for conclusions the reader may draw beyond the one intended. *Omitting a tradeoff can make a recommendation appear stronger than the evidence supports.* Omitting a failure condition can make a guarantee appear unconditional. Include the conditions, costs, and limitations that keep the implied claim within what the evidence establishes.

The **reader objective** is what the reader needs to do, decide, or understand. Use it to frame the document: choose the [document profile](#document-profiles), identify the claims to establish, and determine what belongs in scope.

**Main claims** are the conclusions the document must establish to meet the reader's objective. They determine which supporting material belongs.

**Supporting claims** establish why a main claim holds. Each needs its own evidence and a clear connection to the main claim.

Both main and supporting claims can be explicit or implied. Support each with:

- **Evidence** the reader can check: facts, runtime behavior, schemas, code examples, or measurements.
- **A warrant** when the link is not obvious: the reasoning that explains why the evidence proves the claim.
- **A qualification** when the claim has limits: where it stops holding, attached to that claim instead of a generic disclaimer.

### Examples

For a reader who must rotate production credentials without interrupting service:

- Objective: rotate production credentials without interrupting service.
- Document profile: [How-to guide](templates/how-to.md), because the reader needs a procedure that produces a specific result.
- Main claim: "Following these steps with the stated prerequisites rotates production credentials without interrupting service."
- Supporting claim: "The new credential works before the old one is revoked."
- Evidence: the credential-check command and its expected output.
- Warrant: the command authenticates with only the new credential, so its success proves the new credential works alone.
- Qualification: this holds only for clients that reread the secret on each connection.

For a fix to out-of-order search responses:

- Objective: verify that the fix prevents older search results from replacing newer ones.
- Document profile: [Technical explanation](templates/explanation.md) in a code-review comment.
- Main claim: "This fix prevents stale search responses from overwriting the current results."
- Supporting claim: "The handler rejects responses whose request ID no longer matches the current request."
- Evidence: a regression test completes requests in reverse order. It fails before the fix and passes afterward.
- Warrant: the test reproduces the original failure, and the identity check prevents the older response from reaching the state update.
- Qualification: request IDs must remain unique while requests are outstanding.

## Workflow

Use this workflow when providing explanations to the user, or to develop new documents or assess existing ones. *When revising or reviewing documents, identify which claims, evidence, explanations, or structural relationships need attention within the existing document*. Frame your additions in this context, and prefer holistic document changes, where you may need to update prior claims rather than make narrow patches.

1. **Survey existing documentation.** Search for pages that already cover the topic or depend on it. Update every affected page instead of duplicating its content. When no page covers the topic, add one and link it from the nearest index or parent page.
2. **Select a document profile.** Match the reader's objective to a row in the [document profiles](#document-profiles) table below, and read that profile's template.
3. **Identify the main claims.** Adapt the selected template's Main Claim section and examples, such as the [architectural overview's](templates/architectural-overview.md#main-claim), to the subject and [reader objective](#claims). Include only claims that objective requires. Write each claim as one sentence to identify what the document must establish; not every claim needs to appear explicitly in the published document.
4. **Verify against sources.** Check behavior against source code, schemas, tests, traces, or runtime output before asserting it. For API documentation, inspect implementations, callers, and tests, and document only the caller contract.
5. **Outline and draft.** Order the content as the [reading path](#reading-path) describes, and write it under the [composition](#composition) rules.
6. **Proofread.**
   - **Claims:** Confirm that every main and supporting claim has evidence, whether stated explicitly or deliberately implied. Check for unsupported implications from wording, examples, or omissions, and revise the document to support, qualify, or remove them.
   - **Consistency and links:** Check terms, identifiers, examples, links, cross-references, and other pages that describe the same behavior.
   - **Structure:** After structural edits, recheck scope statements, transitions, and prerequisite order.
   - **Prose:** Review the prose against the global communication rules and the [composition](#composition) and [terminology](#terminology) sections. Repair sentence emphasis, modifier attachment, pronoun reference, tense, and needless words. Preserve the author's tone and the repository's house style.

## Document profiles

Each profile template describes its main claim, gives examples to adapt, and specifies what to include or leave out.

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
- **Buzzwords and promotion:** Replace vague labels and promotional claims with concrete mechanisms, measured results, or explicit guarantees. Keep technical terms when their precise meaning helps explain the subject.


| Category | Before | After |
| --- | --- | --- |
| **Difficulty tiers** | *"This advanced tutorial is not for trivial tasks."* | *"This tutorial assumes a background in async Rust and Redis."* |
| **Generic nouns** | *"Show interactions between two important things."* | *"Show interactions between two components or stores."* |
| **Aphorisms** | *"Fail fast."* | *"Check the config file when the app starts, so a typo fails at launch instead of during a user's request."* |
| **Circular qualifiers** | *"Retry the requests that can be retried."* | *"Retry requests that fail with a timeout or a 503 status."* |
| **Punchy phrasing** | *"Keep implied claims implied and earn them: show, don't tell."* | *"Support implied claims with evidence rather than promotional descriptions."* |
| **Buzzwords** | *"Our battle-tested, fault-tolerant architecture delivers rock-solid reliability."* | *"The queue retains unfinished jobs and reassigns them when a worker disconnects."* |


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

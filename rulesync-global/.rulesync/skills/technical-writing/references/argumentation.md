# Argumentation

*Read when defining the controlling claim, connecting evidence to a point, qualifying a claim, evaluating alternatives, or separating explicit reasoning from hidden conclusions.*

## Treat Every Document as a Supported Claim

Every technical document makes a claim that can succeed or fail:

- An RFC claims that a proposal best satisfies stated pressures.
- An architectural overview claims that an operating model explains the system and its boundaries.
- An ADR claims that one decision responds appropriately to its recorded context.
- An implementation plan claims that its sequence can deliver and verify the target state.
- A how-to guide claims that its instructions produce the stated result under defined conditions.
- Reference documentation claims that it describes an interface accurately and completely.
- A tutorial claims that its sequence develops a capability through a successful experience.
- A technical explanation claims that a mechanism or causal model explains observed behavior.

The document contract determines what counts as support. Do not force alternatives into reference documentation or turn an RFC into a procedural checklist.

## Connect Claim, Evidence, and Warrant

A claim tells the reader what to accept. Evidence supplies the concrete facts, representations, behavior, examples, or measurements supporting it. A warrant explains why that evidence supports the claim.

```text
Claim:
One authoring source can serve visual, textual, and automated editing.

Evidence:
Each client modifies the same source declarations and uses the same generated schema.

Warrant:
Sharing declarations and validation removes the need to synchronize independent representations.
```

Do not confuse the warrant with a favorable adjective. “The workflow is maintainable” evaluates the result. “A schema migration updates one authoritative representation rather than three synchronized models” explains why the evidence matters.

State the warrant when the relationship could reasonably be disputed, when it selects an architectural decision, or when omitting it would leave the reader with facts but no conclusion. Leave only genuinely shared assumptions implicit.

## Keep the Necessary Why Explicit

State these forms of reasoning directly:

- Why the reader or system needs the capability
- Why a pressure selects a decision
- Why a mechanism satisfies a requirement
- Why evidence establishes a point
- What consequence follows
- Where the claim stops holding

Let the reader infer qualitative evaluations only after the explicit chain supports them.

## Qualify Claims at Their Boundary

Name the operating conditions under which a claim holds. A cache may produce constant-time lookups only after indexing. A source patch may preserve comments for literal replacement but require mediated editing for a generated comprehension.

Attach the qualification to the affected claim. Do not weaken the entire document with a generic disclaimer.

## Address Alternatives When They Affect Acceptance

Present a counterargument or alternative when the document contract requires evaluation or when a credible alternative exposes an important tradeoff.

Use one shared requirement:

1. Describe the alternative mechanism and its consequence.
2. Describe the selected mechanism at the same level of detail.
3. Explain why the recorded pressures select one consequence over the other.
4. Preserve drawbacks and conditions that remain after selection.

Do not add an alternative solely to praise the selected design. Do not compare detailed evidence for one design with a label applied to the other.

## Use Hidden Conclusions Without Hiding the Argument

Hidden conclusions guide evidence selection and organization at three levels:

- A document-level conclusion shapes the qualities the complete model must earn.
- A section-level conclusion keeps one architectural subject focused on a meaningful result.
- A paragraph-level conclusion selects the mechanics and consequences that belong together.

Keep the controlling claim, goals, requirements, warrants, and concrete consequences explicit. The hidden conclusion names the qualitative judgment that emerges after the reader follows that reasoning.

```text
Explicit argument:
Goal → pressure → decision → mechanism → evidence → warrant → consequence

Organizing lens:
Which qualitative conclusions should that evidence earn?
```

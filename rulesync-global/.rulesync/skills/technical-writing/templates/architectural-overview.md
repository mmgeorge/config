# Architectural Overview Profile

*Read for an architecture overview or whenever a system or feature design request does not name another document contract.*

## Controlling Claim

Establish that one complete operating model explains how the system satisfies its goals, divides authority, represents its domain, and moves information through its lifecycle.

Treat that operating model as the **golden design**. Describe settled architecture in present tense as fully realized behavior regardless of implementation progress. Do not qualify the model as intended, proposed, aspirational, or incomplete merely because the current repository has not reached it.

## Reasoning Obligations

- State the operational goal and central design early.
- Present every settled part of the golden design as the system's operating contract.
- Define unfamiliar or specialized concepts when they first appear.
- Select an orienting whole and an explanatory arc that give every detail a visible role.
- Define owners, boundaries, identities, representations, and invariants.
- Explain the warrants behind major architectural decisions.
- Keep every declared architectural inventory internally complete.
- Trace the interactions, transformations, lifecycle, and failure behavior needed to understand the model.
- Demonstrate non-obvious representations with compact examples.
- Organize evidence so the intended hidden conclusions become justified.
- Reconstruct any whole the document decomposes, showing how its established parts produce the intended behavior.

## Default Progression

Open with the complete system at an operational scale so readers understand what it enables, why it matters, and which major parts participate. Define every unfamiliar term used in that orientation, but defer internal mechanics until the detailed explanation requires them.

Use whole-part-whole when readers can recognize that system before learning its internals, the whole gives several parts their purpose, and a later synthesis can reveal how those parts produce its behavior. Otherwise select the bottom-up, top-down, linear, or causal arc that matches the subject. Explain detailed concepts in dependency order and define the complete domain model before editors, compilers, cookers, APIs, or other projections that consume it.

Adapt headings to the architecture. Do not expose this progression as a generic sequence of `Goal`, `Model`, or `Mechanism` headings.

## Exclude by Default

Do not add implementation-status disclaimers, rejected alternatives, feasibility studies, generic open questions, implementation phases, rollout checklists, or backlog. Those belong in an RFC or implementation plan unless the architecture cannot be understood without a local qualification.

## Completion Check

A reader should be able to describe what the system must accomplish, which subsystem owns each responsibility, how the central objects relate, how those parts produce the complete operating model, how inputs become outputs or runtime state, and why the major boundaries satisfy their pressures.

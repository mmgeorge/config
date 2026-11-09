# Plan references: removal and restoration

This note records why Forge plans no longer store `Calls`, `Constructs`, and `Accesses`, what the removal changed, and how jump-to-code returns without bringing back stored references. It assumes familiarity with the plan tools (`plan_read`, `plan_edit`, `plan_submit`) and with `DeclarationDesign` in `forge-harness/src/plan/design.rs`.

## Why the sections went

The reference sections made the planner maintain a second copy of facts that the code and the Algorithm already carry. Capture extracted every call, construction, and access from existing bodies, including duplicates, locals, compound expressions, and `<unresolved>` entries, and showed them in the text the agent edited. The agent then had to keep those lists consistent with its Algorithm, and validation rejected mismatches.

The test-bevy run in session `4e76e3df` (state v15) shows the cost. Of 219 seconds, about 31 went to tools and almost none to reasoning (1,116 reasoning tokens against 9,537 output tokens). The rest went to output, retries, and bookkeeping. Three failures traced directly to references:

- A patch failed on a context mismatch because the agent reproduced an `Accesses` list in the wrong order.
- The agent began rewriting a reference list and abandoned the rewrite midway.
- The agent added wrong `Accesses` entries solely to satisfy the construction and access rules.

None of that work improved the plan. The sections recorded what the implementer derives from the Algorithm anyway, and they cost the planner tokens, patch failures, and rejections over entries it never chose to write.

## What changed

Plans now hold exactly one behavior block per callable: `Algorithm` for a new callable, `Change` for an existing one. The removal reaches every layer, so no reference data survives in text, JSON, rendering, or validation.

| Layer | Before | After |
| --- | --- | --- |
| Agent text | Signatures, behavior blocks, and captured `Calls`/`Constructs`/`Accesses` | Signatures and behavior blocks; `calls::parse` rejects a written reference section at its line |
| Stored JSON | `FunctionBody` with `call`, `evidence`, and per-site positions | `FunctionBody { owner, behavior }`; capture leaves `baseline_calls` empty |
| Capture | Classified every baseline call against the snapshot (about 1.3 s for an uncaptured file) | Extracts declaration text only and caches it per source digest |
| Review | Reference sections, grouped `Type { member }` rows, closed folds, Algorithm call coloring | Behavior blocks only, open folds, no Algorithm call coloring |
| Validation | Construction, access, and Algorithm-reference rules (`usage.rs`) | Behavior-block kind and Algorithm syntax only |
| Implementation report | Compared planned and observed calls | Compares declarations only |
| Rename | Rewrote signatures and Algorithm uses | Rewrites signatures only |

The removal deleted `plan/usage.rs`, the `AlgorithmAccess`, `AlgorithmConstruction`, and `AlgorithmReference` diagnostic kinds, `CallSite`, `CallPosition`, `ReferenceEvidence`, `calls::classify`, `declared_algorithm_call`, and the grouped-access renderer. `storage::state::VERSION` moved to 16, so v15 sessions stay with the editors that own them.

Validation is accordingly weaker, and deliberately so. No rule now requires an Algorithm to name only real declarations, because an Algorithm legitimately mixes repository names with abstract operations such as `keyboard_direction(input)`, and any rule strict enough to catch a misspelled `Draft::revison` also rejects the abstractions that keep pseudocode readable.

### What remains

The search and navigation machinery stayed intact, because restoring jump-to-code needs all of it:

- `forge_diff::syntax::DeclarationCalls` still extracts references from executable source.
- The declaration resolver (`declaration/mod.rs`: `callable`, `construct`, `property`, `value`) still resolves qualified names against the design and the workspace.
- `forge_diff::syntax::algorithm_reference` still parses the identifier under the cursor in an Algorithm.
- `calls::resolve_algorithm_reference` now resolves on demand and on a best-effort basis. A signature binding resolves to its parameter; a qualified name, a capitalized type or constructor, or a member of a single-typed binding yields a `Reference { name, kind }` for the resolver; every unqualified lowercase call yields nothing.
- `PlanReviewTarget::Call`, `CallKind`, workspace-definition fallback, and column-ranged navigation anchors remain, although no producer currently sets anchor columns.

## Restoring jump-to-code

The design rule for restoration is that references are derived when asked for and never stored. A stored reference must be kept in sync with the text it describes; a derived one cannot drift, and it costs the planner nothing.

### One derivation function

Add one function, `references::derive(design, path, owner) -> Vec<DerivedReference>`, with two sources selected by the callable's state:

1. **Existing code.** For a callable whose body exists in the workspace, run `DeclarationCalls::extract` over the baseline source. These references are exact, and they serve unchanged callables and the baseline side of a changed one.
2. **Planned code.** For a callable with an `Algorithm`, walk its identifiers with `algorithm_reference` and pass each through `resolve_algorithm_reference` and the resolver. These references are best effort.

Each `DerivedReference` carries the name, `CallKind`, the row and byte columns of the identifier, and the resolver's outcome. Cache the result in memory, keyed by the proposed-source fingerprint that `ParsedDeclarations` already records, so the cache invalidates itself on every edit and never reaches persisted state.

### Abstract operations

An Algorithm name that resolves to nothing gets no link and no diagnostic. That rule keeps `keyboard_direction(input)` legal without a matching function, and it makes the derived set a strict subset of what the reviewer can trust. A lowercase name that happens to match a real function also stays unlinked unless it is qualified or reached through a typed receiver, because linking it guesses at intent the author never stated. Revisit that choice only with evidence that reviewers expect such links.

### Restored features

Once `derive` exists, three removed features return without new storage:

- **Jump to uses.** Reverse navigation asks `derive` for every callable in the design and filters by resolved destination, replacing the occurrence loop that `PlanReferenceIndex` used to build from stored call sites.
- **Algorithm call coloring.** `design_review` colors an Algorithm identifier as a call only when `derive` resolves it, so abstract operations still read as plain identifiers. The derived columns populate `PlanNavigationAnchor::columns`, which the review renderer already turns into per-entry targets.
- **Algorithm-use rename.** `PlanReferenceIndex::renamed` rewrites each derived occurrence whose destination is the renamed declaration, using the derived columns rather than text search.

### Optional warnings

Two warnings would catch real mistakes without blocking submission, and both belong in the successful `plan_submit` result rather than in a rejection:

- An Algorithm uses a qualified name or typed member that the resolver cannot find, which usually means a misspelling or a missing declaration.
- The plan removes a declaration that existing code, according to `DeclarationCalls`, still uses.

Neither warning should become an error. Each depends on best-effort resolution, and a rejection the agent cannot satisfy reproduces exactly the bookkeeping loop this change removed.

## Follow-up housekeeping

The academic-pseudocode skill source at `rulesync-global/.rulesync/skills/academic-pseudocode/SKILL.md` no longer mentions reference sections, but the generated skill files are stale until the next global generation. `rulesync generate --global` also rewrites `.codex/AGENTS.md` and `.codex/config.toml`, so run it deliberately and review those diffs.

Two documents describe this behavior and must change with any restoration: `nvim/contributing/forge/architecture/planning.md` (plan storage, Algorithm navigation, folds) and `nvim/docs/forge/commands.md` (PlanReview navigation and folding).

Restoration needs tests in three places:

- A unit test that `derive` links qualified and typed-receiver names and leaves unqualified lowercase calls unlinked.
- A design test that an edit invalidates the derived cache.
- A review test that Algorithm call coloring appears only on resolved identifiers.

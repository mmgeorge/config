---
root: false
targets:
  - '*'
description: Organize TSX and JSX modules as scannable top-down component trees with cohesive ownership.
globs:
  - '**/*.tsx'
  - '**/*.jsx'
---
# TSX and JSX Component Organization

Organize component modules so a reader encounters the rendered feature first, then each visual section and behavioral dependency in `caller -> callee` order. The file should read like the component tree rather than a collection of unrelated declarations.

## Declaration order

After imports, use this strict priority:

1. Type aliases, interfaces, and enums
2. All module-level `const` declarations, public and private
3. Public classes
4. Public functions
5. Private classes
6. Private functions

Within each tier, preserve `caller -> callee` flow. Place a parent component before the child components, hooks, formatters, and helpers it invokes.

Prefer one consistent component declaration form within a component tree. When the strict declaration tiers would force a child above its parent, use `const` components for both so the public root and its private descendants can remain in visual dependency order.

## Component tree

- Keep the exported root component focused on composing the feature's major visual regions.
- Extract a section when it has a distinct visual role, owns an isolated behavior, or simplifies parent JSX structure.
- Name extracted components after the user-visible role they render, such as `DatasetSelectionPanel`, `MapPanelHeader`, or `ColumnFacts`.
- Declare private child components beneath the first parent component that renders them. Order siblings by their rendered order.
- Continue recursively from each child into its own dependencies so the module reads top-down as `Foo -> FooHeader -> FooHeaderAction`.
- Do not extract wrapper components that only rename one element, forward unchanged props, or hide a mount node without adding ownership or behavior.
- Split components before their render logic combines multiple independently understandable regions, state machines, or interaction lifecycles.

## Props and domain objects

- Pass cohesive domain objects instead of flattening every field into a separate prop.
- Group related operational values into explicit objects when they move together, such as session state, layout state, view state, or file structure state.
- Keep discriminated unions intact instead of replacing them with loosely related booleans and nullable fields.
- Avoid React context when data crosses only one or two explicit component boundaries. Props preserve dependency visibility and simplify isolated tests.
- Do not create one-off prop bags that merely hide unrelated parameters. Every grouped object must represent a real domain concept or ownership boundary.

## State, effects, and hooks

- Keep state and effects with the smallest component or hook that owns their lifecycle.
- Extract a custom hook when setup, cleanup, subscriptions, derived state, or imperative integration forms a cohesive behavior independent from presentation.
- Return domain-level state and actions from hooks. Do not expose incidental implementation details that couple the caller to the hook's internals.
- Keep imperative resources such as maps, views, observers, and event handles behind the component or hook responsible for creating and disposing them.
- Preserve required mount elements in the owning component when extracting them would obscure lifecycle ownership.

## Scannability

- The root component should reveal the page or panel's major parts without forcing a reader through implementation details.
- Keep conditional rendering close to the visual region it controls. Extract repeated or multi-state branches into a named component.
- Replace deeply nested ternaries with an explicit state-routing component when branches render distinct UI layouts.
- Prefer direct composition over layers of forwarding components.
- Remove obsolete feature flags, permanently enabled branches, and redundant wrappers while refactoring.

## Review checklist

- Does the file follow the strict declaration priority?
- Does each tier read in `caller -> callee` order?
- Does the component order mirror the rendered component tree?
- Can the root component be scanned as a short list of major visual regions?
- Does each extracted component or hook own a coherent responsibility?
- Do props expose domain boundaries rather than flattened implementation fields?
- Do state, effects, and imperative resources remain with their lifecycle owner?
- Did the refactor reduce unnecessary structural complexity rather than merely move lines?

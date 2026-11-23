---
name: typescript
description: >-
  Design, implement, refactor, review, and test TypeScript code. Use when
  working on .ts, .tsx, .mts, .cts, or tsconfig.json files, TypeScript type
  modeling and JSDoc, or TypeScript type-checking and test checks.
---

# TypeScript

## Required conventions

- **Parse at the boundary:** Avoid `any`. Accept external data as `unknown`, parse it in the receiving adapter, and keep `unknown`, `object`, and `Record<string, unknown>` out of internal contracts.
- **Validate once:** Enforce invariants in parsers or static factories with private constructors.
- **Unions over strings:** Prefer literal `type` discriminated unions.
- **Narrow, do not cast:** Avoid `instanceof`, `as`, and `!` (except `as const` and brand casts inside validating factories).
- **Flat flow and single-owner async:** Return early, drop `else` after a return, skip single-line wrapper helpers, and centralize complex async mutations behind one owner.

## Naming

- **Methods:** Use `nounVerb` (`sourceGet`, `sourceRemove`), omitting the noun when the receiver makes the target clear (`get`). Put context parameters first.
- **Types and fields:** Name fields by contents (`sources`, `assets`, never `data`), name monotonic counts `<domain>Count`, and skip `I` or `Typed` prefixes.

## Annotations

- **Standalone functions:** Declare explicit return types on named functions, methods, and standalone arrow functions.
- **Contextual closures:** Omit parameter and return annotations on callbacks and typed object-literal methods, placing types on the receiving signature or interface instead.

```ts
// Standalone function: explicit return type.
function displayName(user: User): string {
  return user.nickname ?? user.name;
}

// Contextually typed callback and interface method: omit redundant annotations.
const match = sessions.find((session) => session.id === id);
const provider: ExtentProvider = {
  getFullExtent: async () => fullExtent,
};
```

## Code Comments

- **Doc comments (`/** */`):** Document every function and every exported type, interface, class, and constant using the `technical-writing` API profile. Open function and method docs with `Get`, `Create`, or an imperative verb (never `Return`/`Returns`), omit `{type}` tags, and place comments above all decorators.
- **Inline comments (`//`):** Add only when correctness depends on non-obvious context (invariants, `await`/event ordering, browser or API quirks, non-obvious optimizations), placed above the statement and naming the invariant and failure prevented.
- **Phase labels:** In 40+ line functions spanning multiple phases (validation, mutation, cleanup), mark each phase with a short imperative state-boundary comment.

```ts
// Capture the revision before awaiting, or a concurrent save can publish a newer one first.
const revision = this.revision;
await this.store.write(draft, revision);
```

## Testing

- **Integration first:** Favor integration or screenshot tests covering lifecycles, persistence, concurrency, and UI or API boundaries, mocking external services or requests.
- **Avoid unit tests**, using them **only** for parsers, codecs, and algorithms.
- **Keep tests simple**: avoid loops that create many tests dynamically. Keep tests readable and scannable.

---
name: typescript
description: >-
  Design, implement, refactor, review, and test TypeScript code. Use for .ts, .tsx,
  .mts, or .cts files, type modeling with unions and generics, async state
  coordination, typed boundaries, and tsconfig type-checking behavior.
---

# TypeScript

## Type safety

Unvalidated input must not reach internal consumers. Validate at entry and carry domain types through internal contracts.

- Never use `any`. Accept untrusted input as `unknown`; parse it in the receiving adapter and keep wire spellings there.
- Make values valid by construction. Validate once in a parser or static factory, keep raw constructors private, and avoid downstream revalidation.
- Narrow instead of using `as` or non-null `!` assertions. Allow only `as const` and brand casts inside constructors that validate the brand's invariant.
- Model closed cases with literal `type` discriminants and case-specific payloads, not string tags, boolean flags, or co-dependent optional fields. Keep shared required identity non-optional.
- Narrow with discriminants or type guards, never `instanceof`; class identity requires runtime imports and can fail across duplicate packages.
- Handle unions exhaustively with an `assertNever(value: never): never` fallback.
- Represent open external vocabularies with explicit `{ type: "missing" }` and `{ type: "unknown"; raw: string }` cases, not catch-alls that defer parsing.
- Before using `string`, check whether a literal union or discriminated union can represent the domain values. Use unions for known cases; reserve plain strings for genuinely free-form text or raw input at external boundaries.
- Use validated branded types for structured values such as identifiers and units, so values sharing a primitive cannot be confused. Plain aliases do not provide this distinction.
- Represent absence with `undefined` or optional properties, never sentinels.
- Use `readonly` for properties and arrays callers must not mutate, and `satisfies` to check object literals without losing literal types.
- Return domain types, not strings consumers must decode. Never pass `unknown`, `object`, or `Record<string, unknown>` onward as an internal domain payload.
- Preserve persisted and wire representations during refactors.

## Encapsulation

A class that exposes its internals makes every caller responsible for its invariant. Each class owns its invariant, and callers change state only through methods that keep it valid.

- Keep invariant-bearing state in private fields. Expose `readonly T[]`, `ReadonlyMap`, or iterators instead of the mutable collection.
- Combine check-then-act into one operation. State can change while an `await` is pending between the check and the action.
- Complete every fallible or awaited step before mutating state, so a rejection leaves the object unchanged.
- Tie paired operations, such as subscribe and unsubscribe, to a `Disposable` acquired with `using` when the project's TypeScript and runtime support it. Otherwise release them in `finally`.

## Naming

Name methods `nounVerb`, placing the target before the operation. Omit the noun when the receiver makes the target unambiguous (`get`); otherwise, qualify only as much as needed (`sourceGet`, `assetGet`, `sourceAssetGet`). Keep target names consistent across related operations (`sourceGet`, `sourceSet`, `sourceRemove`).

- Name fields by their contents (`sources`, `assets`), never `data`. Name monotonic counts `<domain>Count`.
- Use camelCase for values, functions, and methods, and PascalCase for types, interfaces, and classes.
- Put context parameters first.
- Do not prefix interface names with `I` or type names with `Typed`.
- Preserve names required by external interfaces, framework conventions, overrides, and wire formats.

## Errors

- Never throw strings or plain objects. Throw `Error` subclasses, or return a discriminated result union when callers branch on the failure.
- Preserve the underlying failure with `new Error(message, { cause })`.
- Render error text only at a UI, log, command, or transport boundary.

## Async

Centralize related state changes so their ordering and interactions are easy to reason about. In complex async flows, have handlers record intent and let one coordinated update path apply changes. Consider commands with completion promises when the state owner should control execution order while callers await results. Use command queues, update passes, or schemas where they clarify that coordination.

## Control flow

- Return early from terminal paths. Drop the `else` after a returning branch.
- Never add a helper that only forwards a call.

## Annotations

Declare explicit return types on named functions, methods, and arrow functions that lack a contextual function type, so a changed body cannot silently change the contract:

```ts
function displayName(user: User): string {
  return user.nickname ?? user.name;
}

const isActive = (session: Session): boolean => session.state.type === "active";
```

Never annotate parameters or return types on contextually typed closures, including callbacks passed as arguments and functions inside typed object literals. Put the types on the receiving interface or function signature instead. This rule takes precedence over the explicit-return-type requirement.

Bad:

```ts
const match = sessions.find((session: Session): boolean => session.id === id);
```

Good:

```ts
const match = sessions.find((session) => session.id === id);
```

For an object property, declare the contract once:

```ts
interface ExtentProvider {
  getFullExtent(): Promise<Extent | null>;
}

const provider: ExtentProvider = {
  getFullExtent: async () => fullExtent,
};
```

## Documentation

- Give every function a concise `/** */` comment that states its responsibility, contract, or non-obvious reason to exist. Document every exported type, interface, class, and constant.
- Start doc comments with `Get` for accessors, `Create` for constructors and factories, and a precise imperative verb otherwise. Never start with `Return` or `Returns`.
- Do not restate types in doc tags. Omit `{type}` annotations, and write `@param` or `@returns` only when it adds a contract the signature does not express.
- Place every doc comment above all decorators on its class, member, or property. Never place it between a decorator and the declaration.
- Use the technical-writing skill's API Documentation profile for doc comments.

```ts
/** Render a provider update for the transcript view. */
@customElement("provider-update")
export class ProviderUpdateElement extends LitElement {
  /** First adapter receipt time, retained through queueing and forwarding. */
  @property({ type: Number })
  receivedAt?: number;
}
```

## Comments

Prefer types, names, and control flow over comments. Add an inline comment only when correctness depends on context the code cannot express.

- Comment domain rules, ordering requirements across `await` or event boundaries, external API or browser quirks, and performance choices that look wrong without context.
- Place the comment directly above the statement or branch that enforces the constraint. State the invariant and the failure it prevents.
- Never narrate what the syntax already says, such as `// Increment the retry count`.
- In a function of 40 or more nonblank lines that combines at least two of validation, state mutation or publication, and reconciliation or cleanup, label each phase with an imperative comment that names its state boundary. Do not append formulaic `so ...` clauses.
- Use `//` line comments. Reserve block comments for ordering rules with several preconditions.
- Delete a comment when a refactor moves its constraint into a type or API. Recheck nearby comments after changing the code they describe.

```ts
// Capture the revision before awaiting, or a concurrent save can publish a newer one first.
const revision = this.revision;
await this.store.write(draft, revision);
```

# Testing

Sparingly use add unit tests for algorithms, parsers, and codecs only. Mock external boundaries when isolation proves the owned logic.

Most tests should be integration, include tests on coverage of lifecycle, persistence, concurrent handoff, plugins, rendering, and host/library boundaries.

When running tests, prefer a name filter or a cached fixture.

## Verification

- Validate in the order type check, test, read-only format check, lint, build. Run the project's existing type-check script, or its configured `tsc --noEmit` command, first, and fix type errors before running tests.
- Finish source-changing preparation, such as code generation, before validation starts. Repeat the affected checks after any later source change.
- Run the type check, lint, tests, and build through the project's package scripts with the same `tsconfig`, package, and working directory as the development build. Lint and test the same affected package instead of widening to the whole workspace.

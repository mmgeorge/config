---
name: typescript
description: >-
  Design, implement, refactor, and review TypeScript code. Use when changing .ts, .tsx,
  .mts, or .cts files, type definitions, unions, generics, or tsconfig type-checking
  behavior.
---

# TypeScript engineering

## Type safety

A value typed `any`, a cast, or a `string` that stands for a closed set compiles while deferring the failure to runtime. Encode each invariant in a type so the type check rejects misuse.

- Keep `strict` enabled. Never weaken compiler options to make code pass.
- Make values valid by construction. Validate once in a parser or static factory, keep the raw constructor private, and let consumers trust the resulting type without rechecking it.
- Never use `any`. Type untrusted input as `unknown` and narrow it with a parser or type guard where it enters the program.
- Never silence the checker with `as` casts or non-null `!` assertions. Narrow with control flow, a literal discriminant, a type guard, or a validating parser. `as const` is allowed.
- Narrow with a `type` property typed as a literal union, such as `type: "raster" | "vector"`, or with a type guard, not `instanceof`. Never type the discriminant as `string`. `instanceof` needs a runtime import of the class, which creates a hard module dependency, pulls the class and its imports into the bundle, and fails across duplicate package copies.
- Model a closed set of cases as a discriminated union on a `type` property typed as a literal union. Never use a `type: string` field or an object of optional fields interpreted by type.
- Replace boolean flags and co-dependent optional fields with a union whose members hold exactly the valid combinations.
- Handle unions exhaustively. End each `switch` with `assertNever`, so a new member fails the type check at every consumer.
- Brand identifiers and units that share a primitive, so swapped arguments fail to compile. A plain `type SessionId = string` alias does not count, because it stays interchangeable.
- Represent absence with `undefined` or an optional property, never a sentinel such as `-1` or `""`.
- Mark properties and arrays `readonly` when callers must not mutate them.
- Check object literals against a type with `satisfies` when widening would lose literal types.

A union whose consumer cannot forget a case:

```ts
type RequestState =
  | { type: "pending" }
  | { type: "ready"; body: Uint8Array }
  | { type: "failed"; error: RequestError };

function assertNever(value: never): never {
  throw new Error(`Unhandled case: ${JSON.stringify(value)}`);
}

function label(state: RequestState): string {
  switch (state.type) {
    case "pending":
      return "Waiting";
    case "ready":
      return `${state.body.length} bytes`;
    case "failed":
      return state.error.message;
    default:
      return assertNever(state);
  }
}
```

A branded identifier that a plain string cannot satisfy:

```ts
type SessionId = string & { readonly brand: unique symbol };

function parseSessionId(raw: string): SessionId | undefined {
  return /^[a-z0-9-]+$/.test(raw) ? (raw as SessionId) : undefined;
}
```

The brand constructor is the one place a cast is allowed, because it runs the validation the brand promises.

A discriminant check needs only a type import. Bad: the value import loads `RasterLayer` and everything it imports:

```ts
import { RasterLayer } from "./layers/raster-layer";

if (layer instanceof RasterLayer) {
```

Good: the check reads a property, and `import type` disappears at compile time:

```ts
import type { RasterLayer } from "./layers/raster-layer";

function isRasterLayer(layer: Layer): layer is RasterLayer {
  return layer.type === "raster";
}
```

## Typed boundaries

Stringly typed contracts compile while hiding unhandled cases, misspelled kinds, and missing fields until runtime. Every module, adapter, event, and message boundary carries typed values.

- Parse external data inside the adapter that receives it. Never type a `JSON.parse` result, `fetch` body, or message payload with `as`. Wire spellings never cross the adapter boundary.
- Return typed values from interfaces and contracts, never strings callers parse or compare.
- Never use `Record<string, unknown>`, `unknown`, or `object` as a contract payload.
- Keep identity that every case requires non-optional.
- Give a field with an open external vocabulary explicit `{ type: "missing" }` and `{ type: "unknown"; raw: string }` members. Never add a catch-all member that defers parsing to consumers.
- Never change a persisted or wire representation as a side effect of a refactor.

## Encapsulated state

A class that exposes its internals makes every caller responsible for its invariant. Each class owns its invariant, and callers change state only through methods that keep it valid.

- Keep invariant-bearing state in private fields. Expose `readonly T[]`, `ReadonlyMap`, or iterators instead of the mutable collection.
- Combine check-then-act into one operation. State can change while an `await` is pending between the check and the action.
- Complete every fallible or awaited step before mutating state, so a rejection leaves the object unchanged.
- Tie paired operations, such as subscribe and unsubscribe, to a `Disposable` acquired with `using` when the project's TypeScript and runtime support it. Otherwise release them in `finally`.

## Errors

- Never throw strings or plain objects. Throw `Error` subclasses, or return a discriminated result union when callers branch on the failure.
- Preserve the underlying failure with `new Error(message, { cause })`.
- Render error text only at a UI, log, command, or transport boundary.

## Control flow

- Return early from terminal paths. Drop the `else` after a returning branch.
- Never add a helper that only forwards a call.

## Annotations

Declare the return type of every named function, method, and arrow function assigned to a variable or property, so a changed body cannot silently change the contract:

```ts
function displayName(user: User): string {
  return user.nickname ?? user.name;
}

const isActive = (session: Session): boolean => session.state.type === "active";
```

Never annotate an inline closure passed as an argument. The receiving signature already types its parameters and return value, and an annotation can only drift from it.

Bad:

```ts
const match = sessions.find((session: Session): boolean => session.id === id);
```

Good:

```ts
const match = sessions.find((session) => session.id === id);
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

## Verification

- Validate in the order type check, test, read-only format check, lint, build. Run the project's existing type-check script, or its configured `tsc --noEmit` command, first, and fix type errors before running tests.
- Finish source-changing preparation, such as code generation, before validation starts. Repeat the affected checks after any later source change. Run the formatter in check mode, such as `prettier --check`, so validation never rewrites files. If a formatting fix changes source, restart from the type check.
- Run the type check, lint, tests, and build through the project's package scripts with the same `tsconfig`, package, and working directory as the development build. Lint and test the same affected package instead of widening to the whole workspace.
- Use isolated unit tests with mocked external boundaries for parsers, state machines, and error decisions. Use real modules for integration coverage of lifecycle, persistence, and rendering boundaries.
- Select the smallest test coverage that exercises the change. Run individual tests through the test runner's name or file filter, batching related tests in one invocation. Escalate to the whole package only when individual tests cannot cover the affected behavior, and to the workspace only for demonstrated cross-package impact or an explicit user request. State the reason before broadening. A passing targeted test, a type error, or an unknown test name does not justify a broader run.
- For a slow test, prefer a name filter or a cached fixture. Run an unavoidable slow suite with an explicit timeout.
- Report the executed command, timeout, exit status, execution duration for slow runs, and the specific error output or generated artifact.

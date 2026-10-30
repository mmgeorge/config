# Typed boundaries

Read before defining or changing traits, adapters, events, protocol messages, or code that parses external data.

## Normalize inside the adapter

Each adapter owns one external vocabulary. It parses wire values, provider status spellings, JSON paths, and envelope quirks into the shared typed contract before emitting anything. Consumers of the contract never inspect raw provider data.

Place provider-specific parsing in the adapter that receives the data, not on the shared type. A shared `ToolStatus::parse` that accepts every provider's spellings couples the contract to all of them. Each adapter instead maps its own spellings to `ToolStatus` and reports an unrecognized value as a typed error.

Keep provider-only messages inside the adapter. An envelope that carries no consumer-visible content, such as a tool-only message with empty text, emits only its meaningful events.

## Shape the contract enum

Give each case exactly the payload it needs. When cases share an identity, carry it on every case rather than on an optional field of a wrapper struct, as `SKILL.md`'s `BackendEvent` example shows.

Nest a second enum for a case's lifecycle updates instead of adding flags or optional fields. Box a large payload when `clippy::large_enum_variant` fires. Do not suppress the lint.

Keep the enum exhaustive. A catch-all `Other(serde_json::Value)` variant moves parsing into every consumer and hides new provider behavior from the compiler.

## Represent open vocabularies explicitly

When an external field has values the adapter cannot enumerate, model the observed states:

```rust
pub enum MessagePhase {
  Commentary,
  FinalAnswer,
  Missing,
  Unknown(String),
}
```

Consumers handle `Missing` and `Unknown` deliberately. Unknown content stays visible without being treated as a recognized phase.

## Separate observations from notifications

Provider observations, internal state changes, and the external wire protocol are distinct contracts. Do not reuse a provider event type as an editor or client notification. Translate at the owner that publishes each one, and validate the outgoing wire shape against its schema.

## Parse representations at entry

Parse serialized identifiers, paths, and protocol strings when they enter the domain. Use an enum for mutually exclusive roles instead of repeating prefix, suffix, or delimiter checks.

Keep syntax markers at parsing and transport boundaries. Retain both raw and parsed forms only when the same owner needs both and preserves their consistency.

## Remove string escape hatches

Do not implement `Deref<Target = str>`, `PartialEq<&str>`, or `AsRef<str>` on a typed value to keep string comparisons compiling. Render strings with `Display` or `as_str` only at a log, UI, or wire boundary. Migrate every consumer to the typed value in the same change.

## Verify the boundary

Test each adapter with recorded provider payloads, including unknown values, missing fields, and empty envelopes. Assert the typed events it emits, not intermediate JSON.

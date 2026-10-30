# Local text service

The two text methods require host readiness and forbid a session identifier. They use ordinary admission and no command recovery. They neither initialize Harness session services nor contact a provider.

## `text.initialize`

Load the bundled English model, or explicitly supplied unigram/bigram files, and atomically publish an isolated terminology profile. Dictionary loading and validation run on an owned blocking worker. A failed replacement leaves the prior profile usable.

The request contains `profile` (a nonempty string), optional `terminology` (individual words), and optional `assets` with `unigram` and `bigram` filesystem paths. The response contains `profile`, `profile_revision`, `model_revision`, and `dictionary_revision`. Both revisions identify immutable content. Later requests for the same profile supersede earlier initialization work.

The Lua adapter merges its configured wordlist file, user terminology, and project terminology into `terminology` before initialization. Those explicit words cannot be replaced by another spelling; the two capitalization rules still apply. File loading stays asynchronous and does not change the host method schema.

## `text.correct`

Evaluate one completed lexical word and return either `unchanged` or `replace`. The request contains `profile`, `profile_revision`, `occurrence`, `context`, `target`, and optional `protected` ranges. `target` and every protected range use zero-based half-open UTF-8 byte offsets into `context`. The service rejects invalid boundaries, protected targets, technical-token targets, unavailable profiles, and stale profile revisions.

Both result variants contain the `decision` tag, occurrence and profile identities, and engine revisions. `replace` also contains `original`, `replacement`, and `reasons`. Reasons are `spelling`, `contextual_choice`, `standalone_i`, and `sentence_start`. The result never rewrites a sentence or authorizes an edit outside the target.

Transport errors preserve the existing editor text. The client validates ownership and source text again before applying a replacement. Initialization and correction are independent of the active session, and the generic client must not add session context or `_command` metadata.

The authoritative request and result types are [contract.rs](../../../rust/forge/crates/forge-text/src/contract.rs). The [model manifest](../../../rust/forge/crates/forge-text/assets/manifest.json) records the packaged assets, preprocessing assumptions, and frozen scoring parameters. Complete development and held-out results are in the [evaluation record](../../../rust/forge/crates/forge-text/evaluation/README.md).

## Captured exchanges

The [native-host fixture capture](examples/text-exchange.json) contains actual client specifications and decoded results for initialization, a contextual replacement, and an unchanged decision. The capture preserves model/profile revisions and the occurrence tokens; transport request identifiers belong to the existing envelope and are not part of these method parameters.

`text_host.lua` recreates this capture against the rebuilt executable. It also checks cold-start typing, Backspace rejection, forbidden session context, provider independence, and host shutdown.

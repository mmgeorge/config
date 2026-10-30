# Local text service

The two text methods require host readiness and forbid a session identifier. They use ordinary admission and no command recovery. They neither initialize Harness session services nor contact a provider.

## `text.initialize`

Load the bundled English model, or explicitly supplied unigram/bigram files, and atomically publish an isolated terminology profile. Dictionary loading and validation run on an owned blocking worker. A failed replacement leaves the prior profile usable.

The request contains `profile` (a nonempty string), optional `terminology` (individual words), and optional `assets` with `unigram` and `bigram` filesystem paths. The response contains `profile`, `profile_revision`, `model_revision`, and `dictionary_revision`. Both revisions identify immutable content. Later requests for the same profile supersede earlier initialization work.

The Lua adapter merges its personal and programming wordlist files, user terminology, and project terminology into `terminology` before initialization. These words are protected from frequency-based spelling replacements. Explicit `im` and negative-contraction expansion and capitalization still apply. File loading stays asynchronous and does not change the host method schema. `forge.text.wordlist` also publishes the merged vocabulary as a content-addressed, plain-word cache file for Harper's `userDictPath`. Cache publication writes a temporary file before renaming it, so Harper never reads a partial dictionary.

The Harper adapter collapses its three dictionary-add commands into one global action per word. The wordlist owner serializes asynchronous append operations, preserves source comments, rejects unsaved source-buffer conflicts, and invalidates correction profiles after a successful addition. The adapter refreshes each running Harper client and republishes current buffer text because Harper's configuration-change handler rereads files from disk. Saving the source wordlist uses the same refresh path. The source vocabulary remains under the dotfiles checkout, while Harper's generated copies stay in Neovim's cache.

`forge.text.harper_rules` owns the repository's `nvim/spell/harper-rules.json` overrides.
It reads complete JSON asynchronously, validates rule-to-false entries, and serializes
atomic replacements under an exclusive writer lock. Unsaved settings buffers and I/O
failures reject the write without changing the prior rule file. `harper.disabled_rules`
selects the path or disables this feature with `false`.

The Harper adapter retains current, eligible diagnostic metadata and offers one global
disable action per overlapping string `code` from source `Harper`. Messages, numeric
codes, other servers, stale diagnostics, and protected regions cannot authorize a rule
change. Action records retain the buffer generation, changed tick, rule name, and settings
path. Only a successful save triggers `refresh_settings`, shared with dictionary reloads.
That path overlays persisted disabled rules onto configured linters for every active
client and republishes unsaved text. Client startup reads the same overrides, and saving
the rule file in Neovim refreshes clients so deleting an entry restores configured/default behavior.

## `text.correct`

Evaluate one completed lexical word and return either `unchanged` or `replace`. The request contains `profile`, `profile_revision`, `occurrence`, `context`, `target`, and optional `protected` ranges. `target` and every protected range use zero-based half-open UTF-8 byte offsets into `context`. The service rejects invalid boundaries, protected targets, technical-token targets, unavailable profiles, and stale profile revisions.

Both result variants contain the `decision` tag, occurrence and profile identities, and engine revisions. `replace` also contains `original`, `replacement`, and `reasons`. Reasons are `spelling`, `contextual_choice`, `standalone_i`, `first_person_contraction`, and `sentence_start`. The result never rewrites a sentence or authorizes an edit outside the target. Model revision `en-US-bigram-3` forces eligible `im` to `I'm` and restores apostrophes in the explicit negative-contraction set documented in [Local text correction](../../../docs/forge/text-correction.md#typing-and-rejection). It protects dictionary-recognized contractions before frequency ranking, including curly-apostrophe forms. First-person contraction capitalization applies independently of sentence position.

Transport errors preserve the existing editor text. The client validates ownership and source text again before applying a replacement. Initialization and correction are independent of the active session, and the generic client must not add session context or `_command` metadata.

Lua owns delimiter corrections independently of `text.correct`. On a typed space
or newline, context capture can propose replacing a letter-adjacent trailing slash
with `?`, then checks the proposed context against the existing protected ranges.
Other slashes and technical-token markers remain protected. The transaction
changes only that delimiter after validating its anchors and cursor, and retains
the original slash for Backspace rejection alongside any later spelling edit.

The authoritative request and result types are [contract.rs](../../../rust/forge/crates/forge-text/src/contract.rs). The [model manifest](../../../rust/forge/crates/forge-text/assets/manifest.json) records the packaged assets, preprocessing assumptions, and frozen scoring parameters. Complete development and held-out results are in the [evaluation record](../../../rust/forge/crates/forge-text/evaluation/README.md).

## Captured exchanges

The [native-host fixture capture](examples/text-exchange.json) contains actual client specifications and decoded results for initialization, a contextual replacement, and an unchanged decision. The capture preserves model/profile revisions and the occurrence tokens; transport request identifiers belong to the existing envelope and are not part of these method parameters.

`text_host.lua` recreates this capture against the rebuilt executable. It also checks cold-start typing, Backspace rejection, forbidden session context, provider independence, and host shutdown.

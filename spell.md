# Contextual spell correction implementation plan

## Scope and architectural delta

Dictionary-only correction can replace valid technical terms and select the wrong word even when a complete sentence is supplied. Implement sentence-aware candidate ranking behind Forge's local Rust host, with Lua owning eligible prose, input boundaries, reversible edits, and stale-response rejection.

This plan records the behavior agreed in the current discussion. Creating this file does not implement the feature. The initial language is US English. Evaluate Windows and macOS using the same Rust and Lua implementation.

### Required behavior

- Evaluate every eligible completed word in sentence context, including correctly spelled dictionary words. Dictionary membership is a scoring signal, not an admission filter.
- Preserve the original word as a candidate. Evaluation can return an unchanged result.
- Automatically apply the chosen correction to the target word. Immediate Backspace provides the rejection mechanism.
- Apply only two capitalization rules: change standalone prose `i` to `I`, and capitalize the first non-whitespace character at a sentence start when that character is alphabetic.
- Skip sentence-start capitalization when that first character is a digit, quote, punctuation, or another nonalphabetic character. Do not search farther into the sentence for a letter.
- Do not add automatic proper-name, acronym, brand, or title capitalization. Preserve existing casing outside the two rules.
- Preserve punctuation, whitespace, Markdown syntax, comment delimiters, code, paths, and all other bytes outside the target edit.
- Keep correction local. Do not send typed text to an AI provider or add a provider-backed per-word generation request.
- Keep ordinary typing asynchronous. Never wait for a process, dictionary load, or scorer in a key handler.

The automatic feature operates on words completed by typed textual boundaries. It does not rewrite a whole document on opening, saving, committing, or submitting a prompt. Submission is not a textual delimiter insertion. Its capture must invalidate pending edits so no correction changes text after the user submits it.

### Verified starting state

The current checkout already uses `require("forge.client").request(spec, callback, progress)`. The specification contains `method`, optional `params`, and optional explicit `session_id`. Its contract determines readiness, admission, and recovery. Do not implement against the earlier `request_host` proposal or reintroduce it.

The inspected protocol contract is version 29. Read the current version again before adding endpoints. The feature belongs to the general host and requires no interactive Harness session or provider initialization.

The separate evaluation project at `D:/scratch/symspell-eval` tested `symspell_rs 7.0.1`. Its `sentence-results.txt` and `observed-output.txt` provide experimental evidence, not an implementation contract:

- Fifteen of sixteen handwritten common-typo sentences had the intended wording with the supplied English dictionaries.
- `releive pressure` became `receive pressure`, including with the official bigram dictionary.
- Valid-word errors such as `form the server` and `greater then zero` remained unchanged.
- Compound correction removed final punctuation and changed paths and inline code.
- Malformed dictionary frequency text panicked, and incomplete rows could silently succeed.
- Dictionary initialization took approximately 575 ms for unigrams and 62 ms for bigrams in that Windows run.
- The reported 1.7 microsecond warm lookup p95 excluded context scoring, Lua, IPC, parsing, and buffer edits.

Do not use `lookup_compound` as a document transformer. The earlier scratch README's suggestion to automatically correct only known typo mappings is superseded by the every-word contextual requirement here.

## Ownership and affected modules

Keep the engine independent of editor buffers, Git repositories, GitHub resources, and Harness session execution. One shared immutable base model serves the host. User and project terminology use explicitly identified profile overlays so one project's terms do not affect another project accidentally.

Existing paths below are relative to this checkout. Proposed paths are implementation destinations, not files that already exist.

| Owner | Existing boundary or proposed module | Responsibility |
| --- | --- | --- |
| Rust engine | Proposed `nvim/rust/forge/crates/forge-text/` | Candidate generation, sentence/token boundaries, contextual ranking, capitalization, model profiles, and typed results. |
| Rust model assets | Proposed `forge-text/assets/` and model manifest | Versioned vocabulary and context counts with source revision, checksum, preprocessing, and license. |
| Shared runtime | [runtime.rs](nvim/rust/forge/src/runtime.rs) | Own `TextCorrectionService`, its workers, model initialization, and shutdown. |
| Host dispatch | [router.rs](nvim/rust/forge/src/router.rs) | Decode typed `text.initialize` and `text.correct` requests and return typed results. |
| Protocol policies | [protocol_contract.json](nvim/lua/forge/protocol_contract.json), [forge-protocol/lib.rs](nvim/rust/forge/crates/forge-protocol/src/lib.rs) | Declare host-only method policies and advance the shared wire version. |
| Existing transport | [client.lua](nvim/lua/forge/client.lua), [protocol.lua](nvim/lua/forge/protocol.lua) | Preserve request correlation, callback delivery, and startup policy. |
| Lua facade | Proposed `nvim/lua/forge/text/init.lua` | Set up correction and attach supported editor owners. |
| Lua input lifecycle | Proposed `text/input.lua` | Observe typed boundaries, track request generations, and compose Backspace handling with existing mappings. |
| Lua prose classification | Proposed `text/prose.lua` | Resolve editable prose and protected spans from syntax captures and explicit Forge regions. |
| Lua context capture | Proposed `text/context.lua` | Capture the current sentence, target range, source mapping, and context identity. |
| Lua edit transactions | Proposed `text/transaction.lua` | Apply one reversible target edit and reject it without undoing unrelated typing. |
| Lua request lifecycle | Proposed `text/service.lua` | Use the current unified request API, initialize model profiles, and distinguish errors from unchanged results. |
| Configuration | [infra/config.lua](nvim/lua/forge/infra/config.lua), Forge plugin setup | Add one cohesive `text_correction` configuration object and validate it before attachment. |
| Source/Markdown eligibility | [blink.lua](nvim/lua/plugins/blink.lua), parser/annotation consumers | Share prose eligibility where valid and preserve explicit `nospell` precedence. |
| Native editable regions | [editable.lua](nvim/lua/forge/editable.lua), [review_document.lua](nvim/lua/forge/review_document.lua) | Validate the whole replacement against the current authoritative editable region. |
| Prompt input | [harness/layout.lua](nvim/lua/forge/views/harness/layout.lua), [prompt_submission.lua](nvim/lua/forge/views/harness/prompt_submission.lua) | Attach to `ForgeHarnessInput` and invalidate pending corrections before submission capture. |
| Commit messages | [integrations/commit.lua](nvim/lua/forge/integrations/commit.lua) | Attach to the actual `gitcommit` input and preserve generated commentary and diff boundaries. |
| Documentation | `nvim/docs/forge/`, `nvim/contributing/forge/` | Document the exact typing, rejection, terminology, and failure behavior. |

Do not introduce forwarding wrappers, duplicate language models in Lua, or a second long-lived process. The proposed crate isolates text processing from the Forge runtime. Its dependency points inward from the host to the engine.

## Correction contract and engine design

### Contextual scoring is a prerequisite

SymSpell supplies lexical candidates. A separate scorer must evaluate those candidates in context. Adding sentence text to the request does not supply that scorer automatically.

Implement a first prototype using a smoothed, interpolated word n-gram language model and a spelling-error prior. Use adjacent-word counts from the verified bigram asset as the first baseline and evaluate whether a pinned trigram corpus is required. Record the chosen data source, redistribution permission, revision, checksums, tokenization, smoothing, and vocabulary before production integration. Do not assume the upstream bigram asset is a complete sentence model.

For each candidate, score the sentence with that candidate substituted only at the target. Combine contextual likelihood with a prior for retaining or editing the typed spelling. The prior must account for edit type and distance. Raw word frequency alone is not the correction score. Keep an identity prior for the original word so frequent alternatives do not rewrite already correct prose indiscriminately.

An n-gram scorer needs only the n-grams affected by the target substitution. Unaffected sentence terms cancel in a comparison. Capture the whole available sentence, but do not claim that this local-context model provides general sentence semantics or grammar checking.

Evaluate and freeze model parameters on development examples, then evaluate separately on held-out examples. Publish corrections, missed corrections, and unintended corrections separately. Do not tune the model against the held-out results and continue reporting those results as independent evidence.

The first implementation phase is a quality gate. It must correct the mandatory contextual regressions and preserve their valid controls before the editor adapter is enabled. If the n-gram baseline fails, replace the scorer behind the same engine interface and rerun the same tests. Do not silently fall back to checking unknown words only or to applying the first SymSpell suggestion.

### Candidate generation

Use the verified `symspell_rs 7.0.1` API as the initial dependency baseline. Validate compatibility with the repository's pinned Rust toolchain before committing the dependency. Include the selected dictionary and context-data licenses.

Use `Verbosity::All` rather than `Top` or `Closest` when collecting alternatives for a target. Begin with dictionary edit distance two and no result-count truncation. Add a regression proving that an exact dictionary hit still exposes nearby candidates. If the dependency's implementation suppresses those neighbors, fix the candidate-generation boundary rather than restoring a dictionary-membership admission gate.

The edit-distance setting defines which spelling alternatives the algorithm models. It must not become a word-length or document-size limit. Keep the original word explicitly, even when it is absent from the dictionary.

Preserve apostrophes within words and preserve Unicode boundaries. Model ordinary contractions as lexical spelling candidates where supported by the licensed vocabulary. Do not split code identifiers, paths, URLs, or command references into prose candidates.

Keep existing casing on spelling candidates, including uppercase and mixed-case terms, except where the two explicit capitalization rules apply. A context score must not introduce a third capitalization rule.

### Sentence and source coordinates

Use Unicode word and sentence boundaries as the Rust tokenizer baseline, with tested handling for abbreviations, decimal points, ellipses, and protected technical spans. The verified `unicode-segmentation` crate implements Unicode Annex #29 boundaries. Check its selected version against the pinned toolchain. Add domain handling where general Unicode sentence boundaries do not distinguish a filename or abbreviation correctly.

Lua retains an incrementally updated paragraph/prose-region index. Capture the surrounding paragraph and resolve the target sentence without rescanning the entire buffer on each word. Continue reading context in chunks until the actual boundary is found. Do not truncate a sentence or disable correction because it is long.

For multiline comments, build a prose context that excludes structural comment markers while retaining a source-range mapping. Preserve semantic punctuation such as an opening quotation mark. The target range in the request uses zero-based, half-open UTF-8 byte offsets into the context string. The anchored edit range remains in Neovim buffer coordinates. Test their conversion across non-ASCII text and multiline comments.

Use all context already present in the sentence, including right-hand text when the user edits an existing sentence. During forward typing, future words are unavailable. Include left-prefix-only cases in the quality gate. Do not evaluate complete sentences in tests and claim the same evidence is available while typing their first words.

### Profiles and initialization

Load and validate assets off the editor thread and outside the asynchronous host input loop. Validate dictionary rows before passing them into SymSpell so malformed counts or indices cannot panic the shared host.

Build a replacement engine profile separately and publish it atomically after validation. Keep the previous usable profile on reload failure. A profile records the immutable base model revision and its explicitly configured user/project terminology. Backspace rejection does not modify that terminology or train a permanent correction map.

Package a reproducible offline baseline. Runtime typing must not depend on downloading dictionaries or a remote model. If model preparation needs a shared script, implement it in Nushell and validate it with `nu-check`.

## Host API

Add two general text-service methods to the existing contract. Both use `session_context: none`, `readiness: host`, `admission: ordinary`, and `recovery: service`. They must not start Harness session services, a provider, or a background-generation conversation.

| Method | Request | Result |
| --- | --- | --- |
| `text.initialize` | Configuration/profile identity and validated asset/terminology configuration. | Ready profile identity, model revision, and dictionary revision, or a structured initialization failure. |
| `text.correct` | Profile identity, context text, target byte range, protected byte ranges, and target occurrence token. | Discriminated `unchanged` or `replace` result with the same target identity and engine revisions. |

The response to `text.correct` contains only a target replacement, never a rewritten sentence. A `replace` result includes the exact original target and replacement, with reasons drawn from spelling, contextual choice, standalone `i`, and sentence-start capitalization. A combined spelling/capitalization correction is one transaction.

Validate ranges, UTF-8 boundaries, profile identity, and protected-span exclusion in Rust. Return failures for unavailable profiles, invalid input, failed assets, and execution errors. Do not encode those conditions as successful `unchanged` responses.

Lua invokes `client.request` with a method specification and no `session_id`. Keep transport request IDs separate from the editor's occurrence/generation tokens. The existing callback already correlates the wire response. The additional editor identities protect buffer lifetime and source text, not transport delivery.

Store candidate caches by normalized word and dictionary revision. Store contextual decisions, if cached, by the complete relevant context, target, profile, and model revision. A word-only correction cache would discard the contextual behavior. Cache unchanged decisions too. Evict cache entries without reducing the content that can be processed.

Initialize the profile when an eligible prose buffer becomes active so the first Space does not trigger synchronous loading. Initialization remains asynchronous. Until initialization finishes, preserve the user's original text. Do not replace editor text with a loading placeholder.

Advance the shared wire version from its current value when adding the methods. Update the Rust constant and JSON contract together. Do not hardcode a version based on this plan's snapshot if another change has already advanced it.

## Editor eligibility and input lifecycle

### Eligible surfaces

| Surface | Correction region | Protected content |
| --- | --- | --- |
| Source buffers | Comment prose identified by current syntax information. | Executable code, literals outside prose, identifiers, snippets, URLs, and paths. |
| Markdown | Paragraph, heading, list-item, and other explicit prose bodies. | Fenced/indented code, inline code, link destinations, autolinks, frontmatter keys/data, and embedded language regions. |
| Git commits | Editable commit subject and message body. | Git-generated commentary, scissors trailers, displayed diffs, and technical tokens. |
| PR descriptions/comments | Current editable draft regions. | Read-only review sections, generated diff content, and protected Markdown spans. |
| Harness prompts | `ForgeHarnessInput` prose owned by the active composer. | Slash commands, attachment/reference tokens, paths, inline code, and fenced code. |

Share a typed prose-context result across correction and applicable completion consumers. `@nospell` takes precedence over `@spell`. The existing Blink predicate is a starting point, not proof that an entire Forge-generated buffer has an attached Neovim parser.

For native Forge buffers, use the owning document's editable-region and syntax metadata. Revalidate the whole edit through `editable.guard_region` immediately before applying it. Never enable correction solely because a buffer is modifiable or has a Forge filetype.

When syntax classification is temporarily unavailable, preserve the current buffer and wait for an authoritative region result. Do not treat unknown syntax as unrestricted prose. Explicit plain-prose owners can provide eligibility without a language parser.

Structural list/comment markers are outside the prose body. The sentence-start capitalization rule examines the first non-whitespace character of the actual prose sentence. An opening quote or number inside that prose sentence causes capitalization to be skipped as agreed.

### Boundary capture

Start with typed Space and cover punctuation, Tab, and inserted newline boundaries through the same lifecycle. A token already evaluated at a boundary does not require a second identical request when a following Space supplies no new context.

Observe typed-input provenance rather than treating every text change as a new word. Do not automatically rewrite paste, generated output, imported drafts, macros, undo, redo, or a completion engine's internal edits. A later ordinary typed delimiter can evaluate the word the user is actively completing.

`InsertCharPre` can record printable delimiters but cannot edit under textlock. Consume the boundary after insertion using the relevant text-change events, including completion-visible behavior. Handle Enter according to the owning buffer's existing semantics. In a composer where Enter submits rather than inserts text, invalidate pending requests before the existing submission captures text.

Create one buffer-owned occurrence record with its owner identity, host generation, profile revision, original target/context, delimiter, extmarks, and monotonic editor generation. Treat a lexical occurrence as a range identity, not a spelling string.

### Asynchronous apply boundary

Apply a result only while the original boundary remains current in the same active Insert-mode owner and the cursor remains immediately after its delimiter. A later edit, navigation, mode change, submission, detached owner, or host replacement invalidates application. Do not change an earlier word behind the user's cursor after they have continued typing.

This rule preserves a predictable immediate-Backspace rejection target. It also makes end-to-end latency a release criterion. Record completed evaluations whose results were too late to apply rather than counting them as successful inline corrections.

At application time, validate extmark positions, exact original target, relevant context, delimiter, profile/model revision, and region ownership. A global changedtick equality test alone is insufficient to express those identities. Capture current cursor/view state immediately before the synchronous commit and preserve it within that commit.

Perform one `nvim_buf_set_text` replacement of the anchored target. Do not move the cursor through Normal-mode spelling commands or replace the whole line. Guard the engine's own edits so they do not schedule another correction. Preserve normal undo/redo without blindly joining unrelated undo blocks.

Use cancellation or obsolescence generations to stop or discard work whose input is no longer current. Do not add a transport-wide cancellation refactor solely for this feature. The current unified client does not expose a generic cancellation handle. Keep worker ownership until any noncancelable dependency call actually settles.

## Backspace rejection state machine

Immediacy is defined by input order, not an elapsed-time threshold. An idle pause does not expire the last correction. A later edit or navigation does.

| State | Event | Transition and effect |
| --- | --- | --- |
| Tracking | The user completes an eligible word. | Capture the occurrence and request contextual evaluation. |
| Awaiting result | The user presses Backspace before a result applies. | Cancel that occurrence's pending application and perform ordinary Backspace. |
| Awaiting result | A current `unchanged` result arrives. | Retain text and return to tracking. |
| Awaiting result | A current replacement arrives. | Apply one transaction and arm rejection. |
| Awaiting result | The input/owner becomes obsolete. | Discard application and preserve the user's newer text. |
| Rejection armed | The user presses Backspace as the next edit. | Restore the original target, consume the triggering delimiter, and suppress that occurrence. |
| Rejection armed | The user types more, navigates, leaves Insert mode, or submits. | Disarm interception. Backspace retains its ordinary meaning. |
| Occurrence suppressed | The user retypes a delimiter after the restored word. | Preserve the rejected original. Do not submit it for another automatic replacement. |
| Occurrence suppressed | A distinct next word is completed. | Evaluate that new occurrence normally. |

The Space lifecycle must produce these exact states, where `|` denotes the cursor:

1. Typing `Please recieve|` and pressing Space produces `Please receive |` when the scorer chooses that correction.
2. Pressing Backspace next restores `Please recieve|`.
3. Pressing Space again produces `Please recieve |` without reapplying the correction.
4. Completing the next word resumes evaluation normally.

Store the original target, replacement, delimiter range, occurrence identity, and post-apply generation in the transaction. Rejection edits that range directly instead of calling general undo, which could delete unrelated typing. Suppression follows the restored occurrence through its delimiter retry and expires when its lifecycle ends. It does not globally whitelist the word or permanently disable correction.

Use the same transaction for capitalization. For example, `i|` plus Space becomes `I |`, and immediate Backspace restores `i|`. Combined corrections such as a sentence-initial misspelling must restore the complete original target in one rejection.

For inserted punctuation or a newline, record the actual triggering delimiter and remove only that insertion when rejecting. Preserve any punctuation already present before the trigger. Verify line-join behavior explicitly for newline rejection.

Compose a buffer-local Backspace handler with the effective existing mapping, completion/snippet behavior, and owner cleanup. Intercept only an armed transaction. Every other Backspace must follow the original behavior. Detaching correction restores the previous mapping without overwriting a newer mapping installed by another owner.

## Capitalization rules

Apply capitalization in Rust after candidate selection so spelling and capitalization return one replacement. Use Unicode alphabetic classification and Unicode case conversion, retaining correct byte accounting when conversion changes the number of bytes or scalars.

| Input condition | Required behavior |
| --- | --- |
| Standalone eligible prose `i` | Return `I`. |
| An alphabetic first non-whitespace character at an actual sentence start | Uppercase that character only. |
| Leading whitespace before that character | Preserve whitespace exactly. |
| First non-whitespace character is a quote, digit, parenthesis, or punctuation | Skip sentence-start capitalization. Do not scan past it. |
| A period in a protected URL, path, decimal, or abbreviation | Do not infer a new sentence from that period alone. |
| Proper name, acronym, brand, or title casing | Apply no additional capitalization rule. |
| Code, identifiers, or other protected spans | Apply neither capitalization rule. |

The feature does not add grammar/style rewriting or a dedicated contraction-capitalization rule. New capitalization behavior requires a separate design change.

## Implementation sequence

### 1. Freeze contracts and prove contextual ranking

1. Recheck the current unified client, method contract, wire version, and editor adapters before editing.
2. Extend the separate evaluation project with every-word candidate generation, a contextual scorer, retained punctuation, and both complete-sentence and typing-prefix fixtures.
3. Add correctly spelled controls and technical prose alongside genuine errors such as `recieve`, `recieved`, `releive`, `definately`, `seperate`, `occured`, and `dependancies`.
4. Add valid-word contrasts such as `form/from`, `then/than`, and `loose/lose` in contexts where each original spelling is also correct.
5. Verify `Verbosity::All` includes nonidentity alternatives for correctly spelled targets and record candidate recall independently from ranking accuracy.
6. Freeze the model/data manifest, preprocessing, scoring/prior parameters, and held-out results before production integration.

This phase completes only when the mandatory contextual pairs pass without rewriting their valid controls. The handwritten 32-sentence sample alone does not establish general correctness. Failure of this gate blocks enabling the editor feature, not evaluation of all words.

### 2. Implement the engine and model lifecycle

1. Add `forge-text` to the Rust workspace and implement typed profile, context, target, and decision models.
2. Implement candidate generation, contextual scoring, deterministic capitalization, and offset validation as independently testable modules.
3. Implement validated streaming asset loading and atomic immutable-profile publication.
4. Add revision-aware candidate/context caches and explicit failure/cancellation outcomes.
5. Add local configuration and terminology-profile loading without automatic learning from rejection.

### 3. Integrate the shared host

1. Add the service to `ForgeRuntime`, including worker ownership and shutdown handling.
2. Add the two typed host routes and the exact method-policy entries.
3. Advance the shared wire version and update raw protocol fixtures.
4. Verify that correction initialization and evaluation never initialize Harness session services or a provider.
5. Verify that text-service failures remain service failures and do not invalidate an unrelated Harness session.

### 4. Implement the Lua input and transaction lifecycle

1. Add the buffer-owned controller, prose/context capture, occurrence identities, and asynchronous request handling.
2. Add target-only edits with current-source validation and self-edit suppression.
3. Add immediate-Backspace rejection, delimiter restoration, and occurrence suppression.
4. Add composition with existing mappings and cleanup on buffer/owner disposal.
5. Add host-restart, mode-change, submission, and cursor-movement invalidation.

### 5. Attach supported surfaces and complete rollout

1. Attach ordinary comments and Markdown through current syntax/prose information.
2. Attach actual commit input, PR draft regions, and Harness composers through their owning lifecycle.
3. Share the eligibility predicate with Blink where the same semantics apply, without requiring a parser in explicitly owned prose inputs.
4. Invalidate pending corrections before every existing submit/save operation captures and dispatches its content.
5. Update configuration documentation and validate isolated Windows and macOS editor sessions before enabling normal automatic correction.

## Verification matrix

Use the existing checks and execution order in [Building and testing](nvim/contributing/forge/building-and-testing.md). Do not add a new runner, alter automatic coverage, or run full workspace tests merely because the feature spans modules.

Finish source-changing preparation before validation. Run affected compiler/type checks first, targeted tests second, read-only formatting third, lint fourth, and the native build last. Build-dependent editor integration tests run after their executable prerequisite. Repeat affected checks after subsequent source edits.

### Engine and quality tests

Create named tests in the new crate for these independent contracts. The names below are planned tests, not existing passing results.

| Planned test | Required assertion |
| --- | --- |
| `known_word_candidates_include_neighbors` | A valid dictionary word is evaluated with nonidentity alternatives. |
| `context_distinguishes_receive_and_relieve` | Receive/package and relieve/pressure examples choose the intended word, with valid controls unchanged. |
| `context_distinguishes_form_and_from` | Context can correct a valid-word error while preserving an actual form. |
| `prefix_context_does_not_use_future_words` | The score uses only text available at the typed boundary. |
| `ordinary_typos_preserve_sentence_bytes` | Mandatory common typos correct while all nontarget bytes remain identical. |
| `capitalization_has_only_two_rules` | Standalone `i` and alphabetic sentence starts change, while names/acronyms receive no extra rule. |
| `nonalphabetic_sentence_start_is_preserved` | Leading quotes, digits, and punctuation prevent sentence-start capitalization. |
| `unicode_offsets_and_case_expansion_are_valid` | Multibyte text and case expansion preserve valid target/source coordinates. |
| `invalid_dictionary_reload_preserves_profile` | Invalid rows return an error and leave the previous engine usable. |
| `profiles_do_not_share_project_terminology` | One project overlay cannot affect another. |
| `long_content_has_no_admission_cutoff` | Long words, sentences, and files remain processable without truncation or size-based disabling. |

Retain the full sentence-results evidence and add an independent held-out report. Count corrected errors, unchanged correct words, unintended corrections, missed corrections, candidate omissions, and results too stale to apply. Do not report process exit zero as proof that the linguistic choices are correct.

### Editor and transport fixtures

Add isolated fixtures under `nvim/tests/forge/` for `text_context.lua`, `text_capitalization.lua`, `text_rejection.lua`, `text_stale_reply.lua`, `text_eligibility.lua`, and `text_host.lua`.

| Boundary | Required coverage |
| --- | --- |
| Boundary provenance | Space, punctuation, Tab, inserted newline, completion menu, completion acceptance, paste, macros, undo, and redo. |
| Rejection | Exact Space lifecycle, rejection after an idle pause, capitalization rejection, combined replacement, newline rejection, normal second Backspace, and a new occurrence of the same spelling. |
| Races | Backspace before response, out-of-order responses, further typing, cursor movement, mode changes, buffer switch/disposal, host restart, and profile reload. |
| Mapping composition | Existing Backspace callback/expression mapping, completion/snippet fallback, owner detach, and a later third-party mapping change. |
| Source scoping | Code versus comments, Markdown inline/fenced code, links, paths, URLs, identifiers, and syntax unavailability. |
| Native editable ownership | An edit crossing a region boundary is rejected without changing surrounding native content. |
| Submission | A pending correction cannot mutate text after prompt, PR, or commit capture. |
| Service policy | `text.*` uses host readiness with no session envelope or provider initialization. |
| Failure preservation | Missing assets, unavailable profile, invalid response, and host errors preserve original text and allow normal Backspace. |

Reuse representative existing fixtures for the boundaries that the implementation changes, including `forge_client.lua`, `client_callback_isolation.lua`, `harness_input_state.lua`, `github_review_draft.lua`, and `harness_host.lua`. Re-read their current names and selectors before execution because the API rename has already changed the checkout.

Check the new crate and affected host/protocol targets with the existing `rust-check` definition or its direct Cargo equivalent. Use targeted `forge-text` test names first. Broaden to the new package only when its full model-lifecycle interactions require that coverage. Keep Cargo-heavy commands serial in the shared target directory.

Run the configured Lua language-server check over changed Forge and GitHub modules before Lua fixtures. Use the existing Rust formatting and lint checks, then rebuild the development `forge` executable before `text_host.lua` or other host-based integration.

After adding the proposed crate and tests, the direct Rust command sequence from `nvim/rust/forge` starts with the following targets. These are future implementation checks, not commands executed while writing this plan. Select the other named regression cases as their implementations change.

```nu
cargo check --profile dev --locked -p forge-text -p forge -p forge-protocol --all-targets
cargo test --profile dev --locked -p forge-text --lib context_distinguishes_receive_and_relieve
cargo fmt --all -- --check
cargo clippy --profile dev --locked -p forge-text -p forge -p forge-protocol --all-targets -- -D warnings
cargo build --profile dev --locked --bin forge
```

From the repository root, run an individual proposed Lua fixture with `nvim --headless -u NONE --cmd 'set rtp^=./nvim' -l nvim/tests/forge/text_rejection.lua`. Run `text_host.lua` only after the native build. Inside Harness, use the corresponding existing check definitions and exact fixture selectors instead of duplicating their executor logic.

Use 300-second deadlines for Rust compilation/build/test-compilation and compilation-bearing lint commands. Use ordinary inspection deadlines of 30 seconds and isolated Lua deadlines up to 120 seconds, honoring shorter fixture deadlines. Record commands, timeout values, elapsed durations, exit statuses, and diagnostics. Diagnose a timeout before increasing a deadline.

### Latency and resource criteria

Measure the complete warm boundary-to-visible-edit path in a fresh editor. The initial target is p95 at or below 16 ms and p99 at or below 32 ms on the recorded Windows and macOS machines. These are acceptance targets to measure, not results established by the existing SymSpell microbenchmark.

Measure cold initialization separately and verify that input and current text remain usable while assets load. Include a sustained workload of 20 completed words per second and report stale applications prevented under faster bursts. No key handler may synchronously wait on a worker.

Benchmark short prefixes, existing complete sentences, multiline comments, and progressively longer sentences. Stream token/context processing, yield between work chunks, evict caches, and cancel obsolete work. Do not impose total-byte, line-count, word-length, sentence-length, or candidate-count truncation as a latency workaround. If one long request occupies shared service capacity, control its scheduling and cancellation rather than disabling that content.

Telemetry records elapsed time, counts, revisions, reasons, failures, and staleness. Do not add raw prompt/comment contents to routine performance logs. Use explicitly retained local fixtures for linguistic debugging.

## Rollout and rollback

Develop and verify with correction disabled by default until the contextual-quality and input-lifecycle gates pass. Then enable automatic correction on the agreed eligible surfaces. The normal correction interaction remains automatic with Backspace rejection, not an approval picker.

Deploy the rebuilt host, updated contract, and Lua adapter together. Use separately created Neovim processes with isolated state for verification. Do not terminate or hot-reload the user's running Harness editor as a test procedure. Verify the actual test process identity before sending input.

Stop rollout on protected-text mutation, stale edit application, incorrect Backspace consumption, unintended submission changes, a contextual-quality gate failure, or an unresponsive input handler. Disable the owning controller, invalidate pending occurrences, restore its mappings without overwriting newer mappings, and leave current text untouched. Do not attempt a bulk undo of previously accepted corrections.

Rollback source, binary, and contract as one unit when required by the wire-version change. No user-text or session-database migration is required by the initial feature design. Keep model/cache formats explicitly versioned and rebuild derived caches instead of treating them as authoritative user data.

## Definition of done

- Every eligible completed word reaches contextual evaluation without a dictionary-membership gate.
- The contextual scorer passes mandatory misspelling and valid-word contrasts, with independent held-out results recorded.
- Spelling returns only a target edit and preserves all surrounding bytes.
- Exactly the two agreed capitalization rules operate, including the nonalphabetic sentence-start exception.
- Immediate Backspace reverses the latest correction and suppresses its occurrence without global learning or repeated correction on the retried delimiter.
- Later edits, navigation, submission, owner disposal, and host replacement prevent stale application.
- All supported surfaces enforce their real editable/prose boundaries.
- The shared host remains session/provider independent for correction, and failures preserve usable text and profiles.
- Warm/cold latency, stale-response behavior, long-content processing, and Windows/macOS verification are recorded.
- The native binary, Lua adapter, contract, documentation, and selected tests agree on the implemented interface.

## Dependency references

- [symspell_rs repository](https://github.com/wolfgarbe/symspell_rs) provides the candidate generator and source dictionaries. The tested baseline is version 7.0.1.
- [unicode-segmentation documentation](https://docs.rs/unicode-segmentation/) documents Unicode word, grapheme, and sentence segmentation. The inspected release is 1.13.3. Compatibility with the Forge toolchain remains an implementation check.
- The isolated evaluation files under `D:/scratch/symspell-eval` record the existing Windows experiments. They are not portable repository test assets until deliberately copied with provenance and license information.

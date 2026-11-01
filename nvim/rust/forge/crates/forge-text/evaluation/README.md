# Contextual correction evaluation

This directory preserves the historical `en-US-bigram-3` evaluation, including its isolated `contextual.rs` harness and complete outputs. Its frozen scorer used the bundled English assets, bigram interpolation, and a 2.5 penalty for changing a known word. These results describe that earlier policy, not the current production model.

The development set contains 18 target occurrences, including valid controls and typing prefixes. All 18 pass. The separate held-out set contains 16 errors and 16 correct-word controls: 15 errors corrected, one error missed, zero unintended corrections, and all 16 controls unchanged. The missed correction is `You may loose the connection`, which retains `loose` rather than selecting `lose`. No parameters were adjusted after this result.

The current `en-US-bigram-4` policy in `../assets/manifest.json` preserves recognized dictionary and vocabulary words instead of allowing contextual replacements. Its production tests cover valid-word preservation, unknown misspellings, and explicit contraction and capitalization rules in `../src/model.rs` and `nvim/tests/forge/text_wordlist_host.lua` from the repository root. The historical harness intentionally retains its original expectations, including `form` to `from` and `then` to `than`, which production autocorrect no longer performs.

These handwritten samples do not establish population-level correctness. Both policies use adjacent-word probabilities rather than general grammar analysis, and neither can use future words during forward typing.

The recorded warm lookup timings cover native scoring and result allocation. They exclude editor input, IPC, eligibility, and text replacement. End-to-end measurements belong to the live editor verification report.

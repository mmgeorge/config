# Contextual correction evaluation

The scorer parameters in `../assets/manifest.json` were frozen before the held-out cases were evaluated. `contextual.rs` is the isolated evaluation harness, retained with its complete outputs. It consumes the same upstream English assets and implements the same bigram interpolation and edit-type prior as the production model.

The development set contains 18 target occurrences, including valid controls and typing prefixes. All 18 pass. The separate held-out set contains 16 errors and 16 correct-word controls: 15 errors corrected, one error missed, zero unintended corrections, and all 16 controls unchanged. The missed correction is `You may loose the connection`, which retains `loose` rather than selecting `lose`. No parameters were adjusted after this result.

These handwritten samples establish regression behavior, not population-level correctness. The model only compares adjacent-word probabilities. It does not perform general grammar checking and cannot use future words during forward typing. The mandatory contrasts and editor-lifecycle gates pass; the user selected Windows-only acceptance for this rollout. Broader linguistic evaluation remains valuable because the recorded held-out miss is still present.

The recorded warm lookup timings cover native scoring and result allocation. They exclude editor input, IPC, eligibility, and text replacement. End-to-end measurements belong to the live editor verification report.

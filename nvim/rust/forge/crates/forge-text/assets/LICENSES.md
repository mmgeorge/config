# English model asset provenance

The unchanged English unigram and bigram files come from `wolfgarbe/symspell_rs` revision `62131c2ca5bfd7fe8432e28b17b24dd98d959ee0`. The manifest records their exact SHA-256 digests. Forge does not download or modify these files at runtime.

The upstream [frequency dictionary documentation](https://github.com/wolfgarbe/symspell_rs/blob/62131c2ca5bfd7fe8432e28b17b24dd98d959ee0/README.md#frequency-dictionary) attributes frequencies to [Google Books Ngram data](https://storage.googleapis.com/books/ngrams/books/datasetsv2.html), distributed under [Creative Commons Attribution 3.0](https://creativecommons.org/licenses/by/3.0/), and vocabulary filtering to [SCOWL](https://wordlist.aspell.net/). Google and SCOWL are the sources of these data, not endorsers of Forge. The contextual scorer is a Forge implementation derived from these counts.

`SCOWL-Copyright.txt` retains the vocabulary project's copyright and permission notices. `SymSpell-LICENSE.txt` retains the MIT notice for the distribution and candidate-generator implementation. The unrelated Chinese dictionary license in upstream `data/license.txt` is not used as the English assets' license.

## Harper recognition vocabulary

`harper-3.2.0.txt` derives from [Harper 3.2.0](https://github.com/Automattic/harper/releases/tag/v3.2.0), commit `ea9ab1b8f95be99a5d4040c53be92a6c2ad7f7f3`, copyright 2024 Automattic Inc., under Apache-2.0. `Harper-LICENSE.txt` retains the complete license and copyright notice. The `harper-cli words` command exports the curated dictionary after Harper applies its annotation and affix rules; the raw annotated dictionary is not a list of all recognized forms.

Forge decodes the exported quoted strings, lowercases them, removes duplicates, sorts them, and writes UTF-8 with LF endings. The resulting 135,845 entries protect spelling only: Forge does not insert them into SymSpell's candidate dictionary or invent frequency counts. The manifest pins the source revision, output digest, and Windows exporter archive digest.

To reproduce the asset, download the `harper-cli` archive for the relevant platform from the pinned release and extract it outside the checkout. From the repository root, run `nu nvim/rust/forge/crates/forge-text/assets/export-harper.nu <path-to-harper-cli>`. The exporter verifies the CLI version and prints the output checksum; compare it with `manifest.json` before rebuilding Forge. Upgrading the snapshot requires reviewing the wordlist diff, updating its license and manifest, updating the model checksum and revision, and rerunning correction tests. Normal builds and startup use the checked-in asset without downloading or running Harper.

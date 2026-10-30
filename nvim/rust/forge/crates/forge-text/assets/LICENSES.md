# English model asset provenance

The unchanged English unigram and bigram files come from `wolfgarbe/symspell_rs` revision `62131c2ca5bfd7fe8432e28b17b24dd98d959ee0`. The manifest records their exact SHA-256 digests. Forge does not download or modify these files at runtime.

The upstream [frequency dictionary documentation](https://github.com/wolfgarbe/symspell_rs/blob/62131c2ca5bfd7fe8432e28b17b24dd98d959ee0/README.md#frequency-dictionary) attributes frequencies to [Google Books Ngram data](https://storage.googleapis.com/books/ngrams/books/datasetsv2.html), distributed under [Creative Commons Attribution 3.0](https://creativecommons.org/licenses/by/3.0/), and vocabulary filtering to [SCOWL](https://wordlist.aspell.net/). Google and SCOWL are the sources of these data, not endorsers of Forge. The contextual scorer is a Forge implementation derived from these counts.

`SCOWL-Copyright.txt` retains the vocabulary project's copyright and permission notices. `SymSpell-LICENSE.txt` retains the MIT notice for the distribution and candidate-generator implementation. The unrelated Chinese dictionary license in upstream `data/license.txt` is not used as the English assets' license.

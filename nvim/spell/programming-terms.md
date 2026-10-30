# Programming vocabulary

`programming.en-US.txt` contains 3,250 imported programming words. Personal additions live separately in `forge.en-US.txt`. Both Harper and automatic correction consume the combined vocabulary. The imported vocabulary comes from CSpell's MIT-licensed software dictionary at revision [`ae9c8d4dc15024377cc64b21d77ce863a35e0c9d`](https://github.com/streetsidesoftware/cspell-dicts/tree/ae9c8d4dc15024377cc64b21d77ce863a35e0c9d/dictionaries/software-terms), package version 5.4.8. The complete copyright and license notice is in [CSPELL-LICENSE.txt](CSPELL-LICENSE.txt).

The import uses these source files, rather than the combined distribution that also includes legacy vocabulary:

- [coding-terms.txt](https://github.com/streetsidesoftware/cspell-dicts/blob/ae9c8d4dc15024377cc64b21d77ce863a35e0c9d/dictionaries/software-terms/src/coding-terms.txt) supplies common coding vocabulary.
- [software-terms.txt](https://github.com/streetsidesoftware/cspell-dicts/blob/ae9c8d4dc15024377cc64b21d77ce863a35e0c9d/dictionaries/software-terms/src/software-terms.txt) supplies software concepts.
- [computing-acronyms.txt](https://github.com/streetsidesoftware/cspell-dicts/blob/ae9c8d4dc15024377cc64b21d77ce863a35e0c9d/dictionaries/software-terms/src/computing-acronyms.txt) supplies computing abbreviations.

## Conversion

The import removes blank lines and full-line or whitespace-prefixed `#` comments, lowercases entries, deduplicates them, and sorts the result. It retains single lexical tokens beginning with a Unicode letter, followed by letters/numbers or internal apostrophes. It does not split compound names into independently allowed words. Hexadecimal examples beginning with digits, phrases, hyphenated names, and underscored identifiers are outside Forge's eligible word boundary and are omitted.

The word `then` is deliberately excluded: globally allowing that programming keyword would suppress the required contextual correction in `greater then zero`. The contextual `then`/`than`, `form`/`from`, and ordinary misspelling regressions remain enabled. The global add-word action writes only to the personal `forge.en-US.txt` file.

The list is vendored locally and needs no network access while typing. Saving edits to the wordlist in Neovim invalidates its cached content and pending corrections; the next eligible word uses the updated profile.

The native-host fixture passes all 18 contextual quality-gate cases with this imported vocabulary active. The [Windows measurement](../rust/forge/crates/forge-text/evaluation/windows-programming-latency.json) applied and redrew all 120 words at 20 words per second, with 1.89 ms p95 and 2.23 ms p99 through redraw. Profile preparation reuses the unchanged wordlist instead of sorting and hashing its contents for each word.

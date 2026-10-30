# Local text correction

Forge can correct the word you finish typing while preserving the rest of the buffer. Correction runs in the local Forge host and does not start an AI provider or send your text to a remote model.

The feature is enabled by default after the contextual and editor-lifecycle checks. Windows is the verified platform for this rollout; macOS has not been verified. Configure personal and project vocabulary in the Forge setup:

```lua
require("forge").setup({
  text_correction = {
    enabled = true,
    language = "en-US",
    terminology = { "Neovim", "tokio", "serde" },
    project_terms = { ["D:/project"] = { "projectword" } },
  },
})
```

Set `text_correction.enabled = false` to disable automatic correction. Disabling it invalidates pending edits and restores the previous Backspace mapping if Forge still owns that mapping.

## Allowed words

Harper and automatic correction share [`nvim/spell/forge.en-US.txt`](../../spell/forge.en-US.txt), a global vocabulary file stored in this configuration repository. Use the **Add "word" to the global dictionary** code action or edit the file directly. Additions apply across projects and become ordinary version-controlled changes, not automatic Git commits. Put one word on each line. Blank lines and lines beginning with `#` are ignored. Saving the file in Neovim refreshes both services without a rebuild or restart.

The separate [`nvim/spell/programming.en-US.txt`](../../spell/programming.en-US.txt) supplies 3,250 programming words from CSpell's coding, software-concept, and computing-acronym lists. Both services merge it with the personal file at load time. [Import details and licensing](../../spell/programming-terms.md) record the pinned source revision and the exclusions that keep contextual spelling checks working. The global add-word action writes only to the personal file.

The files combine with `terminology` and `project_terms` for both services. Allowed words are protected from automatic spelling replacements, case-insensitively, while the standalone `i` and sentence-start capitalization rules still apply. Set `text_correction.wordlist` to another personal file path, or to `false` to omit personal vocabulary and hide dictionary-add actions. Programming vocabulary remains enabled. A missing or invalid configured file reports an error and preserves the text being typed. Save unsaved dictionary edits before using the add-word action.

The language model evaluates dictionary words as well as unknown spellings. It may keep a word unchanged. Its contextual evidence comes from adjacent words, and forward typing provides no words to the right that you have not typed yet. The initial held-out regression sample includes a missed `loose`/`lose` correction. This feature is not a general grammar checker.

## Typing and rejection

Space, punctuation, Tab, and an inserted newline can complete a word. Pressing Backspace as the next edit after a correction restores the original word and consumes that delimiter. An idle pause does not expire rejection. Retyping the delimiter preserves the rejected spelling for that occurrence, while the next word is evaluated normally.

Continuing to type, moving the cursor, leaving Insert mode, switching buffers, or submitting text invalidates a pending result. Paste, generated text, completion edits, macros, undo, and redo do not independently trigger correction. A later ordinary typed delimiter can evaluate the word you are currently completing.

Standalone `im` becomes `I'm` and `cant` becomes `can't` in eligible prose, including mid-sentence. The `cant` rule intentionally takes precedence over that word's uncommon standalone meanings and dictionary exemptions. Dictionary-recognized contractions keep their spelling and straight or curly apostrophe, so `can't` and `can’t` cannot become `cant` or `can`. First-person contractions such as `I'm`, `I'll`, `I've`, and `I'd` capitalize `I` anywhere in a sentence. These rules do not expand ambiguous words such as `ill`, `well`, or `were` into contractions.

Standalone `i` and alphabetic sentence starts also receive automatic capitalization. Opening quotes, digits, and punctuation prevent sentence-start capitalization. Forge does not add proper-name, acronym, brand, or title capitalization. Protected code and technical tokens remain excluded, and Backspace can still reject an individual correction.

Typing `.`, `,`, `;`, `:`, `!`, or `?` after spaces following an eligible word removes those spaces: `This is a sentence .` becomes `This is a sentence.`. Immediate Backspace removes the punctuation and restores the original spacing, together with any spelling change in that transaction. Retyping the rejected punctuation keeps the original spacing for that occurrence. This rule uses the same prose and editable-region protection as spelling.

## Eligible text

Source comments require syntax evidence, with `@nospell` taking precedence. Markdown prose excludes fenced and indented code, inline code, link destinations, frontmatter, paths, URLs, and technical tokens. Git commit messages exclude Git commentary and scissors trailers. Harness composers provide explicit prose ownership. Native PR title, body, and comment drafts must remain wholly inside their authoritative editable region.

If syntax, the host, the selected profile, or an asset is unavailable, Forge preserves your text. Missing or invalid assets report an error. Disabling correction removes its Backspace interception without replacing a newer mapping installed by another plugin.

The [verification report](../../contributing/forge/text-verification.md) records the Windows environment, exact fixtures, quality results, and measured input-to-redraw latency. Large contexts remain eligible, although their processing can take longer than ordinary short prose.

## Harper grammar diagnostics

Harper runs locally alongside automatic correction and supplies grammar diagnostics and standard Neovim LSP code actions. It covers Markdown, plain text, source comments, commit messages, Harness composers, and native PR title/body/comment regions. Diagnostics and fixes use the same prose and editable-region checks; code and generated read-only sections are excluded. Source languages without a native Harper parser use plain-text checking behind those same comment guards.

Mason installs `harper-ls` automatically. Set `harper.enabled = false` in the Forge configuration to disable it, or set `harper.cmd` to an explicit executable command. `harper.settings` contains Harper's server settings. `SpellCheck` is enabled by default to underline misspellings alongside automatic correction. `SentenceCapitalization`, `SpelledNumbers`, and `OkToOkay` remain disabled, allowing `ok` without a style warning. Dictionary additions do not suppress other grammar or style rules.

```lua
require('forge').setup({
  harper = {
    enabled = true,
    settings = {
      dialect = 'American',
      linters = { LongSentences = false },
    },
  },
})
```

Harper suggestions apply only after choosing a code action. Every edit is checked again against the current buffer and its editable regions; typing or submitting invalidates an older suggestion. The server runs from Forge's cache directory to avoid holding a working-directory lock inside a project or build directory.

Forge replaces Harper's user, workspace, and file dictionary actions with one global action per word. Forge appends to the configured source wordlist, invalidates automatic correction's cached vocabulary, and refreshes running Harper clients. Harper reads a plain-word copy in Forge's cache, including configured terminology, rather than rewriting the commented source file. Forge owns `harper.settings.userDictPath` for this integration.

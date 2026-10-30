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

Edit [`nvim/spell/forge.en-US.txt`](../../spell/forge.en-US.txt) to allow words such as `repo`. Put one word on each line; blank lines and lines beginning with `#` are ignored. Saving the file in Neovim reloads it for subsequent corrections without a rebuild or restart.

The file also includes 3,250 programming words from CSpell's coding, software-concept, and computing-acronym lists. [Import details and licensing](../../spell/programming-terms.md) record the pinned source revision and the exclusions that keep contextual spelling checks working. Add personal entries in the small section at the top of the file.

The file combines with `terminology` and `project_terms`. Allowed words are protected from spelling replacements, case-insensitively, while the standalone `i` and sentence-start capitalization rules still apply. Set `text_correction.wordlist` to another file path, or to `false` to use only the configured terminology lists. A missing or invalid configured file reports an error and preserves the text being typed.

The language model evaluates dictionary words as well as unknown spellings. It may keep a word unchanged. Its contextual evidence comes from adjacent words, and forward typing provides no words to the right that you have not typed yet. The initial held-out regression sample includes a missed `loose`/`lose` correction. This feature is not a general grammar checker.

## Typing and rejection

Space, punctuation, Tab, and an inserted newline can complete a word. Pressing Backspace as the next edit after a correction restores the original word and consumes that delimiter. An idle pause does not expire rejection. Retyping the delimiter preserves the rejected spelling for that occurrence, while the next word is evaluated normally.

Continuing to type, moving the cursor, leaving Insert mode, switching buffers, or submitting text invalidates a pending result. Paste, generated text, completion edits, macros, undo, and redo do not independently trigger correction. A later ordinary typed delimiter can evaluate the word you are currently completing.

Only standalone `i` and alphabetic sentence starts receive automatic capitalization. Opening quotes, digits, and punctuation prevent sentence-start capitalization. Forge does not add proper-name, acronym, brand, or title capitalization.

Typing `.`, `,`, `;`, `:`, `!`, or `?` after spaces following an eligible word removes those spaces: `This is a sentence .` becomes `This is a sentence.`. Immediate Backspace removes the punctuation and restores the original spacing, together with any spelling change in that transaction. Retyping the rejected punctuation keeps the original spacing for that occurrence. This rule uses the same prose and editable-region protection as spelling.

## Eligible text

Source comments require syntax evidence, with `@nospell` taking precedence. Markdown prose excludes fenced and indented code, inline code, link destinations, frontmatter, paths, URLs, and technical tokens. Git commit messages exclude Git commentary and scissors trailers. Harness composers provide explicit prose ownership. Native PR title, body, and comment drafts must remain wholly inside their authoritative editable region.

If syntax, the host, the selected profile, or an asset is unavailable, Forge preserves your text. Missing or invalid assets report an error. Disabling correction removes its Backspace interception without replacing a newer mapping installed by another plugin.

The [verification report](../../contributing/forge/text-verification.md) records the Windows environment, exact fixtures, quality results, and measured input-to-redraw latency. Large contexts remain eligible, although their processing can take longer than ordinary short prose.

## Harper grammar diagnostics

Harper runs locally alongside automatic correction and supplies grammar diagnostics and standard Neovim LSP code actions. It covers Markdown, plain text, source comments, commit messages, Harness composers, and native PR title/body/comment regions. Diagnostics and fixes use the same prose and editable-region checks; code and generated read-only sections are excluded. Source languages without a native Harper parser use plain-text checking behind those same comment guards.

Mason installs `harper-ls` automatically. Set `harper.enabled = false` in the Forge configuration to disable it, or set `harper.cmd` to an explicit executable command. `harper.settings` contains Harper's server settings. The default disables Harper's `SpellCheck` and `SentenceCapitalization` rules because Forge's automatic correction owns those behaviors; the other grammar rules remain available.

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

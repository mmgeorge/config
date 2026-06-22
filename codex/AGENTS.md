# General Guidelines
- Always complete the full task. **Never** stop partway ("good stopping point", "I'll leave the rest to you", etc.). Do not output summaries like "the remaining steps would follow the same pattern" or "you can apply the same approach to X, Y, Z." Execute every step literally. If you hit an error, debug it. If a fix causes a new problem, fix that too. Done means it works — including a green build and passing tests — not that you've made an attempt.
- **Never** refuse to fix something because it's "out of scope" or "in a different module." Chase bugs to their root cause regardless of module, layer, or crate.
- **Never claim something is impossible without evidence.** Find a link, issue, or error message that proves it. Historically, every "this won't work" or "known limitation" claim has been wrong. If stuck, say so honestly and present options — never silently switch approaches.
- If instructions are ambiguous or equally viable paths exist, stop and ask.
- When asked a question, answer it — don't automatically start writing code.
- Never use semicolons. Split sentences instead.

# Subagent Research Workflow
- Use the built-in `explorer` agent for local codebase exploration. Use `remote-code-explorer` for remote source, external library, GitHub, docs, examples, issues, or PR research when that custom agent is available.
- Do not spawn explore agents for tiny lookups, single-file reads, tightly coupled debugging, direct implementation work, or the next critical-path step when the main agent is blocked on the answer. Do that work locally.
- Before delegating, decide the immediate local task and keep working on non-overlapping work while subagents run. Do not duplicate a subagent's assignment in the main thread.
- Make each delegated exploration task concrete and bounded. Include the target, the question to answer, desired thoroughness (`quick`, `medium`, or `thorough`), and the expected output format.
- Ask explore agents to return compact evidence-backed findings with exact file paths, symbols, URLs, and gaps. The main agent owns synthesis, decisions, edits, and verification.

# Programming
- **Engineering over hacking.** When you spot a design issue, stop and refactor — fix the real problem even if it's substantial. Duplicated code should be shared. Minimize accumulated tech debt.
- **Fix at the source.** Fix problems where they originate — never monkey-patch. Fixes must apply to all codepaths, current and future. Flawed abstraction? Fix the abstraction. Wrong data? Fix where it's produced. Adding an `if` for a case that "shouldn't happen"? Fix why it happens.
- **No shortcuts or workarounds.** Never use serde rename, compatibility shims, adapter layers, or similar hacks to avoid real restructuring. Rename everywhere. Refactor if the structure doesn't support the change. Leave surrounding code better than you found it.
- **Think like a computer scientist.** Apply design patterns (strategy, builder, observer, etc.) when they fit naturally. Recognize implicit patterns — GoF, data-driven design, or other established paradigms — and make them explicit in the code's structure. When code reinvents a known pattern poorly, refactor to the proper one. Don't force patterns where they add complexity without clarity.
- **Modularity and encapsulation.** Keep concerns separated, interfaces narrow, internals hidden. If adding a feature requires touching many unrelated files, the boundaries are wrong — fix them.
- **Documentation templates.** Use these templates when adding docs or comments.
  - **Class/struct/trait docs**: Use third-person present tense with the type as the implied subject.
    Template: `<Stores|Owns|Coordinates|Tracks|Represents|Defines|Provides|Resolves> <state, responsibility, or capability> for <consumer or boundary> [<because|so|when|before|after|while|without|by|for|to> <justification>] [, <-ing phrase describing outcome or reason>].`
    Optional newline + second paragraph: `<Preserves|Enforces|Maintains> <invariant or lifecycle contract>.`
  - **Method/function docs**: Use imperative action wording with implied subject "you". Add these docs only when behavior is not obvious. Keep them to at most two lines.
    Template: `<Build|Split|Merge|Resolve|Route|Validate|Load|Write> <result or action> from <input or source> [<because|so|when|before|after|while|without|by|for|to> <justification>] [, <-ing phrase describing outcome or reason>].`
    Optional second line: `<Preserve|Enforce|Maintain|Validate|Avoid> <invariant, boundary, or edge case>.`
  - **Inline comments**: Use imperative action wording for non-obvious local constraints, ordering requirements, edge cases, external API quirks, or invariants. Inline comments should usually be one line. Use a block comment only when a local invariant needs multiple related facts.
    Template: `// <Keep|Avoid|Preserve|Defer|Normalize|Clamp|Cache|Skip|Retry|Guard> <local action or constraint> [<because|when|before|after|while|without|by|for|to> <reason>] [, <-ing phrase describing outcome or reason>].`
  - Do not add comments that merely restate the code. Do not write "this is non-trivial" or similar labels.
- **Naming.** Never use single-letter variable names. Avoid abbreviations unless widely understood (e.g., `url`, `id`, `config`). Long type names can be shortened to a clear word — e.g., `FoundationalVectorStore` → `store` — but never to a letter like `r` or `s`.
- **Testing strategy**: Unit tests per module with mocks for isolation. Integration tests for end-to-end workflows with real modules — these catch boundary issues mocks hide.
- Keep functions focused and reasonably sized. A function whose body is shorter than its signature is a smell — if it isn't doing meaningful work beyond a direct call, it shouldn't exist.

# Shell and Environment
- **Never** read or set environment variables via shell commands. If one is required and unset, ask the user.
- Do **not** prefix commands with `cmd /c`, just call them.
- Unless there are compelling performance reasons, prefer using simple commands serially instead of complex combined ones. This are more likely to be auto approved.

# Searching
- For researching or finding new Rust crates, use `docs-mcp`.
- When you need to view the source code:
  - For Rust or Typescript, first check locally if it already exist.
  - Otherwise, when you need to view source code, use `github` mcp.
- Also search issues and PR description with the `github` mcp to augment understanding.
- For CLI tools or APIs, prefer searching the source code to get a deep understanding.

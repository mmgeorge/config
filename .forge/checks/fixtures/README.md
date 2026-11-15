# Script adapter recordings

These are actual tool outputs recorded on Windows on 2026-11-13. Temporary
workspace paths remain in the records; translator tests use an isolated workspace
for source access rather than relying on those directories still existing.

- `nushell-unclosed.jsonl`: Nushell 0.116.1, `nu --no-config-file --ide-check 100`
  against `nushell-unclosed-source.txt`. The first error span points after the
  opening parenthesis, at line 2, UTF-8 byte column 14.
- `git-whitespace.json`: stdout from `git diff --no-ext-diff --no-color --check HEAD`
  after appending a line with trailing spaces to a committed file. JSON preserves
  the whitespace without introducing trailing whitespace in this repository.

`script-adapters.test.nu` tests the Nushell and Git translators and real commands.
The host-level suite is `adapter-integration.nu`; direct script tests do not prove Harness admission
or completion behavior.

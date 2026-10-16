# Make Harness plan reference resolution LSP-based

Use Neovim's attached language server to resolve existing workspace references
to their declarations, instead of extending Harness's partial language-specific
type inference. Keep extraction and save/read handling independent of LSP
availability so unresolved references never block saving.

## Harness rejects its own extracted TypeScript references

### Impact

Harness cannot save required integration declarations for the shared symbol
storage plan. Revision 6 remains saved and unsubmitted, but attempts to add
integration declarations fail in `harness_design_apply_patch` during
`semantic_execution`.

Affected plan: `e6e14c92-ed36-4aa5-a6c1-aa32286c8d4c`.

### Evidence

Harness extracts the following property references from existing workspace code:

```text
FeatureMeshFactory.write
  Accesses
    options.$view.scale
    IArcadeEvalOptions.$view

RenderState._fader
  Calls
    RenderState.getStage
  Accesses
    this.getStage()?.labelManager.symbolFader
    <unresolved>.labelManager
```

Saving declaration patches produces these errors:

```text
Function uses contain only a qualified target name, without arguments: options.$view.scale
Function uses contain only a qualified target name, without arguments: this.getStage()?.labelManager.symbolFader
```

### Cause

Extraction and parsing use incompatible reference formats.
`nvim/rust/forge/crates/forge-diff/src/syntax/declaration_calls.rs` attempts
receiver-type inference but can fall back to the original receiver text.
`nvim/rust/forge/crates/forge-harness/src/plan/calls.rs` then parses both `Calls`
and `Accesses` using a character whitelist that permits alphanumeric characters
and `_:.<>#`, but rejects `$`, `?`, and parentheses.

Removing `?` alone does not solve the problem. The expression still contains a
call-result receiver, and the resolver expects qualified names rather than
arbitrary source expressions. TypeScript already has partial receiver inference
here, but it does not resolve this chain successfully.

`Calls` and `Accesses` provide dependency navigation, review evidence, and
implementation-difference reporting. They are not a full TypeScript type check.
Submission separately validates imports and signature references. The documented
contract treats body references as descriptive evidence, not completion gates.

### Proposed resolution

Make extracted references round-trip safely regardless of whether semantic
resolution succeeds. Preserve the original expression, source location, kind,
and unresolved status when no declaration identity can be established.
Do not weaken malformed-target validation by merely adding punctuation to the
whitelist or discard references to make saving succeed.

Make semantic resolution of existing workspace references LSP-based, starting
with the editor's attached TypeScript language server. LSP availability remains
optional for saving, but LSP supplies declaration identities when available:

1. Locate the referenced member token in the original source. Current positions
   can point to the expression start, so verify or extend extraction to capture
   the member position.
2. Ask Neovim's attached server for `textDocument/definition` at that position.
   Handle both definition locations and location links.
3. Map the returned declaration location to Harness's declaration identity.
   Keep source text separate from that resolved identity. Do not normalize an
   expression by deleting optional-chain punctuation.
4. Associate responses with the source version or digest and reject stale
   enrichment. Bound requests and report request failures through the owning
   notification path while retaining usable unresolved evidence.

For `this.getStage()?.labelManager.symbolFader`, query the `symbolFader` token
to find its declaration. Resolve other member occurrences independently when
their dependency identities are needed.

The language server sees workspace files and editor documents, not Harness's
virtual proposed declarations. Keep the existing proposal-aware resolver for
those declarations. Do not treat a definition from current workspace code as
proof that a changed virtual proposal resolves correctly.

A missing language server, unsupported definition capability, empty result, or
failed request must not prevent saving the plan. Distinguish unavailable
analysis from request failures and preserve both explicitly.

### Acceptance criteria

- Both exact failing expressions survive extraction, presentation, parsing,
  and saving without losing reference metadata.
- A declaration patch can save when its untouched baseline contains either
  expression.
- Valid TypeScript `$` identifiers remain supported.
- TypeScript LSP enrichment resolves member-token positions, supports location
  and location-link responses, and ignores stale responses.
- Saving works without an attached language server and after empty results,
  timeouts, or request failures. Request failures remain user-visible.
- Virtual proposal resolution remains separate from workspace LSP evidence.
- Malformed targets and unattached reference blocks still fail validation.
- After deploying the fix, retry the blocked integration declaration patches
  against saved revision 6 without restarting the plan.

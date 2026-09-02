# Skill Evaluation and Iteration

*When to read this:* When creating evaluation test cases, performing multi-phase validation on draft skills, and refining instructions based on execution observations.

## Evaluation-Driven Development

Establish functional evaluations before drafting extensive documentation:

1. **Record Baseline Gaps:** Execute representative tasks without the skill. Log missing context, unvalidated parameters, and manual interventions.
2. **Define Test Scenarios:** Create 1–3 concrete test cases covering the recorded failure modes.
3. **Measure Baseline Performance:** Evaluate model execution with no skill loaded.
4. **Draft Minimal Guidance:** Author targeted instructions to resolve the recorded failures.
5. **Re-evaluate:** Verify that the skill enables consistent, error-free execution.

### Evaluation Schema Structure

```json
{
  "skills": ["processing-pdfs"],
  "query": "Extract text from test-files/document.pdf and write output to output.txt",
  "files": ["test-files/document.pdf"],
  "expected_behavior": [
    "Invokes pdfplumber to extract text across all pages",
    "Writes normalized UTF-8 text to output.txt",
    "Reports zero extraction errors on valid inputs"
  ]
}
```

## Multi-Phase Validation

Validate the draft in a clean execution context across four sequential phases:

### Phase 1: Trigger and Discovery Validation
Provide only `name` and `description` to the model. Evaluate:
- 3 test queries that must trigger the skill.
- 3 syntactically similar queries that must not trigger the skill.
- Refine the description until trigger accuracy reaches 100%.

### Phase 2: Logic and Determinism Verification
Evaluate full `SKILL.md` and reference files by simulating execution on representative tasks:
- Trace each step and inspect intermediate inputs and outputs.
- Identify missing argument schemas, ambiguous decision branches, or unstated prerequisite dependencies.

### Phase 3: Edge Cases and Failure States
Evaluate behavior against edge cases:
- Missing dependencies or uninstalled binaries.
- Malformed inputs, empty responses, or network timeouts.
- File permission errors and corrupted assets.

### Phase 4: Refinement and Structural Factoring
- Extract oversized sections or detailed schemas into `references/`.
- Ensure all reference files are linked directly from `SKILL.md`.
- Document required error recovery steps.

## Iteration on Production Workflows

Evaluate the skill during real tasks across all supported model families:
1. Observe navigation paths, referenced file reads, and tool invocations.
2. If reference files are frequently skipped or misapplied: promote key invariants directly into `SKILL.md`.
3. If specific reference files remain unused across multiple runs: prune unnecessary files.
4. Verify execution across all target model tiers.

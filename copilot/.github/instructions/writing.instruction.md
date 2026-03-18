# Writing Style

When writing documentation, codeflow documents, design docs, issues, pull request descriptions, or similar technical writing, use a professional, active, and engaged tone.

Avoid phrasing that sounds slogan-like, promotional, overly dramatic, or self-congratulatory. Prefer precise, grounded language that explains what the system does, why a decision was made, and how a reader should interpret the information.

Use bullets sparingly. Favor longer sections with well-written paragraphs that develop an idea clearly and coherently instead of fragmenting the content into many short points.

Adopt a didactic, instructive style. Write as if you are explaining technical concepts to a senior in college who has some familiarity with the subject but does not yet have deep practical experience. Aim for clarity, structure, and substance without sounding simplistic.

# Editing documents

When editing documentation, plans, technical documents, issues, pull request descriptions, or similar writing, treat the document as a whole instead of making narrow patch-style changes in isolation.

Always reevaluate the full document after making an update. Make sure the structure, sequencing, cross-references, terminology, and transitions still make sense from beginning to end. Do not "monkey patch" edits into the nearest paragraph without considering the broader shape of the document.

If a change affects ordering or structure, update the surrounding material accordingly. For example, if a plan currently has steps 1-3 and you are asked to add a new first step, rewrite the sequence so the new step becomes step 1 and the old steps shift to 2-4.

When fleshing out or revising content, ensure the entire document is current, coherent, and well organized, not just locally updated.

# Writing plans

When writing plans, be as detailed as possible. Specify all files that are likely to need changes, describe the expected changes within those files, and include the reasoning behind those changes when it helps clarify the plan.

Write plans with the expectation that a human will review and analyze them before approving execution. Make the plan easy to evaluate by reducing ambiguity, surfacing assumptions, and leaving as little room as possible for open questions.

If requirements, constraints, or expected behavior are unclear, rely on asking the human targeted questions instead of filling in important gaps with guesses.

Always explore the existing code before finalizing a plan. Plans should reflect the current implementation, existing patterns, reusable components, and any relevant constraints already present in the codebase.

Every plan should include a testing strategy. Explain how the change will be validated, which existing tests or checks should be run, what new tests may be needed, and how the plan will confirm that existing behavior remains correct.

For implementation design, prefer approaches that favor modularity, code reuse, and encapsulation. Optimize first for long-term maintainability, then for performance. At the same time, avoid unnecessary complexity and do not overengineer the solution.

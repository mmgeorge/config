# Technical Tables Guide

*Read when designing, reviewing, or structuring comparison tables, configuration matrices, parameter lists, and reference schemas.*

## Purpose and Selection Criteria

Use a table when readers must compare multiple entities across two or more shared dimensions, look up values by a primary key, or evaluate a matrix of constraints.

- **Use a table when:** Items share three or more attributes (such as Name, Type, Default, Description, Required).
- **Use a bulleted list when:** Comparing only a single attribute or presenting sequential steps.
- **Use prose when:** Explaining nuanced causality, edge-case rationale, or conditional behaviors that cannot be summarized in tabular cells.

## Focus on One Comparison Dimension

Design each table around a single primary entity type and lifecycle scope. Do not mix unrelated entities (such as user accounts and database migration records) in the same table.

### The Five Column Quality Checks

Audit every column against these five criteria:

1. **Contribution:** Answers a distinct reader question (such as *"What is the fallback behavior?"*).
2. **Coverage:** Contains populated, meaningful data across rows rather than empty cells or `N/A` placeholders.
3. **Differentiation:** Provides distinct values that help readers distinguish one row from another.
4. **Scope:** Remains strictly within the entity abstraction level and lifecycle phase of the table.
5. **Header Clarity:** Uses a concise, self-explanatory domain label without requiring footnotes.

### Column Pruning and Table Splitting

- **Remove redundant columns:** Drop columns that repeat another column's information, remain constant across all rows, or are mostly empty.
- **Split disparate lifecycles:** When subsets of columns apply to different lifecycle phases (such as Build Time vs Runtime configuration), split them into separate tables.
- **Extract complex edge cases:** When a single cell requires multiple sentences of qualification, move that explanation to adjacent prose and link or cross-reference it.

## Headers as Shared Context

Treat column headers as the shared category prefix for all cells in that column:

- Combine the header with each cell, then remove words in the cells that merely repeat the header label.
- Keep sibling cells grammatically parallel (all noun phrases, all imperative verbs, or all boolean values).
- Contextual shortening within a table does not alter canonical terminology. Use the full canonical identifier in surrounding prose.

## Formatting Standards

- **Alignment:** Left-align text, descriptions, and code identifiers. Right-align numeric quantities and memory/time metrics. Center short status tags or booleans.
- **Units in Headers:** Place units of measurement directly in the header (such as `Timeout (ms)`, `Memory (MiB)`) instead of repeating units in every cell.
- **Empty States:** Never leave a table cell blank. Use an explicit indicator: `None`, `N/A`, or `-`, and define the meaning if ambiguous.
- **Code Formatting:** Wrap literal values, field names, environment variables, and types in backticks.

## Table Review Checklist

1. Does the table track exactly one entity type or comparison dimension?
2. Does every column pass the five quality checks (Contribution, Coverage, Differentiation, Scope, Header Clarity)?
3. Are numeric units declared in column headers rather than repeated in cells?
4. Are all cells populated with explicit values or defined empty-state indicators?
5. Are cell entries parallel in grammar and abstraction level?
6. Does the preceding text state the takeaway or primary pattern shown in the table?

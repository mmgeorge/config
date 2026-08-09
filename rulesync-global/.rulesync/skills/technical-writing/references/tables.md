# Tables

*Read when deciding whether to use a table, selecting its columns, naming its headers, or reviewing whether its rows support one comparison.*

## Give the Table One Job

Give each table one comparison or lookup job. State that job privately before choosing columns, then include only the dimensions required to perform it.

Audit every column against five checks:

- **Contribution:** Identify the distinct reader question the column answers.
- **Coverage:** Confirm that the question applies meaningfully to most rows.
- **Differentiation:** Confirm that the values help readers distinguish or compare rows.
- **Scope:** Keep the column within the table's subject, lifecycle stage, and abstraction level.
- **Header clarity:** Use the shortest specific noun phrase that identifies the values without explanation.

Remove a column when it repeats the table heading or another column, stays constant, contains mostly empty values, or brings in a relationship that belongs elsewhere. Split the table when two groups of columns perform different jobs. Replace the table with prose or a list when only one meaningful comparison dimension remains.

For example, a table under `Authoring Assets` needs `Asset` and `Purpose` to identify each asset type and explain why it exists. `Authoring asset` repeats the section scope. `Conventional export or source` combines two representations, while `Runtime result` introduces a cooking relationship. If readers need that relationship, give it a separate asset-to-artifact table.

Read the header row by itself. It should reveal the table's comparison dimensions. Then read one complete row from left to right. Its cells should form one coherent fact at a consistent level of detail.

## Use Headers as Shared Context

Treat a column header as part of every label beneath it. Read each entry by combining the header with the cell, then remove words in the cell that merely repeat the category already supplied by the header.

Audit the complete column rather than shortening entries one at a time. Sibling labels should use the same naming pattern and abstraction level. If the header makes a suffix redundant for some rows but not others, choose one of three repairs:

- Strengthen the header so every row can omit the suffix.
- Retain the suffix consistently when it belongs to every canonical name.
- Split rows that do not share one naming context into another table or category.

For example, this table repeats its category and then drops it inconsistently:

| Queue | Role |
| --- | --- |
| Work queue | Holds newly accepted jobs. |
| Retry | Holds jobs eligible for another attempt. |
| Dead-letter queue | Holds terminal failures. |

The header already supplies `queue`, so use the distinguishing member names:

| Queue | Role |
| --- | --- |
| Work | Holds newly accepted jobs. |
| Retry | Holds jobs eligible for another attempt. |
| Dead-letter | Holds terminal failures. |

Contextual shortening does not rename the underlying concept. The header and cell together still express `work queue`, `retry queue`, and `dead-letter queue`. Preserve the full canonical term in prose when readers encounter it outside that table context.

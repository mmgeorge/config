# Tables

*Read before adding or reviewing a comparison table, configuration table, parameter list, or schema table.*

## When to use a table

Use a table when the reader compares several items across the same attributes, or looks up a value by name.

- Use a table when items share three or more attributes, such as name, type, default, and description.
- Use a list when items share only one attribute, or when the items are steps in order.
- Use prose when the content explains causes, exceptions, or conditions that do not fit in a cell.

## One kind of row per table

Give each table one kind of item. Do not mix unrelated items, such as user accounts and database migrations, in one table.

Keep a column only when:

- It answers a question the reader has, such as "What happens on failure?"
- Most of its cells hold real values, not blanks or "N/A."
- Its values differ from row to row, so it helps the reader tell rows apart.
- It stays at the same level of detail as the rest of the table.
- Its header is clear without a footnote.

Remove a column that repeats another, has the same value in every row, or is mostly empty. Split the table when some columns apply to only one phase, such as build time or runtime. Move a cell that needs several sentences into the text around the table.

## Headers

Treat each header as a prefix shared by every cell in its column, and drop words from cells that only repeat it. Keep the cells in one column grammatically alike: all nouns, all verbs, or all yes-or-no values. A shortened name in a cell does not replace the full name in the surrounding text.

## Formatting

- Left-align text and code. Right-align numbers. Center short status values.
- Put units in the header, such as `Timeout (ms)`, instead of in every cell.
- Never leave a cell blank. Write `None` or `-`, and say what it means when that is not obvious.
- Put literal values, field names, environment variables, and types in backticks.

## Example

The text before the table states its point: only `retry_delay` takes effect without a restart.

| Setting | Default (ms) | Applies without restart |
| --- | ---: | :---: |
| `retry_delay` | 500 | Yes |
| `max_retry_delay` | 30000 | No |
| `request_timeout` | 10000 | No |

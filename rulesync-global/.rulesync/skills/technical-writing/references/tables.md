# Tables

*Read before adding or reviewing a comparison table, configuration table, parameter list, or schema table.*

## Structure

- Use a table when comparing items across multiple shared attributes or supporting lookup by name. Use a list for ordered steps or single-attribute items, and prose for causes or exceptions.
- Keep one kind of item per table, and include columns whose values actually vary across rows. Move multi-sentence explanations into surrounding prose.
- Lead with a short sentence stating the table's takeaway.

## Headers and Formatting

- Treat each header as a shared prefix, dropping repeated words from cells and keeping cells in a column grammatically parallel.
- Put units in headers (`Timeout (ms)`), wrap literals, fields, and types in backticks, and write `None` or `-` instead of blank cells (defining `-` in prose when not obvious).
- Left-align text and code, right-align numbers, and center short status values.

## Examples

### Comparing Storage Backends

The cache layer supports three storage backends behind the same interface. Each trades persistence and cross-node visibility against local latency.

| Backend | Persistence | Shared across nodes | Eviction policy | Best fit |
| --- | --- | :---: | --- | --- |
| In-memory LRU | None | No | Least recently used | Single-node hot lookups |
| Embedded RocksDB | Local disk | No | Size-bounded compaction | Large read-heavy local caches |
| Redis cluster | Optional snapshots | Yes | TTL and volatile LRU | Shared rate limits and sessions |

### Timeout Configuration

Changing `retry_delay` or `idle_timeout` applies on the next request. Updating `connect_timeout` or `shutdown_grace` requires restarting the process, and `-` marks timeouts with no configured ceiling.

| Setting | Default (ms) | Maximum (ms) | Live reload |
| --- | ---: | ---: | :---: |
| `connect_timeout` | 3000 | 30000 | No |
| `idle_timeout` | 60000 | - | Yes |
| `retry_delay` | 500 | 60000 | Yes |
| `shutdown_grace` | 15000 | 120000 | No |

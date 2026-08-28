# Parquet Inspection

*When to read this:* before inspecting, querying, filtering, or saving Parquet
data with `portal tools parquet`.

## Contents

- Format specification
- Source resolution
- Choose the narrowest command
- Query records
- Inspect statistics
- Save a subset
- Output behavior

## Format specification

Use the Apache Parquet sources when implementation details require more than
the Portal CLI exposes:

- [Apache Parquet format repository](https://github.com/apache/parquet-format/)
  for the format specification, logical types, metadata, encodings, and
  compatibility context.
- [`parquet.thrift`](https://github.com/apache/parquet-format/blob/master/src/main/thrift/parquet.thrift)
  for the authoritative serialized metadata structures used in Parquet files.

## Source resolution

Every inspection command accepts one of these sources:

- A local `.parquet` file.
- An HTTP or HTTPS URL.
- A Portal item URL.
- A 32-character Portal item ID.
- An exact Portal item title.

Omitting the source uses `portal tools parquet select`. Prefer an explicit
source in automation and whenever another task may have changed the selection.
Portal item sources can resolve multiple resource files as one sequential
dataset.

## Choose the narrowest command

| Goal | Command |
| --- | --- |
| Geospatial overview | `portal tools parquet meta <source>` |
| Physical and logical fields | `portal tools parquet schema <source>` |
| File key-value metadata | `portal tools parquet kv <source>` |
| GeoParquet metadata only | `portal tools parquet kv <source> --geo` |
| Esri geodisplay metadata only | `portal tools parquet kv <source> --spatial` |
| Aggregated row-group statistics | `portal tools parquet stats <source>` |
| Row-group internals | `portal tools parquet groups <source>` |
| Compression codecs | `portal tools parquet codecs <source>` |
| Records | `portal tools parquet query <source>` |
| Filtered copy | `portal tools parquet save <source> --output <file>` |

`meta` summarizes primary geometry, CRS, extent, and Esri spatial optimization.
It does not replace `schema`, `kv`, or `stats`.

## Query records

```sh
portal tools parquet query ./data/counties.parquet \
  --filter 'POP>10000,STATE=CA' \
  --columns NAME,POP \
  --start 0 \
  --num 25
```

- Comma-separated predicates combine with AND.
- Supported simple operators are `=`, `>`, and `<`.
- A raw hyparquet JSON filter is also accepted.
- `--num` defaults to 10.
- Column terms perform matching rather than requiring full nested paths.
- GeoParquet WKB renders as GeoJSON.
- Esri-PBF geodisplay geometry receives its multiscale transform unless
  `--raw` is passed.

Use `--raw` only when the task requires encoded geometry lengths and
coordinates before transformation.

## Inspect statistics

```sh
portal tools parquet stats ./data/counties.parquet \
  --column geodisplay,indexkey
```

The result follows `_spatial.json`-style structure with file name, record
count, byte size, key-value metadata, and available column statistics.
Statistics may omit:

- Columns whose row groups contain no statistics.
- Distinct counts when a column spans multiple chunks.
- Min and max values for fields not classified as numeric.

A single `--column` term matches case-insensitive substrings in any path
component. Comma-separated terms match corresponding nested path components.

## Save a subset

```sh
portal tools parquet save ./data/counties.parquet \
  --filter 'POP>10000' \
  --start 0 \
  --num 1000 \
  --output ./data/counties-large.parquet
```

The output preserves the first source file's schema and key-value metadata and
writes Parquet statistics and indexes. Verify the result with `meta`, `schema`,
and a bounded `query`.

## Output behavior

- Commands print JSON except for progress and errors.
- Safe BigInts print as numbers. Larger BigInts print as strings.
- Binary metadata prints as `blob(<length>)`.
- Multi-file Portal items wrap each result with its resource name.
- Missing files and unknown filter columns return specific errors. Report them
  directly instead of substituting another source or field.

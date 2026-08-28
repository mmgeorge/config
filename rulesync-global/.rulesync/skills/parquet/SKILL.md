---
name: parquet
description: >-
  Inspects, filters, subsets, publishes, updates, shares, and visualizes Parquet
  datasets with the portal CLI, including ArcGIS Parquet Feature Layer items,
  Azure Blob or ADLS storage, _spatial.json metadata, and ArcGIS Play apps. Use
  when a request names portal tools parquet, portal item parquet, an ArcGIS
  Parquet Feature Layer, ParquetLayer, _spatial.json, or ArcGIS Play delivery
  from Azure Parquet files. Do not use for unrelated Portal CLI administration,
  PyArrow, Spark, DuckDB, or generic Azure file-transfer work.
targets:
  - copilotcli
---
# Portal Parquet

Use the `portal` CLI as the default interface for Parquet inspection and ArcGIS
delivery. Keep local analysis, Portal item resources, Azure blobs, and ArcGIS
Play documents distinct because they have different ownership and failure modes.

## Workflow

- [ ] 1. Confirm the command surface with `portal --help` and the relevant
      namespace help before mutating remote state.
- [ ] 2. Inspect the source with an explicit path, URL, item ID, or exact item
      title. Prefer explicit values over persisted selections.
- [ ] 3. Choose one destination:
      - Portal item resources: `portal item parquet upload`.
      - Existing Portal item replacement: `portal item parquet update <item-id> <target>`.
      - Azure Blob or ADLS: `portal tools azure file upload`.
- [ ] 4. Run `meta` first. Generate `_spatial.json` when it reports primary
      geometry or spatial optimization and the remote dataset needs
      dataset-level spatial discovery, especially for a multi-file directory.
- [ ] 5. Verify the remote files, item metadata, sharing, and spatial metadata.
      Do not trust command exit status alone for Portal uploads.
- [ ] 6. Open an ArcGIS Play document only after the data URL or item resolves.

## Always-apply rules

- **Never promise a private new Portal upload.** The current implementation
  shares new `portal item parquet upload` items publicly even when `--public`
  is omitted. Stop before upload when public sharing is unacceptable.
- **Use item IDs for mutations.** Exact titles can be ambiguous. Persisted
  selections make `--overwrite`, update, delete, and resource commands easy to
  aim at the wrong item.
- **Prefer `update <item-id> <target>` over `upload --overwrite`.** Update names
  the target explicitly and swaps resource directories before removing the old
  one.
- **Treat `Exited with errors` as failure even if the process returns success.**
  Verify with `portal item parquet meta`, `resource list`, and `spatial get`.
- **Do not call a Parquet Feature Layer a feature service.** Upload creates a
  `Parquet Feature Layer` item whose files live under item resources. It does
  not create a hosted feature service or Portal publish job.
- **Do not call `app` a Portal Application item.** Portal and Azure `app`
  commands create and open ArcGIS Play documents. They do not create durable
  Portal Application items or author Web Maps.
- **Confirm before destructive actions.** Resolve the item ID or Azure path,
  show the user what will change, then run overwrite, update, move, remove, or
  delete operations.

## Quick inspection

```sh
portal tools parquet meta ./data/counties.parquet
portal tools parquet schema ./data/counties.parquet
portal tools parquet query ./data/counties.parquet --columns NAME,POP --num 10
portal tools parquet kv ./data/counties.parquet --spatial
portal tools parquet stats ./data/counties.parquet --column geodisplay,indexkey
```

Read [`references/inspection.md`](references/inspection.md) before choosing
inspection commands, filtering records, or writing a subset.

## Quick Portal publishing

```sh
portal item parquet upload ./data/counties --name "Counties" --spatial
portal item parquet meta <item-id>
portal item parquet resource list <item-id>
portal item parquet spatial get <item-id>
portal item parquet app <item-id>
```

Read [`references/portal-items.md`](references/portal-items.md) before creating,
copying, replacing, sharing, downloading, or deleting Portal Parquet items.

## Quick Azure delivery

```sh
portal config azure storage add https://<account>.blob.core.windows.net <alias>
portal config azure switch <alias>
portal tools azure select <container>
portal tools azure cdn sync
portal tools azure file upload ./data/counties --path parquet/counties
portal tools azure spatial set <alias>/<container>/parquet/counties
portal tools azure file app --path parquet/counties
```

Read [`references/azure-and-apps.md`](references/azure-and-apps.md) before
uploading to Azure, resolving CDN URLs, generating remote spatial metadata, or
opening ArcGIS Play.

## Error handling

- Preserve and report the underlying Portal, Azure, HTTP, JSON, or Parquet
  error. Never convert failure into an empty result.
- A missing source or filter column should stop the workflow. Do not guess a
  replacement path or field.
- Azure upload may finish writing blobs and then fail because no cached CDN
  route matches. Inspect the destination before retrying to avoid redundant
  uploads.
- Private Portal items may fail before ArcGIS Play opens because the app command
  can fetch item metadata anonymously. Browser authentication alone does not
  guarantee that `portal item parquet app` can create the document.

## Reference map

| Read this… | …when you are |
| --- | --- |
| **`references/inspection.md`** | Inspecting schema, metadata, statistics, rows, filters, or subsets. |
| **`references/portal-items.md`** | Publishing or managing ArcGIS Parquet Feature Layer items and resources. |
| **`references/azure-and-apps.md`** | Transferring Parquet through Azure or creating ArcGIS Play documents. |

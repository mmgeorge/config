# Azure Parquet and ArcGIS Play

*When to read this:* before uploading Parquet to Azure Blob or ADLS, generating
remote `_spatial.json`, resolving CDN URLs, or opening an Azure-backed ArcGIS
Play document.

## Configure storage

```sh
portal config azure storage add \
  https://<account>.blob.core.windows.net \
  <alias>
portal config azure switch <alias>
portal tools azure list
portal tools azure select <container>
```

`--sa <alias>` selects a storage account for one command without changing the
active alias. Bucket arguments default to the selected container.

Reads first attempt public Blob access. On authorization failure, the CLI tries
Azure CLI credentials, then device-code authentication with cached tokens.

## Upload and verify

```sh
portal tools azure cdn sync
portal tools azure file upload ./data/counties \
  --path parquet/counties
portal tools azure file list --path parquet/counties
portal tools azure file cdn --path parquet/counties
```

- Azure directory uploads recurse through regular files.
- The destination container must already exist.
- Existing blobs prompt before replacement. Use `--overwrite` for approved,
  noninteractive replacement.
- Partial uploads are not rolled back when a later file fails.
- Successful blob transfer is followed by cached CDN or Front Door lookup.
  Missing routes can make the command fail after the blobs already exist.

Run `cdn sync` before CDN-dependent upload and app workflows. If upload reports
a CDN error or partial failure:

1. List the destination and compare it with the expected local relative paths.
2. Preserve blobs that completed successfully.
3. Retry only missing files when possible.
4. Request confirmation before overwriting or deleting mismatched blobs.

## Generate spatial metadata

```sh
portal tools azure spatial set \
  <alias>/<container>/parquet/counties
portal tools azure spatial get \
  <alias>/<container>/parquet/counties \
  --column geodisplay,indexkey
```

Spatial generation scans the prefix recursively for case-sensitive
`*.parquet` names, reads file footers remotely, and writes `_spatial.json`.
Use `--overwrite` only after confirming replacement of an existing metadata
file. Generate it after local `meta` reports primary geometry or spatial
optimization and the uploaded directory needs dataset-level spatial discovery.

## Resolve URLs and open ArcGIS Play

```sh
portal tools azure file url --path parquet/counties
portal tools azure file cdn --path parquet/counties
portal tools azure file app --path parquet/counties
portal tools azure file app --path parquet/counties --staging
```

`file url` prints the canonical Blob URL without proving public readability.
`file cdn` prints cached CDN and Front Door matches.

`file app` recursively discovers Parquet blobs, chooses the first cached CDN or
Front Door URL for each file, and posts an ArcGIS Play document using
`ParquetFilesData`. It does not create a Portal Application item.

## Move and remove

```sh
portal tools azure file mv parquet/counties parquet/archive/counties
portal tools azure file rm parquet/archive/counties
```

Move and remove require ADLS Gen2 hierarchical namespace and an interactive
confirmation terminal. Move refuses an existing destination. Resolve and show
the exact source and destination before running either command.

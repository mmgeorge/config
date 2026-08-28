# Portal Parquet Items

*When to read this:* before creating, replacing, copying, sharing, downloading,
or deleting an ArcGIS Parquet Feature Layer item.

## Contents

- What upload creates
- Create an item
- Replace an existing item
- Verify every mutation
- Copy, download, and resource operations
- Sharing and selection
- ArcGIS Play

## What upload creates

`portal item parquet upload` creates a Portal item with:

- Type `Parquet Feature Layer`.
- Tag `parquet-dataset`.
- Type keyword `parquet`.
- Item URL `./resources/default/`.
- Parquet files stored as `default/<filename>` item resources.

This path does not create a feature service, run a Portal publish job, or copy
the data into a hosted feature layer.

## Create an item

```sh
portal item parquet upload ./data/counties.parquet \
  --name "Counties" \
  --spatial
```

For a directory target, dataset upload reads only immediate recognized Parquet
files. It does not recurse into subdirectories. The upload accepts at most
1,000 files and rejects duplicate normalized resource names and empty files.

`--spatial` generates `default/_spatial.json` locally and uploads it with the
dataset.

**Public-sharing guard:** the current implementation shares every newly
uploaded item publicly even when `--public` is absent. Do not run this command
when the item must remain private.

## Replace an existing item

Prefer the explicit update form:

```sh
portal item parquet update <item-id> ./data/counties --spatial
```

Update uploads into a new timestamped resource directory, switches the item URL
to that directory, then removes the old directory. If upload fails before the
URL switch, cleanup of the new directory is attempted but can also fail. If
old-directory cleanup fails after the switch, the new data remains active and
the old resources may remain. Inspect `resource list` and remove only confirmed
orphan paths before retrying.

`portal item parquet upload <target> --overwrite` only targets the currently
selected item. Avoid it unless the selected item ID has just been verified.

## Verify every mutation

```sh
portal item parquet meta <item-id>
portal item parquet data <item-id>
portal item parquet resource list <item-id>
portal item parquet spatial get <item-id>
portal item parquet file kv <item-id> default/counties.parquet --spatial
```

The upload command can print `Exited with errors` without returning a failing
process status. Treat that text as failure and verify the item and resources
before reporting success.

## Copy, download, and resource operations

```sh
portal item parquet copy <item-id> --name "Counties Copy" --spatial
portal item parquet download <item-id> --out ./counties-download
portal item parquet resource download <item-id> default/counties.parquet
portal item parquet resource upload <item-id> ./supplemental
```

- Copy downloads all resources but reuploads only Parquet files from the active
  dataset directory.
- Copy does not preserve the old `_spatial.json`. Pass `--spatial` to regenerate
  it.
- Generic resource directory upload recurses and updates existing resource
  paths. Dataset creation from a directory does not recurse.

## Sharing and selection

```sh
portal item parquet share <item-id> --level owner
portal item parquet share <item-id> --level org
portal item parquet share <item-id> --level public
portal item parquet select <item-id>
```

Use item IDs for scripts and mutations. Exact names can be ambiguous.
`--portal <alias>` chooses a configured Portal for one command without changing
the active Portal.

## ArcGIS Play

```sh
portal item parquet app <item-id>
portal item parquet app <item-id> --staging
```

This posts an ArcGIS Play document whose `ParquetLayer` references the Portal
item ID and Portal base URL. It does not create a Portal Application item or a
Web Map. Private items can fail during the command's anonymous metadata lookup
before browser authentication can help.

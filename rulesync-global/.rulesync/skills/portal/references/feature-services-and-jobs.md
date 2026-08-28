# Feature Services and Jobs

*When to read this:* before inspecting Feature Service layers, copying or
cloning items, exporting data, changing cache or multiscale settings, or
monitoring jobs.

## Inspect Feature Layers

```sh
portal item feature list --portal <alias> --hosted --name parcels
portal item feature meta <item-id> --portal <alias>
portal item feature layers <item-id> --portal <alias> --features 5
```

`feature list` adds mutually exclusive `--hosted` and `--remote` filters.
`layers` prints abbreviated service-layer details and can query a bounded sample
of features.

Private item metadata can resolve while service inspection still fails because
some Feature Service requests do not carry the cached token.

## Copy versus clone

```sh
portal item feature copy <item-id> --portal <alias> --name "Parcels Copy"
portal item feature clone <item-id> --portal <alias> \
  --layer 0 --name "Parcels Clone"
portal item feature clone-from <public-url> --portal <alias> \
  --layer 0 --name "Remote Parcels"
```

- `copy` uses Portal's server-side item-copy operation and includes resources.
  It does not recreate relationships or dependent items.
- `clone` creates an independent output through ArcGIS Data Pipelines.
- `clone-from` accepts a public Portal item URL or FeatureServer URL and reads
  it anonymously.
- `--layer` is required unless the source has exactly one layer.
- `--format feature` is the default.
- `--format parquet` creates Parquet output. Use the `parquet` skill for all
  resulting item inspection and management.

Clone creates a temporary Data Pipeline item and polls its run. Successful
output can exist even when later pipeline cleanup fails. Failed runs retain the
pipeline item for inspection.

## Export

```sh
portal item feature export start <item-id> --portal <alias> \
  --format geojson \
  --title "Parcels Export" \
  --parameters '{"layers":[{"id":0,"where":"1=1"}]}'
portal item feature export status <item-id> --portal <alias>
portal item feature export list <item-id> --portal <alias>
portal item feature export download <job-id> --portal <alias> --out ./export.geojson
```

Export requires a title and format. Parameters must be a JSON object and
default to `{}` for all layers. Common formats include shapefile, file
geodatabase, GeoJSON, Excel, GeoPackage, KML, vector tile package, and mobile
geodatabase.

Downloads do not delete the generated export item and can overwrite an
existing local output path.

## Tracked jobs

```sh
portal job list
portal job status <job-id>
```

Jobs are stored locally per Portal alias. Top-level job commands use only the
active alias, so switch before querying another Portal. Job records are not
automatically pruned.

Export refresh can catch a remote error, store it as `statusMessage`, and return
without a failing process status. Inspect job status content.

## Cache and multiscale geometry

```sh
portal item feature cache status <item-id> --portal <alias>
portal item feature cache enable <item-id> --portal <alias> --age 5
portal item feature cache disable <item-id> --portal <alias>

portal item feature multiscale status <item-id> --portal <alias>
portal item feature multiscale enable <item-id> --portal <alias>
portal item feature multiscale disable <item-id> --portal <alias>
```

Cache age uses positive integer minutes and defaults to five. Enterprise can
translate the value into whole-day layer expiration.

Multiscale applies only to eligible non-point layers and may start asynchronous
service jobs. Check status after enabling or disabling. These mutations do not
prompt, so confirm the item ID and Portal first.


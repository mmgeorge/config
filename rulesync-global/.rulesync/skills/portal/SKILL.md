---
name: portal
description: >-
  Configures and operates the portal CLI for ArcGIS Online and Enterprise
  portals, including authentication, item discovery, generic resources, Feature
  Layers, Web Maps, exports, tracked jobs, cache settings, multiscale geometry,
  sharing, and ArcGIS Play previews. Use when a request names the portal CLI,
  portal config, portal item, portal job, Feature Layer items, Web Map items, or
  ArcGIS Portal item management. Use the parquet skill instead for portal tools
  parquet, portal item parquet, Azure Parquet delivery, or ParquetLayer work.
targets:
  - copilotcli
---
# Portal CLI

Use `portal` as the default interface for ArcGIS Portal item inspection and
management. Make the Portal alias, item ID, item type, and mutation explicit
before changing remote state because the CLI persists active aliases and
per-type selections.

## Workflow

- [ ] 1. Confirm the command surface with `portal --help` and the relevant
      namespace help.
- [ ] 2. Inspect `portal config show`. Choose an explicit `--portal <alias>` for
      item commands when more than one Portal exists.
- [ ] 3. Discover the target with `list`, then resolve it to a 32-character item
      ID. Avoid title-based mutations.
- [ ] 4. Inspect `meta`, `data`, `layers`, `inspect`, resources, or job state
      before changing anything.
- [ ] 5. Show the exact Portal alias, item ID, operation, sharing level, and
      affected resource path before a mutation.
- [ ] 6. Run the narrowest command and verify remote state afterward. Inspect
      output text as well as the process exit code.

## Always-apply rules

- **Use IDs and explicit aliases for mutations.** Exact titles are
  case-sensitive and can be ambiguous. Persisted selections can point at a
  different item than the current task expects.
- **Never omit `--level` when sharing.** Share defaults to `public`.
- **Confirm destructive operations.** Item deletion, resource replacement or
  deletion, `config clean`, cache changes, and multiscale changes do not prompt.
- **Do not trust every zero exit code.** Item deletion can print `[Failed]` and
  still complete with success-shaped output. Export refresh can persist an
  error as job state without failing the command.
- **Treat ArcGIS Play as external disclosure.** `app` posts a document to an
  Esri-hosted Play service. Web Map `--mini` modes upload embedded map JSON.
- **Do not call `app` a Portal Application item.** It creates and opens an
  ArcGIS Play document.
- **Route Parquet work to the `parquet` skill.** This includes
  `portal tools parquet`, `portal item parquet`, `_spatial.json`, Azure Parquet,
  and ParquetLayer workflows.

## Quick start

```sh
portal config show
portal item list --portal <alias> --me
portal item any meta <item-id> --portal <alias>
portal item any data <item-id> --portal <alias>
portal item any resource list <item-id> --portal <alias>
```

Read [`references/configuration.md`](references/configuration.md) before adding
or removing Portals, authenticating, switching aliases, using selections, or
cleaning state.

Read [`references/items-and-resources.md`](references/items-and-resources.md)
before searching, sharing, deleting, or modifying generic item resources.

## Feature Layers

```sh
portal item feature layers <item-id> --portal <alias> --features 5
portal item feature copy <item-id> --portal <alias> --name "Copy"
portal item feature export start <item-id> --portal <alias> \
  --format geojson --title "Export"
portal job list
```

Read
[`references/feature-services-and-jobs.md`](references/feature-services-and-jobs.md)
before copying, cloning, exporting, changing cache or multiscale settings, or
working with tracked jobs.

## Web Maps

```sh
portal item webmap inspect <item-id> --portal <alias>
portal item webmap view <item-id> --portal <alias>
```

Read [`references/webmaps-and-play.md`](references/webmaps-and-play.md) before
opening Map Viewer or posting ArcGIS Play documents.

## Error handling

- Preserve the underlying HTTP, ArcGIS REST, authentication, JSON, or file
  error. Never substitute an empty result or a different item.
- Public reads may run anonymously. Use authenticated `--me` searches when
  private current-user content must be included.
- A private Feature Service can pass item lookup and still fail service
  inspection because some service requests do not carry the cached token.
- Feature Layer `app` can embed the active Portal URL in its component shell
  even when item resolution uses `--portal`. Switch to the target alias before
  creating that Play document.

## Reference map

| Read this… | …when you are |
| --- | --- |
| **`references/configuration.md`** | Managing Portal aliases, authentication, persisted selections, or local state. |
| **`references/items-and-resources.md`** | Searching items, inspecting metadata/data, sharing, deleting, or managing resources. |
| **`references/feature-services-and-jobs.md`** | Copying, cloning, exporting, optimizing, or monitoring Feature Layers. |
| **`references/webmaps-and-play.md`** | Inspecting Web Maps, opening Map Viewer, or creating ArcGIS Play documents. |

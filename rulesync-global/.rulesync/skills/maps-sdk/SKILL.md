---
name: maps-sdk
description: >-
  Builds and debugs ArcGIS Maps SDK for JavaScript applications and ArcGIS
  Play documents using map components, FeatureLayer, ParquetLayer, renderers,
  popups, layer views, and view lifecycle. Use when creating, inspecting,
  editing, running, or troubleshooting a JSAPI or Maps SDK app, including an
  ArcGIS Play document through play-mcp. Consults the local WebGIS SDK source
  before web documentation. Use the portal skill for Portal item discovery
  and management, and the parquet skill for Parquet publishing or dataset
  management.
targets:
  - copilotcli
---
# ArcGIS Maps SDK

Use the local WebGIS SDK checkout as the API source of truth. For ArcGIS Play, use Play diagnostics to verify the code against the deployed runtime.

## Workflow

- [ ] 1. Identify the runtime, app surface, content source, Portal URL, and expected interaction. Do not guess item IDs, layer indexes, fields, geometry, renderers, or viewpoints.
- [ ] 2. Read [`references/local-source-research.md`](references/local-source-research.md) before researching a Maps SDK class, method, event, component, or type.
- [ ] 3. When content must be found, load the `portal` skill and resolve an exact item ID and Portal URL. For Parquet upload, replacement, metadata, or resources, load the `parquet` skill.
- [ ] 4. Read [`references/layer-recipes.md`](references/layer-recipes.md) when constructing a `FeatureLayer` from a service or a `ParquetLayer` from a Portal item.
- [ ] 5. For ArcGIS Play, read [`references/arcgis-play.md`](references/arcgis-play.md) before inspecting or changing the document.
- [ ] 6. Build the smallest complete app. In Play, use `esri/...` imports, ArcGIS map components, and Calcite UI without adding CDN tags. In a repository app, inspect and preserve its framework, imports, and build conventions.
- [ ] 7. Validate in the target runtime. For Play, run or refresh the app, inspect editor and runtime diagnostics, and inspect live view state. Fix every unexpected error before reporting success.

## Always-apply rules

- **Search local source before the web.** Start under `/Users/matt9222/Developer/webgis/main/packages/core-packages/core`. Do not use external documentation or code search until the local source, JSDoc, and tests prove insufficient.
- **Validate in the actual runtime.** The source checkout describes the intended API. Play diagnostics prove Play behavior. Repository tests and builds prove behavior for non-Play apps.
- **Keep content discovery separate from app authoring.** The `portal` skill resolves items and inspects services. The `parquet` skill owns dataset inspection, publishing, replacement, and resource management.
- **Use explicit Portal ownership.** When a Portal item does not belong to the default Portal, construct `Portal` and `PortalItem` explicitly. Avoid changing global `config.portalUrl` unless the entire app intentionally uses that Portal.
- **Use layer endpoints.** A `FeatureLayer` service URL must identify a layer, such as `FeatureServer/0` or `MapServer/3`. If given a service root, inspect its REST metadata and resolve the intended layer instead of appending an index.
- **Respect view readiness.** Assign the map and any known initial viewpoint before `await viewElement.viewOnReady()`. Await layer loading before reading fields, full extent, capabilities, or schema-dependent properties.
- **Design for dataset size.** Derive the initial extent and scale range from item metadata, layer metadata, or the requested study area. Avoid opening a national block dataset at a continental scale without a visibility threshold.
- **Preserve failures.** Report load, authentication, Portal, service, editor, and runtime errors directly. Never replace a failed layer with unrelated sample data or claim success because the view alone became ready.

## Portal content lookup

Load the `portal` skill, then discover and inspect the exact content:

```sh
portal config show
portal item any list --portal <alias> --name "census blocks" --me
portal item any meta <item-id> --portal <alias>
```

Use `--public` instead of `--me` only when the user wants public content. For a Feature Layer item, inspect its layers before selecting a service URL. For a Parquet Feature Layer item, load the `parquet` skill and inspect its metadata and resources. Return to this skill once the item ID, type, and Portal URL are known.

## Reference map

| Read this… | …when you are |
| --- | --- |
| **`references/local-source-research.md`** | Resolving a Maps SDK API, import, constructor, lifecycle, event, or type from local source. |
| **`references/layer-recipes.md`** | Building a FeatureLayer service app or ParquetLayer Portal-item app. |
| **`references/arcgis-play.md`** | Connecting to, inspecting, editing, running, authenticating, or saving an ArcGIS Play document. |

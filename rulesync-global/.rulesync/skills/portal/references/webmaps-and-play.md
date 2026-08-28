# Web Maps and ArcGIS Play

*When to read this:* before inspecting Web Map structure, opening Map Viewer, or
posting a Web Map or Feature Layer document to ArcGIS Play.

## Inspect and open

```sh
portal item webmap list --portal <alias> --me --name operations
portal item webmap inspect <item-id> --portal <alias>
portal item webmap data <item-id> --portal <alias>
portal item webmap view <item-id> --portal <alias>
```

`inspect` summarizes authoring metadata, spatial reference, operational and
basemap layers, URLs, item IDs, visibility, renderer types, and visual
variables. `view` opens the item in Map Viewer.

## ArcGIS Play documents

```sh
portal item feature app <feature-item-id> --portal <alias>
portal item webmap app <webmap-item-id> --portal <alias>
portal item webmap app <webmap-item-id> --portal <alias> --mini
portal item webmap app <webmap-item-id> --portal <alias> --mini-no-strip
```

These commands post documents to an Esri-hosted ArcGIS Play service and open
the resulting URL. They do not create Portal Application items.

- Feature `app` references the Feature Layer item.
- Web Map `app` normally references the Web Map item.
- `--mini` embeds minimized Web Map JSON and removes invisible operational
  layers.
- `--mini-no-strip` embeds minimized JSON while retaining invisible layers.
- `--staging` targets the staging ArcGIS Play environment.

## Privacy and alias boundaries

Treat Play creation as external disclosure. Normal mode sends Portal and item
references. Mini modes additionally send the Web Map JSON, which can expose
private layer URLs, renderers, and configuration.

Request explicit approval before using mini mode with private or sensitive map
definitions.

Private Feature Services can fail after item lookup because generated service
inspection does not reliably carry the Portal token.

Web Map Play documents use the resolved Portal alias. Feature Layer `app`
documents use resolved service URLs but can embed the active Portal URL in the
shared component shell. Switch to the target alias before creating a Feature
Layer Play document.

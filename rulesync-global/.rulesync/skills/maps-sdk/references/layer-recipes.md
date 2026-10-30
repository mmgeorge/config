# Maps SDK Layer Recipes

*When to read this:* when creating a Maps SDK or ArcGIS Play app from a Feature Service or Map Service layer URL, or from a Parquet Feature Layer Portal item.

## Contents

- Shared Play shell
- FeatureLayer from a service
- ParquetLayer from a Portal item
- Content and schema checks

## Shared Play shell

ArcGIS Play already loads ArcGIS and Calcite web components. Do not add CDN scripts or styles.

```html
<arcgis-map id="map">
  <arcgis-zoom slot="top-left"></arcgis-zoom>
  <arcgis-home slot="top-left"></arcgis-home>
  <arcgis-legend slot="bottom-left"></arcgis-legend>
</arcgis-map>
```

```css
html,
body,
arcgis-map {
  height: 100%;
  margin: 0;
}
```

Use complete start and end tags for ArcGIS components. Add Calcite components for panels, actions, inputs, notices, and other application UI.

## FeatureLayer from a service

Use a layer endpoint with its layer index. Resolve the URL instead of guessing the index. If the input ends at `FeatureServer` or `MapServer`, inspect `<service-root>?f=json` and select the intended entry from `layers` or `tables`. Stop when the title or geometry remains ambiguous.

```ts
import Map from "esri/Map";
import FeatureLayer from "esri/layers/FeatureLayer";

const featureLayerUrl = "<resolved-layer-url>";
const featureLayer = new FeatureLayer({
  url: featureLayerUrl,
  outFields: ["*"],
});

const viewElement = document.querySelector("arcgis-map");

if (!viewElement) {
  throw new Error("Expected an arcgis-map element");
}

viewElement.map = new Map({
  basemap: "gray-vector",
  layers: [featureLayer],
});

await viewElement.viewOnReady();
await featureLayer.load();

if (featureLayer.fullExtent) {
  await viewElement.view.goTo(featureLayer.fullExtent);
}
```

Preserve a service-defined renderer by default. Add or replace a renderer only after loading the layer and confirming its geometry and fields.

Use `FeatureLayer({ portalItem: { id } })` when the user explicitly wants a Feature Layer Portal item rather than a service URL. Use the `portal` skill to resolve the item and inspect its layers first.

If the service requires credentials, do not assume ArcGIS Play document authentication authorizes the service. Establish the required SDK identity flow or stop with the authentication failure. Preserve CORS and proxy errors instead of swapping in sample content.

## ParquetLayer from a Portal item

Construct the Portal explicitly. `ParquetLayer` receives the item through `ParquetPortalItemData`, not through `FeatureLayer.portalItem`.

```ts
import Map from "esri/Map";
import ParquetLayer from "esri/layers/ParquetLayer";
import ParquetPortalItemData from "esri/layers/support/ParquetPortalItemData";
import Portal from "esri/portal/Portal";
import PortalItem from "esri/portal/PortalItem";

const portalUrl = "<resolved-portal-url>";
const parquetItemId = "<resolved-item-id>";
const portal = new Portal({
  url: portalUrl,
});

const portalItem = new PortalItem({
  id: parquetItemId,
  portal,
});

const parquetLayer = new ParquetLayer({
  data: new ParquetPortalItemData({
    portalItem,
  }),
});

const viewElement = document.querySelector("arcgis-map");

if (!viewElement) {
  throw new Error("Expected an arcgis-map element");
}

viewElement.map = new Map({
  basemap: "gray-vector",
  layers: [parquetLayer],
});

await viewElement.viewOnReady();
await parquetLayer.load();

if (parquetLayer.fullExtent) {
  await viewElement.view.goTo(parquetLayer.fullExtent);
}
```

The item must resolve as supported Parquet content. Use the `parquet` skill for item metadata, resources, spatial metadata, publishing, or replacement. For a dense dataset, set `minScale` and a requested study-area viewpoint before running the app. If item metadata lacks an extent, require a study area or use a full extent proven by `layer.load()`. Do not invent a city from the title.

## Content and schema checks

- Confirm the Portal URL, item ID, item type, service URL, and layer index.
- Await `layer.load()` before reading `fields`, `geometryType`, `fullExtent`, `capabilities`, or schema-dependent renderer fields.
- Choose renderer fields only after inspecting the schema.
- Set a focused viewpoint or use a proven extent. Avoid loading dense national polygons at a scale that draws every feature.
- Use `minScale`, `definitionExpression`, display filters, or generalization only when the dataset and requested experience justify them.
- Add popups from known fields. Do not invent field names from a title.

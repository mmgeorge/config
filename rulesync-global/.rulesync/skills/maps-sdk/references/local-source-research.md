# Local Maps SDK Source Research

*When to read this:* before looking up an ArcGIS Maps SDK class, component,
constructor, property, method, event, import path, or runtime behavior.

## Contents

- Source roots
- Research order
- Direct source index
- Web fallback
- Version mismatch handling

## Source roots

Use these local checkouts before external documentation:

```text
/Users/matt9222/Developer/webgis/main/packages/core-packages/core
/Users/matt9222/Developer/webgis/main/packages/map-packages/map-components
```

The core package embeds public API documentation and examples in source JSDoc.
Its tests show concrete construction patterns and edge cases. Map components
source and stories show component properties, events, slots, and readiness.

If the default checkout does not exist, search only under
`/Users/matt9222/Developer/webgis/` for another
`packages/core-packages/core` checkout. Prefer `main`. If multiple non-main
checkouts could define the target runtime, ask which checkout to use.

## Research order

1. Locate the owning class or component under `src/`.
2. Read its public JSDoc, constructor type, property declaration, and imported
   support types.
3. Read the directly referenced support class when construction crosses an
   ownership boundary. For example, read both `ParquetLayer.ts` and
   `support/ParquetPortalItemData.ts`.
4. Search focused tests for the exact constructor or method. Prefer unit tests
   for serialization and types, then integration tests for runtime behavior.
5. Search component stories or tests when the app uses `<arcgis-map>`,
   `<arcgis-scene>`, slots, component events, or `viewOnReady()`.
6. Use Play diagnostics to validate the resulting code.

If the Maps SDK checkout is the current indexed repository, prefer semantic
code intelligence for the owning entity and its dependencies. Otherwise use
bounded file reads and searches under the absolute paths above.

## Direct source index

Use these links to enter the implementation at the owning boundary.

### Public layer APIs

- [`FeatureLayer.ts`](file:///Users/matt9222/Developer/webgis/main/packages/core-packages/core/src/layers/FeatureLayer.ts)
- [`ParquetLayer.ts`](file:///Users/matt9222/Developer/webgis/main/packages/core-packages/core/src/layers/ParquetLayer.ts)
- [`ParquetPortalItemData.ts`](file:///Users/matt9222/Developer/webgis/main/packages/core-packages/core/src/layers/support/ParquetPortalItemData.ts)

### Layer-view selection and ownership

- [`layerViewModuleImportUtils.ts`](file:///Users/matt9222/Developer/webgis/main/packages/core-packages/core/src/views/2d/layerViewModuleImportUtils.ts)
- [`FeatureLikeLayerView2D.ts`](file:///Users/matt9222/Developer/webgis/main/packages/core-packages/core/src/views/2d/layers/FeatureLikeLayerView2D.ts)
- [`FeatureLayerView2D.ts`](file:///Users/matt9222/Developer/webgis/main/packages/core-packages/core/src/views/2d/layers/FeatureLayerView2D.ts)
- [`ParquetLayerView2D.ts`](file:///Users/matt9222/Developer/webgis/main/packages/core-packages/core/src/views/2d/layers/ParquetLayerView2D.ts)
- [`FeatureLayerView.ts`](file:///Users/matt9222/Developer/webgis/main/packages/core-packages/core/src/views/layers/FeatureLayerView.ts)
- [`ParquetLayerView.ts`](file:///Users/matt9222/Developer/webgis/main/packages/core-packages/core/src/views/layers/ParquetLayerView.ts)

### 2D feature adapters

- [`FeatureLayerAdapter.ts`](file:///Users/matt9222/Developer/webgis/main/packages/core-packages/core/src/views/2d/layers/features/layerAdapters/FeatureLayerAdapter.ts)
- [`ParquetLayerAdapter.ts`](file:///Users/matt9222/Developer/webgis/main/packages/core-packages/core/src/views/2d/layers/features/layerAdapters/ParquetLayerAdapter.ts)
- [`layerAdapters/types.ts`](file:///Users/matt9222/Developer/webgis/main/packages/core-packages/core/src/views/2d/layers/features/layerAdapters/types.ts)

Read `FeatureLikeLayerView2D` and the matching adapter when diagnosing
rendering, filtering, highlighting, schema, or layer-view behavior. Read
`layerViewModuleImportUtils` when tracing which concrete layer view loads.

### Map component

- [`map.tsx`](file:///Users/matt9222/Developer/webgis/main/packages/map-packages/map-components/src/components/map/map.tsx)

### Behavioral tests

- [`FeatureLayer.spec.ts`](file:///Users/matt9222/Developer/webgis/main/packages/core-packages/core/tests/unitTests/src/layers/FeatureLayer.spec.ts)
- [`FeatureLayerView2D.spec.ts`](file:///Users/matt9222/Developer/webgis/main/packages/core-packages/core/tests/integrationTests2d/src/views/2d/layers/FeatureLayerView2D.spec.ts)
- [`FeatureLayerView2D_Query.spec.ts`](file:///Users/matt9222/Developer/webgis/main/packages/core-packages/core/tests/integrationTests2d/src/views/2d/layers/FeatureLayerView2D_Query.spec.ts)
- [`FeatureLayerView2D_Renderer.spec.ts`](file:///Users/matt9222/Developer/webgis/main/packages/core-packages/core/tests/screenshotTests2d/src/views/2d/layers/FeatureLayerView2D_Renderer.spec.ts)
- [`ParquetLayer.spec.ts` unit tests](file:///Users/matt9222/Developer/webgis/main/packages/core-packages/core/tests/unitTests/src/layers/ParquetLayer.spec.ts)
- [`ParquetLayer.spec.ts` integration tests](file:///Users/matt9222/Developer/webgis/main/packages/core-packages/core/tests/integrationTests2d/src/layers/ParquetLayer.spec.ts)
- [`ParquetLayer.spec.ts` screenshot tests](file:///Users/matt9222/Developer/webgis/main/packages/core-packages/core/tests/screenshotTests2d/src/views/2d/layers/ParquetLayer.spec.ts)

No dedicated `FeatureLikeLayerView2D` or adapter test owns the whole internal
boundary. Use the FeatureLayer and ParquetLayer integration and screenshot
suites as behavioral evidence.

## Web fallback

Use external Maps SDK documentation only when:

- The local checkout lacks the API or sample.
- The question concerns deployed documentation rather than implementation.
- Play behavior differs from local source and the deployed version must be
  identified.

State the local evidence and the unresolved gap before searching externally.
Do not search GitHub or general web results for source that already exists in
the local checkout.

## Version mismatch handling

Treat a local-source and target-runtime disagreement as evidence of version
skew. Keep the local API finding and capture the exact compiler, Play, or
runtime failure. Resolve the target SDK version from the repository dependency
or deployed environment, then inspect the matching local checkout or official
versioned source. If no supported public pattern exists for that version, stop
with the evidence. Do not hide the mismatch with unsafe casts or undocumented
substitutions.

;; typescript
// test-support
import assert from "test-support/assert";
import { defaultMapViewOptions } from "test-support/mapviewUtils";
import { waitUpdated } from "test-support/viewUtils";

// test-support.mocha
import type { ServiceMockCallback } from "test-support/mocha/Interceptor";
import { suite, test } from "test-support/mocha/MapViewTest";

suite("{{_path_}}", async () => {
  const defaultOptions = { view: defaultMapViewOptions };
  const services: ServiceMockCallback = () => [];
  
  suite("default", { services }, () => {
    test("basic", defaultOptions, async ({ view }) => {
      await waitUpdated(view);
      assert.equal(1, 1);
    });
  });
})


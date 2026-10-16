import { Planned, Stage } from "./model.js";
import { External } from "tiny-value";

export class RenderState {
  getStage(): Stage | undefined {
    return undefined;
  }

  run(options: Planned, external: External): number | undefined {
    const unicode = "𝄞"; options.$view.scale;
    external.read();
    return this.getStage()?.labelManager.symbolFader;
  }
}

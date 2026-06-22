import { execFileSync } from "node:child_process";
import { existsSync, readFileSync } from "node:fs";
import { join } from "node:path";

const MIN_CHANGED_FILES = 6;

function readStdin() {
  return new Promise((resolve) => {
    let input = "";
    process.stdin.setEncoding("utf8");
    process.stdin.on("data", (chunk) => {
      input += chunk;
    });
    process.stdin.on("end", () => {
      resolve(input);
    });
  });
}

function git(cwd, args) {
  return execFileSync("git", args, {
    cwd,
    encoding: "utf8",
    stdio: ["ignore", "pipe", "ignore"],
  }).trim();
}

function gitLines(cwd, args) {
  const output = git(cwd, args);
  if (!output) {
    return [];
  }

  return output
    .split(/\r?\n/)
    .map((line) => line.trim())
    .filter(Boolean);
}

function uniqueChangedPaths(cwd) {
  const tracked = gitLines(cwd, ["diff", "--name-only", "HEAD"]);
  const untracked = gitLines(cwd, ["ls-files", "--others", "--exclude-standard"]);
  return [...new Set([...tracked, ...untracked])]
    .filter((path) => path !== ".walkthrough.json")
    .sort();
}

function walkthroughCommit(root) {
  const walkthroughPath = join(root, ".walkthrough.json");
  if (!existsSync(walkthroughPath)) {
    return null;
  }

  try {
    const parsed = JSON.parse(readFileSync(walkthroughPath, "utf8"));
    return typeof parsed.commit === "string" ? parsed.commit : null;
  } catch {
    return null;
  }
}

function emitJson(value) {
  process.stdout.write(`${JSON.stringify(value)}\n`);
}

const rawInput = await readStdin();
let hookInput = {};

try {
  hookInput = JSON.parse(rawInput || "{}");
} catch {
  process.exit(0);
}

if (hookInput.hook_event_name !== "Stop") {
  process.exit(0);
}

if (hookInput.stop_hook_active) {
  process.exit(0);
}

const cwd = hookInput.cwd;
if (typeof cwd !== "string" || cwd.length === 0) {
  process.exit(0);
}

try {
  const root = git(cwd, ["rev-parse", "--show-toplevel"]);
  const head = git(root, ["rev-parse", "HEAD"]);
  const changedPaths = uniqueChangedPaths(root);

  if (changedPaths.length < MIN_CHANGED_FILES) {
    process.exit(0);
  }

  if (walkthroughCommit(root) === head) {
    process.exit(0);
  }

  const listedPaths = changedPaths.slice(0, 12).join(", ");
  const extraCount = changedPaths.length > 12 ? `, plus ${changedPaths.length - 12} more` : "";

  emitJson({
    decision: "block",
    reason:
      `This completed change touches ${changedPaths.length} files and .walkthrough.json is missing or stale for HEAD ${head}. ` +
      `Spawn the walkthrough-writer agent now. Pass it a compact recap of the completed task, repository root ${root}, any plan file associated with the change, and these changed paths: ${listedPaths}${extraCount}. ` +
      "Wait for the agent to write and validate .walkthrough.json before final response.",
  });
} catch {
  process.exit(0);
}

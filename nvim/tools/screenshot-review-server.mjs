#!/usr/bin/env node

import crypto from "node:crypto";
import fs from "node:fs";
import http from "node:http";
import path from "node:path";
import { URL } from "node:url";

const DEFAULT_PORT = readPortArg();
const HOST = "127.0.0.1";
const HASH_PATTERN = /^[0-9a-f]+$/u;
const NAMESPACE_PATTERN = /^[\w.-]+$/u;
const STALE_CLIENT_MS = 30_000;
const IDLE_SHUTDOWN_MS = 5_000;

class RunStore {
  constructor() {
    this.runs = new Map();
  }

  reset(run) {
    const branch = sanitizeBranchSlug(run.branch);
    const nextRun = {
      branch,
      cwd: String(run.cwd || process.cwd()),
      runId: String(run.runId || Date.now()),
      scope: String(run.scope || ""),
      target: String(run.target || ""),
      status: "running",
      startedAt: run.startedAt || Date.now(),
      completedAt: null,
      failures: [],
      events: [],
    };

    this.runs.set(branch, nextRun);
    return nextRun;
  }

  get(branch) {
    return this.runs.get(sanitizeBranchSlug(branch));
  }

  apply(branch, event) {
    const run = this.get(branch);
    if (!run) {
      throw httpError(404, `No run registered for branch ${branch}`);
    }

    const normalized = {
      ...event,
      type: String(event.type || "run:update"),
      at: event.at || Date.now(),
    };

    run.events.push(normalized);

    if (normalized.type === "run:start") {
      run.status = "running";
      run.failures = [];
      run.startedAt = normalized.at;
      run.completedAt = null;
    } else if (normalized.type === "run:update" || normalized.type === "run:complete") {
      if (Array.isArray(normalized.failures)) {
        run.failures = dedupeFailures(normalized.failures);
      }
      if (normalized.type === "run:complete") {
        run.status = "complete";
        run.completedAt = normalized.at;
      }
    } else if (normalized.type === "run:error") {
      run.status = "error";
      run.error = String(normalized.message || "Unknown screenshot review error");
      run.completedAt = normalized.at;
    }

    return run;
  }
}

class WebSocketHub {
  constructor(store) {
    this.store = store;
    this.clientsByBranch = new Map();
  }

  add(branch, socket) {
    const branchSlug = sanitizeBranchSlug(branch);
    const clients = this.clientsByBranch.get(branchSlug) ?? new Set();
    clients.add(socket);
    this.clientsByBranch.set(branchSlug, clients);

    socket.on("close", () => {
      clients.delete(socket);
      if (clients.size === 0) {
        this.clientsByBranch.delete(branchSlug);
      }
    });

    socket.on("error", () => {
      socket.destroy();
    });

    this.send(socket, {
      type: "snapshot",
      run: this.store.get(branchSlug) ?? null,
    });
  }

  broadcast(branch, event) {
    const clients = this.clientsByBranch.get(sanitizeBranchSlug(branch));
    if (!clients) {
      return;
    }

    for (const socket of clients) {
      this.send(socket, event);
    }
  }

  send(socket, payload) {
    if (socket.destroyed) {
      return;
    }

    socket.write(encodeWebSocketTextFrame(JSON.stringify(payload)));
  }
}

class ClientLeaseRegistry {
  constructor() {
    this.clients = new Map();
    this.hadClient = false;
    this.emptySince = null;
  }

  register() {
    const clientId = crypto.randomUUID();
    this.clients.set(clientId, { lastSeen: Date.now() });
    this.hadClient = true;
    this.emptySince = null;
    return clientId;
  }

  heartbeat(clientId) {
    const client = this.clients.get(clientId);
    if (!client) {
      throw httpError(404, `Unknown screenshot review client: ${clientId}`);
    }

    client.lastSeen = Date.now();
  }

  release(clientId) {
    this.clients.delete(clientId);
    this.markEmpty();
  }

  expireStale() {
    const now = Date.now();
    for (const [clientId, client] of this.clients) {
      if (now - client.lastSeen > STALE_CLIENT_MS) {
        this.clients.delete(clientId);
      }
    }
    this.markEmpty();
  }

  markEmpty() {
    if (this.hadClient && this.clients.size === 0 && this.emptySince == null) {
      this.emptySince = Date.now();
    }
  }

  shouldShutdown() {
    return this.emptySince != null && Date.now() - this.emptySince > IDLE_SHUTDOWN_MS;
  }
}

const store = new RunStore();
const hub = new WebSocketHub(store);
const leases = new ClientLeaseRegistry();

const server = http.createServer(async (request, response) => {
  try {
    await routeRequest(request, response);
  } catch (error) {
    sendError(response, error);
  }
});

server.on("upgrade", (request, socket) => {
  try {
    handleWebSocketUpgrade(request, socket);
  } catch {
    socket.destroy();
  }
});

server.listen(DEFAULT_PORT, HOST, () => {
  const address = server.address();
  const port = typeof address === "object" && address ? address.port : DEFAULT_PORT;
  console.log(`SCREENSHOT_REVIEW_READY=http://${HOST}:${port}`);
});

setInterval(() => {
  leases.expireStale();
  if (leases.shouldShutdown()) {
    server.close(() => {
      process.exit(0);
    });
  }
}, 5_000).unref();

async function routeRequest(request, response) {
  const requestUrl = new URL(request.url ?? "/", `http://${HOST}:${DEFAULT_PORT}`);
  const pathname = requestUrl.pathname;

  if (request.method === "GET" && pathname === "/api/health") {
    sendJson(response, { ok: true, clients: leases.clients.size });
    return;
  }

  if (request.method === "POST" && pathname === "/api/clients") {
    sendJson(response, { ok: true, clientId: leases.register() });
    return;
  }

  const clientHeartbeatMatch = pathname.match(/^\/api\/clients\/([^/]+)\/heartbeat$/u);
  if (request.method === "POST" && clientHeartbeatMatch) {
    leases.heartbeat(decodeURIComponent(clientHeartbeatMatch[1]));
    sendJson(response, { ok: true });
    return;
  }

  const clientMatch = pathname.match(/^\/api\/clients\/([^/]+)$/u);
  if (request.method === "DELETE" && clientMatch) {
    leases.release(decodeURIComponent(clientMatch[1]));
    sendJson(response, { ok: true });
    return;
  }

  if (request.method === "POST" && pathname === "/api/runs") {
    const run = store.reset(await readRequestJson(request));
    hub.broadcast(run.branch, { type: "snapshot", run });
    sendJson(response, { ok: true, run });
    return;
  }

  const runEventMatch = pathname.match(/^\/api\/runs\/([^/]+)\/events$/u);
  if (request.method === "POST" && runEventMatch) {
    const branch = decodeURIComponent(runEventMatch[1]);
    const run = store.apply(branch, await readRequestJson(request));
    hub.broadcast(branch, { type: "snapshot", run });
    sendJson(response, { ok: true, run });
    return;
  }

  const runMatch = pathname.match(/^\/api\/runs\/([^/]+)$/u);
  if (request.method === "GET" && runMatch) {
    const branch = decodeURIComponent(runMatch[1]);
    sendJson(response, { ok: true, run: store.get(branch) ?? null });
    return;
  }

  const acceptMatch = pathname.match(/^\/api\/runs\/([^/]+)\/accept$/u);
  if (request.method === "POST" && acceptMatch) {
    const branch = decodeURIComponent(acceptMatch[1]);
    const accepted = acceptScreenshot(branch, await readRequestJson(request));
    hub.broadcast(branch, { type: "snapshot", run: store.get(branch) });
    sendJson(response, { ok: true, accepted });
    return;
  }

  const acceptAllMatch = pathname.match(/^\/api\/runs\/([^/]+)\/accept-all$/u);
  if (request.method === "POST" && acceptAllMatch) {
    const branch = decodeURIComponent(acceptAllMatch[1]);
    const accepted = acceptAllScreenshots(branch);
    hub.broadcast(branch, { type: "snapshot", run: store.get(branch) });
    sendJson(response, { ok: true, accepted });
    return;
  }

  const imageMatch = pathname.match(/^\/api\/runs\/([^/]+)\/images\/([^/]+)\/([0-9a-f]+)\.png$/u);
  if (request.method === "GET" && imageMatch) {
    serveImage(response, decodeURIComponent(imageMatch[1]), decodeURIComponent(imageMatch[2]), imageMatch[3]);
    return;
  }

  if (request.method === "GET") {
    serveReviewPage(response);
    return;
  }

  throw httpError(404, "Not found");
}

function handleWebSocketUpgrade(request, socket) {
  const requestUrl = new URL(request.url ?? "/", `http://${HOST}:${DEFAULT_PORT}`);
  const match = requestUrl.pathname.match(/^\/ws\/([^/]+)$/u);
  if (!match) {
    throw httpError(404, "Unknown websocket route");
  }

  const key = request.headers["sec-websocket-key"];
  if (typeof key !== "string") {
    throw httpError(400, "Missing websocket key");
  }

  const accept = crypto
    .createHash("sha1")
    .update(`${key}258EAFA5-E914-47DA-95CA-C5AB0DC85B11`)
    .digest("base64");

  socket.write(
    [
      "HTTP/1.1 101 Switching Protocols",
      "Upgrade: websocket",
      "Connection: Upgrade",
      `Sec-WebSocket-Accept: ${accept}`,
      "",
      "",
    ].join("\r\n"),
  );

  hub.add(decodeURIComponent(match[1]), socket);
}

function encodeWebSocketTextFrame(message) {
  const payload = Buffer.from(message);
  const header = [];
  header.push(0x81);

  if (payload.length < 126) {
    header.push(payload.length);
  } else if (payload.length < 65536) {
    header.push(126, (payload.length >> 8) & 0xff, payload.length & 0xff);
  } else {
    header.push(127, 0, 0, 0, 0);
    header.push((payload.length >> 24) & 0xff, (payload.length >> 16) & 0xff, (payload.length >> 8) & 0xff, payload.length & 0xff);
  }

  return Buffer.concat([Buffer.from(header), payload]);
}

async function readRequestJson(request) {
  const chunks = [];
  for await (const chunk of request) {
    chunks.push(chunk);
  }

  const body = Buffer.concat(chunks).toString("utf8");
  if (!body) {
    return {};
  }

  try {
    return JSON.parse(body);
  } catch {
    throw httpError(400, "Invalid JSON body");
  }
}

function sendJson(response, value) {
  const body = `${JSON.stringify(value)}\n`;
  response.writeHead(200, {
    "content-type": "application/json; charset=utf-8",
    "content-length": Buffer.byteLength(body),
  });
  response.end(body);
}

function serveImage(response, branch, namespace, hash) {
  const run = store.get(branch);
  if (!run) {
    throw httpError(404, `No run registered for branch ${branch}`);
  }

  const imagePath = screenshotPath(run.cwd, namespace, hash);
  if (!fs.existsSync(imagePath)) {
    throw httpError(404, `Screenshot not found: ${namespace}/${hash}.png`);
  }

  response.writeHead(200, {
    "content-type": "image/png",
    "cache-control": "no-store",
  });
  fs.createReadStream(imagePath).pipe(response);
}

function acceptScreenshot(branch, payload) {
  const run = store.get(branch);
  if (!run) {
    throw httpError(404, `No run registered for branch ${branch}`);
  }

  const namespace = String(payload.namespace || "");
  const expectedHash = String(payload.expectedHash || "");
  const actualHash = String(payload.actualHash || "");
  const actualPath = screenshotPath(run.cwd, namespace, actualHash);
  const expectedPath = screenshotPath(run.cwd, namespace, expectedHash);

  if (!fs.existsSync(actualPath)) {
    throw httpError(404, `Actual screenshot not found: ${namespace}/${actualHash}.png`);
  }

  fs.mkdirSync(path.dirname(expectedPath), { recursive: true });
  fs.copyFileSync(actualPath, expectedPath);

  for (const failure of run.failures) {
    if (
      failure.namespace === namespace &&
      failure.expectedHash === expectedHash &&
      failure.actualHash === actualHash
    ) {
      failure.accepted = true;
    }
  }

  return { namespace, expectedHash, actualHash };
}

function acceptAllScreenshots(branch) {
  const run = store.get(branch);
  if (!run) {
    throw httpError(404, `No run registered for branch ${branch}`);
  }

  const accepted = [];
  for (const failure of run.failures) {
    accepted.push(acceptScreenshot(branch, failure));
  }

  return accepted;
}

function screenshotPath(cwd, namespace, hash) {
  if (!NAMESPACE_PATTERN.test(namespace)) {
    throw httpError(400, `Invalid screenshot namespace: ${namespace}`);
  }
  if (!HASH_PATTERN.test(hash)) {
    throw httpError(400, `Invalid screenshot hash: ${hash}`);
  }

  return path.join(cwd, ".screenshots", namespace, `${hash}.png`);
}

function sanitizeBranchSlug(value) {
  const slug = String(value || "unknown").replace(/[^\w.-]+/gu, "-").replace(/^-+|-+$/gu, "");
  return slug || "unknown";
}

function dedupeFailures(failures) {
  const seen = new Set();
  const deduped = [];

  for (const failure of failures) {
    const key = String(failure.id || `${failure.namespace}:${failure.expectedHash}:${failure.actualHash}`);
    if (seen.has(key)) {
      continue;
    }
    seen.add(key);
    deduped.push(failure);
  }

  return deduped;
}

function serveReviewPage(response) {
  const body = REVIEW_HTML;
  response.writeHead(200, {
    "content-type": "text/html; charset=utf-8",
    "content-length": Buffer.byteLength(body),
  });
  response.end(body);
}

function httpError(statusCode, message) {
  const error = new Error(message);
  error.statusCode = statusCode;
  return error;
}

function sendError(response, error) {
  const statusCode = error.statusCode || 500;
  const body = `${JSON.stringify({ ok: false, error: String(error.message || error) })}\n`;
  response.writeHead(statusCode, {
    "content-type": "application/json; charset=utf-8",
    "content-length": Buffer.byteLength(body),
  });
  response.end(body);
}

function readPortArg() {
  const index = process.argv.indexOf("--port");
  if (index !== -1) {
    const port = Number(process.argv[index + 1]);
    if (Number.isInteger(port) && port >= 0 && port <= 65535) {
      return port;
    }
  }

  return 9090;
}

const REVIEW_HTML = `<!doctype html>
<html lang="en">
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Screenshot Review</title>
<style>
  body { margin: 0; font: 14px system-ui, sans-serif; background: #111; color: #eee; }
  header { position: sticky; top: 0; background: #181818; padding: 12px 16px; border-bottom: 1px solid #333; }
  main { padding: 16px; display: grid; gap: 16px; }
  .failure { background: #1b1b1b; border: 1px solid #333; border-radius: 8px; padding: 12px; }
  .images { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 12px; }
  img, canvas { max-width: 100%; height: auto; image-rendering: auto; background: #000; border: 1px solid #333; }
  button { background: #2f6feb; color: white; border: 0; border-radius: 4px; padding: 6px 10px; cursor: pointer; }
  .meta { color: #aaa; overflow-wrap: anywhere; margin: 0 0 8px; }
</style>
<header>
  <strong>Screenshot Review</strong>
  <span id="status">connecting...</span>
  <button id="accept-all">Accept all</button>
</header>
<main id="app"></main>
<script>
const branch = decodeURIComponent(location.pathname.replace(/^\\//, "")) || "unknown";
const app = document.getElementById("app");
const statusNode = document.getElementById("status");
const acceptAllButton = document.getElementById("accept-all");
let currentRun = null;

function imageUrl(failure, hash) {
  return "/api/runs/" + encodeURIComponent(branch) + "/images/" + encodeURIComponent(failure.namespace) + "/" + hash + ".png";
}

function render(run) {
  currentRun = run;
  statusNode.textContent = run ? " " + run.status + " " + (run.target || "") : " no run yet";
  acceptAllButton.disabled = !run || !run.failures || run.failures.length === 0;
  acceptAllButton.textContent = "Accept all";
  app.replaceChildren();
  if (!run || !run.failures || run.failures.length === 0) {
    const empty = document.createElement("p");
    empty.textContent = run && run.status === "complete" ? "No screenshot mismatches." : "Waiting for screenshot results...";
    app.appendChild(empty);
    return;
  }
  for (const failure of run.failures) {
    const card = document.createElement("section");
    card.className = "failure";
    const meta = document.createElement("p");
    meta.className = "meta";
    meta.textContent = (failure.testId || "unknown test") + " | expected " + failure.expectedHash + " | actual " + failure.actualHash;
    const actions = document.createElement("p");
    const accept = document.createElement("button");
    accept.textContent = failure.accepted ? "Accepted" : "Accept local reference";
    accept.disabled = !!failure.accepted;
    accept.onclick = async () => {
      await fetch("/api/runs/" + encodeURIComponent(branch) + "/accept", {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify(failure),
      });
      accept.textContent = "Accepted";
      accept.disabled = true;
    };
    actions.appendChild(accept);
    const images = document.createElement("div");
    images.className = "images";
    const referenceUrl = imageUrl(failure, failure.expectedHash);
    const actualUrl = imageUrl(failure, failure.actualHash);
    images.appendChild(imageFigure("Reference", referenceUrl));
    images.appendChild(imageFigure("Current", actualUrl));
    const diffCanvas = document.createElement("canvas");
    const diffFigure = document.createElement("figure");
    const diffCaption = document.createElement("figcaption");
    diffCaption.textContent = "Diff";
    diffFigure.append(diffCaption, diffCanvas);
    images.appendChild(diffFigure);
    createDiffCanvas(referenceUrl, actualUrl, diffCanvas);
    card.append(meta, actions, images);
    app.appendChild(card);
  }
}

function imageFigure(label, src) {
  const figure = document.createElement("figure");
  const caption = document.createElement("figcaption");
  caption.textContent = label;
  const image = document.createElement("img");
  image.src = src;
  figure.append(caption, image);
  return figure;
}

async function loadImage(src) {
  const image = new Image();
  image.decoding = "async";
  image.src = src;
  await image.decode();
  return image;
}

async function createDiffCanvas(referenceUrl, actualUrl, canvas) {
  try {
    const [reference, actual] = await Promise.all([loadImage(referenceUrl), loadImage(actualUrl)]);
    const width = Math.max(reference.naturalWidth, actual.naturalWidth);
    const height = Math.max(reference.naturalHeight, actual.naturalHeight);
    canvas.width = width;
    canvas.height = height;
    const context = canvas.getContext("2d", { willReadFrequently: true });
    const referenceCanvas = document.createElement("canvas");
    const actualCanvas = document.createElement("canvas");
    referenceCanvas.width = width;
    referenceCanvas.height = height;
    actualCanvas.width = width;
    actualCanvas.height = height;
    const referenceContext = referenceCanvas.getContext("2d", { willReadFrequently: true });
    const actualContext = actualCanvas.getContext("2d", { willReadFrequently: true });
    referenceContext.drawImage(reference, 0, 0);
    actualContext.drawImage(actual, 0, 0);
    const referenceData = referenceContext.getImageData(0, 0, width, height);
    const actualData = actualContext.getImageData(0, 0, width, height);
    const diff = context.createImageData(width, height);
    for (let index = 0; index < diff.data.length; index += 4) {
      const changed =
        referenceData.data[index] !== actualData.data[index] ||
        referenceData.data[index + 1] !== actualData.data[index + 1] ||
        referenceData.data[index + 2] !== actualData.data[index + 2] ||
        referenceData.data[index + 3] !== actualData.data[index + 3];
      diff.data[index] = changed ? 255 : actualData.data[index] * 0.25;
      diff.data[index + 1] = changed ? 0 : actualData.data[index + 1] * 0.25;
      diff.data[index + 2] = changed ? 255 : actualData.data[index + 2] * 0.25;
      diff.data[index + 3] = 255;
    }
    context.putImageData(diff, 0, 0);
  } catch (error) {
    const context = canvas.getContext("2d");
    canvas.width = 320;
    canvas.height = 80;
    context.fillStyle = "#222";
    context.fillRect(0, 0, canvas.width, canvas.height);
    context.fillStyle = "#fff";
    context.fillText("Diff unavailable: " + String(error), 12, 40);
  }
}

async function refresh() {
  const response = await fetch("/api/runs/" + encodeURIComponent(branch));
  const data = await response.json();
  render(data.run);
}

function connect() {
  const socket = new WebSocket("ws://" + location.host + "/ws/" + encodeURIComponent(branch));
  socket.onopen = () => { statusNode.textContent = " connected"; };
  socket.onmessage = (event) => {
    const message = JSON.parse(event.data);
    if (message.type === "snapshot") {
      render(message.run);
    }
  };
  socket.onclose = () => {
    statusNode.textContent = " disconnected; retrying...";
    setTimeout(connect, 1000);
  };
}

acceptAllButton.onclick = async () => {
  await fetch("/api/runs/" + encodeURIComponent(branch) + "/accept-all", { method: "POST" });
  acceptAllButton.textContent = "Accepted all";
  acceptAllButton.disabled = true;
};

refresh().catch(() => {});
connect();
</script>
</html>`;

"use strict";

const path = require("node:path");
const { Worker } = require("node:worker_threads");

/**
 * @type {Worker | null}
 */
let worker = null;

const createWorker = () => {
  if (worker) return;
  worker = new Worker(path.join(__dirname, "./worker.cjs"));

  worker.on("message", (value) => console.log("worker msg:", value));
  worker.postMessage("helo");
};
createWorker();

process.stdin.on("drain", () => console.log("stdin drain"));
process.stdin.on("ready", () => console.log("stdin ready"));
process.stdin.on("connect", () => console.log("stdin connected"));
process.stdin.on("end", () => console.log("stdin end"));
process.stdin.on("error", (err) => console.log("stdin error:", err));
process.stdin.on("close", (hadError) => console.log("stdin closed:", hadError));

process.stdin.on("data", (data) => {
  const str = data.toString("utf8").trim();
  const num = +str;

  if (str === "exit") {
    worker?.terminate().then(
      (value) => {
        worker?.unref();
        worker = null;
        console.log("worker terminated:", value);
      },
      (err) => console.log("worker terminate error:", err),
    );
    return;
  }

  if (str === "worker") {
    createWorker();
  }

  worker?.postMessage(Number.isNaN(num) ? str : num);
});

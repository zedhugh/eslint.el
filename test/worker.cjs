"use strict";

const { parentPort } = require("node:worker_threads");
const { repeat } = require("./mod.cjs");

/**
 * @param {number} count
 */
const cb = (count) => {
  parentPort?.postMessage(count);
};

parentPort?.on("message", (value) => {
  console.log("msg:", value);

  parentPort?.postMessage(`hello ${value}`);

  if (typeof value !== "string") return;

  const result = value.match(/(\d+)\/(\d+)/);
  if (!result) return;

  const time = +(result[1] || 1);
  const delay = +(result[2] || 500);
  repeat(time, delay, cb);
});

const childProcess = require("node:child_process");
const path = require("node:path");
const fs = require("node:fs");

/**
 * @param {string} dir
 */
const hasEslint = (dir) => {
  return fs.existsSync(path.join(dir, "eslint"));
};

/**
 * @param {string} dir
 */
const eslintJsFile = (dir) => path.join(dir, "eslint/bin/eslint.js");

/**
 * @param {string} cmd
 * @param {string[]} args
 */
const spawnSync = (cmd, args) => {
  const result = childProcess.spawnSync(cmd, args);
  return result.stdout.toString("utf8").trim();
};

/**
 * @param {string[]} args
 */
const npm = (args) => spawnSync("npm", args);

/**
 * @param {string[]} args
 */
const pnpm = (args) => spawnSync("pnpm", args);

/**
 * @enum {number}
 */
const SCOPE = {
  PROJECT: 1,
  GLOBAL: 2,
  NONE: 3,
};

/**
 * ESLint install in project or global,
 */
const getESLintInstallScope = () => {
  try {
    const project = pnpm(["root"]);
    if (hasEslint(project)) return SCOPE.PROJECT;
  } catch (_err) {}

  try {
    const root = pnpm(["root", "-g"]);
    if (hasEslint(root)) return SCOPE.GLOBAL;
  } catch (_err) {}

  try {
    const project = npm(["root"]);
    if (hasEslint(project)) return SCOPE.PROJECT;
  } catch (_err) {}

  try {
    const root = npm(["root", "-g"]);
    if (hasEslint(root)) return SCOPE.GLOBAL;
  } catch (_err) {}

  return SCOPE.NONE;
};

const getESLintFile = () => {
  try {
    const project = pnpm(["root"]);
    if (hasEslint(project)) return eslintJsFile(project);
  } catch (_err) {}

  try {
    const root = pnpm(["root", "-g"]);
    if (hasEslint(root)) return eslintJsFile(root);
  } catch (_err) {}

  try {
    const project = npm(["root"]);
    if (hasEslint(project)) return eslintJsFile(project);
  } catch (_err) {}

  try {
    const root = npm(["root", "-g"]);
    if (hasEslint(root)) return eslintJsFile(root);
  } catch (_err) {}

  return null;
};

const recvStdin = () => {
  const { stdin, stdout } = process;
  stdin.on("data", (data) => {
    stdout.write(data);
  });
};

(() => {
  return new Promise((resolve) => {
    recvStdin();
  });
})();

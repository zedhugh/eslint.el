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

/**
 * @typedef {import("eslint").ESLint.LintResult} LintResult
 */

/**
 * @param {string[]} args
 */
const eslint = (args) => {
  const r = childProcess.execSync(getESLintFile(), args);
  console.log(0, r.toString());
  const d = r.stdout.toString("utf8");
  return d.trim();
};

/**
 * @param filepath {string}
 */
const eslintFile = (filepath) => {
  const jsonStr = eslint(["-f", "json", filepath]);
  console.log(1, jsonStr);
  /** @type {LintResult[]} */
  const json = JSON.parse(jsonStr);
  const result = json.find((r) => r.filePath === filepath);
  return result;
};

// console.log(getESLintInstallScope());
console.log(getESLintFile());
// console.log(eslintFile(path.join(__dirname, "./emacs-flymake-eslint.cjs")));

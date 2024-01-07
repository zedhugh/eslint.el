/**
 * @typedef {typeof import("eslint").ESLint} ESLintConstructor
 * @typedef {import("eslint").ESLint} ESLint
 */

const childProcess = require("node:child_process");
const path = require("node:path");
const fs = require("node:fs");

const pkgJson = "package.json";
const nodeModules = "node_modules";
const eslintConfigFiles = [
  "eslint.config.js",
  ".eslintrc.js",
  ".eslintrc.cjs",
  ".eslintrc.yaml",
  ".eslintrc.yml",
  ".eslintrc.json",
];
const eslintConfigField = "eslintConfig";

/**
 * @param {string} dir
 */
const dirContainPackageJson = (dir) => {
  const pkgPath = path.join(dir, pkgJson);
  if (!fs.existsSync(pkgPath)) return false;
  const stat = fs.statSync(pkgPath);
  return stat.isFile();
};

/**
 * @param {string} filepath
 */
const filepathInNodeModulesDir = (filepath) => {
  return filepath.includes(nodeModules);
};

/**
 * @param {string} filepath
 * @returns {string | null}
 */
const findRootDir = (filepath) => {
  let dir = path.dirname(filepath);
  let prevDir = "";
  for (;;) {
    if (dirContainPackageJson(dir) && !filepathInNodeModulesDir(dir)) {
      return dir;
    }

    if (prevDir === dir) return null;

    prevDir = dir;
    dir = path.dirname(dir);
  }
};

/**
 * @param {string} dir
 */
const parseDirPkgJson = (dir) => {
  const pkgJsonPath = path.join(dir, pkgJson);
  if (!fs.existsSync(pkgJsonPath)) return null;

  try {
    const jsonStr = fs.readFileSync(pkgJsonPath, { encoding: "utf8" });
    /**
     * @type {Record<string, unknown>}
     */
    const pkgObj = JSON.parse(jsonStr);
    return pkgObj;
  } catch (err) {
    return null;
  }
};

/**
 * @param {string[]} files
 * @param {string} dir
 * @returns {boolean}
 */
const filesExistInDir = (files, dir) => {
  return files.some((file) => {
    const p = path.join(dir, file);
    const exist = fs.existsSync(p);
    return exist || fs.statSync(p).isFile();
  });
};

/**
 * @param {string} field
 * @param {string} dir
 */
const hasFieldInPkgJson = (field, dir) => {
  const json = parseDirPkgJson(dir);

  return !!json?.[field];
};

/**
 * @param {string} dir
 */
const hasEslintConfig = (dir) => {
  return (
    filesExistInDir(eslintConfigFiles, dir) ||
    hasFieldInPkgJson(eslintConfigField, dir)
  );
};

/**
 * @param {string} filepath
 */
const findEslintConfigDir = (filepath) => {
  const rootDir = findRootDir(filepath);
  let dir = path.dirname(filepath);
  let prevDir = "";
  for (;;) {
    if (filesExistInDir(eslintConfigFiles, dir)) return dir;

    if (dir === rootDir) {
      if (hasFieldInPkgJson(eslintConfigField, dir)) return dir;

      return null;
    }

    if (prevDir === dir) return null;

    prevDir = dir;
    dir = path.dirname(dir);
  }
};

/**
 * @param {string} cmd
 * @param {string[]} args
 * @param {string=} dir
 */
const spawnSync = (cmd, args, dir) => {
  const oldWorkDir = process.cwd();
  if (dir && fs.existsSync(dir) && fs.statSync(dir).isDirectory()) {
    process.chdir(dir);
  }
  const result = childProcess.spawnSync(cmd, args);
  if (oldWorkDir !== process.cwd()) {
    process.chdir(oldWorkDir);
  }
  return result.stdout.toString("utf8").trim();
};

/**
 * @param {string[]} args
 * @param {string=} dir
 */
const npm = (args, dir) => spawnSync("npm", args, dir);

/**
 * @param {string[]} args
 * @param {string=} dir
 */
const pnpm = (args, dir) => spawnSync("pnpm", args, dir);

/**
 * @param {string} root
 */
const hasEslint = (root) => {
  const eslintDir = path.join(root, "eslint");
  return fs.existsSync(eslintDir) && fs.statSync(eslintDir).isDirectory();
};

/**
 * @param {string} root
 */
const importEslint = async (root) => {
  const apiJS = path.join(root, "eslint/lib/unsupported-api.js");
  const mod = require(apiJS);
  /**
   * @type {{
   *    FlatESLint: ESLintConstructor;
   *    shouldUseFlatConfig: () => Promise<boolean>;
   *    LegacyESLint: ESLintConstructor
   * }}
   */
  const { FlatESLint, shouldUseFlatConfig, LegacyESLint } = mod;

  const usingFlatConfig = await shouldUseFlatConfig();
  const eslint = usingFlatConfig ? new FlatESLint() : new LegacyESLint();
  return eslint;
};

/**
 * @param {string} dir
 */
const getESLintInstallDir = (dir) => {
  try {
    const root = pnpm(["root"]);
    if (hasEslint(root)) return root;
  } catch (_err) {}

  try {
    const root = npm(["root"]);
    if (hasEslint(root)) return root;
  } catch (_err) {}

  try {
    const root = pnpm(["root", "-g"]);
    if (hasEslint(root)) return root;
  } catch (_err) {}

  try {
    const root = npm(["root", "-g"]);
    if (hasEslint(root)) return root;
  } catch (_err) {}

  return null;
};

/**
 * @type {Map<string, ESLint>}
 */
const dirESLintMap = new Map();
/**
 * @type {Map<string, ESLint | null>}
 */
const filepathESLintMap = new Map();

/**
 * @param {string} filepath
 */
const getESLint = async (filepath) => {
  if (filepathESLintMap.has(filepath)) {
    return filepathESLintMap.get(filepath);
  }

  const eslintConfigDir = findEslintConfigDir(filepath);
  if (eslintConfigDir && dirESLintMap.has(eslintConfigDir)) {
    return dirESLintMap.get(eslintConfigDir);
  }

  if (!eslintConfigDir) {
    filepathESLintMap.set(filepath, null);
    return null;
  }

  process.chdir(eslintConfigDir);
  const root = getESLintInstallDir(eslintConfigDir);
  if (!root) {
    filepathESLintMap.set(filepath, null);
    dirESLintMap.set(eslintConfigDir, null);
    return null;
  }
  const eslint = await importEslint(root);
  filepathESLintMap.set(filepath, eslint);
  dirESLintMap.set(eslintConfigDir, eslint);
  return eslint;
};

module.exports = {
  getESLint,
};

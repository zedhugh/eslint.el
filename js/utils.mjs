import childProcess from 'node:child_process';
import path from 'node:path';
import fs from 'node:fs';
import {
  eslintConfigField,
  eslintConfigFiles,
  nodeModules,
  packageManagerLockFiles,
  pkgJson,
} from './config.mjs';
import { ReloadReason } from './message.mjs';

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
  return result.stdout.toString('utf8').trim();
};

/**
 * @param {string[]} args
 * @param {string=} dir
 */
const npm = (args, dir) => spawnSync('npm', args, dir);
/**
 * @param {string[]} args
 * @param {string=} dir
 */
const pnpm = (args, dir) => spawnSync('pnpm', args, dir);

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
  return filepath.split(path.sep).includes(nodeModules);
};

/**
 * @param {string} filepath
 */
const findRootDir = (filepath) => {
  let dir = path.dirname(filepath);
  let prevDir = '';
  while (true) {
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
    const jsonStr = fs.readFileSync(pkgJsonPath, { encoding: 'utf8' });
    /** @type {Record<string, unknown>} */
    const pkgObj = JSON.parse(jsonStr);
    return pkgObj;
  } catch (err) {
    return null;
  }
};

/**
 * @param {string} field
 * @param {string} dir
 */
const hasFieldInPkgJson = (field, dir) => !!parseDirPkgJson(dir)?.[field];

/**
 * @param {string[]} files
 * @param {string} dir
 */
const filesExistInDir = (files, dir) => {
  for (const file of files) {
    const filepath = path.join(dir, file);
    const exist = fs.existsSync(filepath);
    if (exist && fs.statSync(filepath).isFile()) {
      return filepath;
    }
  }

  return null;
};

/**
 * @param {string} filepath
 */
export const findEslintConfigFile = (filepath) => {
  // const rootDir = findRootDir(filepath);
  let dir = path.dirname(filepath);
  let prevDir = '';
  for (;;) {
    let configFile = filesExistInDir(eslintConfigFiles, dir);
    if (configFile) return configFile;

    if (hasFieldInPkgJson(eslintConfigField, dir)) {
      return path.join(dir, pkgJson);
    }

    if (prevDir === dir) return null;

    prevDir = dir;
    dir = path.dirname(dir);
  }
};

/**
 * @param {string} root
 */
const hasEslint = (root) => {
  const eslintDir = path.join(root, 'eslint');
  return fs.existsSync(eslintDir) && fs.statSync(eslintDir).isDirectory();
};

/**
 * @param {string} root
 */
const eslintDir = (root) => path.join(root, 'eslint');

/**
 * @param {string} filepath
 */
export const getESLintInstallDir = (filepath) => {
  let dir = path.dirname(filepath);
  let prevDir = '';
  for (;;) {
    const root = path.join(dir, nodeModules);
    if (hasEslint(root)) {
      return eslintDir(root);
    }

    if (prevDir === dir) break;

    prevDir = dir;
    dir = path.dirname(dir);
  }

  try {
    const root = pnpm(['root', '-g'], dir);
    if (hasEslint(root)) return eslintDir(root);
  } catch (_err) {}

  try {
    const root = npm(['root', '-g'], dir);
    if (hasEslint(root)) return eslintDir(root);
  } catch (_err) {}

  return null;
};

/**
 * @param {string} filepath
 */
export const needReloadESLintInstance = (filepath) => {
  if (filepathInNodeModulesDir(filepath)) return false;

  const filename = path.basename(filepath);
  if (filename === pkgJson || packageManagerLockFiles.includes(filename)) {
    return ReloadReason.DepsChange;
  }
  if (eslintConfigFiles.includes(filename)) {
    return ReloadReason.ConfigChange;
  }

  return false;
};

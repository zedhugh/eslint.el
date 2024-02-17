import type { ESLint as ESLintClass } from 'eslint';
import childProcess from 'node:child_process';
import path from 'node:path';
import fs from 'node:fs';
import {
  eslintConfigField,
  eslintConfigFiles,
  nodeModules,
  pkgJson,
} from './config';

const spawnSync = (cmd: string, args: string[], dir?: string) => {
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

const npm = (args: string[], dir?: string) => spawnSync('npm', args, dir);
const pnpm = (args: string[], dir?: string) => spawnSync('pnpm', args, dir);

const dirContainPackageJson = (dir: string) => {
  const pkgPath = path.join(dir, pkgJson);
  if (!fs.existsSync(pkgPath)) return false;
  const stat = fs.statSync(pkgPath);
  return stat.isFile();
};

const filepathInNodeModulesDir = (filepath: string) => {
  return filepath.includes(nodeModules);
};

// @ts-ignore
const findRootDir = (filepath: string) => {
  let dir = path.dirname(filepath);
  let prevDir = '';
  for (;;) {
    if (dirContainPackageJson(dir) && !filepathInNodeModulesDir(dir)) {
      return dir;
    }

    if (prevDir === dir) return null;

    prevDir = dir;
    dir = path.dirname(dir);
  }
};

const parseDirPkgJson = (dir: string) => {
  const pkgJsonPath = path.join(dir, pkgJson);
  if (!fs.existsSync(pkgJsonPath)) return null;

  try {
    const jsonStr = fs.readFileSync(pkgJsonPath, { encoding: 'utf8' });
    const pkgObj = JSON.parse(jsonStr) as Record<string, unknown>;
    return pkgObj;
  } catch (err) {
    return null;
  }
};

const hasFieldInPkgJson = (field: string, dir: string) => {
  const json = parseDirPkgJson(dir);

  return !!json?.[field];
};

const filesExistInDir = (files: string[], dir: string) => {
  for (const file of files) {
    const filepath = path.join(dir, file);
    const exist = fs.existsSync(filepath);
    if (exist && fs.statSync(filepath).isFile()) {
      return filepath;
    }
  }

  return null;
};

export const findEslintConfigFile = (filepath: string) => {
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

const hasEslint = (root: string) => {
  const eslintDir = path.join(root, 'eslint');
  return fs.existsSync(eslintDir) && fs.statSync(eslintDir).isDirectory();
};

export const getESLintInstallDir = (filepath?: string) => {
  const dir = filepath ? path.dirname(filepath) : undefined;

  try {
    const root = pnpm(['root'], dir);
    if (hasEslint(root)) return root;
  } catch (_err) {}

  try {
    const root = npm(['root'], dir);
    if (hasEslint(root)) return root;
  } catch (_err) {}

  try {
    const root = pnpm(['root', '-g'], dir);
    if (hasEslint(root)) return root;
  } catch (_err) {}

  try {
    const root = npm(['root', '-g'], dir);
    if (hasEslint(root)) return root;
  } catch (_err) {}

  return null;
};

interface ESLintModule {
  ESLint: typeof ESLintClass;
}
interface FlatESLintModal {
  FlatESLint: typeof ESLintClass;
  shouldUseFlatConfig?: () => Promise<boolean>;
  findFlatConfigFile?: (cwd: string) => Promise<string | null>;
}

export const importEslint = async (root: string) => {
  const eslintJS = path.join(root, 'eslint/lib/eslint/eslint.js');
  const flatEslintJS = path.join(root, 'eslint/lib/eslint/flat-eslint.js');

  if (!fs.existsSync(eslintJS) && !fs.existsSync(flatEslintJS)) {
    throw new Error('no eslint installed or not supported eslint version');
  }

  const { ESLint } = (await import(eslintJS)) as ESLintModule;

  if (!fs.existsSync(flatEslintJS)) {
    return new ESLint();
  }

  const { FlatESLint, shouldUseFlatConfig, findFlatConfigFile } = (await import(
    flatEslintJS
  )) as FlatESLintModal;
  let usingFlatConfig = false;
  if (typeof shouldUseFlatConfig === 'function') {
    usingFlatConfig = await shouldUseFlatConfig();
  }

  if (typeof findFlatConfigFile === 'function') {
    usingFlatConfig = !!(await findFlatConfigFile(process.cwd()));
  }

  const eslint = usingFlatConfig ? new FlatESLint() : new ESLint();
  return eslint;
};

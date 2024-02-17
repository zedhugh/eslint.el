"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.importEslint = exports.getESLintInstallDir = exports.findEslintConfigFile = void 0;
const node_child_process_1 = __importDefault(require("node:child_process"));
const node_path_1 = __importDefault(require("node:path"));
const node_fs_1 = __importDefault(require("node:fs"));
const config_1 = require("./config");
const spawnSync = (cmd, args, dir) => {
    const oldWorkDir = process.cwd();
    if (dir && node_fs_1.default.existsSync(dir) && node_fs_1.default.statSync(dir).isDirectory()) {
        process.chdir(dir);
    }
    const result = node_child_process_1.default.spawnSync(cmd, args);
    if (oldWorkDir !== process.cwd()) {
        process.chdir(oldWorkDir);
    }
    return result.stdout.toString('utf8').trim();
};
const npm = (args, dir) => spawnSync('npm', args, dir);
const pnpm = (args, dir) => spawnSync('pnpm', args, dir);
const dirContainPackageJson = (dir) => {
    const pkgPath = node_path_1.default.join(dir, config_1.pkgJson);
    if (!node_fs_1.default.existsSync(pkgPath))
        return false;
    const stat = node_fs_1.default.statSync(pkgPath);
    return stat.isFile();
};
const filepathInNodeModulesDir = (filepath) => {
    return filepath.includes(config_1.nodeModules);
};
// @ts-ignore
const findRootDir = (filepath) => {
    let dir = node_path_1.default.dirname(filepath);
    let prevDir = '';
    for (;;) {
        if (dirContainPackageJson(dir) && !filepathInNodeModulesDir(dir)) {
            return dir;
        }
        if (prevDir === dir)
            return null;
        prevDir = dir;
        dir = node_path_1.default.dirname(dir);
    }
};
const parseDirPkgJson = (dir) => {
    const pkgJsonPath = node_path_1.default.join(dir, config_1.pkgJson);
    if (!node_fs_1.default.existsSync(pkgJsonPath))
        return null;
    try {
        const jsonStr = node_fs_1.default.readFileSync(pkgJsonPath, { encoding: 'utf8' });
        const pkgObj = JSON.parse(jsonStr);
        return pkgObj;
    }
    catch (err) {
        return null;
    }
};
const hasFieldInPkgJson = (field, dir) => {
    const json = parseDirPkgJson(dir);
    return !!json?.[field];
};
const filesExistInDir = (files, dir) => {
    for (const file of files) {
        const filepath = node_path_1.default.join(dir, file);
        const exist = node_fs_1.default.existsSync(filepath);
        if (exist && node_fs_1.default.statSync(filepath).isFile()) {
            return filepath;
        }
    }
    return null;
};
const findEslintConfigFile = (filepath) => {
    // const rootDir = findRootDir(filepath);
    let dir = node_path_1.default.dirname(filepath);
    let prevDir = '';
    for (;;) {
        let configFile = filesExistInDir(config_1.eslintConfigFiles, dir);
        if (configFile)
            return configFile;
        if (hasFieldInPkgJson(config_1.eslintConfigField, dir)) {
            return node_path_1.default.join(dir, config_1.pkgJson);
        }
        if (prevDir === dir)
            return null;
        prevDir = dir;
        dir = node_path_1.default.dirname(dir);
    }
};
exports.findEslintConfigFile = findEslintConfigFile;
const hasEslint = (root) => {
    const eslintDir = node_path_1.default.join(root, 'eslint');
    return node_fs_1.default.existsSync(eslintDir) && node_fs_1.default.statSync(eslintDir).isDirectory();
};
const getESLintInstallDir = (filepath) => {
    const dir = filepath ? node_path_1.default.dirname(filepath) : undefined;
    try {
        const root = pnpm(['root'], dir);
        if (hasEslint(root))
            return root;
    }
    catch (_err) { }
    try {
        const root = npm(['root'], dir);
        if (hasEslint(root))
            return root;
    }
    catch (_err) { }
    try {
        const root = pnpm(['root', '-g'], dir);
        if (hasEslint(root))
            return root;
    }
    catch (_err) { }
    try {
        const root = npm(['root', '-g'], dir);
        if (hasEslint(root))
            return root;
    }
    catch (_err) { }
    return null;
};
exports.getESLintInstallDir = getESLintInstallDir;
const importEslint = async (root) => {
    const eslintJS = node_path_1.default.join(root, 'eslint/lib/eslint/eslint.js');
    const flatEslintJS = node_path_1.default.join(root, 'eslint/lib/eslint/flat-eslint.js');
    if (!node_fs_1.default.existsSync(eslintJS) && !node_fs_1.default.existsSync(flatEslintJS)) {
        throw new Error('no eslint installed or not supported eslint version');
    }
    const { ESLint } = (await import(eslintJS));
    if (!node_fs_1.default.existsSync(flatEslintJS)) {
        return new ESLint();
    }
    const { FlatESLint, shouldUseFlatConfig, findFlatConfigFile } = (await import(flatEslintJS));
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
exports.importEslint = importEslint;

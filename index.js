"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const node_path_1 = __importDefault(require("node:path"));
const node_worker_threads_1 = require("node:worker_threads");
const utils_1 = require("./utils");
const configWorkerMap = new Map();
const configFilesMap = new Map();
const addConfigFile2Map = (config, filepath) => {
    const fileSet = configFilesMap.get(config);
    if (fileSet)
        fileSet.add(config);
    else
        configFilesMap.set(config, new Set([filepath]));
};
const getFilepathWorker = (filepath) => {
    const config = (0, utils_1.findEslintConfigFile)(filepath);
    if (!config)
        return null;
    addConfigFile2Map(config, filepath);
    if (configWorkerMap.has(config)) {
        return configWorkerMap.get(config) || null;
    }
    const root = (0, utils_1.getESLintInstallDir)(filepath);
    if (!root) {
        configWorkerMap.set(config, null);
        return null;
    }
    const workerFile = node_path_1.default.join(__dirname, './worker.js');
    process.chdir(node_path_1.default.dirname(config));
    const workerConfig = { root, config };
    const worker = new node_worker_threads_1.Worker(workerFile, { workerData: workerConfig });
    configWorkerMap.set(config, worker);
    return worker;
};
const lintFile = async (filepath, code) => {
    const worker = getFilepathWorker(filepath);
    if (!worker)
        return null;
    return new Promise((resolve) => {
        const listener = (value) => {
            if (value.filepath !== filepath)
                return;
            resolve(value.messages);
            worker.off('message', listener);
        };
        worker.on('message', listener);
        const msg = { code, filepath };
        worker.postMessage(msg);
    });
};
const closeFile = async (filepath) => {
    const config = (0, utils_1.findEslintConfigFile)(filepath);
    if (!config)
        return;
    const fileSet = configFilesMap.get(config);
    fileSet?.delete(filepath);
    if (fileSet?.size)
        return;
    const worker = configWorkerMap.get(config);
    await worker?.terminate();
    configWorkerMap.delete(config);
    if (!configWorkerMap.size)
        process.exit(0);
};
const sendResultToEmacs = (result) => {
    console.log(JSON.stringify(result));
};
process.stdin.on('data', async (data) => {
    const start = performance.now();
    const str = data.toString('utf8');
    const json = JSON.parse(str);
    switch (json.cmd) {
        case "lint" /* Command.Lint */: {
            const { file, code } = json;
            const result = await lintFile(file, code);
            sendResultToEmacs({
                file,
                cost: performance.now() - start,
                messages: result?.length ? result : undefined,
            });
            break;
        }
        case "close" /* Command.Close */:
            json.file && closeFile(json.file);
            break;
        case "exit" /* Command.Exit */:
            process.exit(0);
        case "log" /* Command.Log */:
            console.error(5, configWorkerMap.entries(), configFilesMap.entries());
            break;
    }
});
process.on('unhandledRejection', (reason) => {
    console.error(reason);
});

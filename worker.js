"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const node_worker_threads_1 = require("node:worker_threads");
const message_1 = require("./message");
const utils_1 = require("./utils");
const { root, config } = node_worker_threads_1.workerData;
const waitingFileCodeMap = new Map();
let eslintInstance = null;
const getESLint = async () => {
    if (eslintInstance)
        return eslintInstance;
    eslintInstance = await (0, utils_1.importEslint)(root);
    return eslintInstance;
};
const onMessage = async (input) => {
    const { code, filepath } = input;
    waitingFileCodeMap.set(filepath, code);
    if (filepath === config) {
        eslintInstance = null;
    }
    const eslint = await getESLint();
    waitingFileCodeMap.forEach(async (code, filepath) => {
        waitingFileCodeMap.delete(filepath);
        const result = await eslint.lintText(code, { filePath: filepath });
        const output = {
            filepath,
            messages: (0, message_1.parseLintResult)(result, filepath),
        };
        node_worker_threads_1.parentPort?.postMessage(output);
    });
};
node_worker_threads_1.parentPort?.on('message', onMessage);

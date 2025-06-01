import path from 'node:path';
import readline from 'node:readline';
import { Worker } from 'node:worker_threads';
import {
  findEslintConfigFile,
  getESLintInstallDir,
  needReloadESLintInstance,
} from './utils.mjs';
import { Command, ReloadReason } from './message.mjs';

/**
 * @typedef {import("./message.mjs").ESLintMessage} ESLintMessage
 * @typedef {import("./message.mjs").InteractiveData} InteractiveData
 * @typedef {import("./message.mjs").Result} Result
 * @typedef {import("./worker").WorkerConfig} WorkerConfig
 * @typedef {import("./worker").WorkerInput} WorkerInput
 * @typedef {import("./worker").WorkerOutput} WorkerOutput
 */

/** @type {Map<string, Worker | null>} */
const configWorkerMap = new Map();
/** @type {Map<string, Set<string>>} */
const configFilesMap = new Map();

/**
 * @param {string} config
 * @param {string} filepath
 */
const addConfigFile2Map = (config, filepath) => {
  const fileSet = configFilesMap.get(config);

  if (fileSet) {
    fileSet.add(filepath);
  } else {
    configFilesMap.set(config, new Set([filepath]));
  }
};

/**
 * @param {string} config
 * @param {string=} filepath
 */
const assignESLintWorker = (config, filepath) => {
  const root = getESLintInstallDir(filepath || config);
  if (!root) {
    configWorkerMap.set(config, null);
    return null;
  }

  const workerFile = path.join(import.meta.dirname, './worker.mjs');
  process.chdir(path.dirname(config));
  /** @type {WorkerConfig} */
  const workerConfig = { root, config };
  const worker = new Worker(workerFile, { workerData: workerConfig });
  configWorkerMap.set(config, worker);
  return worker;
};

/**
 * @param {string} filepath
 */
const getFilepathWorker = (filepath) => {
  const config = findEslintConfigFile(filepath);
  if (!config) return null;

  addConfigFile2Map(config, filepath);

  if (configWorkerMap.has(config)) {
    return configWorkerMap.get(config) || null;
  }

  return assignESLintWorker(config, filepath);
};

/**
 * @param {string} filepath
 * @param {string} code
 */
const lintFile = async (filepath, code) => {
  const worker = getFilepathWorker(filepath);
  if (!worker) return null;

  /**
   * @type {Promise<ESLintMessage[]>}
   */
  const messagePromise = new Promise((resolve) => {
    /**
     * @param {WorkerOutput} value
     */
    const listener = (value) => {
      if (value.filepath !== filepath) return;
      resolve(value.messages);
      worker.off('message', listener);
    };
    worker.on('message', listener);

    /** @type {WorkerInput} */
    const msg = { code, filepath };
    worker.postMessage(msg);
  });
  return messagePromise;
};

/**
 * @param {string} config
 */
const terminateAndRemoveWorker = (config) => {
  const worker = configWorkerMap.get(config);
  worker?.terminate();
  configWorkerMap.delete(config);
};

const exitProcessIfNeeded = () => {
  if (configWorkerMap.size) return;

  process.exit(0);
};

/**
 * @param {string} filepath
 */
const closeFile = (filepath) => {
  const config = findEslintConfigFile(filepath);
  if (!config) return;

  const fileSet = configFilesMap.get(config);
  fileSet?.delete(filepath);

  if (fileSet?.size) return;

  terminateAndRemoveWorker(config);
  exitProcessIfNeeded();
};

/**
 * @param {string} dir
 */
const reloadESLintWorkers = (dir) => {
  configWorkerMap.forEach((oldWorker, config) => {
    if (!config.startsWith(dir)) return;

    assignESLintWorker(config);
    oldWorker?.terminate();
  });
};

/**
 * @param {string} config
 */
const reassignESLintWorkers = (config) => {
  const notNeedChangeConfigFilesMap = configWorkerMap.has(config);
  const oldWorker = configWorkerMap.get(config);
  assignESLintWorker(config);
  oldWorker?.terminate();

  if (notNeedChangeConfigFilesMap) return;

  /** @type {[config: string, filepath: string][]} */
  const configFilePairNeedChange = [];
  configFilesMap.forEach((files, oldConf) => {
    /** @type {string[]} */
    const deleteFiles = [];
    files.forEach((file) => {
      const conf = findEslintConfigFile(file);
      if (conf === oldConf) return;

      deleteFiles.push(file);
      configFilePairNeedChange.push([conf, file]);
    });
    deleteFiles.forEach((file) => files.delete(file));
  });
  addConfigFile2Map;
};

/**
 * @param {string} filepath
 */
const savedFile = (filepath) => {
  const reason = needReloadESLintInstance(filepath);
  if (!reason) return;

  switch (reason) {
    case ReloadReason.DepsChange:
      reloadESLintWorkers(path.dirname(filepath));
      break;
    case ReloadReason.ConfigChange:
      reassignESLintWorkers(filepath);
      break;
  }
};

/**
 * @param {Result} result
 */
const sendResultToEmacs = (result) => {
  console.log(JSON.stringify(result));
};

/** @type {Map<string, string>} */
const fileCodeMap = new Map();
/** @type {Map<string, ESLintMessage[] | null>} */
const cacheResultMap = new Map();

/**
 * @param {string} str
 */
const handler = async (str) => {
  const start = performance.now();
  /** @type {InteractiveData} */
  const json = JSON.parse(str);

  switch (json?.cmd) {
    case Command.Lint: {
      const { file, code } = json;
      console.error(json.cmd, file);
      const lastCode = fileCodeMap.get(file);
      if (lastCode === code) {
        sendResultToEmacs({
          file,
          cost: performance.now() - start,
          messages: cacheResultMap.get(file),
        });
        break;
      }

      fileCodeMap.set(file, code);
      const result = await lintFile(file, code);
      if (code === fileCodeMap.get(file)) {
        cacheResultMap.set(file, result);
        sendResultToEmacs({
          file,
          cost: performance.now() - start,
          messages: result?.length ? result : undefined,
        });
      }

      break;
    }
    case Command.Close:
      json.file && closeFile(json.file);
      break;
    case Command.Exit:
      process.exit(0);
    case Command.Log: {
      if (process.stderr.bytesWritten) {
        console.error('=================================================');
      }
      const workers = [...configWorkerMap.entries()].map(([file, worker]) => {
        return [file, worker?.threadId ?? null];
      });
      console.error(workers, configFilesMap.entries());
      break;
    }
    case Command.Save:
      json.file && savedFile(json.file);
      break;
  }
};

/**
 * Due to the subprocess interaction mechanism of Emacs, a piece of data sent
 * by Emacs to the subprocess may be divided into multiple pieces of data and
 * sent separately.
 * Using readline can avoid this problem, but it is necessary to ensure that
 * each data sent by Emacs has only one newline character, and the newline
 * character is at the end of the data.
 */
const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
});

rl.on('line', (line) => {
  handler(line);
});

process.on('unhandledRejection', (reason) => {
  console.error(reason);
});

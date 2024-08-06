import path from 'node:path';
import { Worker } from 'node:worker_threads';
import { findEslintConfigFile, getESLintInstallDir } from './utils.mjs';
import { Command } from './message.mjs';

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
 * @param {string} filepath
 */
const getFilepathWorker = (filepath) => {
  const config = findEslintConfigFile(filepath);
  if (!config) return null;

  addConfigFile2Map(config, filepath);

  if (configWorkerMap.has(config)) {
    return configWorkerMap.get(config) || null;
  }

  const root = getESLintInstallDir(filepath);
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
 * @param {string} filepath
 */
const closeFile = async (filepath) => {
  const config = findEslintConfigFile(filepath);
  if (!config) return;

  const fileSet = configFilesMap.get(config);
  fileSet?.delete(filepath);

  if (fileSet?.size) return;

  const worker = configWorkerMap.get(config);
  await worker?.terminate();
  configWorkerMap.delete(config);
  if (!configWorkerMap.size) process.exit(0);
};

/**
 * @param {Result} result
 */
const sendResultToEmacs = (result) => {
  console.log(JSON.stringify(result));
};

process.stdin.on('data', async (data) => {
  const start = performance.now();
  const str = data.toString('utf8');
  /** @type {InteractiveData} */
  const json = JSON.parse(str);

  switch (json.cmd) {
    case Command.Lint: {
      const { file, code } = json;
      const result = await lintFile(file, code);
      sendResultToEmacs({
        file,
        cost: performance.now() - start,
        messages: result?.length ? result : undefined,
      });
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
  }
});

process.on('unhandledRejection', (reason) => {
  console.error(reason);
});

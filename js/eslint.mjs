import path from 'node:path';
import { Worker } from 'node:worker_threads';
import { findEslintConfigFile, getESLintInstallDir } from './utils.mjs';
import {
  serverError,
  workerExitError,
  workerReloadError,
} from './jsonrpc-error.mjs';
import { WorkerReloadExitCode } from './config.mjs';

/**
 * @typedef {import("./worker").WorkerConfig} WorkerConfig
 * @typedef {import("./worker").WorkerInput} WorkerInputWorkerConfig
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
 * @returns {Worker | null}
 */
const assignESLintWorker = (config) => {
  const root = getESLintInstallDir(config);
  if (!root) {
    configWorkerMap.set(config, null);
    return null;
  }

  const workerFile = path.join(import.meta.dirname, './worker.mjs');
  process.chdir(path.dirname(config));
  /** @type {WorkerConfig} */
  const workerConfig = { root, config };
  const worker = new Worker(workerFile, { workerData: workerConfig });
  const onWorkerDead = () => {
    configWorkerMap.delete(config);
    configFilesMap.delete(config);
  };
  worker.on('exit', onWorkerDead);
  worker.on('error', onWorkerDead);
  configWorkerMap.set(config, worker);
  return worker;
};

/**
 * @param {string} filepath
 * @returns {string | null}
 */
const getEslintConfigFile = (filepath) => {
  for (const [config, set] of configFilesMap.entries()) {
    if (set.has(filepath)) return config;
  }
  return findEslintConfigFile(filepath);
};

/**
 * @param {string} filepath
 * @returns {Worker | null}
 */
const getFilepathWorker = (filepath) => {
  const config = getEslintConfigFile(filepath);
  if (!config) return null;

  addConfigFile2Map(config, filepath);

  if (configWorkerMap.has(config)) {
    return configWorkerMap.get(config) || null;
  }

  return assignESLintWorker(config);
};

const getId = (() => {
  let nextId = 0;

  return () => {
    if (nextId >= Number.MAX_SAFE_INTEGER) {
      nextId = Number.MIN_SAFE_INTEGER;
    }
    return nextId++;
  };
})();

/**
 * @param {string} filepath
 * @param {string} code
 * @returns {Promise<WorkerOutput | null>}
 */
export const lintFile = async (filepath, code) => {
  const worker = getFilepathWorker(filepath);
  if (!worker) return null;

  const promise = new Promise((resolve, reject) => {
    const id = getId();
    const clean = () => {
      worker.off('exit', onExit);
      worker.off('error', onError);
      worker.off('message', onMessage);
    };
    /**
     * @param {WorkerOutput} value
     */
    const onMessage = (value) => {
      if (value.id !== id) return;
      clean();
      resolve(value.messages);
    };
    /**
     * @param {number} exitCode
     */
    const onExit = (exitCode) => {
      clean();
      switch (exitCode) {
        case WorkerReloadExitCode:
          reject(workerReloadError());
          break;
        default:
          reject(workerExitError({ exitCode }));
          break;
      }
    };
    /**
     * @param {Error} error
     */
    const onError = (error) => {
      clean();
      reject(serverError(error.stack));
    };

    worker.on('exit', onExit);
    worker.on('error', onError);
    worker.on('message', onMessage);
    /** @type {import('./worker').WorkerInput} */
    const msg = { id, code, filepath };
    worker.postMessage(msg);
  });
  return promise;
};

/**
 * @param {string} filepath
 */
export const closeFile = (filepath) => {
  configFilesMap.forEach((fileSet, config) => {
    if (!fileSet.has(filepath)) return;

    fileSet.delete(filepath);
    if (fileSet.size) return;

    const worker = configWorkerMap.get(config);
    worker?.terminate();
    configWorkerMap.delete(config);
    configFilesMap.delete(config);
  });

  if (!configWorkerMap.size) {
    process.exit(0);
  }
};

export const getConfigMaps = () => {
  return {
    workers: [...configWorkerMap.entries()].map(([config, worker]) => [
      config,
      worker?.threadId ?? null,
    ]),
    files: [...configFilesMap.entries()].map(([config, fileSet]) => [
      config,
      [...fileSet],
    ]),
  };
};

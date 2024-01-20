import path from 'node:path';
import { Worker } from 'node:worker_threads';
import { findEslintConfigFile, getESLintInstallDir } from './utils';
import {
  Command,
  ESLintMessage,
  InteractiveData,
  Result,
  WorkerInput,
  WorkerOutput,
} from './message';

const configWorkerMap = new Map<string, Worker | null>();
const configFilesMap = new Map<string, Set<string>>();

const addConfigFile2Map = (config: string, filepath: string) => {
  const fileSet = configFilesMap.get(config);

  if (fileSet) fileSet.add(config);
  else configFilesMap.set(config, new Set([filepath]));
};

const getFilepathWorker = (filepath: string) => {
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

  const workerFile = path.join(__dirname, './worker.js');
  process.chdir(path.dirname(config));
  const worker = new Worker(workerFile, { workerData: root });
  configWorkerMap.set(config, worker);

  return worker;
};

const lintFile = async (filepath: string, code: string) => {
  const worker = getFilepathWorker(filepath);
  if (!worker) return null;

  return new Promise<ESLintMessage[]>((resolve) => {
    const listener = (value: WorkerOutput) => {
      if (value.filepath !== filepath) return;
      resolve(value.messages);
      worker.off('message', listener);
    };
    worker.on('message', listener);

    const msg: WorkerInput = { code, filepath };
    worker.postMessage(msg);
  });
};

const closeFile = async (filepath: string) => {
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

const sendResultToEmacs = (result: Result) => {
  console.log(JSON.stringify(result));
};

process.stdin.on('data', async (data) => {
  const start = performance.now();
  const str = data.toString('utf8');
  const json: InteractiveData = JSON.parse(str);

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
    case Command.Log:
      console.error(5, configWorkerMap.entries(), configFilesMap.entries());
      break;
  }
});

process.on('unhandledRejection', (reason) => {
  console.error(reason);
});

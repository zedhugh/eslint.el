import { parentPort, workerData } from 'node:worker_threads';
import type { ESLint } from 'eslint';
import {
  parseLintResult,
  type WorkerInput,
  type WorkerOutput,
} from './message';
import { importEslint } from './utils';

const root: string = workerData;
const waitingFileCodeMap = new Map<string, string>();

let eslintInstance: ESLint | null = null;
const getESLint = async () => {
  if (eslintInstance) return eslintInstance;

  eslintInstance = await importEslint(root);
  return eslintInstance;
};

const onMessage = async (input: WorkerInput) => {
  const { code, filepath } = input;
  waitingFileCodeMap.set(filepath, code);

  const eslint = await getESLint();
  waitingFileCodeMap.forEach(async (code, filepath) => {
    waitingFileCodeMap.delete(filepath);
    const result = await eslint.lintText(code, { filePath: filepath });
    const output: WorkerOutput = {
      filepath,
      messages: parseLintResult(result, filepath),
    };
    parentPort?.postMessage(output);
  });
};

parentPort?.on('message', onMessage);

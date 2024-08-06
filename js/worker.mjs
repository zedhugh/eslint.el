import { parentPort, workerData } from 'node:worker_threads';
import { parseLintResult } from './message.mjs';

/**
 * @typedef {import("eslint").ESLint} ESLint
 * @typedef {import("eslint/use-at-your-own-risk").FlatESLint} FlatESLint
 * @typedef {import("eslint/use-at-your-own-risk").LegacyESLint} LegacyESLint
 * @typedef {import("./worker").WorkerConfig} WorkerConfig
 * @typedef {import("./worker").WorkerInput} WorkerInput
 * @typedef {import("./worker").WorkerOutput} WorkerOutput
 */

const loadESLint = async () => {
  const { ESLint } = await import('eslint');
  /** @type {typeof import("eslint/use-at-your-own-risk")} */
  const { FlatESLint, LegacyESLint } = await (async () => {
    try {
      const mod = await import('eslint/use-at-your-own-risk');
      return mod.default || mod;
    } catch (_err) {
      return {};
    }
  })();

  const Flat = FlatESLint || ESLint;
  const Legacy = LegacyESLint || ESLint;

  const flat = new Flat();
  const legacy = new Legacy();
  return (await flat.findConfigFile?.()) ? flat : legacy;
};

/** @type {WorkerConfig} */
const { root, config } = workerData;
/** @type {Map<string, string>} */
const waitingFileCodeMap = new Map();

/** @type {ESLint | LegacyESLint | null} */
let eslintInstance = null;
const getESLint = async () => {
  if (eslintInstance) return eslintInstance;

  eslintInstance = await loadESLint();
  return eslintInstance;
};

/**
 * @param {WorkerInput} input
 */
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
    /** @type {WorkerOutput} */
    const output = {
      filepath,
      messages: parseLintResult(result, filepath),
    };
    parentPort?.postMessage(output);
  });
};

parentPort?.on('message', onMessage);

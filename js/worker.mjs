import path from 'node:path';
import { parentPort, workerData } from 'node:worker_threads';
import { pkgJson } from './config.mjs';
import { parseLintResult } from './message.mjs';

/** @type {WorkerConfig} */
const { root, config } = workerData;

/**
 * @typedef {import("eslint").ESLint} ESLint
 * @typedef {import("eslint/use-at-your-own-risk").FlatESLint} FlatESLint
 * @typedef {import("eslint/use-at-your-own-risk").LegacyESLint} LegacyESLint
 * @typedef {import("./worker").WorkerConfig} WorkerConfig
 * @typedef {import("./worker").WorkerInput} WorkerInput
 * @typedef {import("./worker").WorkerOutput} WorkerOutput
 */

const loadESLint = async () => {
  /** @type {{default: typeof import("eslint/package.json")}} */
  const json = await import(path.join(root, pkgJson), {
    with: { type: 'json' },
  }).then((obj) => obj.default || obj);
  /** @type {string} */
  const apiJs = json.exports?.['.'] || json.main;
  /** @type {string | undefined} */
  const riskJs =
    json.exports?.['./use-at-your-own-risk'] ||
    json.exports?.['use-at-your-own-risk'];
  const { ESLint } = await import(path.join(root, apiJs));
  /** @type {typeof import("eslint/use-at-your-own-risk")} */
  const { FlatESLint, LegacyESLint } = await (async () => {
    try {
      const mod = await import(path.join(root, riskJs));
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

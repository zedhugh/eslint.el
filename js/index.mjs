import { closeFile, lintFile } from './eslint.mjs';
import { invalidParams, methodNotFound } from './jsonrpc-error.mjs';
import {
  addNotifyFn,
  createInputDataHandler,
  setRequestFn,
} from './jsonrpc.mjs';

/**
 * @param {{code: string, file: string}} params
 */
const lint = async (params) => {
  if (typeof params !== 'object' || !params.code || !params.file) {
    const data = 'params need to be structuredas {code: string, file: string}';
    throw methodNotFound(data);
  }
  const { file, code } = params;
  return lintFile(file, code);
};

/**
 * @param {{file: string}} params
 */
const close = (params) => {
  if (typeof params !== 'object' || typeof params.file !== 'string') {
    throw invalidParams('params need to be structured as {file: string}');
  }
  closeFile(params.file);
};

setRequestFn('lint', lint);
addNotifyFn('close', close);

process.stdin.on('data', createInputDataHandler());

import { lintFile } from './eslint.mjs';
import { methodNotFound } from './jsonrpc-error.mjs';
import { createInputDataHandler, setRequestFn } from './jsonrpc.mjs';

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

setRequestFn('lint', lint);

process.stdin.on('data', createInputDataHandler());

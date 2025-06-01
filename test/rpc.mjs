import {
  addNotifyFn,
  createInputDataHandler,
  setRequestFn,
} from '../js/jsonrpc.mjs';
import { invalidParams } from '../js/jsonrpc-error.mjs';
import { logToDebugFile } from '../js/utils.mjs';

/**
 * @type {import('../js/jsonrpc.mjs').JSONRPCNotify}
 */
const notify = (params) => {
  logToDebugFile(params, 'notify');
};
addNotifyFn('notify', notify);

const sum = (list) => {
  if (!Array.isArray(list)) {
    throw invalidParams(`params need a Array but got a ${typeof list}`);
  }

  let s = 0;
  list.forEach((item) => {
    if (typeof item === 'number') {
      s += item;
      return;
    }
    throw new Error('hello');
    throw invalidParams(
      `params need a number Array but got a ${typeof item} in array`,
    );
  });

  return s;
};

setRequestFn('sum', sum);

process.stdin.on('data', createInputDataHandler());

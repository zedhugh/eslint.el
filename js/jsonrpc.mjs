import {
  getJSONRPCError,
  methodNotFound,
  parseError,
} from './jsonrpc-error.mjs';
import { logToDebugFile } from './utils.mjs';

/**
 * @typedef {import("./jsonrpc.mjs").ID} ID
 * @typedef {import("./jsonrpc.mjs").JSONRPCRequest} JSONRPCRequest
 * @typedef {import("./jsonrpc.mjs").JSONRPCRequestBody} JSONRPCRequestBody
 * @typedef {import('./jsonrpc.mjs').JSONRPCResult} JSONRPCResult
 * @typedef {import("./jsonrpc.mjs").JSONRPCNotify} JSONNotify
 * @typedef {import("./jsonrpc.mjs").JSONRPCNotifyBody} JSONRPCNotifyBody
 * @typedef {import("./jsonrpc.mjs").JSONRPCError} JSONRPCError
 * @typedef {import("./jsonrpc.mjs").JSONRPCErrorBody} JSONRPCErrorBody
 */

/**
 * @type {Map<string, JSONRPCRequest>}
 */
const requestMap = new Map();
/**
 * @template P, R
 * @param {string} methodName
 * @param {JSONRPCRequest<P, R>} method
 */
export const setRequestFn = (methodName, method) => {
  requestMap.set(methodName, method);
};

/**
 * @type {Map<string, Set<JSONNotify>}
 */
const notifyMap = new Map();
/**
 * @template P
 * @param {string} methodName
 * @param {JSONNotify<P>} method
 */
export const addNotifyFn = (methodName, method) => {
  const fnSet = notifyMap.get(methodName);
  if (fnSet) {
    fnSet.add(method);
  } else {
    notifyMap.set(methodName, new Set([method]));
  }
};

/**
 * @template J
 * @param {J} json
 */
const sendJson = (json) => {
  const body = JSON.stringify(json);
  const response = `Content-Length: ${byteLength(body)}\r\n\r\n${body}\r\n`;
  process.stdout.write(response);
};

/**
 * @template R
 * @param {ID} id
 * @param {R} result
 */
const sendResult = (id, result) => {
  /** @type {JSONRPCResult} */
  const json = { jsonrpc: '2.0', id, result };
  sendJson(json);
};

/**
 * @template D
 * @param {ID} id
 * @param {JSONRPCErrorBody<D>} error
 */
const sendError = (id, error) => {
  /** @type {JSONRPCError} */
  const json = { jsonrpc: '2.0', id, error };
  sendJson(json);
};

/**
 * @param {JSONRPCNotifyBody | JSONRPCRequestBody} json
 */
const methodHandler = (json) => {
  const { method, params, id } = json;
  if ((id ?? null) === null) {
    const set = notifyMap.get(method);
    if (set?.size) {
      set.forEach((fn) => {
        try {
          fn(params);
        } catch (err) {
          logToDebugFile(getJSONRPCError(err), 'Notification Error');
        }
      });
    } else {
      logToDebugFile(methodNotFound({ method, params }), 'Notification');
    }
  } else {
    const fn = requestMap.get(method);
    if (!fn) {
      sendError(id, methodNotFound({ method }));
      return;
    }

    try {
      Promise.resolve(fn(params)).then(
        (r) => sendResult(id, r),
        (e) => sendError(id, getJSONRPCError(e)),
      );
    } catch (err) {
      sendError(id, getJSONRPCError(err));
    }
  }
};

/**
 * @param {string} str
 * @param {number} [start]
 * @param {number} [end]
 */
const byteSlice = (str, start, end) => {
  const bytes = new TextEncoder().encode(str);
  const sliceBytes = bytes.slice(start, end);
  return new TextDecoder().decode(sliceBytes);
};

/**
 * @param {string} str
 */
const byteLength = (str) => new TextEncoder().encode(str).byteLength;

export const createInputDataHandler = () => {
  let jsonBuffer = '';
  const regExp = /(.*)?(\r\n)*Content-Length: (\d+)(\r\n)*(.*)/gm;

  /**
   * @param {Buffer} data
   */
  return (data) => {
    const chunk = data.toString('utf8');
    jsonBuffer += chunk;

    const jsonTextList = [];
    let match;
    while ((match = regExp.exec(jsonBuffer))) {
      const len = +match[3];
      const text = match[5];
      if (len > byteLength(text)) break;

      const jsonText = byteSlice(text, 0, len);
      jsonTextList.push(jsonText);

      const r = byteSlice(text, len);
      jsonBuffer = `${r}${jsonBuffer.slice(regExp.lastIndex)}`;
      regExp.lastIndex = 0;
    }
    regExp.lastIndex = 0;

    jsonTextList.forEach((jsonText) => {
      let json;
      try {
        json = JSON.parse(jsonText);
      } catch (err) {
        sendError(null, parseError(err?.stack));
        return;
      }
      methodHandler(json);
    });
  };
};

/**
 * @typedef {import("./jsonrpc.mjs").JSONRPCErrorBody} JSONRPCErrorBody
 * @typedef {<D = unknown>(data?: D) => JSONRPCErrorBody<D>} ErrorCreator
 */

/** @type {JSONRPCErrorBody} */
const ParseError = { code: -32700, message: 'Parse error' };
/** @type {JSONRPCErrorBody} */
const InvalidRequest = { code: -32600, message: 'Invalid Request' };
/** @type {JSONRPCErrorBody} */
const MethodNotFound = { code: -32601, message: 'Method not found' };
/** @type {JSONRPCErrorBody} */
const InvalidParams = { code: -32601, message: 'Invalid params' };
/** @type {JSONRPCErrorBody} */
const InternalError = { code: -32603, message: 'Internal error' };
/** @type {JSONRPCErrorBody} */
const ServerError = { code: -32000, message: 'Server error' };

/** @type {ErrorCreator} */
export const parseError = (data) => ({ ...ParseError, data });
/** @type {ErrorCreator} */
export const invalidRequest = (data) => ({ ...InvalidRequest, data });
/** @type {ErrorCreator} */
export const methodNotFound = (data) => ({ ...MethodNotFound, data });
/** @type {ErrorCreator} */
export const invalidParams = (data) => ({ ...InvalidParams, data });
/** @type {ErrorCreator} */
export const internalError = (data) => ({ ...InternalError, data });
/** @type {ErrorCreator} */
export const serverError = (data) => ({ ...ServerError, data });

/**
 * @param {JSONRPCErrorBody} err
 */
const checkJSONRPCError = (err) => {
  if (typeof err === 'object' && err) {
    const { code, message } = err;
    if (code >= -32099 && code <= -32000 && typeof message === 'string') {
      return true;
    }
  }
  return false;
};

/**
 * @param {Error | JSONRPCErrorBody} error
 * @returns {JSONRPCErrorBody}
 */
export const getJSONRPCError = (error) => {
  if (error instanceof Error) {
    return serverError(error.stack || error.message);
  }
  if (checkJSONRPCError(error)) {
    return error;
  }
  return serverError(error);
};

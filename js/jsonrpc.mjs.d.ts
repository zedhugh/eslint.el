type ID = string | number;

export type JSONRPCNotifyBody<P = unknown> = {
  jsonrpc: '2.0';
  method: string;
  params: P;
};

export type JSONRPCRequestBody<P = unknown> = {
  jsonrpc: '2.0';
  id: ID;
  method: string;
  params: P;
};

export type JSONRPCResult<R = unknown> = {
  jsonrpc: '2.0';
  id: ID;
  result: R;
};

export type JSONRPCErrorBody<D = unknown> = {
  code: number;
  message: string;
  data?: D | null;
};
export type JSONRPCError<D = unknown> = {
  jsonrpc: '2.0';
  id: ID;
  error: JSONRPCErrorBody<D>;
};

export type JSONRPCRequest = <P = unknown, R = unknown>(
  params: P,
) => R | Promise<R>;

export type JSONRPCNotify = <P>(params: P) => void;

export const setRequestFn: <P = unknown, R = unknown>(
  methodName: string,
  method: JSONRPCRequest<P, R>,
) => void;
export const addNotifyFn: <P = unknown>(
  methodName: string,
  method: JSONRPCNotify<P>,
) => void;
export const createInputDataHandler: () => (data: Buffer) => void;

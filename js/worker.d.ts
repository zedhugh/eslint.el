import type { ESLintMessage } from './message.mjs';

export interface WorkerInput {
  id: number;
  code: string;
  filepath: string;
}

export interface WorkerOutput {
  id: number;
  messages: ESLintMessage[];
}

export interface WorkerConfig {
  root: string;
  config: string;
}

export interface ESLintWorker {
  root: string;
  config: string;
  worker: Worker;
  files: Set<string>;
}

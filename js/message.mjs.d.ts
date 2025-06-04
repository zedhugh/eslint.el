import type { Linter, ESLint } from 'eslint';

export interface ESLintMessage {
  ruleId: string;
  /**
   * The numeric severity level for a rule.
   * - `1` means warn.
   * - `2` means error.
   */
  severity: Exclude<Linter.Severity, 0>;
  message: string;
  line: number;
  column: number;
  endLine?: number;
  endColumn?: number;
}

export interface Result {
  file: string;
  cost: number;
  messages?: ESLintMessage[];
}

export const enum Command {
  Lint = 'lint',
  Close = 'close',
  Exit = 'exit',
  Log = 'log',
  Save = 'save',
}

interface BaseData {
  cmd: Command;
}

interface LintData extends BaseData {
  cmd: Command.Lint;
  file: string;
  code: string;
}

interface CloseFileData extends BaseData {
  cmd: Command.Close;
  file: string;
}

interface ExitData extends BaseData {
  cmd: Command.Exit;
}

interface LogData extends BaseData {
  cmd: Command.Log;
}

interface SaveData extends BaseData {
  cmd: Command.Save;
  file: string;
}

export type InteractiveData =
  | LintData
  | CloseFileData
  | ExitData
  | LogData
  | SaveData;

export const parseLintResult: (
  result: ESLint.LintResult[],
  filepath: string,
) => ESLintMessage[];

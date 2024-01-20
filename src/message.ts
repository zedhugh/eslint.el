import type { ESLint, Linter } from 'eslint';

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

export type InteractiveData = LintData | CloseFileData | ExitData | LogData;

export interface WorkerInput {
  code: string;
  filepath: string;
}

export interface WorkerOutput {
  filepath: string;
  messages: ESLintMessage[];
}

export const parseLintResult = (
  result: ESLint.LintResult[],
  filepath: string,
) => {
  const list: ESLintMessage[] = [];
  const fileResult = result.filter((r) => r.filePath === filepath);
  fileResult.forEach((r) => {
    r.messages.forEach((m) => {
      if (!m.ruleId || !m.severity) return;

      list.push({
        ruleId: m.ruleId,
        severity: m.severity,
        message: m.message,
        line: m.line,
        column: m.column,
        endLine: m.endLine,
        endColumn: m.endColumn,
      });
    });
  });

  return list;
};

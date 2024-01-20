import type { ESLint, Linter } from 'eslint';

interface ESLintMessage {
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

type Command = 'lint' | 'close' | 'exit';

interface BaseData {
  cmd: Command;
}

interface LintData extends BaseData {
  cmd: 'lint';
  filename: string;
  code: string;
}

interface CloseFileData extends BaseData {
  cmd: 'close';
  filename: string;
}

interface ExitData extends BaseData {
  cmd: 'exit';
}

type InteractiveData = LintData | CloseFileData | ExitData;

import type { ESLint, Linter } from "eslint";

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

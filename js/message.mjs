/**
 * @typedef {import("eslint").ESLint.LintResult} LintResult
 * @typedef {import("./message").ESLintMessage} ESLintMessage
 */

export const Command = Object.freeze({
  Lint: 'lint',
  Close: 'close',
  Exit: 'exit',
  Log: 'log',
});

/**
 * @param {LintResult[]} result
 * @param {string} filepath
 */
export const parseLintResult = (result, filepath) => {
  /** @type {ESLintMessage[]} */
  const list = [];
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

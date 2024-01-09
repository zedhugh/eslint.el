/**
 * @typedef {import("eslint").ESLint.LintResult} LintResult
 * @typedef {import("eslint").Linter.LintMessage} LintMessage
 * @typedef {import("eslint").Linter} Linter
 * @typedef {import("eslint").ESLint} ESLint
 * @typedef {import("./emacs-flymake-eslint").ESLintMessage} ESLintMessage
 * @typedef {{
 *    cost: number;
 *    filename: string;
 *    messages?: ESLintMessage[]
 * }} Result
 */

const { getESLint } = require("./core.cjs");

/**
 * @param {LintResult[]} result
 * @param {string} filepath
 */
const parseLintResult = (result, filepath) => {
  /**
   * @type {ESLintMessage[]}
   */
  const list = [];
  result
    .filter((r) => r.filePath === filepath)
    .forEach((r) => {
      r.messages.forEach((m) => {
        if (!m.ruleId || m.severity === 0) return;
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

const recvStdin = () => {
  const { stdin, stdout } = process;
  stdin.on("data", async (data) => {
    const start = globalThis.performance.now();
    const str = data.toString();
    const json = JSON.parse(str);
    const filename = json.filename;
    const code = json.code;
    const eslint = await getESLint(filename);

    /**
     * @type {Result}
     */
    const obj = { filename, cost: globalThis.performance.now() - start };
    if (!eslint) {
      stdout.write(JSON.stringify(obj));
      return;
    }

    const result = await eslint.lintText(code, {
      filePath: filename,
      warnIgnored: true,
    });

    obj.messages = parseLintResult(result, filename);
    obj.cost = globalThis.performance.now() - start;
    stdout.write(JSON.stringify(obj));
  });
};

(() => {
  return new Promise(() => {
    recvStdin();
  });
})();

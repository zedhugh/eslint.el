/**
 * @typedef {import("eslint").ESLint.LintResult} LintResult
 * @typedef {import("eslint").Linter.LintMessage} LintMessage
 * @typedef {import("eslint").Linter} Linter
 * @typedef {import("eslint").ESLint} ESLint
 * @typedef {import("./emacs-flymake-eslint").ESLintMessage} ESLintMessage
 * @typedef {import("./emacs-flymake-eslint").InteractiveData} InteractiveData
 * @typedef {{
 *    cost: number;
 *    filename: string;
 *    messages?: ESLintMessage[]
 * }} Result
 */

const { getESLint, closeFile } = require("./core.cjs");

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

process.on("unhandledRejection", (reason) => {
  process.stderr.write(`${reason}\n`);
});

/**
 * @param {string} code
 * @param {string} filename
 */
const lintFile = async (code, filename) => {
  const start = globalThis.performance.now();
  const eslint = await getESLint(filename);

  /**
   * @type {Result}
   */
  const obj = { filename, cost: globalThis.performance.now() - start };
  if (!eslint) {
    return obj;
  }

  const result = await eslint.lintText(code, {
    filePath: filename,
    warnIgnored: true,
  });

  obj.messages = parseLintResult(result, filename);
  obj.cost = globalThis.performance.now() - start;
  return obj;
};

const recvStdin = () => {
  const { stdin, stdout } = process;
  stdin.on("data", async (data) => {
    const str = data.toString();
    /** @type {InteractiveData} */
    const json = JSON.parse(str);

    switch (json.cmd) {
      case "lint": {
        const { code, filename } = json;
        const obj = await lintFile(code, filename);
        stdout.write(JSON.stringify(obj));
        break;
      }
      case "close": {
        const { filename } = json;
        filename && closeFile(filename);
      }
    }
  });
};

(() => {
  return new Promise(() => {
    recvStdin();
  });
})();

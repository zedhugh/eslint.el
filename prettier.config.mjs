/**
 * @type {import("prettier").Config}
 */
const prettierConfig = {
  // 缩进
  printWidth: 80,
  tabWidth: 2,
  useTabs: false,

  // 引号
  singleQuote: true,
  quoteProps: "as-needed",
  jsxSingleQuote: false,

  // 括号
  bracketSpacing: true,
  bracketSameLine: false,
  arrowParens: "always",

  // 结束符
  semi: true,
  trailingComma: "all",
  endOfLine: "lf",

  // 其他
  insertPragma: false,
};

export default prettierConfig;

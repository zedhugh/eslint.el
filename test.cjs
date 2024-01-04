const path = require("node:path");

const apiJs = path.join(
  __dirname,
  "node_modules/eslint/lib/unsupported-api.js",
);

const { FlatESLint, shouldUseFlatConfig, LegacyESLint } = require(apiJs);

const flatEslint = new FlatESLint();
const legacyEslint = new LegacyESLint();

/**
 * @param {string} filepath
 */
const lintFile = async (filepath) => {
  console.log(filepath);
  const label = "cost";
  console.time(label);
  const dir = path.dirname(filepath);
  process.chdir(dir);

  const usingFlatConfig = await shouldUseFlatConfig();
  const eslint = usingFlatConfig ? flatEslint : legacyEslint;
  const results = await eslint.lintFiles(filepath);
  console.timeEnd(label);
};

(async () => {
  for (const file of [
    "./emacs-flymake-eslint.cjs",
    "./eslint.config.js",
    "./test/emacs-flymake-eslint.cjs",
    "./test/eslint.config.js",
    "./test.cjs",
  ]) {
    await lintFile(path.join(__dirname, file));
  }
})();

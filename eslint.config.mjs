/**
 * @type {import("eslint").Linter.Config[]}
 */
const config = [
  {
    rules: {
      'no-console': 'error',
      'no-await-in-loop': 'warn',
      'array-callback-return': 'error',
    },
  },
];

export default config;

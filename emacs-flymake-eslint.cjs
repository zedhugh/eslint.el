const { getESLint } = require("./core.cjs");

const recvStdin = () => {
  const { stdin, stdout } = process;
  stdin.on("data", async (data) => {
    console.time("cost");
    const str = data.toString();
    const json = JSON.parse(str);
    const filename = json.filename;
    const eslint = await getESLint(filename);
    if (!eslint) {
      stdout.write(null);
      return;
    }

    const result = await eslint.lintFiles(filename);
    console.timeEnd("cost");
    stdout.write(JSON.stringify(result.map((r) => r.messages)));
  });
};

(() => {
  return new Promise((resolve) => {
    recvStdin();
  });
})();

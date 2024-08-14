const log = (...msg) => {
  console.error('==================================\n');
  console.error(...msg, '\n');
};

let tmp = '';
let time = 0;
let count = 0;

process.stdin
  .on('drain', () => log('drain'))
  .on('close', (hasError) => log('close', hasError))
  .on('connect', () => log('connect'))
  .on('ready', () => log('ready'))
  .on('timeout', () => log('timeout'))
  .on('lookup', () => log('lookup'))
  .on('error', (err) => log('error', err))
  .on('data', (data) => {
    const start = time || performance.now();
    const chunk = data.toString('utf8');
    try {
      if (!chunk.endsWith('}') && !chunk.endsWith(']')) {
        throw new SyntaxError('JSON data is incomplete.');
      }
      const str = tmp + chunk;
      const json = JSON.parse(str);
      const end = performance.now();
      tmp = '';
      time = 0;
      const c = count + 1;
      count = 0;
      delete json.code;
      console.error('----------------', chunk.slice(0, 100));
      log('data', { json, start, end, count: c, cost: end - start });
    } catch (err) {
      tmp += chunk;
      ++count;
      time = time || start;
      console.error('----------------', err, chunk.slice(0, 100));
    }
  });

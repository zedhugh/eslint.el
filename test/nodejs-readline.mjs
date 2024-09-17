import readline from 'node:readline';

const log = (...msg) => {
  console.error('==================================\n');
  console.error(...msg, '\n');
};

let time = performance.now();
let count = 0;

/**
 * @param {number} length
 * @param {number} peace
 * @param {string} unit
 */
const format = (length, peace, unit) => {
  const len = length / peace;
  return `${len.toFixed(2)}${unit}`;
};

/**
 * @param {number} length
 */
const stringByte = (length) => {
  let peace = 1;
  let unit = 'B';
  if (length < peace * 1024) {
    return format(length, peace, unit);
  }
  [peace, unit] = [peace * 1024, 'KiB'];
  if (length < peace * 1024) {
    return format(length, peace, unit);
  }
  [peace, unit] = [peace * 1024, 'MiB'];
  return format(length, peace, unit);
};

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
});
rl.on('line', (line) => {
  const start = performance.now();
  console.error('read cost:', start - time);

  try {
    const json = JSON.parse(line);
    const end = performance.now();
    time = end;
    const c = count + 1;
    count = 0;
    const code = json.code;
    delete json.code;
    log('data', {
      json,
      start,
      end,
      count: c,
      cost: end - start,
      codeLength: stringByte(code?.length ?? 0),
    });
  } catch (err) {
    ++count;
    time = time || start;
  }
});

process.stdin
  .on('drain', () => log('drain'))
  .on('close', (hasError) => log('close', hasError))
  .on('connect', () => log('connect'))
  .on('ready', () => log('ready'))
  .on('timeout', () => log('timeout'))
  .on('lookup', () => log('lookup'))
  .on('error', (err) => log('error', err));

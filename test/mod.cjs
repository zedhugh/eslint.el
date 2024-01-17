"use strict";

/**
 * @param {number} time
 * @param {number} delay
 * @param {(count: number) => void} fn
 */
const repeat = (time, delay, fn) => {
  /**
   * @type {NodeJS.Timeout}
   */
  let timer;

  let remain = 0;
  const exceute = () => {
    if (remain >= time) {
      return;
    }
    remain++;
    clearTimeout(timer);
    timer = setTimeout(() => {
      fn(remain);
      exceute();
    }, delay);
  };
  exceute();
};

module.exports = { repeat };

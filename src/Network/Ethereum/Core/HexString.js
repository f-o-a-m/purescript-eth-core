"use strict";

var BigNumber = require('bn.js');

exports.toBigNumber = function(str) {
  return new BigNumber(str, 16);
};

"use strict";
var keccak = require('keccak');

exports._keccak256 = function (a) {
    return keccak('keccak256').update(a).digest();
};

"use strict";

export const toBigNumber = (x) => BigInt('0x' + x);

var signedIsNegative = function (value) {
    var head = toBigNumber(value.substring(0, 1)).toString(2);
    var msb;
    if (head.length == 4) {
        msb = head.substring(0,1);
    } else {
        msb = '0';
    }
    return msb === '1';
};

const BN256_MAX_SUB_1 = BigInt('0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff') - 1n;

export const toBigNumberFromSignedHexString = function (value) {
    if (signedIsNegative(value)) {
        return toBigNumber(value) - BN256_MAX_SUB_1;
    }
    return toBigNumber(value);
};

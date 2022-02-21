"use strict";
var BigNumber = require('bn.js');

// NOTE: According to the documentation, many binary operators can take a normal js Number
// by suffixing the version for BN with 'n'.

exports._intToBigNumber = function(value) {
  return new BigNumber(value.toString(10), 10);
};

exports._numberToBigNumber = function(value) {
    return new BigNumber(value);
};

exports._eqBigNumber = function(n) {
    return function(m) { return m.eq(n); };
};

exports._addBigNumber = function(n) {
    return function (m) { return n.add(m); };
};

exports._mulBigNumber = function(n) {
    return function (m) { return n.mul(m); };
};

exports._subBigNumber = function(n) {
    return function (m) { return n.sub(m); };
};

exports._divBigNumber = function(n) {
  return function (m) { return n.div(m); }
}

exports._modBigNumber = function(n) {
  return function(m) { return n.mod(m); }
}

exports.comparedTo = function (a) {
  return function (b) {
    return a.cmp(b);
  };
};

// TODO(srghma): the specialized functions would improve preformance
//
// do like here https://github.com/throughnothing/purescript-bignum/blob/master/src/Data/BigNum.purs#L13-L24
// fromStringInt :: String -> BN
// fromStringHex :: String -> BN -- example `0xdeed` to `57069`
// fromStringHexString :: HexString -> BN -- example `deed` to `57069`

// also because native BigInt doesnt support custom radixes
exports.fromStringAsImpl = function (just) {
  return function (nothing) {
    return function (radix) {
      var digits;
      if (radix < 11) {
        digits = "[0-" + (radix - 1).toString() + "]";
      } else if (radix === 11) {
        digits = "[0-9a]";
      } else {
        digits = "[0-9a-" + String.fromCharCode(86 + radix) + "]";
      }
      var pattern = new RegExp("^[\\+\\-]?" + digits + "+$", "i");
      return function (s) {
        try {
          if (radix === 16 && (s.indexOf('0x') === 0 || s.indexOf('-0x') === 0)) {
            s = s.replace('0x','')
          }
          if (pattern.test(s)) {
            return just(new BigNumber(s, radix))
          } else {
            return nothing;
          }
        } catch (_) {
          return nothing;
        }
      };
    };
  };
};

exports.toString = function (radix) {
  return function (bn) { return bn.toString(radix); };
};

// TODO: this function is useless, because floating point numbers/ decimals are not possible with bignumber
exports.floorBigNumber = function(bn) {
    var bnStr = bn.toString(10);
    var newBn = new BigNumber(bnStr, 10);
    return newBn;
};

exports.pow = function(n) {
    return function (m) {
        var exp = new BigNumber(m, 10);
        return n.pow(exp);
    };
};

exports.toNumber = function (n) {
    var newN = new BigNumber(n); // TODO(srghma): Why make a copy?
    return newN.toNumber();
};

var isBigNumber = function (object) {
    return object instanceof BigNumber ||
        (object && object.constructor && object.constructor.name === 'BigNumber');
};

var isString = function (object) {
    return typeof object === 'string' ||
        (object && object.constructor && object.constructor.name === 'String');
};

exports.divide = function (n) {
    return function (d) {
        var newN = n.div(d);
        return newN;
    };
};

// from specification
// int<M>: two’s complement signed integer type of M bits, 0 < M <= 256, M % 8 == 0.

// int256:
//   min
//     0x8000000000000000000000000000000000000000000000000000000000000000
//     0b1000000....
//     -57896044618658097711785492504343953926634992332820282019728792003956564819968
//   max
//     0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
//     0b0000000....
//     57896044618658097711785492504343953926634992332820282019728792003956564819967

// uint256:
//   min
//     0x0000000000000000000000000000000000000000000000000000000000000000
//     0b0000000...
//     0
//   max
//     0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
//     0b1111111...
//     115792089237316195423570985008687907853269984665640564039457584007913129639935

// to confirm that -1 maps to fff...
// one can use https://adibas03.github.io/online-ethereum-abi-encoder-decoder/#/encode
// type is int256, value is -1
var minusOneAsEncodedInt256 = new BigNumber("ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff", 16);

// inputs can be only negative
// returns only positive (without `-` prepended) BigNumbers with 32 bytes / 64 bits
exports.bigNumberToTwosComplementInt256 = function (bn) {
  if (bn >= 0) { return bn }
  // since bn is negative `-x`
  // the result is `MAX_INT_256 - x + 1`
  return minusOneAsEncodedInt256.add(bn).addn(1);
};

// example input - "fffffff..."
exports.twosComplementInt256ToBigNumber = function (bn) {
  var isNegative = bn.testn(255) // test if bit "1" in "1000000..." is eq "1"
  if (isNegative) {
    return bn.sub(minusOneAsEncodedInt256).subn(1);
  }
  return bn;
};

// Test
// testRound = (x, n) => {
//   const bn = new BigNumber(x, n)
//   const complement = exports.bigNumberToTwosComplementInt256(bn)
//   const orig = exports.twosComplementInt256ToBigNumber(complement)
//   console.log({
//     bn: bn.toString(2),
//     complement: complement.toString(2),
//     orig: orig.toString(2)
//   })
//   if (!bn.eq(orig)) {
//     throw new Error(x)
//   }
// }
// testRound("57896044618658097711785492504343953926634992332820282019728792003956564819967", 10)
// testRound("-57896044618658097711785492504343953926634992332820282019728792003956564819968", 10)
// testRound("0", 10)
// testRound("-0", 10)
// testRound("1", 10)
// testRound("-1", 10)

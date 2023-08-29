"use strict";
import BigNumber from "bn.js";

//NOTE: According to the documentation, many binary operators can take a normal js Number
//by suffixing the version for BN with 'n'.

export const _intToBigNumber = function(value) {
  return new BigNumber(value.toString(10), 10);
};

export const _numberToBigNumber = function(value) {
    return new BigNumber(value);
};

export const _eqBigNumber = function(n) {
    return function(m) { return m.eq(n); };
};

export const _addBigNumber = function(n) {
    return function (m) { return n.add(m); };
};

export const _mulBigNumber = function(n) {
    return function (m) { return n.mul(m); };
};

export const _subBigNumber = function(n) {
    return function (m) { return n.sub(m); };
};

export const _divBigNumber = function(n) {
  return function (m) { return n.div(m); }
}

export const _modBigNumber = function(n) {
  return function(m) { return n.mod(m); }
}

export const comparedTo = function (a) {
  return function (b) {
    return a.cmp(b);
  };
};

export const fromStringAsImpl = function (just) {
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

export const toString = function (radix) {
  return function (bn) { return bn.toString(radix); };
};

export const toTwosComplement = function (bn) {
  if (bn.ltn(0)) {
      return new BigNumber("ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff", 16).add(bn).addn(1);
  } else {
    return bn;
  }
};

export const floorBigNumber = function(bn) {
    var bnStr = bn.toString(10);
    var newBn = new BigNumber(bnStr, 10);
    return newBn;
};

export const pow = function(n) {
    return function (m) {
        var exp = new BigNumber(m, 10);
        return n.pow(exp);
    };
};

export const toNumber = function (n) {
    var newN = new BigNumber(n);
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

export const divide = function (n) {
    return function (d) {
        var newN = n.div(d);
        return newN;
    };
};

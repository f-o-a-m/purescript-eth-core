import BigNumber from 'bn.js';

//NOTE: According to the documentation, many binary operators can take a normal js Number
//by suffixing the version for BN with 'n'.

export function _intToBigNumber (value) {
  return new BigNumber(value.toString(10), 10);
}

export function _numberToBigNumber (value) {
    return new BigNumber(value);
};

export function _eqBigNumber (n) {
    return function(m) { return m.eq(n); };
};

export function _addBigNumber (n) {
    return function (m) { return n.add(m); };
};

export function _mulBigNumber (n) {
    return function (m) { return n.mul(m); };
};

export function _subBigNumber (n) {
    return function (m) { return n.sub(m); };
};

export function _divBigNumber (n) {
  return function (m) { return n.div(m); }
}

export function _modBigNumber (n) {
  return function(m) { return n.mod(m); }
}

export function comparedTo (a) {
  return function (b) {
    return a.cmp(b);
  };
};

export function fromStringAsImpl (just) {
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

export function toString (radix) {
  return function (bn) { return bn.toString(radix); };
};

export function toTwosComplement (bn) {
  if (bn.ltn(0)) {
      return new BigNumber("ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff", 16).add(bn).addn(1);
  } else {
    return bn;
  }
};

export function floorBigNumber (bn) {
    var bnStr = bn.toString(10);
    var newBn = new BigNumber(bnStr, 10);
    return newBn;
};

export function pow (n) {
    return function (m) {
        var exp = new BigNumber(m, 10);
        return n.pow(exp);
    };
};

export function toNumber (n) {
    var newN = new BigNumber(n);
    return newN.toNumber();
};

export function divide (n) {
    return function (d) {
        var newN = n.div(d);
        return newN;
    };
};

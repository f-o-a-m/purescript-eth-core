const secp256k1 = require('secp256k1');

const apiVer = ((typeof secp256k1.ecdsaSign === 'function') && (typeof secp256k1.ecdsaRecover === 'function')) ? 4 : 3;

const signFn =  (apiVer === 4) ? secp256k1.ecdsaSign : secp256k1.sign;
const recoverFn = (apiVer === 4) ? secp256k1.ecdsaRecover : secp256k1.recover;

if (signFn === undefined || recoverFn === undefined) {
    throw new Error("Unsupported version of secp256k1");
}

// copied from ethereumjs-util
exports.isValidPublic = function (publicKey) {
    if (publicKey.length === 64) {
        // Convert to SEC1 for secp256k1
        return secp256k1.publicKeyVerify(Buffer.concat([Buffer.from([4]), publicKey]));
    } else {
        return false;
    }
};

// copied from ethereumjs-util
exports.isValidPrivate = function (privateKey) {
    return secp256k1.privateKeyVerify(privateKey);
};

// copied from ethereumjs-util, but more flexible with chainId
exports.ecSign = function (privateKey, msgHash) {
    var sig = signFn(msgHash, privateKey);
    var ret = {};
    ret.r = Buffer.from(sig.signature.slice(0, 32));
    ret.s = Buffer.from(sig.signature.slice(32, 64));
    switch (apiVer) {
        case 4:
            ret.v = sig.recid;
            break;
        default:
            ret.v = sig.recovery;
    }
    return ret;
};

// copied from ethereumjs-util, but more flexible with chainId
exports.ecRecover = function (msgHash, signature, v) {
    var senderPubKey = recoverFn(msgHash, signature, v);
    return Buffer.from(secp256k1.publicKeyConvert(senderPubKey, false).slice(1));
};

// copied from ethereumjs-util
exports.privateToPublic = function (privateKey) {
    // skip the type flag and use the X, Y points
    return Buffer.from(secp256k1.publicKeyCreate(privateKey, false).slice(1));
};

exports.generatePrivateKey = function () {
  var prv;
  do {
    prv = crypto.randomBytes(32);
  } while (!secp256k1.privateKeyVerify(prv));
  return Buffer.from(prv);
}

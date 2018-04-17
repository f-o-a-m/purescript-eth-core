var secp256k1 = require('secp256k1');

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
    var sig = secp256k1.sign(msgHash, privateKey);
    var ret = {};
    ret.r = sig.signature.slice(0, 32);
    ret.s = sig.signature.slice(32, 64);
    ret.v = sig.recovery;
    return ret;
};

// copied from ethereumjs-util, but more flexible with chainId
exports.ecRecover = function (msgHash, signature, v) {
    var senderPubKey = secp256k1.recover(msgHash, signature, v);
    return secp256k1.publicKeyConvert(senderPubKey, false).slice(1);
};

// copied from ethereumjs-util
exports.privateToPublic = function (privateKey) {
    // skip the type flag and use the X, Y points
    return secp256k1.publicKeyCreate(privateKey, false).slice(1);
};

"use strict";
import crypto from "crypto";
import secp256k1 from "secp256k1";

// copied from ethereumjs-util
export const isValidPublic = function (publicKey) {
    if (publicKey.length === 64) {
        // Convert to SEC1 for secp256k1
        return secp256k1.publicKeyVerify(Buffer.concat([Buffer.from([4]), publicKey]));
    } else {
        return false;
    }
};

// copied from ethereumjs-util
export const isValidPrivate = function (privateKey) {
    return secp256k1.privateKeyVerify(privateKey);
};

// copied from ethereumjs-util, but more flexible with chainId
export const ecSign = function (privateKey, msgHash) {
    var sig = secp256k1.ecdsaSign(msgHash, privateKey);
    var ret = {};
    ret.r = Buffer.from(sig.signature.slice(0, 32));
    ret.s = Buffer.from(sig.signature.slice(32, 64));
    ret.v = sig.recid;
    return ret;
};

// copied from ethereumjs-util, but more flexible with chainId
export const ecRecover = function (msgHash, signature, v) {
    let s = new Uint8Array(signature)
    var senderPubKey = secp256k1.ecdsaRecover(new Uint8Array(signature), v, msgHash);
    return Buffer.from(secp256k1.publicKeyConvert(senderPubKey, false).slice(1));
};

// copied from ethereumjs-util
export const privateToPublic = function (privateKey) {
    // skip the type flag and use the X, Y points
    return Buffer.from(secp256k1.publicKeyCreate(privateKey, false).slice(1));
};

export const generatePrivateKey = function () {
  var prv;
  do {
    prv = crypto.randomBytes(32);
  } while (!secp256k1.privateKeyVerify(prv));
  return Buffer.from(prv);
}

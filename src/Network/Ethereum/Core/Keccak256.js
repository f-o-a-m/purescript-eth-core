"use strict";
import keccak from "keccak";

export const _keccak256 = function (a) {
    return keccak('keccak256').update(a).digest();
};

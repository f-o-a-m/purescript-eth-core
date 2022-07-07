import keccak from 'keccak';

export function _keccak256 (a) {
    return keccak('keccak256').update(a).digest();
};
import bigInt from "big-integer";
import JSONbig from "json-bigint";

export function decodeBigInt(fail, succ, json) {
  if (Number.isInteger(json) || typeof json === "bigint") {
    return succ(bigInt(json));
  } else {
    return fail;
  }
}

export function encodeBigInt(a) {
  if (JSON.stringify !== JSONbig.stringify) {
    console.warn(
      "Tried to encode BitInt without patching JSON.stringify. Wrap your app in Data.BigInt.Argonaut.patchers.patchStringify() to enable BigInt support."
    );
    return a.toJSNumber();
  } else {
    return a.value;
  }
}

export const patchersImpl = (function () {
  const { stringify, parse } = JSONbig({ useNativeBigInt: true });

  const parseImpl = function (resultHandlers, reviver, jsonStr) {
    try {
      return resultHandlers.success(JSON.parse(jsonStr, reviver));
    } catch (e) {
      return resultHandlers.failure(e.message);
    }
  };

  const stringifyImpl = function (replacer, space, value) {
    return JSON.stringify(value, replacer, space);
  };

  return {
    parseImpl: parseImpl,
    stringifyImpl: stringifyImpl,
    patchStringify: () => {
      // We need to patch the JSON.stringify in order for BigInt serialization to work.
      JSON.stringify = stringify;
    },
    patchParse: () => {
      JSON.parse = parse;
    },
  };
})();

export const isBigInt = (value) => typeof value === "bigint";

"use strict";

//module Data.Serializable
/* global exports, parseFloat */

exports.parseFloatImpl = function parseFloatImpl(just){
  return function(nothing){
    return function(a){
      var parsed = parseFloat(a);
      return isNaN(parsed) || !isFinite(parsed) ? nothing : just(a);
    };
  };
};
exports.parseIntImpl = function parseIntImpl(just){
  return function(nothing){
    return function(a){
      // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/parseInt
      return (/^(\-|\+)?([0-9]+)$/.test(a)) ? just(Number(a)) : nothing;
    };
  };
};
exports.parseBoolImpl = function parseBoolImpl(just){
  return function(nothing){
    return function(a){
      if (a === "true"){return just(true); }
      if (a === "false"){return just(false); }
      return nothing;
    };
  };
};
exports.toString = function toString(a){
  return a.toString();
};
exports.tupleRegexImpl = function tupleRegexImpl(tuple){
  return function(just){
    return function(nothing){
      var regex = /^\((.*)\,(.*)\)$/;
      return function(string){
        var results = string.match(regex);
        return results[1] && results[2] ? just(tuple(results[1])(results[2])) : nothing;
      };
    };
  };
};

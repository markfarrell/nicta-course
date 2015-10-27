"use strict";

// module Course.Validation

exports.concatString = function (s1) {
  return function (s2) {
    return s1 + s2;
  };
};

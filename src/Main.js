"use strict";
/* jshint browser: true */

// getElementById :: String -> Eff eff Node
exports.getElementById = function(id) {
  return function() {
    return document.getElementById(id);
  };
};

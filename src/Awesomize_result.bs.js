// Generated by BUCKLESCRIPT VERSION 4.0.1, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Js_dict = require("bs-platform/lib/js/js_dict.js");
var Js_primitive = require("bs-platform/lib/js/js_primitive.js");
var Belt_MapString = require("bs-platform/lib/js/belt_MapString.js");

function map(fn, result) {
  return result.then((function (param) {
                if (param[0] >= 106380200) {
                  return Promise.resolve(/* `Error */[
                              106380200,
                              param[1]
                            ]);
                } else {
                  return Promise.resolve(/* `Ok */[
                              17724,
                              Curry._1(fn, param[1])
                            ]);
                }
              }));
}

function bimap(left, right, result) {
  return result.then((function (param) {
                if (param[0] >= 106380200) {
                  return Promise.resolve(/* `Error */[
                              106380200,
                              Curry._1(left, param[1])
                            ]);
                } else {
                  return Promise.resolve(/* `Ok */[
                              17724,
                              Curry._1(right, param[1])
                            ]);
                }
              }));
}

function fold(left, right, result) {
  return result.then((function (param) {
                if (param[0] >= 106380200) {
                  return Promise.resolve(Curry._1(left, param[1]));
                } else {
                  return Promise.resolve(Curry._1(right, param[1]));
                }
              }));
}

function getMessage(key, err) {
  var match = Belt_MapString.get(err, key);
  if (match !== undefined) {
    var match$1 = Js_primitive.valFromOption(match);
    if (match$1 !== undefined) {
      return Js_primitive.some(Js_primitive.valFromOption(match$1));
    } else {
      return undefined;
    }
  }
  
}

function listToJson(errorList) {
  return Js_dict.fromArray(Belt_MapString.toArray(Belt_MapString.map(errorList, (function (param) {
                        if (param !== undefined) {
                          return param;
                        } else {
                          return null;
                        }
                      }))));
}

var $$Error = /* module */[
  /* getMessage */getMessage,
  /* listToJson */listToJson
];

function toJs(result) {
  return fold((function (err) {
                return {
                        awesomeResultType: "Error",
                        data: null,
                        messages: listToJson(err)
                      };
              }), (function (result) {
                return {
                        awesomeResultType: "Ok",
                        data: Js_dict.fromArray(Belt_MapString.toArray(result)),
                        messages: null
                      };
              }), result);
}

exports.map = map;
exports.bimap = bimap;
exports.fold = fold;
exports.$$Error = $$Error;
exports.toJs = toJs;
/* Js_dict Not a pure module */

// Generated by BUCKLESCRIPT VERSION 4.0.1, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Js_dict = require("bs-platform/lib/js/js_dict.js");
var Js_json = require("bs-platform/lib/js/js_json.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");
var Js_primitive = require("bs-platform/lib/js/js_primitive.js");
var Belt_MapString = require("bs-platform/lib/js/belt_MapString.js");
var Js_null_undefined = require("bs-platform/lib/js/js_null_undefined.js");

var falsePromise = Promise.resolve(false);

function reply(msg, res) {
  return res.then((function (isOk) {
                return Promise.resolve(isOk ? undefined : Js_primitive.some(msg));
              }));
}

function classified(fn, maybe, sanitized) {
  return Promise.resolve(maybe !== undefined ? Curry._2(fn, Js_json.classify(Js_primitive.valFromOption(maybe)), sanitized) : undefined);
}

function numberTest(fn, maybe, sanitized) {
  var fn2 = function (classified, _) {
    if (typeof classified === "number" || classified.tag !== 1) {
      return "not_number";
    } else {
      return Curry._1(fn, classified[0]);
    }
  };
  return classified(fn2, maybe, sanitized);
}

function stringTest(fn, maybe, sanitized) {
  var fn2 = function (classified, _) {
    if (typeof classified === "number" || classified.tag) {
      return "not_string";
    } else {
      return Curry._1(fn, classified[0]);
    }
  };
  return classified(fn2, maybe, sanitized);
}

function required(maybe, _) {
  return Promise.resolve(maybe !== undefined ? undefined : "required");
}

function requireArray(maybe, _) {
  var tmp;
  if (maybe !== undefined) {
    var match = Js_json.classify(Js_primitive.valFromOption(maybe));
    tmp = typeof match === "number" || match.tag !== 3 ? "require_array" : undefined;
  } else {
    tmp = "required";
  }
  return Promise.resolve(tmp);
}

function notEqualNumber(x) {
  return (function (param, param$1) {
      return numberTest((function (str) {
                    var match = str === x;
                    if (match) {
                      return "cannot_be_equal";
                    }
                    
                  }), param, param$1);
    });
}

function notEqualString(x) {
  return (function (param, param$1) {
      return stringTest((function (str) {
                    var match = str === x;
                    if (match) {
                      return "cannot_be_equal";
                    }
                    
                  }), param, param$1);
    });
}

function isString(param, param$1) {
  return classified((function (classified, _) {
                if (typeof classified === "number" || classified.tag) {
                  return "not_string";
                }
                
              }), param, param$1);
}

function isInt(param, param$1) {
  return numberTest((function (n) {
                var match = Math.ceil(n) === n;
                if (match) {
                  return undefined;
                } else {
                  return "not_int";
                }
              }), param, param$1);
}

function isBigInt(param, param$1) {
  return stringTest((function (str) {
                var isZero = function (s) {
                  return (/^\-\d+$|^0+$/).test(s);
                };
                var isBigIntLike = function (s) {
                  return (/^\d{1,20}$/).test(s);
                };
                if (str.length > 20) {
                  return "maximum";
                } else if (isZero(str)) {
                  return "minimum";
                } else {
                  var match = isBigIntLike(str);
                  if (match) {
                    return undefined;
                  } else {
                    return "not_big_int";
                  }
                }
              }), param, param$1);
}

function isEqualNumber(x) {
  return (function (param, param$1) {
      return numberTest((function (n) {
                    var match = n === x;
                    if (match) {
                      return undefined;
                    } else {
                      return "not_equal";
                    }
                  }), param, param$1);
    });
}

function isEqualString(x) {
  return (function (param, param$1) {
      return stringTest((function (str) {
                    var match = str === x;
                    if (match) {
                      return undefined;
                    } else {
                      return "not_equal";
                    }
                  }), param, param$1);
    });
}

function minStringLength(length) {
  return (function (param, param$1) {
      return stringTest((function (str) {
                    var match = str.length >= length;
                    if (match) {
                      return undefined;
                    } else {
                      return "min_length";
                    }
                  }), param, param$1);
    });
}

function maxStringLength(length) {
  return (function (param, param$1) {
      return stringTest((function (str) {
                    var match = str.length <= length;
                    if (match) {
                      return undefined;
                    } else {
                      return "max_length";
                    }
                  }), param, param$1);
    });
}

function maxNumber(max) {
  return (function (param, param$1) {
      return numberTest((function (n) {
                    var match = n <= max;
                    if (match) {
                      return undefined;
                    } else {
                      return "maximum";
                    }
                  }), param, param$1);
    });
}

function minNumber(min) {
  return (function (param, param$1) {
      return numberTest((function (n) {
                    var match = n >= min;
                    if (match) {
                      return undefined;
                    } else {
                      return "minimum";
                    }
                  }), param, param$1);
    });
}

function matchRegex(re) {
  return (function (param, param$1) {
      return stringTest((function (str) {
                    var match = re.test(str);
                    if (match) {
                      return undefined;
                    } else {
                      return "not_match";
                    }
                  }), param, param$1);
    });
}

var isUSEIN = matchRegex((/^(0[1-6]|1[0-6]|2[0-7]|[345]\d|[68][0-8]|7[1-7]|9[0-58-9])-?\d{7}$/));

function externRaw(fn, msg, maybe, sanitized) {
  return reply(msg, Promise.resolve(Curry._2(fn, maybe, sanitized)));
}

function extern(fn, msg, maybe, sanitized) {
  if (maybe !== undefined) {
    return reply(msg, Curry._2(fn, Js_json.classify(Js_primitive.valFromOption(maybe)), sanitized));
  } else {
    return Promise.resolve(undefined);
  }
}

function externCompiler(fn, msg, maybe, sanitized) {
  return extern((function (classified, _) {
                return Curry._1(fn, classified);
              }), msg, maybe, sanitized);
}

function externString(fn, msg, maybe, sanitized) {
  var handler = function (param) {
    var fn$1 = function (str) {
      return Curry._2(fn, str, sanitized);
    };
    var classified = param;
    var tmp;
    tmp = typeof classified === "number" || classified.tag ? false : Curry._1(fn$1, classified[0]);
    return Promise.resolve(tmp);
  };
  return externCompiler(handler, msg, maybe, sanitized);
}

function externNumber(fn, msg, maybe, sanitized) {
  var handler = function (param) {
    if (typeof param === "number" || param.tag !== 1) {
      return falsePromise;
    } else {
      return Promise.resolve(Curry._2(fn, param[0], sanitized));
    }
  };
  return externCompiler(handler, msg, maybe, sanitized);
}

function externArray(fn, msg, maybe, sanitized) {
  var handler = function (param) {
    if (typeof param === "number" || param.tag !== 3) {
      return falsePromise;
    } else {
      return Promise.resolve(Curry._2(fn, param[0], sanitized));
    }
  };
  return externCompiler(handler, msg, maybe, sanitized);
}

function externDependentFactory(decoder, fn, key, msg, maybe, sanitized) {
  var handler = function (v) {
    var __x = Belt_MapString.get(sanitized, key);
    return Curry._3(fn, v, Belt_Option.flatMap(__x, (function (__x) {
                      return Belt_Option.flatMap(__x, decoder);
                    })), sanitized);
  };
  var __x = Belt_Option.flatMap(maybe, decoder);
  return reply(msg, Belt_Option.mapWithDefault(__x, falsePromise, handler));
}

function externDependentRaw(fn, key, msg, maybe, sanitized) {
  if (maybe !== undefined) {
    var __x = Belt_MapString.get(sanitized, key);
    return reply(msg, Curry._3(fn, Js_primitive.valFromOption(maybe), Belt_Option.flatMap(__x, (function (x) {
                          return x;
                        })), sanitized));
  } else {
    return Promise.resolve(undefined);
  }
}

function externDependentNumber(fn, key, msg) {
  return (function (param, param$1) {
      return externDependentFactory(Js_json.decodeNumber, fn, key, msg, param, param$1);
    });
}

function externDependentString(fn, key, msg) {
  return (function (param, param$1) {
      return externDependentFactory(Js_json.decodeString, fn, key, msg, param, param$1);
    });
}

function recursive(validator) {
  var awesomizer = function (jsonList) {
    if (jsonList) {
      var xs = jsonList[1];
      var match = Js_json.classify(jsonList[0]);
      if (typeof match === "number" || match.tag !== 2) {
        return falsePromise;
      } else {
        return Curry._1(validator, match[0]).then((function (result) {
                      if (result[0] >= 106380200) {
                        return falsePromise;
                      } else {
                        return awesomizer(xs);
                      }
                    }));
      }
    } else {
      return Promise.resolve(true);
    }
  };
  return (function (param, param$1) {
      return extern((function (taggedJson, _) {
                    if (typeof taggedJson === "number" || taggedJson.tag !== 3) {
                      return falsePromise;
                    } else {
                      return awesomizer(Belt_List.fromArray(taggedJson[0]));
                    }
                  }), "invalid_scope", param, param$1);
    });
}

function nullOrValue(param) {
  if (param !== undefined) {
    return Js_primitive.valFromOption(param);
  } else {
    return null;
  }
}

function extern$1(fn, msg) {
  return (function (maybe, sanitized) {
      var jsonSanitized = Js_dict.fromArray(Belt_MapString.toArray(Belt_MapString.map(sanitized, nullOrValue)));
      var __x = Belt_Option.map(maybe, (function (json) {
              return Curry._3(fn, json, jsonSanitized, sanitized);
            }));
      return reply(msg, Belt_Option.getWithDefault(__x, Promise.resolve(true)));
    });
}

function externDependent(fn, key, msg) {
  var executor = function (json, jsonSanitized, sanitized) {
    var maybe = Belt_MapString.get(sanitized, key);
    return Curry._3(fn, json, Belt_Option.mapWithDefault(maybe, null, Js_null_undefined.fromOption), jsonSanitized);
  };
  return extern$1(executor, msg);
}

var Promise$1 = /* module */[
  /* extern */extern$1,
  /* externDependent */externDependent
];

function extern$2(fn, msg) {
  return extern$1((function (a, b, c) {
                return Promise.resolve(Curry._3(fn, a, b, c));
              }), msg);
}

function externDependent$1(fn, key, msg) {
  return externDependent((function (a, b, c) {
                return Promise.resolve(Curry._3(fn, a, b, c));
              }), key, msg);
}

var JavaScript = [
  Promise$1,
  extern$2,
  externDependent$1
];

exports.externArray = externArray;
exports.externDependentNumber = externDependentNumber;
exports.externDependentRaw = externDependentRaw;
exports.externDependentString = externDependentString;
exports.isBigInt = isBigInt;
exports.externNumber = externNumber;
exports.externRaw = externRaw;
exports.externString = externString;
exports.isEqualNumber = isEqualNumber;
exports.isEqualString = isEqualString;
exports.isInt = isInt;
exports.isString = isString;
exports.isUSEIN = isUSEIN;
exports.matchRegex = matchRegex;
exports.minStringLength = minStringLength;
exports.maxStringLength = maxStringLength;
exports.maxNumber = maxNumber;
exports.minNumber = minNumber;
exports.notEqualNumber = notEqualNumber;
exports.notEqualString = notEqualString;
exports.recursive = recursive;
exports.required = required;
exports.requireArray = requireArray;
exports.JavaScript = JavaScript;
/* falsePromise Not a pure module */

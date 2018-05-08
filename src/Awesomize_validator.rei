type maybe = option(Js.Json.t);

type result = Js.Promise.t(option(string));

type sanitized = Belt.Map.String.t(maybe);

let externArray:
  ((array(Js.Json.t), 'a) => bool, string, maybe, 'a) => result;

let externDependentNumber:
  (
    (float, option(float), sanitized) => Js.Promise.t(bool),
    string,
    string,
    maybe,
    sanitized
  ) =>
  result;

let externDependentRaw:
  (
    (Js.Json.t, option(Js.Json.t), sanitized) => Js.Promise.t(bool),
    string,
    string,
    maybe,
    sanitized
  ) =>
  result;

let externDependentString:
  (
    (string, option(string), sanitized) => Js.Promise.t(bool),
    string,
    string,
    maybe,
    sanitized
  ) =>
  result;

let externNumber: ((float, 'a) => bool, string, maybe, 'a) => result;

let externRaw:
  ((maybe, sanitized) => bool, string, maybe, sanitized) => result;

let externString: ((string, 'a) => bool, string, maybe, 'a) => result;

let isEqualNumber: (float, maybe, sanitized) => result;

let isEqualString: (string, maybe, sanitized) => result;

let isInt: (maybe, sanitized) => result;

let isString: (maybe, sanitized) => result;

let isBigInt: (maybe, sanitized) => result;

let notEqualNumber: (float, maybe, sanitized) => result;

let notEqualString: (string, maybe, sanitized) => result;

let recursive:
  (
    Js.Dict.t(Js.Json.t) => Js.Promise.t([< | `Error('a) | `Ok('b)]),
    maybe,
    'c
  ) =>
  result;

let required: (maybe, sanitized) => result;

let requireArray: (maybe, sanitized) => result;

let externNumber: ((float, 'a) => bool, string, maybe, 'a) => result;

let externRaw:
  ((maybe, sanitized) => bool, string, maybe, sanitized) => result;

let externString: ((string, 'a) => bool, string, maybe, 'a) => result;

let isEqualNumber: (float, maybe, sanitized) => result;

let isEqualString: (string, maybe, sanitized) => result;

let isInt: (maybe, sanitized) => result;

let isString: (maybe, sanitized) => result;

let isUSEIN: (maybe, sanitized) => result;

let matchRegex: (Js.Re.t, maybe, sanitized) => result;

let minStringLength: (int, maybe, sanitized) => result;

let maxStringLength: (int, maybe, sanitized) => result;

let maxNumber: (float, maybe, sanitized) => result;

let minNumber: (float, maybe, sanitized) => result;

let notEqualNumber: (float, maybe, sanitized) => result;

let notEqualString: (string, maybe, sanitized) => result;

let recursive:
  (
    Js.Dict.t(Js.Json.t) => Js.Promise.t([< | `Error('a) | `Ok('b)]),
    maybe,
    'c
  ) =>
  result;

let required: (maybe, sanitized) => result;

let requireArray: (maybe, sanitized) => result;

module JavaScript: {
  module Promise: {
    let extern:
      (. (Js.Json.t, Js.Json.t, sanitized) => Js.Promise.t(bool), string) =>
      (. maybe, sanitized) => result;
    let externDependent:
      (
        . (Js.Json.t, Js.Nullable.t(Js.Json.t), Js.Json.t) =>
          Js.Promise.t(bool),
        string,
        string
      ) =>
      (. maybe, sanitized) => result;
  };
  let extern:
    (. (Js.Json.t, Js.Json.t, sanitized) => bool, string) =>
    (. maybe, sanitized) => result;
  let externDependent:
    (
      . (Js.Json.t, Js.Nullable.t(Js.Json.t), Js.Json.t) => bool,
      string,
      string
    ) =>
    (. maybe, sanitized) => result;
};

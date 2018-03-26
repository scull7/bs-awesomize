type maybe = option(Js.Json.t);

type parsed = Belt.Map.String.t(option(Js.Json.t));

type sanitized = Belt.Map.String.t(maybe);

type read = Js.Dict.t(Js.Json.t) => Js.Promise.t(option(Js.Json.t));

type sanitize = (maybe, parsed) => Js.Promise.t(maybe);

type validate = (maybe, sanitized) => Js.Promise.t(option(string));

type normalize = (maybe, sanitized) => Js.Promise.t(maybe);

type definition = {
  read,
  sanitize: option(sanitize),
  validate: list(validate),
  normalize: option(normalize),
};

type definitionMap = Belt.Map.String.t(definition);

type errorMap = Belt.Map.String.t(option(string));

type resultMap = Belt.Map.String.t(Js.Json.t);

type schema = Js.Array.t((string, definition));

let make:
  (schema, Js.Dict.t(Js.Json.t)) =>
  Js.Promise.t([> | `Error(errorMap) | `Ok(resultMap)]);

module Read = Awesomize_read;

module Result = Awesomize_result;

module Normalizer = Awesomize_data_scrub;

module Sanitizer = Awesomize_data_scrub;

module Validator = Awesomize_validator;

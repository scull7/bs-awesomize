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

let unwrap =
  fun
  | None => None
  | Some(None) => None
  | Some(Some(x)) => Some(x);

module Compiler = {
  let make = (generator, definitionMap) => {
    let step =
      Belt.Map.String.mapWithKey(definitionMap, generator)
      |> Belt.Map.String.valuesToArray;
    input =>
      Belt_Array.map(step, fn => fn(input))
      |> Js.Promise.all
      |> Js.Promise.then_(res =>
           Belt.Map.String.ofArray(res) |> Js.Promise.resolve
         );
  };
  let resolver = (key, value) => (key, value) |> Js.Promise.resolve;
  let actionOrDefault = (parser, key, definition) =>
    switch (parser(definition)) {
    | None => ((maybe, _) => resolver(key, maybe))
    | Some(fn) => (
        (maybe, map) => fn(maybe, map) |> Js.Promise.then_(resolver(key))
      )
    };
};

module Reader = {
  let compiler =
    Compiler.make((key, definition, input) =>
      definition.read(input) |> Js.Promise.then_(Compiler.resolver(key))
    );
};

module Sanitize = {
  let parser = definition => definition.sanitize;
  let compiler =
    Compiler.make((key, definition) => {
      let sanitizer = Compiler.actionOrDefault(parser, key, definition);
      parsed =>
        Belt.Map.String.get(parsed, key) |> unwrap |> sanitizer(_, parsed);
    });
};

module Normalize = {
  let parser = definition => definition.normalize;
  let compiler =
    Compiler.make((key, definition) => {
      let normalizer = Compiler.actionOrDefault(parser, key, definition);
      parsed =>
        Belt.Map.String.get(parsed, key) |> unwrap |> normalizer(_, parsed);
    });
};

module Validate = {
  let iterate = (key, test, input, next) =>
    Belt.Map.String.get(input, key)
    |> unwrap
    |> test(_, input)
    |> Js.Promise.then_(
         fun
         | None => next(input)
         | Some(message) => `Invalid(message) |> Compiler.resolver(key),
       );
  let rec run = (key, listOfValidations, input) =>
    switch (listOfValidations) {
    | [] => `Valid |> Compiler.resolver(key)
    | [test] => iterate(key, test, input, run(key, []))
    | [test, ...rest] => iterate(key, test, input, run(key, rest))
    };
  let compiler =
    ((key, definition) => run(key, definition.validate)) |> Compiler.make;
  module Response = {
    let hasError = res =>
      Belt.Map.String.some(res, (_, v) =>
        switch (v) {
        | `Invalid(_) => true
        | `Valid => false
        }
      );
    let toErrorMap = res =>
      Belt.Map.String.map(
        res,
        fun
        | `Invalid(msg) => Some(msg)
        | `Valid => None,
      );
  };
};

module Awesomize = {
  open Js.Promise;
  let make = array => {
    let definitionMap = Belt.Map.String.ofArray(array);
    let read = Reader.compiler(definitionMap);
    let sanitize = Sanitize.compiler(definitionMap);
    let validate = Validate.compiler(definitionMap);
    let normalize = Normalize.compiler(definitionMap);
    let resolveError = validated =>
      validated
      |> Validate.Response.toErrorMap
      |> (map => `Error(map) |> resolve);
    let unwrapMap = m1 =>
      Belt.Map.String.(
        reduce(m1, empty, (m2, key, value) =>
          switch (value) {
          | None => m2
          | Some(v) => set(m2, key, v)
          }
        )
      );
    let resolveOk = sanitized =>
      normalize(sanitized)
      |> then_(normalized =>
           normalized |> unwrapMap |> (x => `Ok(x) |> resolve)
         );
    input =>
      read(input)
      |> then_(sanitize)
      |> then_(sanitized =>
           validate(sanitized)
           |> then_(validated => resolve((validated, sanitized)))
         )
      |> then_(((validated, sanitized)) =>
           validated |> Validate.Response.hasError ?
             resolveError(validated) : resolveOk(sanitized)
         );
  };
};

let make = Awesomize.make;

module Read = Awesomize_read;

module Result = Awesomize_result;

module Normalizer = Awesomize_data_scrub;

module Sanitizer = Awesomize_data_scrub;

module Validator = Awesomize_validator;

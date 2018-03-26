type maybe = option(Js.Json.t);
type parsed = Belt.Map.String.t(option(Js.Json.t));
type sanitized = Belt.Map.String.t(maybe);

type read = (Js.Dict.t(Js.Json.t)) => Js.Promise.t(option(Js.Json.t));
type sanitize = (maybe, parsed) => Js.Promise.t(maybe);
type validate = (maybe, sanitized) => Js.Promise.t(option(string));
type normalize = (maybe, sanitized) => Js.Promise.t(maybe);

type definition = {
  read: read,
  sanitize: option(sanitize),
  validate: list(validate),
  normalize: option(normalize),
};

type definitionMap =
  Belt.Map.String.t(definition);

type errorMap = Belt.Map.String.t(option(string));
type resultMap = Belt.Map.String.t(option(Js.Json.t));
type schema = Js.Array.t((string, definition));

let unwrap = fun
  | None => None
  | Some(None) => None
  | Some(Some(x)) => Some(x);

module Compiler = {
  let make = (generator, definitionMap) => {
   let step =
    Belt_MapString.mapWithKey(definitionMap, generator)
    |> Belt_MapString.valuesToArray;

    (input) =>
      Belt_Array.map(step, (fn) => fn(input))
      |> Js.Promise.all
      |> Js.Promise.then_(res => Belt_MapString.ofArray(res) |> Js.Promise.resolve);
 };

 let resolver = (key, value) => (key, value) |> Js.Promise.resolve;

 let actionOrDefault = (parser, key, definition) =>
   switch(parser(definition)) {
   | None => (maybe, _) => resolver(key, maybe)
   | Some(fn) => (maybe, map) => fn(maybe, map) |> Js.Promise.then_(resolver(key))
   };
};

module Read = {
  let compiler = Compiler.make((key, definition) => {
    (input) =>
      definition.read(input)
      |> Js.Promise.then_(Compiler.resolver(key));
  });
};

module Sanitize = {
  let parser = (definition) => definition.sanitize;
  let compiler = Compiler.make((key, definition) => {
    let sanitizer = Compiler.actionOrDefault(parser, key, definition);

    (parsed) =>
      Belt.Map.String.get(parsed, key)
      |> unwrap
      |> sanitizer(_, parsed);
  });
};

module Normalize = {
  let parser = (definition) => definition.normalize;
  let compiler = Compiler.make((key, definition) => {
    let normalizer = Compiler.actionOrDefault(parser, key, definition);

    (parsed) =>
      Belt.Map.String.get(parsed, key)
      |> unwrap
      |> normalizer(_, parsed);
  })
};

module Validate = {
  let iterate = (key, test, input, next) =>
    Belt_MapString.get(input, key)
    |> unwrap
    |> test(_, input)
    |> Js.Promise.then_(fun
    | None => next(input)
    | Some(message) => `Invalid(message) |> Compiler.resolver(key)
    );

  let rec run = (key, listOfValidations, input) =>
    switch (listOfValidations) {
    | [] => `Valid |> Compiler.resolver(key)
    | [test] => iterate(key, test, input, run(key, []))
    | [test, ...rest] => iterate(key, test, input, run(key, rest))
    };

  let compiler =
    ((key, definition) => run(key, definition.validate))
    |> Compiler.make;

  module Response = {
    let hasError = (res) =>
      Belt_MapString.some(res, (_, v) =>
        switch (v) {
        | `Invalid(_) => true
        | `Valid => false
        }
      );

    let toErrorMap = (res) =>
      Belt_MapString.map(res, fun
      | `Invalid(msg) => Some(msg)
      | `Valid => None
      );
  };
};

module Awesomize = {
  open Js.Promise;

  let make = (array) => {
    let definitionMap = Belt.Map.String.ofArray(array);
    let read = Read.compiler(definitionMap);
    let sanitize = Sanitize.compiler(definitionMap);
    let validate = Validate.compiler(definitionMap);
    let normalize = Normalize.compiler(definitionMap);

    let resolveError = validated =>
      validated
      |> Validate.Response.toErrorMap
      |> (map) => `Error(map)
      |> resolve;

    let resolveOk = sanitized =>
      normalize(sanitized)
      |> then_(normalized =>
        normalized
        |> (x) => `Ok(x)
        |> resolve
      );

    let toMap = json =>
      switch(Js.Json.classify(json)) {
      | Js.Json.JSONObject(obj) => obj
      | _ => failwith("invalid_input")
      };

    (input) =>
      toMap(input)
      |> read
      |> then_(sanitize)
      |> then_(sanitized =>
        validate(sanitized)
        |> then_(validated => resolve((validated, sanitized)))
      )
      |> then_(((validated, sanitized)) =>
        switch (validated |> Validate.Response.hasError) {
        | true => resolveError(validated)
        | false => resolveOk(sanitized)
        }
      )
  };
};

let make = Awesomize.make;

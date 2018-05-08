type maybe = option(Js.Json.t);

type result = Js.Promise.t(option(string));

type sanitized = Belt.Map.String.t(maybe);

let falsePromise = Js.Promise.resolve(false);

let reply = (msg, res) =>
  res
  |> Js.Promise.then_(isOk => Js.Promise.resolve(isOk ? None : Some(msg)));

let mapString = (fn, classified) =>
  (
    switch (classified) {
    | Js.Json.JSONString(str) => fn(str)
    | _ => false
    }
  )
  |> Js.Promise.resolve;

module Compile = {
  let classified = (fn, maybe, sanitized: sanitized) =>
    (
      switch (maybe) {
      | None => None
      | Some(json) => json |> Js.Json.classify |> fn(_, sanitized)
      }
    )
    |> Js.Promise.resolve;
  let numberTest = (fn, maybe, sanitized: sanitized) => {
    let fn2 = (classified, _) =>
      switch (classified) {
      | Js.Json.JSONNumber(n) => fn(n)
      | _ => Some("not_number")
      };
    classified(fn2, maybe, sanitized);
  };
  let stringTest = (fn, maybe, sanitized) => {
    let fn2 = (classified, _) =>
      switch (classified) {
      | Js.Json.JSONString(str) => fn(str)
      | _ => Some("not_string")
      };
    classified(fn2, maybe, sanitized);
  };
};

let required = (maybe, _) =>
  (
    switch (maybe) {
    | None => Some("required")
    | Some(_) => None
    }
  )
  |> Js.Promise.resolve;

let requireArray = (maybe, _) =>
  (
    switch (maybe) {
    | None => Some("required")
    | Some(json) =>
      switch (Js.Json.classify(json)) {
      | Js.Json.JSONArray(_) => None
      | _ => Some("require_array")
      }
    }
  )
  |> Js.Promise.resolve;

let notEqualNumber = x =>
  Compile.numberTest(str => str == x ? Some("cannot_be_equal") : None);

let notEqualString = x =>
  Compile.stringTest(str => str == x ? Some("cannot_be_equal") : None);

let isString =
  Compile.classified((classified, _) =>
    switch (classified) {
    | Js.Json.JSONString(_) => None
    | _ => Some("not_string")
    }
  );

let isInt =
  Compile.numberTest(n =>
    Js.Math.ceil_float(n) == n ? None : Some("not_int")
  );

let isBigInt =
  Compile.stringTest(str => {
    let isZero = s => Js.Re.test(s, [%bs.re "/^\\-\\d+|0+$/"]);
    let isBigIntLike = s => Js.Re.test(s, [%bs.re "/^\\d{1,20}$/"]);

    if (String.length(str) > 20) {
      Some("maximum");
    } else if (isZero(str)) {
      Some("minimum");
    } else {
      isBigIntLike(str) ? None : Some("not_big_int");
    }
  });

let isEqualNumber = x =>
  Compile.numberTest(n => n == x ? None : Some("not_equal"));

let isEqualString = x =>
  Compile.stringTest(str => str == x ? None : Some("not_equal"));

let minStringLength = length =>
  Compile.stringTest(str =>
    String.length(str) >= length ? None : Some("min_length")
  );

let maxStringLength = length =>
  Compile.stringTest(str =>
    String.length(str) <= length ? None : Some("max_length")
  );

let maxNumber = max =>
  Compile.numberTest(n => n <= max ? None : Some("maximum"));

let minNumber = min =>
  Compile.numberTest(n => n >= min ? None : Some("minimum"));

let matchRegex = re =>
  Compile.stringTest(str => Js.Re.test(str, re) ? None : Some("not_match"));

let externRaw = (fn, msg, maybe, sanitized) =>
  fn(maybe, sanitized) |> Js.Promise.resolve |> reply(msg);

let extern = (fn, msg, maybe, sanitized) =>
  switch (maybe) {
  | None => None |> Js.Promise.resolve
  | Some(json) => json |> Js.Json.classify |> fn(_, sanitized) |> reply(msg)
  };

let externCompiler = (fn, msg, maybe, sanitized) =>
  extern((classified, _) => fn(classified), msg, maybe, sanitized);

let externString = (fn, msg, maybe, sanitized) => {
  let handler = mapString(str => fn(str, sanitized));
  externCompiler(handler, msg, maybe, sanitized);
};

let externNumber = (fn, msg, maybe, sanitized) => {
  let handler =
    fun
    | Js.Json.JSONNumber(n) => fn(n, sanitized) |> Js.Promise.resolve
    | _ => falsePromise;
  externCompiler(handler, msg, maybe, sanitized);
};

let externArray = (fn, msg, maybe, sanitized) => {
  let handler =
    fun
    | Js.Json.JSONArray(arr) => fn(arr, sanitized) |> Js.Promise.resolve
    | _ => falsePromise;
  externCompiler(handler, msg, maybe, sanitized);
};

let externDependentFactory = (decoder, fn, key, msg, maybe, sanitized) => {
  let handler = v =>
    Belt.Map.String.get(sanitized, key)
    |> Belt.Option.flatMap(_, Belt.Option.flatMap(_, decoder))
    |> fn(v, _, sanitized);
  Belt.Option.flatMap(maybe, decoder)
  |> Belt.Option.mapWithDefault(_, falsePromise, handler)
  |> reply(msg);
};

let externDependentRaw = (fn, key, msg, maybe, sanitized) =>
  switch (maybe) {
  | None => None |> Js.Promise.resolve
  | Some(json) =>
    Belt.Map.String.get(sanitized, key)
    |> Belt.Option.flatMap(_, x => x)
    |> fn(json, _, sanitized)
    |> reply(msg)
  };

let externDependentNumber = (fn, key, msg) =>
  externDependentFactory(Js.Json.decodeNumber, fn, key, msg);

let externDependentString = (fn, key, msg) =>
  externDependentFactory(Js.Json.decodeString, fn, key, msg);

let recursive = validator => {
  let rec awesomizer = jsonList =>
    switch (jsonList) {
    | [] => Js.Promise.resolve(true)
    | [x, ...xs] =>
      switch (x |> Js.Json.classify) {
      | Js.Json.JSONObject(dict) =>
        validator(dict)
        |> Js.Promise.then_(result =>
             switch (result) {
             | `Error(_) => falsePromise
             | `Ok(_) => awesomizer(xs)
             }
           )
      | _ => falsePromise
      }
    };
  extern(
    (taggedJson, _) =>
      switch (taggedJson) {
      | Js.Json.JSONArray(a) => a |> Belt_List.fromArray |> awesomizer
      | _ => falsePromise
      },
    "invalid_scope",
  );
};

module JavaScript = {
  [@bs.scope "Promise"] [@bs.val]
  external jsResolve : 'a => Js.Promise.t('a) = "resolve";
  /*
   let extractDependentValue =
     fun
     | None => Js.Nullable.null
     | Some(v) => Js.Nullable.fromOption(v);
     */
  let extractDependentValue = maybe =>
    Belt.Option.mapWithDefault(
      maybe,
      Js.Nullable.null,
      Js.Nullable.fromOption,
    );
  let nullOrValue =
    fun
    | None => Js.Json.null
    | Some(v) => v;
  module Promise = {
    let extern =
      (. fn, msg) =>
        (. maybe, sanitized) => {
          let jsonSanitized =
            Belt.Map.String.map(sanitized, nullOrValue)
            |> Belt.Map.String.toArray
            |> Js.Dict.fromArray
            |> Js.Json.object_;
          Belt.Option.map(maybe, json => fn(json, jsonSanitized, sanitized))
          |> Belt.Option.getWithDefault(_, Js.Promise.resolve(true))
          |> reply(msg);
        };
    let externDependent =
      (. fn, key, msg) => {
        let executor = (json, jsonSanitized, sanitized) =>
          Belt.Map.String.get(sanitized, key)
          |> extractDependentValue
          |> fn(json, _, jsonSanitized);
        extern(. executor, msg);
      };
  };
  let extern =
    (. fn, msg) =>
      Promise.extern(. (a, b, c) => fn(a, b, c) |> jsResolve, msg);
  let externDependent =
    (. fn, key, msg) =>
      Promise.externDependent(.
        (a, b, c) => fn(a, b, c) |> jsResolve,
        key,
        msg,
      );
};

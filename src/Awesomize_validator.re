type maybe = option(Js.Json.t);

type sanitized = Belt.Map.String.t(maybe);

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

let isEqualNumber = x =>
  Compile.numberTest(n => n == x ? None : Some("not_equal"));

let isEqualString = x =>
  Compile.stringTest(str => str == x ? None : Some("not_equal"));

let externRaw = (fn, msg, maybe, sanitized) =>
  fn(maybe, sanitized)
  |> Js.Promise.resolve
  |> Js.Promise.then_(isOk =>
       (
         switch (isOk) {
         | false => Some(msg)
         | true => None
         }
       )
       |> Js.Promise.resolve
     );

let extern = (fn, msg, maybe, sanitized) =>
  switch (maybe) {
  | None => None |> Js.Promise.resolve
  | Some(json) =>
    json
    |> Js.Json.classify
    |> fn(_, sanitized)
    |> Js.Promise.then_(isOk => Js.Promise.resolve(isOk ? None : Some(msg)))
  };

let externCompiler = (fn, msg, maybe, sanitized) =>
  extern((classified, _) => fn(classified), msg, maybe, sanitized);

let externString = (fn, msg, maybe, sanitized) => {
  let handler =
    fun
    | Js.Json.JSONString(str) => fn(str, sanitized) |> Js.Promise.resolve
    | _ => Js.Promise.resolve(false);
  externCompiler(handler, msg, maybe, sanitized);
};

let externNumber = (fn, msg, maybe, sanitized) => {
  let handler =
    fun
    | Js.Json.JSONNumber(n) => fn(n, sanitized) |> Js.Promise.resolve
    | _ => Js.Promise.resolve(false);
  externCompiler(handler, msg, maybe, sanitized);
};

let externArray = (fn, msg, maybe, sanitized) => {
  let handler =
    fun
    | Js.Json.JSONArray(arr) => fn(arr, sanitized) |> Js.Promise.resolve
    | _ => Js.Promise.resolve(false);
  externCompiler(handler, msg, maybe, sanitized);
};

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
             | Result.Error(_) => Js.Promise.resolve(false)
             | Result.Ok(_) => awesomizer(xs)
             }
           )
      | _ => Js.Promise.resolve(false)
      }
    };
  extern(
    (taggedJson, _) =>
      switch (taggedJson) {
      | Js.Json.JSONArray(a) => a |> Belt_List.fromArray |> awesomizer
      | _ => Js.Promise.resolve(false)
      },
    "invalid_scope",
  );
};

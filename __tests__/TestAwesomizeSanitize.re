open Jest;

let parseNumber = x =>
  switch (x |> Js.Json.classify) {
  | Js.Json.JSONNumber(y) => y
  | _ => failwith("invalid_json")
  };

let parseString = x =>
  switch (x |> Js.Json.classify) {
  | Js.Json.JSONString(y) => y
  | _ => failwith("invalid_json")
  };

describe("Awesomize Sanitize", () => {
  let schema =
    Awesomize.make([|
      (
        "test",
        {
          read: Awesomize.Read.key("test"),
          sanitize:
            Awesomize.Sanitizer.makeClassified(
              fun
              | Js.Json.JSONString(_) => Some(Js.Json.number(42.0))
              | Js.Json.JSONNumber(n) => Some(Js.Json.number(n))
              | _ => None,
            ),
          validate: [Awesomize.Validator.required],
          normalize: None,
        },
      ),
    |]);
  let schema2 =
    Awesomize.make([|
      (
        "test2",
        {
          read: Awesomize.Read.key("test"),
          sanitize:
            Awesomize.Sanitizer.makeClassified(
              fun
              | Js.Json.JSONString(_) => Some(Js.Json.string("crazy"))
              | _ => None,
            ),
          validate: [Awesomize.Validator.required],
          normalize: None,
        },
      ),
    |]);
  testPromise("Sanitize (string)", () =>
    schema2(
      [|("test", Js.Json.string("crazy_thing"))|] |> Js.Dict.fromArray,
    )
    |> Awesomize.Result.fold(
         err => {
           Js.log2("Awesome Sanitize:1 - ", err);
           "fail_err";
         },
         res =>
           switch (Belt.Map.String.get(res, "test2")) {
           | None => "fail"
           | Some(x) => parseString(x)
           },
       )
    |> Js.Promise.then_(result =>
         Expect.expect(result) |> Expect.toBe("crazy") |> Js.Promise.resolve
       )
  );
  testPromise("Sanitize should run when the value exists", () =>
    schema([|("test", Js.Json.string("moo"))|] |> Js.Dict.fromArray)
    |> Awesomize.Result.fold(
         err => {
           Js.log2("Awesome Sanitize:1 - ", err);
           (-1.0);
         },
         res =>
           switch (Belt.Map.String.get(res, "test")) {
           | None => (-2.0)
           | Some(x) => parseNumber(x)
           },
       )
    |> Js.Promise.then_(result =>
         Expect.expect(result) |> Expect.toBe(42.0) |> Js.Promise.resolve
       )
  );
  testPromise("Sanitize should not run with the value is None", () => {
    let schema =
      Awesomize.make([|
        (
          "test",
          {
            read: Awesomize.Read.key("test"),
            sanitize:
              Awesomize.Sanitizer.makeClassified(
                fun
                | _ => failwith("should not run"),
              ),
            validate: [Awesomize.Validator.isString],
            normalize: None,
          },
        ),
      |]);
    schema([||] |> Js.Dict.fromArray)
    |> Awesomize.Result.fold(
         err => {
           Js.log2("Awesomize Sanitize:2 - ", err);
           (-1.0);
         },
         _res => 42.0,
       )
    |> Js.Promise.then_(result =>
         Expect.expect(result) |> Expect.toBe(42.0) |> Js.Promise.resolve
       );
  });
});

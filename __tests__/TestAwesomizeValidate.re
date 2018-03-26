open Jest;

let parseNumber = x =>
  switch (x |> Js.Json.classify) {
  | Js.Json.JSONNumber(y) => y
  | _ => failwith("invalid_json")
  };

let parseString = x =>
  switch (x |> Js.Json.classify) {
  | Js.Json.JSONString(s) => s
  | _ => failwith("invalid_json")
  };

describe("Awesomize Validation", () => {
  testPromise("Required Field", () => {
    let schema =
      Awesomize.make([|
        (
          "test",
          {
            read: Awesomize.Read.key("test"),
            sanitize: None,
            validate: [Awesomize.Validator.required],
            normalize: None,
          },
        ),
      |]);
    let input = [|("test", Js.Json.string("thing"))|] |> Js.Dict.fromArray;
    schema(input)
    |> Js.Promise.then_(result =>
         (
           switch (result) {
           | `Ok(res) =>
             switch (Belt.Map.String.get(res, "test")) {
             | None => "missing"
             | Some(x) => parseString(x)
             }
           | `Error(err) =>
             Js.log2("ErrorMap: ", err);
             "fail";
           }
         )
         |> Js.Promise.resolve
       )
    |> Js.Promise.then_(result =>
         Expect.expect(result) |> Expect.toBe("thing") |> Js.Promise.resolve
       );
  });
  describe("Require Integer", () => {
    let schema =
      Awesomize.make([|
        (
          "test",
          {
            read: Awesomize.Read.key("test"),
            sanitize: None,
            validate: [
              Awesomize.Validator.required,
              Awesomize.Validator.isInt,
            ],
            normalize: None,
          },
        ),
      |]);
    testPromise("Success", () =>
      schema([|("test", Js.Json.number(1.0))|] |> Js.Dict.fromArray)
      |> Awesomize.Result.fold(
           err => {
             Js.log2("Require Integer:Sucesss - failed: ", err);
             (-1.0);
           },
           res =>
             switch (Belt.Map.String.get(res, "test")) {
             | None => (-2.0)
             | Some(x) => parseNumber(x)
             },
         )
      |> Js.Promise.then_(result =>
           Expect.expect(result) |> Expect.toBe(1.0) |> Js.Promise.resolve
         )
    );
    let unexpectedSuccess = res => {
      Js.log2("Require Integer:Failure - unexpected success: ", res);
      "unexpected_success";
    };
    let getMessage = (key, err) =>
      switch (Awesomize.Result.Error.getMessage(key, err)) {
      | None => "missing_message"
      | Some(message) => message
      };
    testPromise("Failure", () =>
      schema([|("test", Js.Json.number(1.1))|] |> Js.Dict.fromArray)
      |> Awesomize.Result.fold(getMessage("test"), unexpectedSuccess)
      |> Js.Promise.then_(message =>
           Expect.expect(message)
           |> Expect.toBe("not_int")
           |> Js.Promise.resolve
         )
    );
    testPromise("Missing", () =>
      schema([||] |> Js.Dict.fromArray)
      |> Awesomize.Result.fold(getMessage("test"), unexpectedSuccess)
      |> Js.Promise.then_(message =>
           Expect.expect(message)
           |> Expect.toBe("required")
           |> Js.Promise.resolve
         )
    );
  });
});

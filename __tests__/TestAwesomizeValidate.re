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
           | Result.Ok(res) =>
             switch (Belt.Map.String.get(res, "test")) {
             | None => "missing"
             | Some(x) => parseString(x)
             }
           | Result.Error(err) =>
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
      |> Result.Promise.fold(
           err => {
             Js.log2("Require Integer:Sucesss - failed: ", err);
             (-1.0) |> Js.Promise.resolve;
           },
           res =>
             switch (Belt.Map.String.get(res, "test")) {
             | None => (-2.0) |> Js.Promise.resolve
             | Some(x) => parseNumber(x) |> Js.Promise.resolve
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
      switch (Belt.Map.String.get(err, key)) {
      | None => "missing_message"
      | Some(None) => "missing_message"
      | Some(Some(message)) => message
      };
    testPromise("Failure", () =>
      schema([|("test", Js.Json.number(1.1))|] |> Js.Dict.fromArray)
      |> Result.Promise.fold(
        err => err |> getMessage("test") |> Js.Promise.resolve,
        res => res |> unexpectedSuccess |> Js.Promise.resolve
      )
      |> Js.Promise.then_(message =>
            Expect.expect(message)
           |> Expect.toBe("not_int")
           |> Js.Promise.resolve
         )
    );
    testPromise("Missing", () =>
      schema([||] |> Js.Dict.fromArray)
      |> Result.Promise.fold(
        res => res |> getMessage("test") |> Js.Promise.resolve,
        err => err |> unexpectedSuccess |> Js.Promise.resolve
      )
      |> Js.Promise.then_(message =>
           Expect.expect(message)
           |> Expect.toBe("required")
           |> Js.Promise.resolve
         )
    );
  });
});

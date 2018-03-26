open Jest;

let parseNumber = x =>
  switch (x |> Js.Json.classify) {
  | Js.Json.JSONNumber(y) => y
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
            Some(
              (maybe, _) =>
                (
                  switch (maybe) {
                  | None => None
                  | Some(json) =>
                    switch (Js.Json.classify(json)) {
                    | Js.Json.JSONString(_) => Some(Js.Json.number(42.0))
                    | Js.Json.JSONNumber(_) => Some(json)
                    | _ => None
                    }
                  }
                )
                |> Js.Promise.resolve,
            ),
          validate: [Awesomize.Validator.required],
          normalize: None,
        },
      ),
    |]);
  testPromise("Sanitize should run when the value exists", () =>
    schema([|("test", Js.Json.string("moo"))|] |> Js.Dict.fromArray)
    |> Js.Promise.then_(result =>
         (
           switch (result) {
           | `Error(err) =>
             Js.log2("Awesome Sanitize:1 - ", err);
             (-1.0);
           | `Ok(res) =>
             switch (Belt.Map.String.get(res, "test")) {
             | None => (-2.0)
             | Some(x) => parseNumber(x)
             }
           }
         )
         |> Js.Promise.resolve
       )
    |> Js.Promise.then_(result =>
         Expect.expect(result) |> Expect.toBe(42.0) |> Js.Promise.resolve
       )
  );
});

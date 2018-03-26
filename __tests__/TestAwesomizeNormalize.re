open Jest;

let parseNumber = x =>
  switch (x |> Js.Json.classify) {
  | Js.Json.JSONNumber(y) => y
  | _ => failwith("invalid_json")
  };

describe("Awesomize Normalize", () => {
  let schema =
    Awesomize.make([|
      (
        "test",
        {
          read: Awesomize.Read.key("test"),
          sanitize:
            Awesomize.Normalizer.makeClassified(
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
  testPromise("Normalize should run when the value exists", () =>
    schema([|("test", Js.Json.string("moo"))|] |> Js.Dict.fromArray)
    |> Awesomize.Result.fold(
         err => {
           Js.log2("Awesome Normalize:1 - ", err);
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
});

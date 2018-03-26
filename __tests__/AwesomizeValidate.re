open Jest;

describe("Awesomize Validation", () => {
  testPromise("Required Field", () => {
    let schema = Awesomize.make([|
      ("test", {
        read: (input) => Js.Dict.get(input, "test") |> Js.Promise.resolve,
        sanitize: None,
        validate: [ AwesomizeValidator.required ],
        normalize: None
      })
    |]);

    let input =
      [| ("test", Js.Json.string("thing")) |]
      |> Js.Dict.fromArray
      |> Js.Json.object_;

    schema(input)
    |> Js.Promise.then_(result => {
      switch(result) {
      | `Ok(res) => {
          switch(Belt.Map.String.get(res, "test")) {
          | None => "missing"
          | Some(None) => "missing.missing"
          | Some(Some(x)) =>
              switch(Js.Json.classify(x)) {
              | Js.Json.JSONString(str) => str
              | _ => "invalid_json"
              }
          }
        }
      | `Error(err) => {
          Js.log2("ErrorMap: ", err);
          "fail"
        }
      }
      |> Js.Promise.resolve;
    })
    |> Js.Promise.then_(result => {
      Expect.expect(result)
      |> Expect.toBe("thing")
      |> Js.Promise.resolve
    });
  });
});

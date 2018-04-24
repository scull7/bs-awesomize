open Jest;

describe("Awesomize", () => {
  let schema =
    Awesomize.make([|
      (
        "foo",
        {
          read: Awesomize.Read.key("foo"),
          sanitize: None,
          validate: [Awesomize.Validator.required],
          normalize: None,
        },
      ),
      (
        "bar",
        {
          read: Awesomize.Read.key("bar"),
          sanitize: None,
          validate: [Awesomize.Validator.required],
          normalize: None,
        },
      ),
    |]);
  test(
    "Should throw a compilation exception when passing an empty validation list",
    () => {
    let compile = () =>
      Awesomize.make([|
        (
          "bad",
          {
            read: Awesomize.Read.key("bad"),
            sanitize: None,
            validate: [],
            normalize: None,
          },
        ),
      |]);
    let message = "You must provide at least one validator for key: bad";
    Expect.expect(compile) |> Expect.toThrowMessage(message);
  });
  testPromise("Should return an error for the missing bar element", () => {
    let input = [|("foo", Js.Json.string("thing"))|] |> Js.Dict.fromArray;
    schema(input)
    |> Result.Promise.fold(
         err => [|
           Belt.Map.String.get(err, "foo"),
           Belt.Map.String.get(err, "bar"),
         |] |> Js.Promise.resolve,
         _res => failwith("unexpected_success"),
       )
    |> Js.Promise.then_(result =>
         Expect.expect(result)
         |> Expect.toEqual([|Some(None), Some(Some("required"))|])
         |> Js.Promise.resolve
       );
  });
});

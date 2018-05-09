open Jest;

let toJsonObj = list => Js.Json.object_(Js.Dict.fromList(list));

let data =
  Js.Dict.fromList([
    (
      "foo",
      toJsonObj([("bar", toJsonObj([("cow", Js.Json.string("moo"))]))]),
    ),
    ("bar", toJsonObj([("baz", Js.Json.string("moo"))])),
    ("baz", Js.Json.boolean(true)),
  ]);

describe("Awesomize.Read", () =>
  describe("Awesomize.Read.path", () => {
    testPromise("should read through a multi level object", () =>
      Awesomize.Read.path(["foo", "bar", "cow"], data)
      |> Js.Promise.then_(result =>
           (
             switch (result) {
             | None => fail("unexpected_none_result")
             | Some(json) =>
               switch (json |> Js.Json.classify) {
               | Js.Json.JSONString(actual) =>
                 actual |> Expect.expect |> Expect.toBe("moo")
               | _ => fail("unexpected_json_type")
               }
             }
           )
           |> Js.Promise.resolve
         )
    );
    testPromise(
      "should return None when attempting to access an invalid path", () =>
      Awesomize.Read.path(["bar", "baz", "cow"], data)
      |> Js.Promise.then_(result =>
           (
             switch (result) {
             | None => pass
             | Some(_) => fail("unexpected_json_result")
             }
           )
           |> Js.Promise.resolve
         )
    );
    testPromise(
      "should return None when attempting to access an invalid root node", () =>
      Awesomize.Read.path(["dne", "cow"], data)
      |> Js.Promise.then_(result =>
           (
             switch (result) {
             | None => pass
             | Some(_) => fail("unexpected_json_result")
             }
           )
           |> Js.Promise.resolve
         )
    );
    test("should throw when given an empty list", () => {
      let message = "Path must have at least one item";
      Expect.expect(() =>
        Awesomize.Read.path([], data)
      )
      |> Expect.toThrowMessage(message);
    });
    testPromise(
      "should return None when attempting to access a path on a boolean", () =>
      Awesomize.Read.path(["baz", "buzz"], data)
      |> Js.Promise.then_(result =>
           (
             switch (result) {
             | None => pass
             | Some(_) => fail("unexpected_json_result")
             }
           )
           |> Js.Promise.resolve
         )
    );
  })
);

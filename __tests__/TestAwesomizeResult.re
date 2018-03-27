open Jest;

describe("Awesomize Result", () => {
  describe("Awesomize Error", () => {
    test("getMessage on a non-existant key", () => {
      let actual =
        Awesomize.Result.Error.getMessage("foo", Belt.Map.String.empty);
      let expected = None;
      Expect.expect(actual) |> Expect.toBe(expected);
    });
    test("getMessage on an empty key", () => {
      let result = Belt.Map.String.ofArray([|("test", None)|]);
      let actual = Awesomize.Result.Error.getMessage("test", result);
      let expected = None;
      Expect.expect(actual) |> Expect.toBe(expected);
    });
  });
  describe("Awesomize Result.map", () => {
    let errorProducer = Js.Promise.resolve(`Error("foo"));
    let successProducer = Js.Promise.resolve(`Ok("bar"));
    let transform = str => str ++ "boo";
    testPromise("map over error", () =>
      Awesomize.Result.map(transform, errorProducer)
      |> Js.Promise.then_(result =>
           (
             switch (result) {
             | `Ok(_) => failwith("unexpected_success")
             | `Error(err) => Expect.expect(err) |> Expect.toBe("foo")
             }
           )
           |> Js.Promise.resolve
         )
    );
    testPromise("map over success", () =>
      Awesomize.Result.map(transform, successProducer)
      |> Js.Promise.then_(result =>
           (
             switch (result) {
             | `Error(_) => failwith("unexpected_error")
             | `Ok(str) => Expect.expect(str) |> Expect.toBe("barboo")
             }
           )
           |> Js.Promise.resolve
         )
    );
  });
  describe("Awesomize Result.bimap", () => {
    let errorProducer = Js.Promise.resolve(`Error("foo"));
    let successProducer = Js.Promise.resolve(`Ok("bar"));
    let left = str => str ++ "moo";
    let right = str => str ++ "cow";
    testPromise("bimap over error", () =>
      Awesomize.Result.bimap(left, right, errorProducer)
      |> Js.Promise.then_(result =>
           (
             switch (result) {
             | `Ok(str) => failwith(str)
             | `Error(str) => Expect.expect(str) |> Expect.toBe("foomoo")
             }
           )
           |> Js.Promise.resolve
         )
    );
    testPromise("bimap over success", () =>
      Awesomize.Result.bimap(left, right, successProducer)
      |> Js.Promise.then_(result =>
           (
             switch (result) {
             | `Error(str) => failwith(str)
             | `Ok(str) => Expect.expect(str) |> Expect.toBe("barcow")
             }
           )
           |> Js.Promise.resolve
         )
    );
  });
});

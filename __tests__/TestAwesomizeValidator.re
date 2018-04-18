open Jest;

let empty = Belt.Map.String.empty;

let maybeString = str => Some(str |> Js.Json.string);

let maybeNumber = n => Some(n |> Js.Json.number);

let expectFail = (title, testFn, expectedMessage) =>
  testPromise(title, () =>
    testFn()
    |> Js.Promise.then_(result =>
         (
           switch (result) {
           | None => fail("should not pass validation")
           | Some(message) =>
             Expect.expect(message) |> Expect.toBe(expectedMessage)
           }
         )
         |> Js.Promise.resolve
       )
  );

let expectPass = (title, testFn) =>
  testPromise(title, () =>
    testFn()
    |> Js.Promise.then_(result =>
         (
           switch (result) {
           | None => pass
           | Some(message) => fail(message)
           }
         )
         |> Js.Promise.resolve
       )
  );

describe("Awesomize Validator", () => {
  describe("externString", () => {
    let isMoo = (str, _) => str == "moo";
    let extern = Awesomize.Validator.externString(isMoo, "not_moo");
    expectFail(
      "should fail when the given function returns false",
      () => extern(maybeString("not_moo"), empty),
      "not_moo",
    );
    expectFail(
      "should fail when given a non-string value",
      () => extern(maybeNumber(42.0), empty),
      "not_moo",
    );
    expectPass("should pass when given a None", () => extern(None, empty));
    expectPass("should pass when the given function returns true", () =>
      extern(maybeString("moo"), empty)
    );
  });
  describe("externNumber", () => {
    let is7 = (n, _) => n == 7.0;
    let extern = Awesomize.Validator.externNumber(is7, "not_seven");
    expectFail(
      "should fail when the given function returns false",
      () => extern(maybeNumber(8.0), empty),
      "not_seven",
    );
    expectFail(
      "should fail when given a non-number input",
      () => extern(maybeString("fail"), empty),
      "not_seven",
    );
    expectPass("should pass when the given function returns true", () =>
      extern(maybeNumber(7.0), empty)
    );
  });
  describe("externArray", () => {
    let has42 = (a, _) =>
      Belt_Array.some(a, x =>
        switch (Js.Json.classify(x)) {
        | Js.Json.JSONNumber(n) => n == 42.0
        | _ => false
        }
      );
    let extern = Awesomize.Validator.externArray(has42, "missing_42");
    let goodArray = Some(Js.Json.array([|Js.Json.number(42.0)|]));
    let badArray = Some(Js.Json.array([||]));
    expectFail(
      "should fail when the given function returns false",
      () => extern(badArray, empty),
      "missing_42",
    );
    expectFail(
      "should fail when a non array is given",
      () => extern(maybeNumber(42.0), empty),
      "missing_42",
    );
    expectPass("should pass when the given function returns true", () =>
      extern(goodArray, empty)
    );
  });
  describe("externRaw", () => {
    let isFoo = (maybe, _) =>
      switch (maybe) {
      | None => true
      | Some(json) =>
        switch (json |> Js.Json.classify) {
        | Js.Json.JSONString(str) => str == "foo"
        | _ => false
        }
      };
    let extern = Awesomize.Validator.externRaw(isFoo, "failed");
    expectFail(
      "should fail when the given function return false",
      () => extern(maybeString("not_foo"), empty),
      "failed",
    );
    expectPass("should pass when the given function returns true", () =>
      extern(maybeString("foo"), empty)
    );
  });
  describe("required", () => {
    expectFail(
      "should fail when the value is not present",
      () => Awesomize.Validator.required(None, empty),
      "required",
    );
    expectPass("should pass when the value is present", () =>
      Awesomize.Validator.required(maybeString("foo"), empty)
    );
  });
  describe("requireArray", () => {
    expectFail(
      "should fail when the value is not an array",
      () => Awesomize.Validator.requireArray(maybeString("moo"), empty),
      "require_array",
    );
    expectFail(
      "should fail when given None",
      () => Awesomize.Validator.requireArray(None, empty),
      "required",
    );
    expectFail(
      "should fail when given a boolean",
      () =>
        Awesomize.Validator.requireArray(
          Some(Js.Json.boolean(Js.true_)),
          empty,
        ),
      "require_array",
    );
    expectPass("should pass when given an array", () =>
      Awesomize.Validator.requireArray(Some(Js.Json.array([||])), empty)
    );
  });
  describe("isEqualString", () => {
    let isFoo = Awesomize.Validator.isEqualString("foo");
    expectFail(
      "should fail when given non equal strings",
      () => isFoo(maybeString("bar"), empty),
      "not_equal",
    );
    expectFail(
      "should fail when given a number",
      () => isFoo(maybeNumber(42.0), empty),
      "not_string",
    );
    expectPass("should pass when given equal strings", () =>
      isFoo(maybeString("foo"), empty)
    );
  });
  describe("notEqualString", () => {
    let notThing = Awesomize.Validator.notEqualString("thing");
    expectFail(
      "should fail when given an equal string",
      () => notThing(maybeString("thing"), empty),
      "cannot_be_equal",
    );
    expectFail(
      "should fail when given a boolean",
      () => notThing(Some(Js.Json.boolean(Js.true_)), empty),
      "not_string",
    );
    expectFail(
      "should fail when given a number",
      () => notThing(maybeNumber(42.0), empty),
      "not_string",
    );
    expectPass("should pass when given a non equal string", () =>
      notThing(maybeString("thing2"), empty)
    );
  });
  describe("isEqualNumber", () => {
    let is42 = Awesomize.Validator.isEqualNumber(42.0);
    expectFail(
      "should fail when given a non equal number",
      () => is42(maybeNumber(7.0), empty),
      "not_equal",
    );
    expectFail(
      "should fail when given a non-numeric value",
      () => is42(maybeString("NaN"), empty),
      "not_number",
    );
    expectPass("should pass when given an equal number", () =>
      is42(maybeNumber(42.0), empty)
    );
  });
  describe("notEqualNumber", () => {
    let not42 = Awesomize.Validator.notEqualNumber(42.0);
    expectFail(
      "should fail when given an equal number",
      () => not42(maybeNumber(42.0), empty),
      "cannot_be_equal",
    );
    expectFail(
      "should fail when given a boolean",
      () => not42(Some(Js.Json.boolean(Js.true_)), empty),
      "not_number",
    );
    expectFail(
      "should fail when given a non-numeric value",
      () => not42(maybeString("NaN"), empty),
      "not_number",
    );
    expectPass("should pass when given None", () => not42(None, empty));
    expectPass("should pass when given a non equal number", () =>
      not42(maybeNumber(43.0), empty)
    );
  });
  describe("isInt", () => {
    expectFail(
      "should fail when given a float",
      () => Awesomize.Validator.isInt(maybeNumber(42.7), empty),
      "not_int",
    );
    expectFail(
      "should fail when given a string",
      () => Awesomize.Validator.isInt(maybeString("moo"), empty),
      "not_number",
    );
    expectPass("should pass when given an integer", () =>
      Awesomize.Validator.isInt(maybeNumber(42.0), empty)
    );
  });
  describe("isString", () => {
    expectFail(
      "should fail when given a float",
      () => Awesomize.Validator.isString(maybeNumber(42.0), empty),
      "not_string",
    );
    let failBool = Some(Js.Json.boolean(Js.true_));
    expectFail(
      "should fail when giving a boolean",
      () => Awesomize.Validator.isString(failBool, empty),
      "not_string",
    );
    expectPass("should pass when given a string", () =>
      Awesomize.Validator.isString(maybeString("moo"), empty)
    );
  });
  describe("recursive", () => {
    let schema =
      Awesomize.make([|
        (
          "valid",
          {
            read: Awesomize.Read.key("valid"),
            sanitize: None,
            validate: [
              Awesomize.Validator.required,
              Awesomize.Validator.isString,
            ],
            normalize: None,
          },
        ),
      |]);
    let constructJson = input =>
      input
      |> Js.Dict.fromList
      |> (x => [|x|] |> Js.Json.objectArray |> (x => Some(x)));
    testPromise(
      "should pass when everything in the schema maps to the input", () => {
      let input = [("valid", Js.Json.string("thing"))];
      Awesomize.Validator.recursive(schema, constructJson(input), empty)
      |> Js.Promise.then_(result =>
           (
             switch (result) {
             | Some(msg) => fail(msg)
             | None => pass
             }
           )
           |> Js.Promise.resolve
         );
    });
    testPromise(
      "should fail when something in the schema does not map to the input", () => {
      let input = [("invalid", Js.Json.string("thing"))];
      Awesomize.Validator.recursive(schema, constructJson(input), empty)
      |> Js.Promise.then_(result =>
           (
             switch (result) {
             | Some(msg) =>
               Expect.toBe("invalid_scope") @@ Expect.expect(msg)
             | None => fail("should not pass validation")
             }
           )
           |> Js.Promise.resolve
         );
    });
  });
});

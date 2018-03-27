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
  describe("externRaw", () => {
    let isFoo = (maybe, _) => switch(maybe) {
      | None => true
      | Some(json) =>
        switch(json |> Js.Json.classify) {
          | Js.Json.JSONString(str) => str == "foo"
          | _ => false
        }
    };
    let extern = Awesomize.Validator.externRaw(isFoo, "failed");
    expectFail(
      "should fail when the given function return false",
      () => extern(maybeString("not_foo"), empty),
      "failed"
    );
    expectPass(
      "should pass when the given function returns true",
      () => extern(maybeString("foo"), empty)
    );
  });
  describe("required", () => {
    expectFail(
      "should fail when the value is not present",
      () => Awesomize.Validator.required(None, empty),
      "required"
    );
    expectPass(
      "should pass when the value is present",
      () => Awesomize.Validator.required(maybeString("foo"), empty)
    );
  });
  describe("requireArray", () => {
    expectFail(
      "should fail when the value is not an array",
      () => Awesomize.Validator.requireArray(maybeString("moo"), empty),
      "require_array"
    );
    expectPass(
      "should pass when given an array",
      () => Awesomize.Validator.requireArray(Some(Js.Json.array([||])), empty)
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
      "not_string"
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
      "should fail when given a number",
      () => notThing(maybeNumber(42.0), empty),
      "not_string"
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
      "not_number"
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
      "should fail when given a non-numeric value",
      () => not42(maybeString("NaN"), empty),
      "not_number"
    );
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
    expectPass("should pass when given a string", () =>
      Awesomize.Validator.isString(maybeString("moo"), empty)
    );
  });
});

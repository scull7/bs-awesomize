open Jest;

describe("Awesomize Utilities", () => {
  describe("unwrap", () => {
    let unwrap = Awesomize_util.unwrap;
    test("unwrap should return a None given a None", () =>
      unwrap(None) |> Expect.expect |> Expect.toBe(None)
    );
    test("unwrap should return None given a Some(None)", () =>
      unwrap(Some(None)) |> Expect.expect |> Expect.toBe(None)
    );
    test("unwrap should return Some(x) given Some(Some(x))", () =>
      unwrap(Some(Some("foo")))
      |> Expect.expect
      |> Expect.toEqual(Some("foo"))
    );
  });
  ();
});

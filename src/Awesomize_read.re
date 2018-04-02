let key = (key, input) => Js.Dict.get(input, key) |> Js.Promise.resolve;

let rec path = (list, input) =>
  switch (list) {
  | [] => failwith("Path must have at least one item")
  | [key] => Js.Dict.get(input, key) |> Js.Promise.resolve
  | [key, ...xs] =>
    switch (Js.Dict.get(input, key)) {
    | None => None |> Js.Promise.resolve
    | Some(thing) =>
      switch (thing |> Js.Json.classify) {
      | Js.Json.JSONObject(dict) => path(xs, dict)
      | _ => None |> Js.Promise.resolve
      }
    }
  };

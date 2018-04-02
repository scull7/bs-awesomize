let key = (key, input) => Js.Dict.get(input, key) |> Js.Promise.resolve;

let path = (path, input) =>
  Belt_List.reduce(path, input, (found, key) =>
    switch (found) {
    | None => None
    | Some(thing) =>
      switch (thing |> Js.Json.classify) {
      | Js.Json.JSONObject(dict) => Js.Dict.get(dict, key)
      | _ => None
      }
    }
  )
  |> Js.Promise.resolve;

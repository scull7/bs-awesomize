
let key = (key, input) => Js.Dict.get(input, key) |> Js.Promise.resolve;

open Js.Promise;

let map = (fn, result) =>
  result
  |> then_(
       fun
       | `Error(err) => `Error(err) |> resolve
       | `Ok(res) => `Ok(fn(res)) |> resolve,
     );

let bimap = (left, right, result) =>
  result
  |> then_(
       fun
       | `Error(e) => `Error(left(e)) |> resolve
       | `Ok(res) => `Ok(right(res)) |> resolve,
     );

let fold = (left, right, result) =>
  result
  |> then_(
       fun
       | `Error(e) => left(e) |> resolve
       | `Ok(res) => right(res) |> resolve,
     );

module Error = {
  let getMessage = (key, err) =>
    switch (Belt.Map.String.get(err, key)) {
    | None => None
    | Some(None) => None
    | Some(Some(message)) => Some(message)
    };
  let listToJson = errorList =>
    Belt.Map.String.map(
      errorList,
      fun
      | None => Js.Json.null
      | Some(msg) => Js.Json.string(msg),
    )
    |> Belt.Map.String.toArray
    |> Js.Dict.fromArray
    |> Js.Json.object_;
};

let toJs = result =>
  fold(
    err => {
      "awesomeResultType": "Error",
      "data": Js.Nullable.null,
      "messages": Js.Nullable.return(Error.listToJson(err)),
    },
    result => {
      "awesomeResultType": "Ok",
      "data":
        Js.Nullable.return(
          result
          |> Belt.Map.String.toArray
          |> Js.Dict.fromArray
          |> Js.Json.object_,
        ),
      "messages": Js.Nullable.null,
    },
    result,
  );

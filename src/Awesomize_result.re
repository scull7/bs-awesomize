module Error = {
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
  Result.fold(
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

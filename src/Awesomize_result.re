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
};

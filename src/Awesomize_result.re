open Js.Promise;

let map = (fn, result) =>
  switch (result) {
  | `Error(err) => `Error(err)
  | `Ok(res) => `Ok(fn(res))
  };

let bimap = (ok, err, result) =>
  result
  |> then_(
       fun
       | `Error(e) => `Error(err(e)) |> resolve
       | `Ok(res) => `Ok(ok(res)) |> resolve,
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

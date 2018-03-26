let make = fn =>
  Some(
    (maybe, _) =>
      (
        switch (maybe) {
        | None => None
        | Some(json) => fn(json)
        }
      )
      |> Js.Promise.resolve,
  );

let makeClassified = fn => make(json => json |> Js.Json.classify |> fn);

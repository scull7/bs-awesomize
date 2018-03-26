let required = (maybe, _) =>
  (
    switch (maybe) {
    | None => Some("required")
    | Some(_) => None
    }
  )
  |> Js.Promise.resolve;

let requireArray = (maybe, _) =>
  (
    switch (maybe) {
    | None => Some("required")
    | Some(Js.Json.JSONArray(_)) => None
    | Some(_) => Some("require_array")
    }
  )
  |> Js.Promise.resolve;

let notEqualNumber = (x, maybe, _) =>
  (
    switch (maybe) {
    | None => None
    | Some(Js.Json.JSONNumber(number)) =>
      x == number ? Some("cannot_be_equal") : None
    | Some(_) => None
    }
  )
  |> Js.Promise.resolve;

let notEqualString = (x, maybe, _) =>
  (
    switch (maybe) {
    | None => None
    | Some(Js.Json.JSONString(string)) =>
      x == string ? Some("cannot_be_equal") : None
    | Some(_) => None
    }
  )
  |> Js.Promise.resolve;

let notEqual = (x, maybe, sanitized) =>
  (
    switch (Js.Json.classify(x)) {
    | Js.Json.JSONNumber(number) => notEqualNumber(number, maybe, sanitized)
    | Js.Json.JSONString(string) => notEqualString(string, maybe, sanitized)
    | _ => failwith("invalid_arg")
    }
  )
  |> Js.Promise.resolve;

let isString = (maybe, _) =>
  (
    switch (maybe) {
    | None => None
    | Some(Js.Json.JSONString(_)) => None
    | Some(_) => Some("not_string")
    }
  )
  |> Js.Promise.resolve;

let isInt = (maybe, _) =>
  (
    switch (maybe) {
    | None => None
    | Some(x) =>
      switch (Js.Json.classify(x)) {
      | Js.Json.JSONNumber(number) =>
        Js.Math.ceil_float(number) == number ? None : Some("not_int")
      | _ => Some("not_int")
      }
    }
  )
  |> Js.Promise.resolve;

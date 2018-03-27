let unwrap =
  fun
  | None => None
  | Some(None) => None
  | Some(Some(x)) => Some(x);

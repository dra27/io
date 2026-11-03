type%template ('a : k) t =
  | Ok of 'a
  | Eof
[@@deriving sexp, compare, equal] [@@kind k = (value, word)]

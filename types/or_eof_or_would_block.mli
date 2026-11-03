type%template ('a : k) t =
  | Ok of 'a
  | Eof
  | Would_block
[@@kind k = (value, word, word & value)]

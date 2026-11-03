type%template ('a : k) t =
  | Ok of 'a
  | Would_block
[@@kind k = (value, word, word & value)]

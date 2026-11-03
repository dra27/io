open Base

let in_ = Int63.of_int 0b01
let out = Int63.of_int 0x10

include Flags.Make (struct
    let known = [ in_, "in"; out, "out" ]
    let remove_zero_flags = false
    let allow_intersecting = false
    let should_print_error = true
  end)

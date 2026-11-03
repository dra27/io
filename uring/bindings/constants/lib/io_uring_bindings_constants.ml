let[@inline] print () =
  let open struct
    external print
      :  unit#
      -> unit#
      = "caml_no_bytecode_impl" "ocaml_io_uring_constants_print_noalloc"
    [@@noalloc]
  end in
  let #() = print #() in
  ()
;;

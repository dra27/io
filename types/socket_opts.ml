open Base

type t =
  { keep_alive : bool or_null
  ; permit_broadcast : bool or_null
  ; linger : int option or_null
  ; recv_buffer : int or_null
  ; recv_timeout : float or_null
  ; reuse_addr : bool or_null
  ; reuse_port : bool or_null
  ; send_buffer : int or_null
  ; send_timeout : float or_null
  ; nodelay : bool or_null
  }

let default =
  { keep_alive = Null
  ; permit_broadcast = Null
  ; linger = Null
  ; recv_buffer = Null
  ; recv_timeout = Null
  ; reuse_addr = Null
  ; reuse_port = Null
  ; send_buffer = Null
  ; send_timeout = Null
  ; nodelay = This true
  }
;;

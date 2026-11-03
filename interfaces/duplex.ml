open Ox_kernel

type t =
  { readable : Readable.t
  ; writable : Writable.t
  }

let close io #{ readable; writable } =
  Vnit.box (Readable.close io readable);
  Writable.close io writable
;;

let%template use io t ~f =
  (Private_resource_helper.use [@mode p] [@kind value & value]) ~close io t ~f
[@@mode p = (nonportable, portable)]
;;

module Owned = struct
  type unwrapped = t

  type t =
    { readable : Readable.t Capsule.Owned.t
    ; writable : Writable.t Capsule.Owned.t
    }

  let unwrap #{ readable; writable } : unwrapped# =
    #{ readable = Capsule.Owned.unwrap readable
     ; writable = Capsule.Owned.unwrap writable
     }
  ;;

  let close io t = close io (unwrap t) [@nontail]
end

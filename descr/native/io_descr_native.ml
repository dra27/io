open Base
include Io_descr

module Actions = struct
  include Actions

  let to_shutdown_command t =
    let shutdown_actions = (intersect (shutdown_rd + shutdown_wr)) t in
    if is_empty shutdown_actions
    then Null
    else
      This
        (Bool.select
           (shutdown_actions = shutdown_rd)
           Core_unix.SHUTDOWN_RECEIVE
           (Bool.select
              (shutdown_actions = shutdown_wr)
              Core_unix.SHUTDOWN_SEND
              Core_unix.SHUTDOWN_ALL))
  ;;
end

let create roles ~shutdown raw_fd : _ @ unique =
  let open struct
    external of_unix : Core_unix.File_descr.t -> raw_fd @@ stateless = "%identity"
  end in
  create roles ~shutdown (of_unix raw_fd)
;;

let unsafe_get (t @ local) =
  let open struct
    external to_unix : raw_fd -> Core_unix.File_descr.t @@ stateless = "%identity"
  end in
  to_unix (unsafe_get t)
;;

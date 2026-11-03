open Filesystem_types

type t =
  | Never
  | If_missing of File_permissions.t
  | Or_truncate of File_permissions.t
  | Always of File_permissions.t

let[@inline] file_permissions = function
  | Never -> File_permissions.empty
  | If_missing perm | Or_truncate perm | Always perm -> perm
;;

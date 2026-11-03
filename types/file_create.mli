@@ stateless

open Filesystem_types

(** Policy for creating a file when opening it. *)

(** Specifies a policy for creating a file when opening it. *)
type t =
  | Never (** Never create a new file: fail if the file doesn't already exist. *)
  | If_missing of File_permissions.t (** Create a new file if one doesn't exist. *)
  | Or_truncate of File_permissions.t
  (** Create a new file if one doesn't exist, or truncate the existing file if it does. *)
  | Always of File_permissions.t
  (** Always create a new file: fail if the file already exists. *)

(** [file_permissions t] is the explicitly specified file permissions of {!t} or
    {{!File_permissions.empty} [empty]} when {!Never}. *)
val file_permissions : t @ local -> File_permissions.t

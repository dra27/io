open Ox_kernel
open Filesystem_types
include Filesystem_intf

type -'perms t =
  | T :
      'perms ('s : value mod contended).
      { state : 's @@ aliased many unyielding
      ; prim : 's Prim.t @@ aliased contended many unyielding
      }
      -> 'perms t

let close io (T { state; prim = (module Prim) }) = Prim.close #(io, state)

let%template use io t ~f = (Private_resource_helper.use [@mode p]) ~close io t ~f
[@@mode p = (nonportable, portable)]
;;

let%template stat io (T { state; prim = (module Prim) }) path =
  (Prim.stat [@alloc a]) #(io, state, path) [@exclave_if_stack a]
[@@alloc a @ l = (heap_global, stack_local)]
;;

let%template lstat io (T { state; prim = (module Prim) }) path =
  (Prim.lstat [@alloc a]) #(io, state, path) [@exclave_if_stack a]
[@@alloc a @ l = (heap_global, stack_local)]
;;

let%template ls_dir io (T { state; prim = (module Prim) }) path =
  (Prim.ls_dir [@alloc a]) #(io, state, path) [@exclave_if_stack a]
[@@alloc a @ l = (heap_global, stack_local)]
;;

let mkdir ?(parents = false) ?(perm = File_permissions.t_0755) io t path =
  let (T { state; prim = (module Prim) }) = t in
  Vnit.box (Prim.mkdir #(io, state, path, ~parents, perm))
;;

let rmdir io (T { state; prim = (module Prim) }) path =
  Vnit.box (Prim.rmdir #(io, state, path))
;;

let rename io (T { state; prim = (module Prim) }) ~src ~dst =
  Vnit.box (Prim.rename #(io, state, ~src, ~dst))
;;

let unlink io (T { state; prim = (module Prim) }) path =
  Vnit.box (Prim.unlink #(io, state, path))
;;

let symlink io (T { state; prim = (module Prim) }) path ~referring_to =
  Vnit.box (Prim.symlink #(io, state, path, ~referring_to))
;;

let%template readlink io (T { state; prim = (module Prim) }) path =
  (Prim.readlink [@alloc a]) #(io, state, path) [@exclave_if_stack a]
[@@alloc a @ l = (heap_global, stack_local)]
;;

let%template open_file_unique_to_read io (T { state; prim = (module Prim) }) path =
  (Prim.open_file_to_read [@mode p]) #(io, state, path)
[@@mode p = (nonportable, portable)]
;;

let%template open_file_to_read io t path ~f =
  (File.use [@mode p]) io ~f @@ (open_file_unique_to_read [@mode p]) io t path
[@@mode p = (nonportable, portable)]
;;

let%template open_or_create_file_unique io t ~create path =
  let (T { state; prim = (module Prim) }) = t in
  (Prim.open_or_create_file [@mode p]) #(io, state, ~create, path)
[@@mode p = (nonportable, portable)]
;;

let%template open_or_create_file io t ~create path ~f =
  (File.use [@mode p]) io ~f @@ (open_or_create_file_unique [@mode p]) io t ~create path
[@@mode p = (nonportable, portable)]
;;

let%template open_or_create_file_unique_to_append io t ~create name =
  let (T { state; prim = (module Prim) }) = t in
  (Prim.open_or_create_file_to_append [@mode p]) #(io, state, ~create, name)
[@@mode p = (nonportable, portable)]
;;

let%template open_or_create_file_to_append io t ~create name ~f =
  (Writable.use [@mode p]) io ~f
  @@ (open_or_create_file_unique_to_append [@mode p]) io t ~create name
[@@mode p = (nonportable, portable)]
;;

module Expert = struct
  let%template create state prim = T { state; prim } [@@mode p = (nonportable, portable)]
end

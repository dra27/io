open Core
open Io_types
open Io_kernel
open Filesystem_types
open Common_intf

module Prim = struct
  module type S = sig
    type t

    val close : #(Io.t * t) @ local -> unit#

    val%template stat : #(Io.t * t * File_path.t) @ local -> File_stats.t @ l
    [@@alloc a @ l = (heap_global, stack_local)]

    val%template lstat : #(Io.t * t * File_path.t) @ local -> File_stats.t @ l
    [@@alloc a @ l = (heap_global, stack_local)]

    val%template ls_dir : #(Io.t * t * File_path.t) @ local -> File_path.Part.t iarray @ l
    [@@alloc a @ l = (heap_global, stack_local)]

    val mkdir
      :  #(Io.t * t * File_path.t * parents:bool * File_permissions.t) @ local
      -> unit#

    val rmdir : #(Io.t * t * File_path.t) @ local -> unit#
    val rename : #(Io.t * t * src:File_path.t * dst:File_path.t) @ local -> unit#
    val unlink : #(Io.t * t * File_path.t) @ local -> unit#
    val symlink : #(Io.t * t * File_path.t * referring_to:File_path.t) @ local -> unit#

    val%template readlink : #(Io.t * t * File_path.t) @ local -> File_path.t @ l
    [@@alloc a @ l = (heap_global, stack_local)]

    val%template open_file_to_read
      :  #(Io.t * t * File_path.t) @ local
      -> read File.t @ p unique
    [@@mode p = (nonportable, portable)]

    val%template open_or_create_file
      :  #(Io.t * t * create:File_create.t * File_path.t) @ local
      -> read_write File.t @ p unique
    [@@mode p = (nonportable, portable)]

    val%template open_or_create_file_to_append
      :  #(Io.t * t * create:File_create.t * File_path.t) @ local
      -> Writable.t @ p unique
    [@@mode p = (nonportable, portable)]
  end

  type 's t = (module S with type t = 's)
end

module type Filesystem = sig @@ portable
  (** Abstract hierarchical file system of directories, regular files, and symbolic links. *)

  (** {1 Interface} *)

  (** Represents a file system. *)
  type -'perms t : value mod contended many non_float unyielding

  (** {2 Examining contents} *)

  (** [ls_dir io t path_to_directory] reads the entries of the specified directory. *)
  val%template ls_dir
    :  Io.t @ local
    -> [> read ] t @ local
    -> File_path.t @ local
    -> File_path.Part.t iarray @ l
  [@@alloc a @ l = (heap_global, stack_local)]

  (** [stat io t path] returns information on the specified path. If the path points to a
      symbolic link the link is followed. *)
  val%template stat
    :  Io.t @ local
    -> [> read ] t @ local
    -> File_path.t @ local
    -> File_stats.t @ l
  [@@alloc a @ l = (heap_global, stack_local)]

  (** [lstat io t path] returns information on the specified path. If the path points to a
      symbolic link it is not followed and information is returned on the link itself. *)
  val%template lstat
    :  Io.t @ local
    -> [> read ] t @ local
    -> File_path.t @ local
    -> File_stats.t @ l
  [@@alloc a @ l = (heap_global, stack_local)]

  (** {2 Manipulating contents} *)

  (** [mkdir ?parents ?perm io t path_to_new_directory] creates a directory at the
      specified path.

      When [~parents:true] and the [path_to_new_directory] implies multiple nested
      directories, the operation may not be atomic.

      When [~parents:true], it is not considered an error if parents of the path already
      exist. The permissions of existing directories will not be changed. *)
  val mkdir
    :  ?parents:bool (** default: [false] *) @ local
    -> ?perm:File_permissions.t (** default [t_0755] *) @ local
    -> Io.t @ local
    -> [> read_write ] t @ local
    -> File_path.t @ local
    -> unit

  (** [rmdir io t path_to_directory] removes the specified directory. *)
  val rmdir : Io.t @ local -> [> read_write ] t @ local -> File_path.t @ local -> unit

  (** [rename io t ~src ~dst] renames the specified file. *)
  val rename
    :  Io.t @ local
    -> [> read_write ] t @ local
    -> src:File_path.t @ local
    -> dst:File_path.t @ local
    -> unit

  (** [unlink io t path_to_file] removes the specified regular file or symbolic link. *)
  val unlink : Io.t @ local -> [> read_write ] t @ local -> File_path.t @ local -> unit

  (** {2 Symbolic links} *)

  (** [symlink io t path_to_link ~referring_to] creates the given path as a symbolic link
      to [referring_to]. *)
  val symlink
    :  Io.t @ local
    -> [> read_write ] t @ local
    -> File_path.t @ local
    -> referring_to:File_path.t @ local
    -> unit

  (** [readlink io t path_to_link] reads the contents of a symbolic link. *)
  val%template readlink
    :  Io.t @ local
    -> [> read ] t @ local
    -> File_path.t @ local
    -> File_path.t @ l
  [@@alloc a @ l = (heap_global, stack_local)]

  (** {2 Opening files} *)

  (** {3 For reading} *)

  (** [open_file_to_read io t path_to_file ~f] is {{!open_file_unique_to_read}
      [File.use io ~f @@ open_file_unique_to_read io  path_to_file]} which ensures that no
      resources are leaked. *)
  val%template open_file_to_read
    : ('r : value_or_null).
    Io.t @ local
    -> [> read ] t @ local p
    -> File_path.t @ local
    -> f:(read File.t @ local p -> 'r @ once unique) @ local once
    -> 'r @ once unique
  [@@mode p = (nonportable, portable)]

  include sig
    (** @closed *)

    (** [open_file_unique_to_read io t path_to_file] opens a file for reading. *)
    val%template open_file_unique_to_read
      :  Io.t @ local
      -> [> read ] t @ local p
      -> File_path.t @ local
      -> read File.t @ p unique
    [@@mode p = (nonportable, portable)]
  end

  (** {3 For writing} *)

  (** [open_or_create_file io t ~create path_to_file ~f] is {{!open_or_create_file_unique}
      [File.use io ~f @@ open_or_create_file_unique io ~create path_to_file]} which
      ensures that no resources are leaked. *)
  val%template open_or_create_file
    : ('r : value_or_null).
    Io.t @ local
    -> [> read_write ] t @ local p
    -> create:File_create.t @ local
    -> File_path.t @ local
    -> f:(read_write File.t @ local p -> 'r @ once unique) @ local unique
    -> 'r @ once unique
  [@@mode p = (nonportable, portable)]

  include sig
    (** @closed *)

    (** [open_or_create_file_unique io t ~create path_to_file] opens or creates a file for
        reading and writing. *)
    val%template open_or_create_file_unique
      :  Io.t @ local
      -> [> read_write ] t @ local p
      -> create:File_create.t @ local
      -> File_path.t @ local
      -> read_write File.t @ p unique
    [@@mode p = (nonportable, portable)]
  end

  (** {3 For appending} *)

  (** [open_or_create_file_to_append io t ~create path_to_file ~f] is
      {{!open_or_create_file_unique_to_append}
      [Writable.use io ~f @@ open_or_create_file_unique_to_append io ~create path_to_file]}
      which ensures that no resources are leaked. *)
  val%template open_or_create_file_to_append
    : ('r : value_or_null).
    Io.t @ local
    -> [> read_write ] t @ local p
    -> create:File_create.t @ local
    -> File_path.t @ local
    -> f:(Writable.t @ local p -> 'r @ once unique) @ local once
    -> 'r @ once unique
  [@@mode p = (nonportable, portable)]

  include sig
    (** @closed *)

    (** [open_or_create_file_unique_to_append io t ~create path_to_file] opens or creates
        a file for appending to the end of the file. *)
    val%template open_or_create_file_unique_to_append
      :  Io.t @ local
      -> [> read_write ] t @ local p
      -> create:File_create.t @ local
      -> File_path.t @ local
      -> Writable.t @ p unique
    [@@mode p = (nonportable, portable)]
  end

  (** {1 Resource interface} *)

  include Resource_with_perms with type 'perms t := 'perms t (** @inline *)

  (** {1 Expert interface for implementations} *)

  include sig
    (** @closed *)

    (** These interfaces are intended for implementations only. *)

    module Prim = Prim (** @inline *)

    module Expert : sig
      val%template create
        : ('s : value mod contended).
        's @ p -> 's Prim.t @ p -> 'perms t @ p unique
      [@@mode p = (nonportable, portable)]
    end
  end
end

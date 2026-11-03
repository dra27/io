open Ox
open Core
include Io_types

module Io_slice = struct
  include Io_slice

  module Expert = struct
    let%template with_ext_out (bs : t @ local read) ~(f : Ext.t @ local read -> _) =
      match (Slice.to_ext [@mode local read]) bs with
      | T #(Some, ext_bs) -> f ext_bs [@nontail]
      | T #(None, _) ->
        let len = Slice.length bs in
        let%with.tilde.stack ext_bs =
          (Slice.Ext.with_bigstring [@kind k]) (Bigstring.create (I64.to_int_exn len))
        in
        for i = 0 to I64.to_int_trunc len - 1 do
          (Slice.Ext.unsafe_set [@kind bits8])
            ext_bs
            (I64.of_int i)
            ((Slice.unsafe_get [@kind bits8] [@mode local read]) bs (I64.of_int i))
        done;
        f ext_bs
    [@@kind k = word]
    ;;

    let%template with_ext_in (bs : t @ local) ~len ~(f : Ext.t @ local -> _) =
      match (Slice.to_ext [@mode local]) bs with
      | T #(Some, ext_bs) -> f ext_bs [@nontail]
      | T #(None, _) ->
        let%with.tilde.stack ext_bs =
          (Slice.Ext.with_bigstring [@kind k])
            (Bigstring.create (I64.to_int_exn (Slice.length bs)))
        in
        let result = f ext_bs in
        let n = Isize.to_int_trunc (len result) in
        assert%debug (0 <= n && n <= I64.to_int_trunc (Slice.Ext.length ext_bs));
        for i = 0 to n - 1 do
          (Slice.unsafe_set [@kind bits8])
            bs
            (I64.of_int i)
            ((Slice.Ext.unsafe_get [@kind bits8] [@mode local]) ext_bs (I64.of_int i))
        done;
        result
    [@@kind k = (word, word & value_or_null)]
    ;;
  end
end

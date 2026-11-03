#define _GNU_SOURCE

/* We specify [CAML_INTERNALS] to get [cpu_relax]. */
#define CAML_INTERNALS
#include <caml/platform.h>
#undef CAML_INTERNALS

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/socketaddr.h>
#include <caml/unixsupport.h>

#include <endian.h> /* liburing.h needs this for __BYTE_ORDER */
#include <fcntl.h>
#include <poll.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/sysmacros.h>
#include <sys/uio.h>
#include <unistd.h>

#include <liburing.h>

#include <errno.h>
#include <string.h>
#include <assert.h>

////////////////////////////////////////////////////////////////////////////////

value ocaml_io_uring_get_device_noalloc(const void *t, int64_t major_offset,
                                        int64_t minor_offset) {
  return Val_long(makedev(*(const __u32 *)((const char *)t + major_offset),
                          *(const __u32 *)((const char *)t + minor_offset)));
}

////////////////////////////////////////////////////////////////////////////////


void ocaml_io_uring_sockaddr_in_set_noalloc(struct sockaddr_in *sockaddr_in,
                                            value v_sockaddr) {
  // Deconstructs [v_sockaddr : Sockaddr.t] into a [struct sockaddr_in].
  sockaddr_in->sin_family = AF_INET;
  sockaddr_in->sin_addr.s_addr = htonl(Long_val(v_sockaddr) >> 16);
  sockaddr_in->sin_port = htons((Long_val(v_sockaddr) & 0xffff));
}

value ocaml_io_uring_sockaddr_in_get_noalloc(struct sockaddr_in *sockaddr_in) {
  // Constructs a [Sockaddr.t] from a [sruct sockaddr_in].
  assert(AF_INET == sockaddr_in->sin_family);
  intnat value = ((intnat)(ntohs(sockaddr_in->sin_port) & 0xffff)) |
                 (intnat)(ntohl(sockaddr_in->sin_addr.s_addr)) << 16;
  return Val_long(value);
}

////////////////////////////////////////////////////////////////////////////////


static inline struct io_uring *Ring_val(value v_ring) { return Ptr_val(v_ring); }

value ocaml_io_uring_submit_noalloc(value v_uring) {
  return Val_int(io_uring_submit(Ring_val(v_uring)));
}

// We build the results list recursively to get the list in original order.  The
// expectation is that the lists should usually be short.
value ocaml_io_uring_drain_noalloc(value v_uring) {
  struct io_uring *ring = Ring_val(v_uring);
  struct io_uring_cqe *cqe;

  if (0 == io_uring_peek_cqe(ring, &cqe)) {
    intptr_t tag = (intptr_t)io_uring_cqe_get_data(cqe);
    int32_t res = cqe->res;
    uint32_t flags = cqe->flags;
    uint64_t res_and_flags = ((uint32_t)res) | (((uint64_t)flags) << 32);
    io_uring_cqe_seen(ring, cqe);

    value v_tl = ocaml_io_uring_drain_noalloc(v_uring);

    Assert_mixed_block_layout_v6;
    value v_cons = caml_alloc_local_reserved(
        3, 0 /* tag */, Reserved_mixed_block_scannable_wosize_native(1));

    Field(v_cons, 0) = v_tl;
    Field(v_cons, 1) = tag;
    Field(v_cons, 2) = res_and_flags;

    return v_cons;
  }
  return Val_int(0);
}

value ocaml_io_uring_register_eventfd_noalloc(value v_uring, value v_fd) {
  return Val_int(io_uring_register_eventfd(Ring_val(v_uring), Int_val(v_fd)));
}

////////////////////////////////////////////////////////////////////////////////

static inline void set_sqe_data_and_flags(struct io_uring_sqe *sqe, void *tag,
                                          value sqe_flags) {
  io_uring_sqe_set_data(sqe, tag);
  io_uring_sqe_set_flags(sqe, Int_val(sqe_flags));
}

value ocaml_io_uring_ensure_can_push_noalloc(value v_uring, value v_n) {
  struct io_uring *ring = Ring_val(v_uring);

  int n = Int_val(v_n);

  // We expect there to almost always be enough space already.
  if (io_uring_sq_space_left(ring) >= n)
    return Val_int(0);

  // We check that the ring is not configured to have too little space.
  if (ring->sq.ring_entries < n)
    return Val_int(-EOVERFLOW);

  // Finally we submit.  With [IORING_SETUP_SQPOLL] this ensures the poll thread is
  // awake and will (at some point) consume all the requests published earlier,
  // otherwise this submits anything left previously unsubmitted as we use
  // [IORING_SETUP_SUBMIT_ALL].
  return Val_int(io_uring_submit(ring));
}

static inline struct io_uring_sqe *wait_to_get_sqe(struct io_uring *ring) {
  struct io_uring_sqe *sqe;

  // The loop is needed when using [IORING_SETUP_SQPOLL] to wait for the kernel to consume
  // entries from the submission queue.
  while (!(sqe = io_uring_get_sqe(ring))) {
    assert(ring->flags & IORING_SETUP_SQPOLL);
    assert(*ring->sq.ktail != io_uring_smp_load_acquire(ring->sq.khead) ||
           io_uring_sq_space_left(ring) > 0);
    // Calling [io_uring_sqring_wait] doesn't really help unless entries are submitted
    // one-by-one, because the kernel doesn't know about the entries we have filled, but
    // haven't yet submitted.  We instead use OCaml's internal [cpu_relax] assuming that
    // the SQPOLL thread runs in parallel.
    cpu_relax();

  }

  return sqe;
}

void ocaml_io_uring_push_cancel_noalloc(value v_uring, void *tag, value sqe_flags,
                                        void *tag_to_cancel, value cancel_flags) {
  struct io_uring_sqe *sqe = wait_to_get_sqe(Ring_val(v_uring));
  io_uring_prep_cancel(sqe, tag_to_cancel, Int_val(cancel_flags));
  set_sqe_data_and_flags(sqe, tag, sqe_flags);
}

void ocaml_io_uring_push_nop_noalloc(value v_uring, void *tag, value sqe_flags) {
  struct io_uring_sqe *sqe = wait_to_get_sqe(Ring_val(v_uring));
  io_uring_prep_nop(sqe);
  set_sqe_data_and_flags(sqe, tag, sqe_flags);
}

void ocaml_io_uring_push_close_noalloc(value v_uring, void *tag, value sqe_flags,
                                       value v_fd) {
  struct io_uring_sqe *sqe = wait_to_get_sqe(Ring_val(v_uring));
  io_uring_prep_close(sqe, Int_val(v_fd));
  set_sqe_data_and_flags(sqe, tag, sqe_flags);
}

void ocaml_io_uring_push_shutdown_noalloc(value v_uring, void *tag, value sqe_flags,
                                          value v_fd, value v_command) {
  struct io_uring_sqe *sqe = wait_to_get_sqe(Ring_val(v_uring));
  io_uring_prep_shutdown(sqe, Int_val(v_fd), Int_val(v_command));
  set_sqe_data_and_flags(sqe, tag, sqe_flags);
}

void ocaml_io_uring_push_socket_noalloc(value v_uring, void *tag, value sqe_flags,
                                        value v_socket_domain,
                                        value v_socket_type_and_flags, value v_protocol) {
  struct io_uring_sqe *sqe = wait_to_get_sqe(Ring_val(v_uring));
  io_uring_prep_socket(sqe, Int_val(v_socket_domain), Int_val(v_socket_type_and_flags),
                       Int_val(v_protocol), 0);
  set_sqe_data_and_flags(sqe, tag, sqe_flags);
}

void ocaml_io_uring_push_bind_noalloc(value v_uring, void *tag, value sqe_flags,
                                      value v_fd, struct sockaddr *sockaddr,
                                      const socklen_t *socklen) {
  struct io_uring_sqe *sqe = wait_to_get_sqe(Ring_val(v_uring));
  io_uring_prep_bind(sqe, Int_val(v_fd), sockaddr, *socklen);
  set_sqe_data_and_flags(sqe, tag, sqe_flags);
}

void ocaml_io_uring_push_listen_noalloc(value v_uring, void *tag, value sqe_flags,
                                        value v_fd, value v_max_pending_requests) {
  struct io_uring_sqe *sqe = wait_to_get_sqe(Ring_val(v_uring));
  io_uring_prep_listen(sqe, Int_val(v_fd), Int_val(v_max_pending_requests));
  set_sqe_data_and_flags(sqe, tag, sqe_flags);
}

void ocaml_io_uring_push_accept_noalloc(value v_uring, void *tag, value sqe_flags,
                                        value v_fd, struct sockaddr *sockaddr,
                                        socklen_t *socklen, value v_socket_flags) {
  struct io_uring_sqe *sqe = wait_to_get_sqe(Ring_val(v_uring));
  io_uring_prep_accept(sqe, Int_val(v_fd), sockaddr, socklen, Int_val(v_socket_flags));
  set_sqe_data_and_flags(sqe, tag, sqe_flags);
}

void ocaml_io_uring_push_connect_noalloc(value v_uring, void *tag, value sqe_flags,
                                         value v_fd, const struct sockaddr *sockaddr,
                                         const socklen_t *socklen) {
  struct io_uring_sqe *sqe = wait_to_get_sqe(Ring_val(v_uring));
  io_uring_prep_connect(sqe, Int_val(v_fd), sockaddr, *socklen);
  set_sqe_data_and_flags(sqe, tag, sqe_flags);
}

void ocaml_io_uring_push_recvmsg_noalloc(value v_uring, void *tag, value sqe_flags,
                                         value v_fd, struct msghdr *msghdr,
                                         value recv_flags) {
  struct io_uring_sqe *sqe = wait_to_get_sqe(Ring_val(v_uring));
  io_uring_prep_recvmsg(sqe, Int_val(v_fd), msghdr, Int_val(recv_flags));
  set_sqe_data_and_flags(sqe, tag, sqe_flags);
}

void ocaml_io_uring_push_sendmsg_noalloc(value v_uring, void *tag, value sqe_flags,
                                         value v_fd, struct msghdr *msghdr,
                                         value send_flags) {
  struct io_uring_sqe *sqe = wait_to_get_sqe(Ring_val(v_uring));
  io_uring_prep_sendmsg(sqe, Int_val(v_fd), msghdr, Int_val(send_flags));
  set_sqe_data_and_flags(sqe, tag, sqe_flags);
}

void ocaml_io_uring_push_write_noalloc(value v_uring, void *tag, value sqe_flags,
                                       value v_fd, size_t offset, void *buf,
                                       ssize_t len) {
  struct io_uring_sqe *sqe = wait_to_get_sqe(Ring_val(v_uring));
  io_uring_prep_write(sqe, Int_val(v_fd), buf, len, offset);
  set_sqe_data_and_flags(sqe, tag, sqe_flags);
}

void ocaml_io_uring_push_read_noalloc(value v_uring, void *tag, value sqe_flags,
                                      value v_fd, size_t offset, void *buf, ssize_t len) {
  struct io_uring_sqe *sqe = wait_to_get_sqe(Ring_val(v_uring));
  io_uring_prep_read(sqe, Int_val(v_fd), buf, len, offset);
  set_sqe_data_and_flags(sqe, tag, sqe_flags);
}

void ocaml_io_uring_push_poll_add_noalloc(value v_uring, void *tag, value sqe_flags,
                                          value v_fd, value v_poll_mask) {
  struct io_uring_sqe *sqe = wait_to_get_sqe(Ring_val(v_uring));
  io_uring_prep_poll_add(sqe, Int_val(v_fd), Int_val(v_poll_mask));
  set_sqe_data_and_flags(sqe, tag, sqe_flags);
}

void ocaml_io_uring_push_fsync_noalloc(value v_uring, void *tag, value sqe_flags,
                                       value v_fd) {
  struct io_uring_sqe *sqe = wait_to_get_sqe(Ring_val(v_uring));
  io_uring_prep_fsync(sqe, Int_val(v_fd), 0);
  set_sqe_data_and_flags(sqe, tag, sqe_flags);
}

void ocaml_io_uring_push_ftruncate_noalloc(value v_uring, void *tag, value sqe_flags,
                                           value v_fd, int64_t size) {
  struct io_uring_sqe *sqe = wait_to_get_sqe(Ring_val(v_uring));
  io_uring_prep_ftruncate(sqe, Int_val(v_fd), size);
  set_sqe_data_and_flags(sqe, tag, sqe_flags);
}

void ocaml_io_uring_push_openat_noalloc(value v_uring, void *tag, value sqe_flags,
                                        value v_relative_to, const char *path,
                                        value v_flags, value v_mode) {
  struct io_uring_sqe *sqe = wait_to_get_sqe(Ring_val(v_uring));
  io_uring_prep_openat(sqe, Int_val(v_relative_to), path, Int_val(v_flags),
                       Int_val(v_mode));
  set_sqe_data_and_flags(sqe, tag, sqe_flags);
}

void ocaml_io_uring_push_unlinkat_noalloc(value v_uring, void *tag, value sqe_flags,
                                          value v_relative_to, const char *path,
                                          value flags) {
  struct io_uring_sqe *sqe = wait_to_get_sqe(Ring_val(v_uring));
  io_uring_prep_unlinkat(sqe, Int_val(v_relative_to), path, Int_val(flags));
  set_sqe_data_and_flags(sqe, tag, sqe_flags);
}

void ocaml_io_uring_push_renameat_noalloc(value v_uring, void *tag, value sqe_flags,
                                          value v_relative_to_before, const char *before,
                                          value v_relative_to_after, const char *after) {
  struct io_uring_sqe *sqe = wait_to_get_sqe(Ring_val(v_uring));
  io_uring_prep_renameat(sqe, Int_val(v_relative_to_before), before,
                         Int_val(v_relative_to_after), after, 0);
  set_sqe_data_and_flags(sqe, tag, sqe_flags);
}

void ocaml_io_uring_push_symlinkat_noalloc(value v_uring, void *tag, value sqe_flags,
                                           const char *into, value v_relative_to,
                                           const char *path) {
  struct io_uring_sqe *sqe = wait_to_get_sqe(Ring_val(v_uring));
  io_uring_prep_symlinkat(sqe, into, Int_val(v_relative_to), path);
  set_sqe_data_and_flags(sqe, tag, sqe_flags);
}

void ocaml_io_uring_push_mkdirat_noalloc(value v_uring, void *tag, value sqe_flags,
                                         value v_relative_to, const char *path,
                                         value v_perm) {
  struct io_uring_sqe *sqe = wait_to_get_sqe(Ring_val(v_uring));
  io_uring_prep_mkdirat(sqe, Int_val(v_relative_to), path, Int_val(v_perm));
  set_sqe_data_and_flags(sqe, tag, sqe_flags);
}

void ocaml_io_uring_push_statx_noalloc(value v_uring, void *tag, value sqe_flags,
                                       value v_relative_to, const char *path,
                                       value v_flags, value v_mask, struct statx *statx) {
  struct io_uring_sqe *sqe = wait_to_get_sqe(Ring_val(v_uring));
  io_uring_prep_statx(sqe, Int_val(v_relative_to), path, Int_val(v_flags),
                      Int_val(v_mask), statx);
  set_sqe_data_and_flags(sqe, tag, sqe_flags);
}

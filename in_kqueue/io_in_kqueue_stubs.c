/* Stubs for the kqueue based implementation of [Io.t] (macOS).

   Conventions:

   - Functions named [*_noalloc] are bound with [@@noalloc] and must not allocate,
     raise or release the runtime lock. They return a non-negative result or a negated
     [errno] value.

   - Other functions may release the runtime lock around system calls that can take a
     long time (e.g. reads and writes of regular files, which may hit the disk). Buffers
     passed to them always live in external memory (outside the OCaml heap), so it is safe
     to access them without holding the runtime lock.

   - Waiting for readiness uses one-shot [EV_UDATA_SPECIFIC] knotes, i.e. every waiter
     gets its own knote identified by [(fd, filter, udata)] where [udata] is the id of the
     waiter. This allows any number of waiters on the same descriptor and precise
     cancellation of an individual wait. [EV_UDATA_SPECIFIC] is Darwin specific. */

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/socketaddr.h>
#include <caml/unixsupport.h>

#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <stdint.h>
#include <sys/event.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#ifndef EV_UDATA_SPECIFIC
#error "EV_UDATA_SPECIFIC is required (Darwin kqueue)"
#endif

/* Filter bits, shared with the OCaml side. */
#define IO_KQ_IN 1
#define IO_KQ_OUT 2
#define IO_KQ_ERR 4

/* udata of the EVFILT_USER event used to wake up the poller. Waiter ids are
   non-negative OCaml integers, so this cannot clash with them. */
#define IO_KQ_WAKEUP_UDATA ((void *)(intptr_t)-1)

/* Maximum number of events returned by a single [ocaml_io_kq_wait]. */
#define IO_KQ_MAX_EVENTS 128

/* Errno constants needed by the OCaml side. */
intnat ocaml_io_kq_errno_noalloc(intnat which) {
  switch (which) {
    case 0: return EAGAIN;
    case 1: return EINTR;
    case 2: return EINPROGRESS;
    case 3: return EEXIST;
    case 4: return ENOENT;
    case 5: return EISDIR;
    default: return 0;
  }
}

/* Raises [Unix.Unix_error] for the given [errno]. Only used on error paths. */
value ocaml_io_kq_raise_unix_error(value v_errno, value v_name) {
  caml_unix_error(Int_val(v_errno), String_val(v_name), Nothing);
  return Val_unit; /* not reached */
}

intnat ocaml_io_kq_create_noalloc(value v_unit) {
  (void)v_unit;
  int kq = kqueue();
  if (kq < 0) return -errno;
  if (fcntl(kq, F_SETFD, FD_CLOEXEC) < 0) {
    int err = errno;
    close(kq);
    return -err;
  }
  struct kevent change;
  EV_SET(&change, 0, EVFILT_USER, EV_ADD | EV_CLEAR, 0, 0, IO_KQ_WAKEUP_UDATA);
  if (kevent(kq, &change, 1, NULL, 0, NULL) < 0) {
    int err = errno;
    close(kq);
    return -err;
  }
  return kq;
}

intnat ocaml_io_kq_wakeup_noalloc(intnat kq) {
  struct kevent change;
  EV_SET(&change, 0, EVFILT_USER, 0, NOTE_TRIGGER, 0, IO_KQ_WAKEUP_UDATA);
  return kevent((int)kq, &change, 1, NULL, 0, NULL) < 0 ? -errno : 0;
}

static int fill_changes(struct kevent *changes, intnat fd, intnat filters,
                        unsigned short flags, intnat id) {
  int n = 0;
  void *udata = (void *)(intptr_t)id;
  if (filters & IO_KQ_IN)
    EV_SET(&changes[n++], (uintptr_t)fd, EVFILT_READ, flags, 0, 0, udata);
  if (filters & IO_KQ_OUT)
    EV_SET(&changes[n++], (uintptr_t)fd, EVFILT_WRITE, flags, 0, 0, udata);
  return n;
}

/* Deletes the knotes of waiter [id] for [fd] and [filters]. Errors, e.g. [ENOENT] in case
   a one-shot knote has already fired, are ignored. This is called from cancellation
   callbacks and must not raise. */
intnat ocaml_io_kq_del_noalloc(intnat kq, intnat fd, intnat filters, intnat id) {
  struct kevent changes[2], receipts[2];
  int n = fill_changes(changes, fd, filters, EV_DELETE | EV_UDATA_SPECIFIC | EV_RECEIPT,
                       id);
  if (n == 0) return 0;
  /* With [EV_RECEIPT] all changes are processed even if some of them fail. */
  (void)kevent((int)kq, changes, n, receipts, n, NULL);
  return 0;
}

/* Registers one-shot knotes for waiter [id]. Either all or none of the knotes are
   registered. */
intnat ocaml_io_kq_add_noalloc(intnat kq, intnat fd, intnat filters, intnat id) {
  struct kevent changes[2], receipts[2];
  int n = fill_changes(changes, fd, filters,
                       EV_ADD | EV_ONESHOT | EV_UDATA_SPECIFIC | EV_RECEIPT, id);
  if (n == 0) return -EINVAL;
  int r = kevent((int)kq, changes, n, receipts, n, NULL);
  if (r < 0) return -errno;
  int err = 0;
  for (int i = 0; i < r; i++)
    if ((receipts[i].flags & EV_ERROR) && receipts[i].data != 0) err = receipts[i].data;
  if (err != 0) {
    (void)ocaml_io_kq_del_noalloc(kq, fd, filters, id);
    return -err;
  }
  return 0;
}

/* Blocks (with the runtime lock released) until at least one event is available and
   stores [(udata, result)] pairs into the OCaml [int array] [v_out]. The result is a set
   of [IO_KQ_*] bits. Returns the number of events or a negated [errno]. */
value ocaml_io_kq_wait(value v_kq, value v_out) {
  CAMLparam1(v_out);
  struct kevent events[IO_KQ_MAX_EVENTS];
  int max = (int)(Wosize_val(v_out) / 2);
  if (max > IO_KQ_MAX_EVENTS) max = IO_KQ_MAX_EVENTS;
  int kq = Int_val(v_kq);
  caml_enter_blocking_section();
  int n = kevent(kq, NULL, 0, events, max, NULL);
  int err = errno;
  caml_leave_blocking_section();
  if (n < 0) CAMLreturn(Val_long(-err));
  for (int i = 0; i < n; i++) {
    intnat result = 0;
    if (events[i].filter == EVFILT_READ) result |= IO_KQ_IN;
    if (events[i].filter == EVFILT_WRITE) result |= IO_KQ_OUT;
    if (events[i].flags & (EV_EOF | EV_ERROR)) result |= IO_KQ_ERR;
    /* Immediate values, so no write barrier is needed. */
    Field(v_out, 2 * i) = Val_long((intnat)(intptr_t)events[i].udata);
    Field(v_out, 2 * i + 1) = Val_long(result);
  }
  CAMLreturn(Val_long(n));
}

/* Non-blocking readiness probe. Returns the [IO_KQ_*] bits that are ready (0 if none) or
   a negated [errno]. Unlike kqueue, [poll] reports regular files as always ready, which
   is what we want (kqueue never reports a regular file at EOF as readable). */
intnat ocaml_io_kq_poll_noalloc(intnat fd, intnat filters) {
  struct pollfd p;
  p.fd = (int)fd;
  p.events = 0;
  p.revents = 0;
  if (filters & IO_KQ_IN) p.events |= POLLIN;
  if (filters & IO_KQ_OUT) p.events |= POLLOUT;
  int r = poll(&p, 1, 0);
  if (r < 0) return -errno;
  if (r == 0) return 0;
  if (p.revents & POLLNVAL) return -EBADF;
  intnat result = 0;
  if (p.revents & POLLIN) result |= IO_KQ_IN;
  if (p.revents & POLLOUT) result |= IO_KQ_OUT;
  if (p.revents & (POLLERR | POLLHUP)) result |= IO_KQ_ERR | (filters & (IO_KQ_IN | IO_KQ_OUT));
  return result;
}

intnat ocaml_io_kq_set_nosigpipe_noalloc(intnat fd) {
  int one = 1;
  return setsockopt((int)fd, SOL_SOCKET, SO_NOSIGPIPE, &one, sizeof(one)) < 0 ? -errno : 0;
}

intnat ocaml_io_kq_set_nonblock_noalloc(intnat fd) {
  int flags = fcntl((int)fd, F_GETFL);
  if (flags < 0) return -errno;
  if (flags & O_NONBLOCK) return 0;
  return fcntl((int)fd, F_SETFL, flags | O_NONBLOCK) < 0 ? -errno : 0;
}

/* Stream reads and writes never block the calling thread for an unbounded time:

   - On sockets, [MSG_DONTWAIT] makes the call non-blocking regardless of [O_NONBLOCK],
     so the common case is a single system call.

   - On other descriptors (pipes, ttys, regular files) we first probe readiness with
     [poll] and only perform the call if the descriptor is ready, returning [-EAGAIN]
     otherwise. The call itself is made with the runtime lock released, as it may be a
     regular file on a slow disk. */

intnat ocaml_io_kq_read_stream(intnat fd, void *buf, intnat len) {
  ssize_t n = recv((int)fd, buf, (size_t)len, MSG_DONTWAIT);
  if (n >= 0) return n;
  if (errno != ENOTSOCK) return -errno;
  intnat ready = ocaml_io_kq_poll_noalloc(fd, IO_KQ_IN);
  if (ready < 0) return ready;
  if (ready == 0) return -EAGAIN;
  caml_enter_blocking_section();
  n = read((int)fd, buf, (size_t)len);
  int err = errno;
  caml_leave_blocking_section();
  return n >= 0 ? n : -err;
}

intnat ocaml_io_kq_write_stream(intnat fd, void *buf, intnat len) {
  ssize_t n = send((int)fd, buf, (size_t)len, MSG_DONTWAIT);
  if (n >= 0) return n;
  if (errno != ENOTSOCK) return -errno;
  intnat ready = ocaml_io_kq_poll_noalloc(fd, IO_KQ_OUT);
  if (ready < 0) return ready;
  if (ready == 0) return -EAGAIN;
  caml_enter_blocking_section();
  n = write((int)fd, buf, (size_t)len);
  int err = errno;
  caml_leave_blocking_section();
  return n >= 0 ? n : -err;
}

intnat ocaml_io_kq_pread(intnat fd, void *buf, intnat len, int64_t offset) {
  caml_enter_blocking_section();
  ssize_t n = pread((int)fd, buf, (size_t)len, (off_t)offset);
  int err = errno;
  caml_leave_blocking_section();
  return n >= 0 ? n : -err;
}

intnat ocaml_io_kq_pwrite(intnat fd, void *buf, intnat len, int64_t offset) {
  caml_enter_blocking_section();
  ssize_t n = pwrite((int)fd, buf, (size_t)len, (off_t)offset);
  int err = errno;
  caml_leave_blocking_section();
  return n >= 0 ? n : -err;
}

/* Datagram operations raise [Unix.Unix_error] on failure (including [EAGAIN]), as the
   result includes an allocated socket address. */

value ocaml_io_kq_sendto(value v_fd, void *buf, intnat len, value v_addr) {
  CAMLparam1(v_addr);
  union sock_addr_union addr;
  socklen_param_type addr_len;
  caml_unix_get_sockaddr(v_addr, &addr, &addr_len);
  ssize_t n = sendto(Int_val(v_fd), buf, (size_t)len, MSG_DONTWAIT, &addr.s_gen, addr_len);
  if (n < 0) caml_uerror("sendto", Nothing);
  CAMLreturn(Val_long(n));
}

value ocaml_io_kq_recvfrom(value v_fd, void *buf, intnat len) {
  CAMLparam0();
  CAMLlocal2(v_addr, v_result);
  union sock_addr_union addr;
  socklen_param_type addr_len = sizeof(addr);
  ssize_t n = recvfrom(Int_val(v_fd), buf, (size_t)len, MSG_DONTWAIT, &addr.s_gen,
                       &addr_len);
  if (n < 0) caml_uerror("recvfrom", Nothing);
  v_addr = caml_unix_alloc_sockaddr(&addr, addr_len, -1);
  v_result = caml_alloc_small(2, 0);
  Field(v_result, 0) = Val_long(n);
  Field(v_result, 1) = v_addr;
  CAMLreturn(v_result);
}

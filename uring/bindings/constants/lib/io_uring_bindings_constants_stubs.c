#define _GNU_SOURCE

#include <endian.h> /* liburing.h needs this for __BYTE_ORDER */
#include <fcntl.h>
#include <netinet/in.h>
#include <poll.h>
#include <stddef.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/sysmacros.h>
#include <sys/uio.h>
#include <unistd.h>

#include <liburing.h>

#include <stdio.h>
#include <string.h>

#define SIZEOF(typename)                                                                 \
  { #typename, sizeof(typename) }

#define SIZEOF_STRUCT(struct_name)                                                       \
  { #struct_name, sizeof(struct struct_name) }

#define OFFSETOF_STRUCT(struct_name, field_name)                                         \
  { #struct_name "__" #field_name, offsetof(struct struct_name, field_name) }

#define CONSTANT(name)                                                                   \
  { #name, name }

static const struct let {
  const char *lhs;
  int64_t rhs;
} let[] = {
    SIZEOF_STRUCT(sockaddr_in),
    OFFSETOF_STRUCT(sockaddr_in, sin_family),
    //
    SIZEOF(socklen_t),
    SIZEOF(sa_family_t),
    //
    SIZEOF_STRUCT(io_uring),
    OFFSETOF_STRUCT(io_uring, ring_fd),
    OFFSETOF_STRUCT(io_uring, sq),
    //
    SIZEOF_STRUCT(io_uring_sq),
    OFFSETOF_STRUCT(io_uring_sq, ring_entries),
    //
    SIZEOF_STRUCT(io_uring_params),
    OFFSETOF_STRUCT(io_uring_params, cq_entries),
    OFFSETOF_STRUCT(io_uring_params, flags),
    OFFSETOF_STRUCT(io_uring_params, sq_thread_cpu),
    OFFSETOF_STRUCT(io_uring_params, sq_thread_idle),
    OFFSETOF_STRUCT(io_uring_params, wq_fd),
    //
    CONSTANT(IORING_SETUP_SQPOLL),
    CONSTANT(IORING_SETUP_SQ_AFF),
    CONSTANT(IORING_SETUP_CQSIZE),
    CONSTANT(IORING_SETUP_CLAMP),
    CONSTANT(IORING_SETUP_ATTACH_WQ),
    CONSTANT(IORING_SETUP_SUBMIT_ALL),
    CONSTANT(IORING_SETUP_COOP_TASKRUN),
    CONSTANT(IORING_SETUP_NO_SQARRAY),
    //
    CONSTANT(IORING_CQE_F_BUFFER),
    CONSTANT(IORING_CQE_F_MORE),
    CONSTANT(IORING_CQE_F_SOCK_NONEMPTY),
    CONSTANT(IORING_CQE_F_NOTIF),
    CONSTANT(IORING_CQE_F_BUF_MORE),
    //
    SIZEOF_STRUCT(statx),
    OFFSETOF_STRUCT(statx, stx_nlink),
    OFFSETOF_STRUCT(statx, stx_gid),
    OFFSETOF_STRUCT(statx, stx_uid),
    OFFSETOF_STRUCT(statx, stx_mode),
    OFFSETOF_STRUCT(statx, stx_ino),
    OFFSETOF_STRUCT(statx, stx_size),
    OFFSETOF_STRUCT(statx, stx_atime),
    OFFSETOF_STRUCT(statx, stx_ctime),
    OFFSETOF_STRUCT(statx, stx_mtime),
    OFFSETOF_STRUCT(statx, stx_rdev_major),
    OFFSETOF_STRUCT(statx, stx_rdev_minor),
    OFFSETOF_STRUCT(statx, stx_dev_major),
    OFFSETOF_STRUCT(statx, stx_dev_minor),
    //
    OFFSETOF_STRUCT(statx_timestamp, tv_sec),
    OFFSETOF_STRUCT(statx_timestamp, tv_nsec),
    //
    CONSTANT(AT_FDCWD),
    //
    CONSTANT(AT_SYMLINK_NOFOLLOW),
    CONSTANT(AT_REMOVEDIR),
    //
    CONSTANT(STATX_TYPE),
    CONSTANT(STATX_BASIC_STATS),
    //
    CONSTANT(S_IFMT),
    CONSTANT(S_IFSOCK),
    CONSTANT(S_IFBLK),
    CONSTANT(S_IFCHR),
    CONSTANT(S_IFIFO),
    CONSTANT(S_IFREG),
    CONSTANT(S_IFDIR),
    CONSTANT(S_IFLNK),
    //
    SIZEOF_STRUCT(msghdr),
    OFFSETOF_STRUCT(msghdr, msg_name),
    OFFSETOF_STRUCT(msghdr, msg_namelen),
    OFFSETOF_STRUCT(msghdr, msg_iov),
    OFFSETOF_STRUCT(msghdr, msg_iovlen),
    OFFSETOF_STRUCT(msghdr, msg_control),
    OFFSETOF_STRUCT(msghdr, msg_controllen),
    OFFSETOF_STRUCT(msghdr, msg_flags),
    //
    CONSTANT(MSG_DONTWAIT),
    CONSTANT(MSG_OOB),
    //
    CONSTANT(MSG_PEEK),
    CONSTANT(MSG_TRUNC),
    CONSTANT(MSG_WAITALL),
    //
    CONSTANT(MSG_CONFIRM),
    CONSTANT(MSG_DONTROUTE),
    CONSTANT(MSG_EOR),
    CONSTANT(MSG_MORE),
    CONSTANT(MSG_NOSIGNAL),
    CONSTANT(MSG_FASTOPEN),
    //
    SIZEOF_STRUCT(iovec),
    OFFSETOF_STRUCT(iovec, iov_base),
    OFFSETOF_STRUCT(iovec, iov_len),
    //
    CONSTANT(O_RDONLY),
    CONSTANT(O_WRONLY),
    CONSTANT(O_RDWR),
    //
    CONSTANT(O_CLOEXEC),
    CONSTANT(O_CREAT),
    CONSTANT(O_DIRECTORY),
    CONSTANT(O_EXCL),
    CONSTANT(O_NOCTTY),
    CONSTANT(O_NOFOLLOW),
    CONSTANT(O_TMPFILE),
    CONSTANT(O_TRUNC),
    //
    CONSTANT(O_APPEND),
    CONSTANT(O_ASYNC),
    CONSTANT(O_DIRECT),
    CONSTANT(O_DSYNC),
    CONSTANT(O_LARGEFILE),
    CONSTANT(O_NOATIME),
    CONSTANT(O_NONBLOCK),
    CONSTANT(O_PATH),
    CONSTANT(O_SYNC),
    //
    CONSTANT(AF_UNIX),
    CONSTANT(AF_INET),
    CONSTANT(AF_INET6),
    //
    CONSTANT(SOCK_STREAM),
    CONSTANT(SOCK_DGRAM),
    CONSTANT(SOCK_RAW),
    CONSTANT(SOCK_SEQPACKET),
    //
    CONSTANT(SOCK_NONBLOCK),
    CONSTANT(SOCK_CLOEXEC),
    //
    CONSTANT(IORING_ASYNC_CANCEL_ALL),
    CONSTANT(IORING_ASYNC_CANCEL_ANY),
    CONSTANT(IORING_ASYNC_CANCEL_USERDATA),
    //
    CONSTANT(IOSQE_ASYNC),
    CONSTANT(IOSQE_CQE_SKIP_SUCCESS),
    CONSTANT(IOSQE_IO_DRAIN),
    CONSTANT(IOSQE_IO_HARDLINK),
    CONSTANT(IOSQE_IO_LINK),
    //
    CONSTANT(EALREADY),
    CONSTANT(ECANCELED),
    CONSTANT(EEXIST),
    CONSTANT(EISDIR),
    CONSTANT(ENOENT),
};

void ocaml_io_uring_constants_print_noalloc() {
  printf("(* This file is automatically generated. *)\n\n");
  for (size_t i = 0; i < sizeof(let) / sizeof(let[0]); ++i)
    printf("let c_%s = #0x%" PRIx64 "L\n", let[i].lhs, let[i].rhs);
}

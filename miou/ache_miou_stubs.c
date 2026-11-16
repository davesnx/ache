#include <errno.h>
#include <limits.h>
#include <sys/socket.h>
#include <sys/uio.h>

#include <caml/bigarray.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

#if !defined(IOV_MAX) || IOV_MAX > 1024
#define ACHE_IOV_MAX 1024
#else
#define ACHE_IOV_MAX IOV_MAX
#endif

value ache_miou_sendmsg(value fd, value iovecs) {
  struct iovec iov[ACHE_IOV_MAX];
  int count = 0;
  for (value cell = iovecs; cell != Val_emptylist && count < ACHE_IOV_MAX;
       cell = Field(cell, 1)) {
    value iovec = Field(cell, 0);
    iov[count].iov_base =
        (char *)Caml_ba_data_val(Field(iovec, 0)) + Long_val(Field(iovec, 1));
    iov[count].iov_len = Long_val(Field(iovec, 2));
    count++;
  }
  struct msghdr msg = {0};
  msg.msg_iov = iov;
  msg.msg_iovlen = count;
  ssize_t written;
  do {
    written = sendmsg(Int_val(fd), &msg, MSG_DONTWAIT);
  } while (written < 0 && errno == EINTR);
  if (written >= 0) return Val_long(written);
  if (errno == EAGAIN || errno == EWOULDBLOCK) return Val_long(-1);
  caml_uerror("sendmsg", Nothing);
}

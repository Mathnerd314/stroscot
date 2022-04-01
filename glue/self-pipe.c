// SPDX-License-Identifier: ISC
/*
Copyright (c) 2011-2021 Laurent Bercot <ska-skaware@skarnet.org>

Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/


/*
the "self-pipe trick":  At process start time, create a pipe and set the O_NONBLOCK flag. Hold onto both ends. From the signal handler, write into the pipe (ignoring EAGAIN). In your event loop, read from the pipe in the poll/select/whatever. When there's something to read, a signal is pending. And you can dispatch the signal however you want. like signalfd but cross-platform.

but signalfd uses one less fd and signal handler, so just use that for now.
*/

extern sigset_t selfpipe_caught ;
extern int selfpipe_fd ;

int selfpipe_init (void)
{
  sigemptyset(&selfpipe_caught) ;
  selfpipe_fd = signalfd(selfpipe_fd, &selfpipe_caught, SFD_NONBLOCK | SFD_CLOEXEC) ;
  sig_blocknone() ;
  return selfpipe_fd ;
}

int selfpipe_trapset (sigset_t const *set)
{
  sigset_t old ;
  if (selfpipe_fd < 0) return (errno = EBADF, 0) ;
  if (sigprocmask(SIG_SETMASK, set, &old) < 0) return 0 ;
  if (signalfd(selfpipe_fd, set, SFD_NONBLOCK | SFD_CLOEXEC) < 0)
  {
    int e = errno ;
    sigprocmask(SIG_SETMASK, &old, 0) ;
    errno = e ;
    return 0 ;
  }
  selfpipe_caught = *set ;
  return 1 ;
}

int selfpipe_trap (int sig)
{
  sigset_t ss = selfpipe_caught ;
  sigset_t old ;
  if (selfpipe_fd < 0) return (errno = EBADF, 0) ;
  if ((sigaddset(&ss, sig) < 0) || (sigprocmask(SIG_BLOCK, &ss, &old) < 0)) return 0 ;
  if (signalfd(selfpipe_fd, &ss, SFD_NONBLOCK | SFD_CLOEXEC) < 0)
  {
    int e = errno ;
    sigprocmask(SIG_SETMASK, &old, 0) ;
    errno = e ;
    return 0 ;
  }
  selfpipe_caught = ss ;
  return 1 ;
}

int selfpipe_read (void)
{
  struct signalfd_siginfo buf ;
  ssize_t r = sanitize_read(fd_read(selfpipe_fd, (char *)&buf, sizeof(struct signalfd_siginfo))) ;
  return (r <= 0) ? r : buf.ssi_signo ;
}
void selfpipe_finish (void)
{
  int e = errno ;
  fd_close(selfpipe_fd) ;
  sigprocmask(SIG_UNBLOCK, &selfpipe_caught, 0) ;
  sigemptyset(&selfpipe_caught) ;
  selfpipe_fd = -1 ;
  errno = e ;
}

int fd = selfpipe_init() ;
int r = selfpipe_trap(SIGTERM) ;
sigset_t set ;
sigemptyset(&set) ;
sigaddset(&set, SIGTERM) ;
sigaddset(&set, SIGHUP) ;
r = selfpipe_trapset(&set) ;

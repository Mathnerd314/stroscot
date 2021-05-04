#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <jni.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <utime.h>
#include <string>
#include <vector>

#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__)

StatCommon(const char* path) {
  int r;
  int saved_errno = 0;
  struct stat statbuf;
  while ((r = ::lstat(path_chars, &statbuf)) == -1 && errno == EINTR) { }
  if (r == -1) {
    // EACCES ENOENT ENOTDIR ELOOP -> IOException
    // ENAMETOOLONGEFAULT          -> RuntimeException
    // ENOMEM                      -> OutOfMemoryError
    PostRuntimeException(env, errno, path);
  }

  return env->NewObject(
      file_status_class, method, statbuf.st_mode,
      StatSeconds(statbuf, STAT_ATIME), StatNanoSeconds(statbuf, STAT_ATIME),
      StatSeconds(statbuf, STAT_MTIME), StatNanoSeconds(statbuf, STAT_MTIME),
      StatSeconds(statbuf, STAT_CTIME), StatNanoSeconds(statbuf, STAT_CTIME),
      static_cast<long>(statbuf.st_size),
      static_cast<int>(statbuf.st_dev), static_cast<long>(statbuf.st_ino));
}

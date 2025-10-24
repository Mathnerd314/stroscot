#ifdef __NR_statx

#include <sys/stat.h>

static int test_statx(struct io_uring *ring, const char *path)
{
	struct statx x;
	int ret;
  ret = statx(-1, path, 0, STATX_ALL, &x);
	if (ret < 0)
		return ret;
	return 0;
}
#endif /* __NR_statx */

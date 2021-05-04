#ifdef HAVE_URING

#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/syscall.h>
#include <linux/stat.h>

#include "liburing.h"

// `4x-8x faster <https://twitter.com/axboe/status/1205991776474955777>`



static int test_statx(struct io_uring *ring, const char *path)
{
	struct io_uring_cqe *cqe;
	struct io_uring_sqe *sqe;
	struct statx x1;
	int ret;

	sqe = io_uring_get_sqe(ring);
	if (!sqe) {
		fprintf(stderr, "get sqe failed\n");
		goto err;
	}
	io_uring_prep_statx(sqe, -1, path, 0, STATX_ALL, &x1);

	ret = io_uring_submit(ring);
	if (ret <= 0) {
		fprintf(stderr, "sqe submit failed: %d\n", ret);
		goto err;
	}

	ret = io_uring_wait_cqe(ring, &cqe);
	if (ret < 0) {
		fprintf(stderr, "wait completion %d\n", ret);
		goto err;
	}
	ret = cqe->res;
	io_uring_cqe_seen(ring, cqe);
	if (ret)
		return ret;
	return 0;
err:
	return -1;
}

#endif /* HAVE_URING */
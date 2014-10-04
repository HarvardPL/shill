#include <sys/types.h>
#include <sys/mac.h>

#include <errno.h>
#include <fcntl.h>

#include "syscalls.h"

int mkopendirat(int fd, const char *path, mode_t mode, int flags) {
  struct mkopendirat_args args = {
	fd = fd,
	path = path,
	mode = mode,
	flags = flags
  };

  return (mac_syscall("shill", MKOPENDIRAT, &args));
}

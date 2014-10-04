#ifndef SHILL_SYSCALLS_H
#define SHILL_SYSCALLS_H

#define MKOPENDIRAT 3

struct mkopendirat_args {
  int			fd;
  const char	*path;
  mode_t		mode;
  int			flags;
};

#ifdef _KERNEL

struct thread;

int mkopendirat(struct thread *, void *);

#else /* !_KERNEL */

int mkopendirat(int fd, const char *path, mode_t mode, int flags);

#endif /* _KERNEL */

#endif /* SHILL_SYSCALLS_H */

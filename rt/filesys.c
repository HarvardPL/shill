#include <sys/stat.h>
#include <sys/types.h>
#include <sys/module.h>
#include <sys/syscall.h>
#include <sys/param.h>

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "filesys.h"
#include "syscalls.h"

#define COPYBUFSIZE (8 * 1024 * 1024)

bool shill_check_basename(const char *path) {
  return (strchr(path, '/') == NULL)
    && (strcmp(path, ".") != 0)
    && (strcmp(path, "..") != 0);
}

int shill_stdin() {
  return fcntl(STDIN_FILENO, F_DUPFD_CLOEXEC, 0);
}
int shill_stdout() {
  return fcntl(STDOUT_FILENO, F_DUPFD_CLOEXEC, 1);
}
int shill_stderr() {
  return fcntl(STDERR_FILENO, F_DUPFD_CLOEXEC, 2);
}

int shill_openfile(const char *path) {
  int fd = open(path, O_RDWR | O_CREAT | O_CLOEXEC, S_IRUSR | S_IWUSR);

  if (-1 == fd)
    fd = open(path, O_RDONLY | O_CREAT | O_CLOEXEC, S_IRUSR | S_IWUSR);

  if (-1 == fd)
    fd = open(path, O_WRONLY | O_CREAT | O_CLOEXEC, S_IRUSR | S_IWUSR);

  return fd;
}

int shill_openforexec(const char *path) {
  return open(path, O_EXEC | O_CLOEXEC);
}

int shill_opendir(const char *path) {
  return open(path, O_RDONLY | O_DIRECTORY | O_CLOEXEC);
}

int shill_cwd() {
  return open(".", O_RDONLY | O_DIRECTORY | O_CLOEXEC);
}

int shill_lookupfile(int dirfd, const char *name) {
  if (!shill_check_basename(name)) {
    errno = EINVAL;
    return -1;
  }

  int fd = openat(dirfd, name, O_RDWR | O_NOFOLLOW | O_CLOEXEC);

  if (-1 == fd)
    fd = openat(dirfd, name, O_RDONLY | O_NOFOLLOW | O_CLOEXEC);

  if (-1 == fd)
    fd = openat(dirfd, name, O_WRONLY | O_NOFOLLOW | O_CLOEXEC);

  return fd;
}

int shill_lookupforexec(int dirfd, const char *name) {
  if (!shill_check_basename(name)) {
    errno = EINVAL;
    return -1;
  }

  return openat(dirfd, name, O_EXEC | O_NOFOLLOW | O_CLOEXEC);
}

int shill_lookupdir(int dirfd, const char *name) {
  if (!shill_check_basename(name)) {
    errno = EINVAL;
    return -1;
  }

  return openat(dirfd, name, O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
}

int shill_chdir(int dirfd) {
  return fchdir(dirfd);
}

int shill_readlink(int dirfd, const char *name,
		   void (*record_link)(const char *)) {
  struct stat sb;
  char *buf;
  ssize_t len;
  int savederrno;

  if (!shill_check_basename(name)) {
    errno = EINVAL;
    return -1;
  }

  if (0 != fstatat(dirfd, name, &sb, AT_SYMLINK_NOFOLLOW)) {
    return -1;
  }

  buf = malloc(sb.st_size + 1);
  if (buf == NULL) {
    return -1;
  }
    
  len = readlinkat(dirfd, name, buf, sb.st_size);
  if (-1 == len) {
    savederrno = errno;
    free(buf);
    errno = savederrno;
    return -1;
  }

  buf[len] = 0;
  record_link(buf);
  free(buf);

  return 0;
}

int shill_addfile(int dirfd, int filefd, const char *name) {
  struct stat filest;
  int cpfd = 0;
  int savederrno;

  if (!shill_check_basename(name)) {
    errno = EINVAL;
    return -1;
  }

  if (fstat(filefd, &filest) == -1) return -1;

  // Create the new file to write on
  cpfd = openat(dirfd, name, O_RDWR | O_CREAT | O_EXCL | O_CLOEXEC,
		filest.st_mode & ~(S_ISUID | S_ISGID));
  if (cpfd == -1) return -1;

  char *buf = malloc(COPYBUFSIZE);
  if (buf == NULL) goto err;

  if (0 != lseek(filefd, 0, SEEK_SET)) goto err;

  ssize_t towrite;
  while ((towrite = read(filefd, buf, COPYBUFSIZE)) > 0) {
    ssize_t wrote;
    for (char *wbuf = buf; towrite > 0; wbuf += wrote, towrite -= wrote) {
      wrote = write(cpfd, wbuf, towrite);
      if (wrote == -1) goto err;
    }
  }

  close(cpfd);
  return cpfd;

 err:
  savederrno = errno;
  close(cpfd);
  errno = savederrno;
  return -1;
}

static int flinkat_num = -1;

int shill_addlink(int dirfd, const char *name, int filefd) {
  int modid;
  struct module_stat stat;
  
  if (!shill_check_basename(name)) {
	errno = EINVAL;
	return -1;
  }

  if (flinkat_num == -1) {
	stat.version = sizeof(stat);
	modid = modfind("sys/flinkat");
	if (modid == -1) {
	  errno = ENOSYS;
	  return -1;
	}
	if (modstat(modid, &stat) == -1) {
	  return -1;
	}
	flinkat_num = stat.data.intval;
  }

  return syscall(flinkat_num, filefd, dirfd, name);
}

static int funlinkat_num = -1;

int shill_unlinkfile(int dirfd, const char *name, int tgtfd) {
  int modid;
  struct module_stat stat;

  if (!shill_check_basename(name)) {
    errno = EINVAL;
    return -1;
  }

  if (funlinkat_num == -1) {
	stat.version = sizeof(stat);
	modid = modfind("sys/funlinkat");
	if (modid == -1) {
	  errno = ENOSYS;
	  return -1;
	}
	if (modstat(modid, &stat) == -1) {
	  return -1;
	}
	funlinkat_num = stat.data.intval;
  }

  return syscall(funlinkat_num, dirfd, name, 0, tgtfd);
}

int shill_unlinkdir(int dirfd, const char *name, int tgtfd) {
  int modid;
  struct module_stat stat;

  if (!shill_check_basename(name)) {
    errno = EINVAL;
    return -1;
  }

  if (funlinkat_num == -1) {
	stat.version = sizeof(stat);
	modid = modfind("sys/funlinkat");
	if (modid == -1) {
	  errno = ENOSYS;
	  return -1;
	}
	if (modstat(modid, &stat) == -1) {
	  return -1;
	}
	funlinkat_num = stat.data.intval;
  }

  return syscall(funlinkat_num, dirfd, name, AT_REMOVEDIR, tgtfd);
}

int shill_addsymlink(int dirfd, const char *name, const char *path) {
  if (!shill_check_basename(name)) {
    errno = EINVAL;
    return -1;
  }

  return symlinkat(path, dirfd, name);
}

int shill_rmsymlink(int dirfd, const char *name) {
  if (!shill_check_basename(name)) {
    errno = EINVAL;
    return -1;
  }

  return unlinkat(dirfd, name, 0);
}

int shill_rename(int olddir, const char *old, int newdir, const char *new) {
  if (!(shill_check_basename(old) && shill_check_basename(new))) {
    errno = EINVAL;
    return -1;
  }

  return renameat(olddir, old, newdir, new);
}

int shill_createfile(int dirfd, const char *name) {
  if (!shill_check_basename(name)) {
    errno = EINVAL;
    return -1;
  }

  return openat(dirfd, name,
		O_RDWR | O_CREAT | O_EXCL | O_CLOEXEC,
		S_IRWXU | S_IRWXG | S_IRWXO);
}

int shill_createdir(int dirfd, const char *name) {
  if (!shill_check_basename(name)) {
    errno = EINVAL;
    return -1;
  }

  return (mkopendirat(dirfd, name, S_IRWXU | S_IRWXG | S_IRWXO, O_CLOEXEC));
}

size_t shill_filelength(int filefd) {
  struct stat stat_buf;
  if (fstat(filefd, &stat_buf) == -1) return (size_t)-1;
  if (stat_buf.st_size < 0) {
    errno = EDOM;
    return (size_t)-1;
  }
  return (size_t)stat_buf.st_size;
}

static int path_num = -1;

int shill_path(int fd, void (*record_path)(const char *)) {
  int modid;
  struct module_stat stat;
  char buf[MAXPATHLEN + 1];

  if (path_num == -1) {
	stat.version = sizeof(stat);
	modid = modfind("sys/shill_path");
	if (modid == -1) {
	  errno = ENOSYS;
	  return -1;
	}
	if (modstat(modid, &stat) == -1) {
	  return -1;
	}
	path_num = stat.data.intval;
  }

  memset(buf, 0, MAXPATHLEN + 1);
  if (-1 != syscall(path_num, fd, buf, MAXPATHLEN)) {
    record_path(buf);
    return 0;
  }
  
  return -1;
}

int shill_read(int fd, char *buf, size_t len) {
  if (0 != lseek(fd, 0, SEEK_SET))
    return -1;
  size_t count = 0;
  while (count < len) {
    ssize_t r = read(fd, &buf[count], len-count);
    if (r < 0) return -1;
    count += (size_t) r;
  }
  return 0;
}

int dowrite(int fd, char *buf, size_t len) {
  size_t count = 0;
  while (count < len) {
    ssize_t w = write(fd, &buf[count], len-count);
    if (w < 0) return -1;
    count += (size_t) w;
  }
  return 0;
}

int shill_write(int fd, char *buf, size_t len) {
  int flags = fcntl(fd, F_GETFL);
  if (flags == -1) return -1;

  int ret = fcntl(fd, F_SETFL, flags & ~O_APPEND);
  if (ret == -1) return -1;

  if (-1 == ftruncate(fd, 0)) return -1;
  
  return dowrite(fd, buf, len);
}

int shill_append(int fd, char *buf, size_t len) {
  int flags = fcntl(fd, F_GETFL);
  if (flags == -1) return -1;

  int ret = fcntl(fd, F_SETFL, flags | O_APPEND);
  if (ret == -1) return -1;

  return dowrite(fd, buf, len);
}

int shill_contents(int fd, void (*record_entry)(const char *)) {
  /* Create a copy so we don't end up closing fd */
  int dirfd = dup(fd);
  if (dirfd == -1) return -1;

  DIR *dir = fdopendir(dirfd);
  int err = errno;
  if (dir == NULL) {
    close(dirfd);
    errno = err;
    return -1;
  }

  /* clear errno so that we can check for error conditions */
  errno = 0;
  struct dirent *entry;
  while ((entry = readdir(dir)) != NULL) {
    record_entry(entry->d_name);
    errno = 0;
  }

  int last_errno = errno;
  closedir(dir);
  errno = last_errno;
  return (errno != 0) ? -1 : 0;
}

int shill_pipe(int filedes[2]) {
  if (-1 == pipe(filedes))
    return -1;

  if ((-1 == fcntl(filedes[0], F_SETFD, FD_CLOEXEC))
      || (-1 == fcntl(filedes[1], F_SETFD, FD_CLOEXEC))) {
    int err = errno;
    close(filedes[0]);
    close(filedes[1]);
    errno = err;
    return -1;
  }

  return 0;
}

int shill_writepipe(int fd, char *buf, size_t len) {
  size_t count = 0;
  while (count < len) {
    ssize_t w = write(fd, &buf[count], len-count);
    if (w < 0) return -1;
    count += (size_t) w;
  }
  return 0;
}

int shill_readpipe(int fd, char *buf, size_t len) {
  return read(fd, buf, len);
}

int shill_stat(int fd, struct shillstat *ssp) {
  struct stat sb;
  if (0 != fstat(fd, &sb)) {
	return -1;
  }

  ssp->size = sb.st_size;
  ssp->access_sec = sb.st_atim.tv_sec;
  ssp->access_nsec = sb.st_atim.tv_nsec;
  ssp->modify_sec = sb.st_mtim.tv_sec;
  ssp->modify_nsec = sb.st_mtim.tv_nsec;
  ssp->change_sec = sb.st_ctim.tv_sec;
  ssp->change_nsec = sb.st_ctim.tv_nsec;
  ssp->create_sec = sb.st_birthtim.tv_sec;
  ssp->create_nsec = sb.st_birthtim.tv_nsec;
  ssp->uid = sb.st_uid;
  ssp->gid = sb.st_gid;
  ssp->perms = sb.st_mode & (S_IRWXU | S_IRWXG | S_IRWXO);
  ssp->suid = sb.st_mode & S_ISUID;
  ssp->sgid = sb.st_mode & S_ISGID;

  return 0;
}

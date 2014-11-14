#ifndef FILESYS_H
#define FILESYS_H

#include <stdbool.h>
#include <stddef.h>

bool shill_check_basename(const char *path);

/* root file system access */
int shill_openfile(const char *path);
int shill_openforexec(const char *path);
int shill_opendir(const char *path);

/* directory manipulation */
int shill_lookupfile(int dirfd, const char *name);
int shill_lookupforexec(int dirfd, const char *name);
int shill_lookupdir(int dirfd, const char *name);
int shill_chdir(int dirfd);
int shill_readlink(int dirfd, const char *name,
		   void (*record_link)(const char *));
int shill_addlink(int dirfd, const char *name, int filefd);
int shill_addsymlink(int dirfd, const char *name, const char *path);
int shill_rmsymlink(int dirfd, const char *name);
int shill_unlinkfile(int dirfd, const char *name, int tgt);
int shill_unlinkdir(int dirfd, const char *name, int tgt);
int shill_rename(int dirfd, const char *old, int newdirfd, const char *new);
int shill_addfile(int dirfd, int filefd, const char *name);
int shill_createfile(int dirfd, const char *name);
int shill_createdir(int dirfd, const char *name);
int shill_contents(int fd, void (*record_entry)(const char *));
int shill_path(int fd, void (*record_path)(const char *));

/* file manipulation */
size_t shill_filelength(int fd);
int shill_read(int fd, char *buf, size_t len);
int shill_write(int fd, char *buf, size_t len);
int shill_append(int fd, char *buf, size_t len);

/* pipes */
int shill_pipe(int filedes[2]);
int shill_stdin();
int shill_stdout();
int shill_stderr();

struct shillstat {
  uint64_t size;
  uint64_t access_sec;
  uint64_t access_nsec;
  uint64_t modify_sec;
  uint64_t modify_nsec;
  uint64_t change_sec;
  uint64_t change_nsec;
  uint64_t create_sec;
  uint64_t create_nsec;
  uint32_t uid;
  uint32_t gid;
  uint32_t perms;
  bool     suid;
  bool     sgid;
};

int shill_stat(int fd, struct shillstat *);

#endif /* FILESYS_H */

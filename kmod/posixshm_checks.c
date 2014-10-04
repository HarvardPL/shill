#include <sys/param.h>
#include <sys/kernel.h>
#include <sys/lock.h>
#include <sys/mac.h>
#include <sys/queue.h>
#include <sys/systm.h>
#include <sys/ucred.h>
#include <sys/mman.h>

#include "shill.h"
#include "shilldev.h"
#include "posixshm_checks.h"

int
shill_posixshm_check_create(struct ucred *cred,
                              const char *path) {
  return shill_deny_all("posixshm_create", cred, NULL);
}

int
shill_posixshm_check_mmap(struct ucred *cred,
                          struct shmfd *shmfd, struct label *shmlabel, int prot,
                          int flags) {
  return shill_deny_all("posixshm_mmap", cred, NULL);
}

int
shill_posixshm_check_open(struct ucred *cred,
                          struct shmfd *shmfd, struct label *shmlabel,
                          accmode_t accmode) {
  return shill_deny_all("posixshm_open", cred, NULL);
}

int
shill_posixshm_check_setmode(struct ucred *cred,
                             struct shmfd *shmfd, struct label *shmlabel,
                             mode_t mode) {
  return shill_deny_all("posixshm_setmode", cred, NULL);
}

int
shill_posixshm_check_setowner(struct ucred *cred,
                              struct shmfd *shmfd, struct label *shmlabel,
                              uid_t uid, gid_t gid) {
  return shill_deny_all("posixshm_setowner", cred, NULL);
}

int
shill_posixshm_check_stat(struct ucred *active_cred,
                          struct ucred *file_cred, struct shmfd *shmfd,
                          struct label *shmlabel) {
  return shill_deny_all("posixshm_stat", active_cred, NULL);
}

int
shill_posixshm_check_truncate(struct ucred *active_cred,
                              struct ucred *file_cred, struct shmfd *shmfd,
                              struct label *shmlabel) {
  return shill_deny_all("posixshm_truncate", active_cred, NULL);
}

int
shill_posixshm_check_unlink(struct ucred *cred,
                            struct shmfd *shmfd, struct label *shmlabel) {
  return shill_deny_all("posixshm_unlink", cred, NULL);
}

#include <sys/param.h>
#include <sys/kernel.h>
#include <sys/systm.h>
#include <sys/ucred.h>
#include <sys/shm.h>

#include "shill.h"
#include "sysvshm_checks.h"

int
shill_sysvshm_check_shmat(struct ucred *cred,
                          struct shmid_kernel *shmsegptr,
                          struct label *shmseglabel, int shmflg) {
  return shill_deny_all("sysvshm_shmat", cred, shmsegptr);
}

int
shill_sysvshm_check_shmctl(struct ucred *cred,
                           struct shmid_kernel *shmsegptr,
                           struct label *shmseglabel, int cmd) {
  return shill_deny_all("sysvshm_shmctl", cred, shmsegptr);
}

int
shill_sysvshm_check_shmdt(struct ucred *cred,
                          struct shmid_kernel *shmsegptr,
                          struct label *shmseglabel) {
  return shill_deny_all("sysvshm_shmdt", cred, shmsegptr);
}

int
shill_sysvshm_check_shmget(struct ucred *cred,
                           struct shmid_kernel *shmsegptr,
                           struct label *shmseglabel, int shmflg) {
  return shill_deny_all("sysvshm_shmget", cred, shmsegptr);
}

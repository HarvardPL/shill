#include <sys/param.h>
#include <sys/kernel.h>
#include <sys/systm.h>
#include <sys/ucred.h>
#include <sys/sem.h>

#include "shill.h"
#include "sysvsem_checks.h"

int
shill_sysvsem_check_semctl(struct ucred *cred,
                           struct semid_kernel *semakptr,
                           struct label *semaklabel,
                           int cmd) {
  return shill_deny_all("sysvsem_semctl", cred, semakptr);
}

int
shill_sysvsem_check_semget(struct ucred *cred,
                           struct semid_kernel *semakptr,
                           struct label *semaklabel) {
  return shill_deny_all("sysvsem_semget", cred, semakptr);
}

int
shill_sysvsem_check_semop(struct ucred *cred,
                          struct semid_kernel *semakptr,
                          struct label *semaklabel,
                          size_t accesstype) {
  return shill_deny_all("sysvsem_semop", cred, semakptr);
}

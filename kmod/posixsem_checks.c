#include <sys/param.h>
#include <sys/kernel.h>
#include <sys/lock.h>
#include <sys/mac.h>
#include <sys/queue.h>
#include <sys/systm.h>
#include <sys/ucred.h>
#include <sys/ksem.h>

#include "shill.h"
#include "shilldev.h"
#include "posixsem_checks.h"

int
shill_posixsem_check_getvalue(struct ucred *active_cred,
                              struct ucred *file_cred, struct ksem *ks,
                              struct label *kslabel) {
  return shill_deny_all("posixsem_getvalue", active_cred, ks);
}

int
shill_posixsem_check_open(struct ucred *cred,
                          struct ksem *ks, struct label *kslabel) {
  return shill_deny_all("posixsem_open", cred, ks);
}

int
shill_posixsem_check_post(struct ucred *active_cred,
                          struct ucred *file_cred, struct ksem *ks,
                          struct label *kslabel) {
  return shill_deny_all("posixsem_post", active_cred, ks);
}

int
shill_posixsem_check_setmode(struct ucred *cred,
                             struct ksem *ks, struct label *shmlabel,
                             mode_t mode) {
  return shill_deny_all("posixsem_setmode", cred, ks);
}

int
shill_posixsem_check_setowner(struct ucred *cred,
                              struct ksem *ks, struct label *shmlabel,
                              uid_t uid, gid_t gid) {
  return shill_deny_all("posixsem_setowner", cred, ks);
}

int
shill_posixsem_check_stat(struct ucred *active_cred,
                          struct ucred *file_cred, struct ksem *ks,
                          struct label *kslabel) {
  return shill_deny_all("posixsem_stat", active_cred, ks);
}

int
shill_posixsem_check_unlink(struct ucred *cred,
                            struct ksem *ks, struct label *kslabel) {
  return shill_deny_all("posixsem_unlink", cred, ks);
}

int
shill_posixsem_check_wait(struct ucred *active_cred,
                          struct ucred *file_cred, struct ksem *ks,
                          struct label *kslabel) {
  return shill_deny_all("posixsem_wait", active_cred, ks);
}

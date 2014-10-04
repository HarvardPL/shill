#include <sys/param.h>
#include <sys/lock.h>
#include <sys/mutex.h>
#include <sys/kernel.h>
#include <sys/module.h>
#include <sys/mman.h>
#include <sys/namei.h>
#include <sys/queue.h>
#include <sys/systm.h>
#include <sys/ucred.h>
#include <sys/queue.h>
#include <sys/sysctl.h>
#include <sys/syslog.h>

#include "shill.h"
#include "shilldev.h"
#include "cred_checks.h"

void
shill_cred_associate_nfsd(struct ucred *cred) {
  // Intentionally blank
}

int
shill_cred_check_setaudit(struct ucred *cred,
                          struct auditinfo *ai) {
  return shill_allow_all("cred_setaudit", cred, ai);
}

int
shill_cred_check_setaudit_addr(struct ucred *cred,
                               struct auditinfo_addr *aia) {
  return shill_allow_all("cred_setaudit_addr", cred, aia);
}

int
shill_cred_check_setauid(struct ucred *cred, uid_t auid) {
  return shill_allow_all("cred_setauid", cred, cred);
}

int
shill_cred_check_setegid(struct ucred *cred, gid_t egid) {
  return shill_allow_all("cred_setegid", cred, cred);
}

int
shill_cred_check_seteuid(struct ucred *cred, uid_t euid) {
  return shill_allow_all("cred_seteuid", cred, cred);
}

int
shill_cred_check_setgid(struct ucred *cred, gid_t gid) {
  return shill_allow_all("cred_setgid", cred, cred);
}

int
shill_cred_check_setgroups(struct ucred *cred, int ngroups,
                           gid_t *gidset) {
  return shill_allow_all("cred_setgroups", cred, cred);
}

int
shill_cred_check_setregid(struct ucred *cred, gid_t rgid,
                          gid_t egid) {
  return shill_allow_all("cred_setregid", cred, cred);
}

int
shill_cred_check_setresgid(struct ucred *cred, gid_t rgid,
                           gid_t egid, gid_t sgid) {
  return shill_allow_all("cred_setresgid", cred, cred);
}

int
shill_cred_check_setresuid(struct ucred *cred, uid_t ruid,
                           uid_t euid, uid_t suid) {
  return shill_allow_all("cred_setresuid", cred, cred);
}

int
shill_cred_check_setreuid(struct ucred *cred, uid_t ruid,
                          uid_t euid) {
  return shill_allow_all("cred_setreuid", cred, cred);
}

int
shill_cred_check_setuid(struct ucred *cred, uid_t uid) {
  return shill_allow_all("cred_setuid", cred, cred);
}

int
shill_cred_check_visible(struct ucred *cr1,
                         struct ucred *cr2) {
  struct shill_session *subject = shill_get_session(cr1);
  if (subject == NULL) {
    // Don't enforce restrictions on un-sandboxed processes
    return 0;
  }

  // A process can interact with a process in the same session
  // or in a descendent session.
  struct shill_session *object = shill_get_session(cr2);
  while (object != NULL) {
    if (subject == object) { return 0; }
    object = shill_session_parent(object);
  }

  if (shill_session_debug(subject)) {
    shilld_log((uintptr_t)subject, "Permitting check_visible on %lx for debugging",
               (uintptr_t)cr2);
    return 0;
  } else {
    shilld_log((uintptr_t)subject, "Denied check_visisble on %lx",
               (uintptr_t)cr2);
    return EACCES;
  }
}

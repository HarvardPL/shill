#include <sys/param.h>
#include <sys/kernel.h>
#include <sys/lock.h>
#include <sys/systm.h>
#include <sys/ucred.h>
#include <sys/vnode.h>
#include <sys/sysctl.h>

#include "shill.h"
#include "system_checks.h"

int
shill_system_check_acct(struct ucred *cred,
                        struct vnode *vp, struct label *vplabel) {
  return shill_deny_all("system_acct", cred, vp);
}

int
shill_system_check_audit(struct ucred *cred, void *record,
                         int length) {
  return shill_deny_all("system_audit", cred, NULL);
}

int
shill_system_check_auditctl(struct ucred *cred,
                            struct vnode *vp, struct label *vplabel) {
  return shill_deny_all("system_auditctl", cred, NULL);
}

int
shill_system_check_auditon(struct ucred *cred, int cmd) {
  return shill_deny_all("system_auditon", cred, NULL);
}

int
shill_system_check_reboot(struct ucred *cred, int howto) {
  return shill_deny_all("system_reboot", cred, NULL);
}

int
shill_system_check_swapon(struct ucred *cred,
                          struct vnode *vp, struct label *vplabel) {
  return shill_deny_all("system_swapon", cred, NULL);
}

int
shill_system_check_swapoff(struct ucred *cred,
                           struct vnode *vp, struct label *vplabel) {
  return shill_deny_all("system_swapoff", cred, NULL);
}

int
shill_system_check_sysctl(struct ucred *cred,
                          struct sysctl_oid *oidp, void *arg1, int arg2,
                          struct sysctl_req *req) {
  return shill_allow_all("system_sysctl", cred, NULL);
}

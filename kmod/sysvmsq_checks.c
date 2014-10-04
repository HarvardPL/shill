#include <sys/param.h>
#include <sys/kernel.h>
#include <sys/systm.h>
#include <sys/ucred.h>
#include <sys/msg.h>

#include "shill.h"
#include "sysvmsq_checks.h"

int
shill_sysvmsq_check_msgmsq(struct ucred *cred,
                           struct msg *msgptr, struct label *msglabel,
                           struct msqid_kernel *msqkptr,
                           struct label *msqklabel) {
  return shill_deny_all("sysvmsq_msgmsq", cred, NULL);
}

int
shill_sysvmsq_check_msgrcv(struct ucred *cred,
                           struct msg *msgptr, struct label *msglabel) {
  return shill_deny_all("sysvmsq_msgrcv", cred, NULL);
}

int
shill_sysvmsq_check_msgrmid(struct ucred *cred,
                            struct msg *msgptr, struct label *msglabel) {
  return shill_deny_all("sysvmsq_msgrmid", cred, NULL);
}

int
shill_sysvmsq_check_msqget(struct ucred *cred,
                           struct msqid_kernel *msqkptr,
                           struct label *msqklabel) {
  return shill_deny_all("sysvmsq_msqget", cred, NULL);
}

int
shill_sysvmsq_check_msqctl(struct ucred *cred,
                           struct msqid_kernel *msqkptr,
                           struct label *msqklabel,
                           int cmd) {
  return shill_deny_all("sysvmsq_msqctl", cred, NULL);
}

int
shill_sysvmsq_check_msqrcv(struct ucred *cred,
                           struct msqid_kernel *msqkptr,
                           struct label *msqklabel) {
  return shill_deny_all("sysvmsq_msqrcv", cred, NULL);
}
int
shill_sysvmsq_check_msqsnd(struct ucred *cred,
                           struct msqid_kernel *msqkptr,
                           struct label *msqklabel) {
  return shill_deny_all("sysvmsq_msqsnd", cred, NULL);
}

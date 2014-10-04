#include <sys/param.h>
#include <sys/kernel.h>
#include <sys/systm.h>
#include <sys/ucred.h>
#include <sys/vnode.h>

#include "shill.h"
#include "kld_checks.h"

int
shill_kld_check_load(struct ucred *cred, struct vnode *vp,
                     struct label *vplabel) {
  return shill_deny_all("kld_load", cred, vp);
}

int
shill_kld_check_stat(struct ucred *cred) {
  return shill_deny_all("kld_stat", cred, NULL);
}

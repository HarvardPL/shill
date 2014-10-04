#include <sys/param.h>
#include <sys/kernel.h>
#include <sys/systm.h>
#include <sys/ucred.h>
#include <sys/mount.h>

#include "shill.h"
#include "mount_checks.h"

int
shill_mount_check_stat(struct ucred *cred,
                         struct mount *mp, struct label *mplabel) {
  return shill_allow_all("mount_stat", cred, mp);
}

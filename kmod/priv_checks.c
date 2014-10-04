#include <sys/param.h>
#include <sys/kernel.h>
#include <sys/lock.h>
#include <sys/mac.h>
#include <sys/queue.h>
#include <sys/systm.h>
#include <sys/ucred.h>
#include <sys/priv.h>

#include "shill.h"
#include "priv_checks.h"

int
shill_priv_check(struct ucred *cred, int priv) {
  // XXX see if there are privileges we want to deny
  return 0;
}

int
shill_priv_grant(struct ucred *cred, int priv) {
  // XXX see if there are privileges we want to grant
  return 0;
}

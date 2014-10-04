#include <sys/param.h>
#include <sys/kernel.h>
#include <sys/systm.h>
#include <sys/ucred.h>

#include "shill.h"
#include "kenv_checks.h"

int
shill_kenv_check_dump(struct ucred *cred) {
  return shill_deny_all("kenv_dump", cred, NULL);
}

int
shill_kenv_check_get(struct ucred *cred, char *name) {
  return shill_deny_all("kenv_get", cred, NULL);
}

int
shill_kenv_check_set(struct ucred *cred, char *name,
                     char *value) {
  return shill_deny_all("kenv_set", cred, NULL);
}

int
shill_kenv_check_unset(struct ucred *cred, char *name) {
  return shill_deny_all("kenv_unset", cred, NULL);
}

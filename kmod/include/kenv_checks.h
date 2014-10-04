#ifndef KENV_CHECKS_H
#define KENV_CHECKS_H

int
shill_kenv_check_dump(struct ucred *cred);
int
shill_kenv_check_get(struct ucred *cred, char *name);
int
shill_kenv_check_set(struct ucred *cred, char *name,
                     char *value);
int
shill_kenv_check_unset(struct ucred *cred, char *name);

#endif // KENV_CHECKS_H

#ifndef PRIV_CHECKS_H
#define PRIV_CHECKS_H

int
shill_priv_check(struct ucred *cred, int priv);
int
shill_priv_grant(struct ucred *cred, int priv);

#endif // PRIV_CHECKS_H

#ifndef KLD_CHECKS_H
#define KLD_CHECKS_H

int
shill_kld_check_load(struct ucred *cred, struct vnode *vp,
                     struct label *vplabel);
int
shill_kld_check_stat(struct ucred *cred);

#endif // KLD_CHECKS_H

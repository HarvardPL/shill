#ifndef SYSTEM_CHECKS_H
#define SYSTEM_CHECKS_H

int
shill_system_check_acct(struct ucred *cred,
                        struct vnode *vp, struct label *vplabel);
int
shill_system_check_audit(struct ucred *cred, void *record,
                         int length);
int
shill_system_check_auditctl(struct ucred *cred,
                            struct vnode *vp, struct label *vplabel);
int
shill_system_check_auditon(struct ucred *cred, int cmd);
int
shill_system_check_reboot(struct ucred *cred, int howto);
int
shill_system_check_swapon(struct ucred *cred,
                          struct vnode *vp, struct label *vplabel);
int
shill_system_check_swapoff(struct ucred *cred,
                           struct vnode *vp, struct label *vplabel);
int
shill_system_check_sysctl(struct ucred *cred,
		    struct sysctl_oid *oidp, void *arg1, int arg2,
		    struct sysctl_req *req);

#endif // SYSTEM_CHECKS_H

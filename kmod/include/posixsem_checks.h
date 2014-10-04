#ifndef POSIXSEM_CHECKS_H
#define POSIXSEM_CHECKS_H

int
shill_posixsem_check_getvalue(struct ucred *active_cred,
                              struct ucred *file_cred, struct ksem *ks,
                              struct label *kslabel);
int
shill_posixsem_check_open(struct ucred *cred,
                          struct ksem *ks, struct label *kslabel);
int
shill_posixsem_check_post(struct ucred *active_cred,
                          struct ucred *file_cred, struct ksem *ks,
                          struct label *kslabel);
int
shill_posixsem_check_setmode(struct ucred *cred,
                             struct ksem *ks, struct label *shmlabel,
                             mode_t mode);
int
shill_posixsem_check_setowner(struct ucred *cred,
                              struct ksem *ks, struct label *shmlabel,
                              uid_t uid, gid_t gid);
int
shill_posixsem_check_stat(struct ucred *active_cred,
                          struct ucred *file_cred, struct ksem *ks,
                          struct label *kslabel);
int
shill_posixsem_check_unlink(struct ucred *cred,
                            struct ksem *ks, struct label *kslabel);
int
shill_posixsem_check_wait(struct ucred *active_cred,
                          struct ucred *file_cred, struct ksem *ks,
                          struct label *kslabel);
void
shill_posixsem_create(struct ucred *cred,
                      struct ksem *ks, struct label *kslabel);

#endif // POSIXSEM_CHECKS_H

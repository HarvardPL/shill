#ifndef SYSVSEM_CHECKS_H
#define SYSVSEM_CHECKS_H

int
shill_sysvsem_check_semctl(struct ucred *cred,
                           struct semid_kernel *semakptr,
                           struct label *semaklabel,
                           int cmd);
int
shill_sysvsem_check_semget(struct ucred *cred,
                           struct semid_kernel *semakptr,
                           struct label *semaklabel);
int
shill_sysvsem_check_semop(struct ucred *cred,
                          struct semid_kernel *semakptr,
                          struct label *semaklabel,
                          size_t accesstype);

#endif // SYSVSEM_CHECKS_H

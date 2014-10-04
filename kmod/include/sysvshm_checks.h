#ifndef SYSVSHM_CHECKS_H
#define SYSVSHM_CHECKS_H

int
shill_sysvshm_check_shmat(struct ucred *cred,
                          struct shmid_kernel *shmsegptr,
                          struct label *shmseglabel, int shmflg);
int
shill_sysvshm_check_shmctl(struct ucred *cred,
                           struct shmid_kernel *shmsegptr,
                           struct label *shmseglabel, int cmd);
int
shill_sysvshm_check_shmdt(struct ucred *cred,
                          struct shmid_kernel *shmsegptr,
                          struct label *shmseglabel);
int
shill_sysvshm_check_shmget(struct ucred *cred,
                           struct shmid_kernel *shmsegptr,
                           struct label *shmseglabel, int shmflg);
void
shill_sysvshm_cleanup(struct label *shmlabel);
void
shill_sysvshm_create(struct ucred *cred,
                     struct shmid_kernel *shmsegptr, struct label *shmlabel);
void
shill_sysvshm_destroy_label(struct label *label);
void
shill_sysvshm_init_label(struct label *label);

#endif // SYSVSHM_CHECKS_H

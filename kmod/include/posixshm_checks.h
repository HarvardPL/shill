#ifndef POSIXSHM_CHECKS_H
#define POSIXSHM_CHECKS_H


int
shill_posixshm_check_create(struct ucred *cred,
                            const char *path);
int
shill_posixshm_check_mmap(struct ucred *cred,
                          struct shmfd *shmfd, struct label *shmlabel, int prot,
                          int flags);
int
shill_posixshm_check_open(struct ucred *cred,
                          struct shmfd *shmfd, struct label *shmlabel,
                          accmode_t accmode);
int
shill_posixshm_check_setmode(struct ucred *cred,
                             struct shmfd *shmfd, struct label *shmlabel,
                             mode_t mode);
int
shill_posixshm_check_setowner(struct ucred *cred,
                              struct shmfd *shmfd, struct label *shmlabel,
                              uid_t uid, gid_t gid);
int
shill_posixshm_check_stat(struct ucred *active_cred,
                          struct ucred *file_cred, struct shmfd *shmfd,
                          struct label *shmlabel);
int
shill_posixshm_check_truncate(struct ucred *active_cred,
                              struct ucred *file_cred, struct shmfd *shmfd,
                              struct label *shmlabel);
int
shill_posixshm_check_unlink(struct ucred *cred,
                            struct shmfd *shmfd, struct label *shmlabel);
void
shill_posixshm_create(struct ucred *cred,
                      struct shmfd *shmfd, struct label *shmlabel);
void
shill_posixshm_destroy_label(struct label *label);
void
shill_posixshm_init_label(struct label *label);

#endif // POSIXSHM_CHECKS_H

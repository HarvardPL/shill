#ifndef SYSVMSQ_CHECKS_H
#define SYSVMSQ_CHECKS_H

int
shill_sysvmsq_check_msgmsq(struct ucred *cred,
                           struct msg *msgptr, struct label *msglabel,
                           struct msqid_kernel *msqkptr,
                           struct label *msqklabel);
int
shill_sysvmsq_check_msgrcv(struct ucred *cred,
                           struct msg *msgptr, struct label *msglabel);
int
shill_sysvmsq_check_msgrmid(struct ucred *cred,
                            struct msg *msgptr, struct label *msglabel);
int
shill_sysvmsq_check_msqget(struct ucred *cred,
                           struct msqid_kernel *msqkptr,
                           struct label *msqklabel);
int
shill_sysvmsq_check_msqctl(struct ucred *cred,
                           struct msqid_kernel *msqkptr,
                           struct label *msqklabel,
                           int cmd);
int
shill_sysvmsq_check_msqrcv(struct ucred *cred,
                           struct msqid_kernel *msqkptr,
                           struct label *msqklabel);
int
shill_sysvmsq_check_msqsnd(struct ucred *cred,
                           struct msqid_kernel *msqkptr,
                           struct label *msqklabel);
void
shill_sysvmsq_cleanup(struct label *msqlabel);
void
shill_sysvmsq_create(struct ucred *cred,
                     struct msqid_kernel *msqkptr, struct label *msqlabel);
void
shill_sysvmsq_destroy_label(struct label *label);
void
shill_sysvmsq_init_label(struct label *label);

#endif // SYSVMSQ_CHECKS_H

#ifndef SYSVMSG_CHECKS_H
#define SYSVMSG_CHECKS_H

void
shill_sysvmsg_cleanup(struct label *msglabel);
void
shill_sysvmsg_create(struct ucred *cred,
                     struct msqid_kernel *msqkptr, struct label *msqlabel,
                     struct msg *msgptr, struct label *msglabel);
void
shill_sysvmsg_destroy_label(struct label *label);
void
shill_sysvmsg_init_label(struct label *label);

#endif // SYSVMSG_CHECKS_H

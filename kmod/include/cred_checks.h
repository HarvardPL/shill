#ifndef CRED_CHECKS_H
#define CRED_CHECKS_H

void
shill_cred_associate_nfsd(struct ucred *cred);
int
shill_cred_check_relabel(struct ucred *cred,
                         struct label *newlabel);
int
shill_cred_check_setaudit(struct ucred *cred,
                          struct auditinfo *ai);
int
shill_cred_check_setaudit_addr(struct ucred *cred,
                               struct auditinfo_addr *aia);
int
shill_cred_check_setauid(struct ucred *cred, uid_t auid);
int
shill_cred_check_setegid(struct ucred *cred, gid_t egid);
int
shill_cred_check_seteuid(struct ucred *cred, uid_t euid);
int
shill_cred_check_setgid(struct ucred *cred, gid_t gid);
int
shill_cred_check_setgroups(struct ucred *cred, int ngroups,
                           gid_t *gidset);
int
shill_cred_check_setregid(struct ucred *cred, gid_t rgid,
                          gid_t egid);
int
shill_cred_check_setresgid(struct ucred *cred, gid_t rgid,
                           gid_t egid, gid_t sgid);
int
shill_cred_check_setresuid(struct ucred *cred, uid_t ruid,
                           uid_t euid, uid_t suid);
int
shill_cred_check_setreuid(struct ucred *cred, uid_t ruid,
                          uid_t euid);
int
shill_cred_check_setuid(struct ucred *cred, uid_t uid);
int
shill_cred_check_visible(struct ucred *cr1,
                         struct ucred *cr2);
void
shill_cred_copy_label(struct label *src,
                      struct label *dest);
void
shill_cred_create_init(struct ucred *cred);
void
shill_cred_create_swapper(struct ucred *cred);
void
shill_cred_destroy_label(struct label *label);
int
shill_cred_externalize_label(struct label *label,
                             char *element_name,
                             struct sbuf *sb, int *claimed);
void
shill_cred_init_label(struct label *label);
int
shill_cred_internalize_label(struct label *label,
                             char *element_name,
                             char *element_data, int *claimed);
void
shill_cred_relabel(struct ucred *cred,
                   struct label *newlabel);

#endif // CRED_CHECKS_H

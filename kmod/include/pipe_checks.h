#ifndef PIPE_CHECKS_H
#define PIPE_CHECKS_H

struct sbuf;

int	shill_pipe_check_ioctl(struct ucred *cred,
                               struct pipepair *pp, struct label *pplabel,
                               unsigned long cmd, void *data);
int	shill_pipe_check_poll(struct ucred *cred,
                              struct pipepair *pp, struct label *pplabel);
int	shill_pipe_check_read(struct ucred *cred,
                              struct pipepair *pp, struct label *pplabel);
int	shill_pipe_check_relabel(struct ucred *cred,
                                 struct pipepair *pp, struct label *pplabel,
                                 struct label *newlabel);
int	shill_pipe_check_stat(struct ucred *cred,
                              struct pipepair *pp, struct label *pplabel);
int	shill_pipe_check_write(struct ucred *cred,
                               struct pipepair *pp, struct label *pplabel);
void	shill_pipe_create(struct ucred *cred, struct pipepair *pp,
                          struct label *pplabel);
void	shill_pipe_destroy_label(struct label *label);
void	shill_pipe_init_label(struct label *label);
void	shill_pipe_copy_label(struct label *src, struct label *dst);
int	shill_pipe_internalize_label(struct label *label,
                                     char *element_name,
                                     char *element_data,
                                     int *claimed);
int	shill_pipe_externalize_label(struct label *label,
                                     char *element_name,
                                     struct sbuf *sb, int *claimed);
void	shill_pipe_relabel(struct ucred *cred, struct pipepair *pp,
                           struct label *oldlabel, struct label *newlabel);

#endif /* PIPE_CHECKS_H */

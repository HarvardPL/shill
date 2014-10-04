#ifndef INPCB_CHECKS_H
#define INPCB_CHECKS_H

int
shill_inpcb_check_deliver(struct inpcb *inp,
                          struct label *inplabel, struct mbuf *m,
                          struct label *mlabel);
int
shill_inpcb_check_visible(struct ucred *cred,
                          struct inpcb *inp, struct label *inplabel);
void
shill_inpcb_create(struct socket *so,
                   struct label *solabel, struct inpcb *inp,
                   struct label *inplabel);
void
shill_inpcb_create_mbuf(struct inpcb *inp,
                        struct label *inplabel, struct mbuf *m,
                        struct label *mlabel);
void
shill_inpcb_destroy_label(struct label *label);
int
shill_inpcb_init_label(struct label *label, int flag);
void
shill_inpcb_sosetlabel(struct socket *so,
		    struct label *label, struct inpcb *inp,
		    struct label *inplabel);

#endif // INPCB_CHECKS_H

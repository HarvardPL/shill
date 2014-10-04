#ifndef IP6Q_CHECKS_H
#define IP6Q_CHECKS_H

void
shill_ip6q_create(struct mbuf *m, struct label *mlabel,
                  struct ip6q *q6, struct label *q6label);
void
shill_ip6q_destroy_label(struct label *label);
int
shill_ip6q_init_label(struct label *label, int flag);
int
shill_ip6q_match(struct mbuf *m, struct label *mlabel,
                 struct ip6q *q6, struct label *q6label);
void
shill_ip6q_reassemble(struct ip6q *q6, struct label *q6label,
                       struct mbuf *m, struct label *mlabel);
void
shill_ip6q_update(struct mbuf *m, struct label *mlabel,
                  struct ip6q *q6, struct label *q6label);

#endif // IP6Q_CHECKS_H

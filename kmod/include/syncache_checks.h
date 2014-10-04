#ifndef SYNCACHE_CHECKS_H
#define SYNCACHE_CHECKS_H

void
shill_syncache_create(struct label *label,
                      struct inpcb *inp);
void
shill_syncache_create_mbuf(struct label *sc_label,
                           struct mbuf *m, struct label *mlabel);
void
shill_syncache_destroy_label(struct label *label);
int
shill_syncache_init_label(struct label *label, int flag);

#endif // SYNCACHE_CHECKS_H

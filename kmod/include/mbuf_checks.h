#ifndef MBUF_CHECKS_H
#define MBUF_CHECKS_H

void
shill_mbuf_copy_label(struct label *src,
                      struct label *dest);
void
shill_mbuf_destroy_label(struct label *label);
int
shill_mbuf_init_label(struct label *label, int flag);

#endif // MBUF_CHECKS_H

#ifndef BPFDESC_CHECKS_H
#define BPFDESC_CHECKS_H

struct bpf_d;
struct ifnet;
struct mbuf;

int
shill_bpfdesc_check_receive(struct bpf_d *d,
                          struct label *dlabel, struct ifnet *ifp,
                          struct label *ifplabel);
void
shill_bpfdesc_create(struct ucred *cred,
                   struct bpf_d *d, struct label *dlabel);
void
shill_bpfdesc_create_mbuf(struct bpf_d *d,
                        struct label *dlabel, struct mbuf *m,
                        struct label *mlabel);
void
shill_bpfdesc_destroy_label(struct label *label);
void
shill_bpfdesc_init_label(struct label *label);

#endif /* BPFDESC_CHECKS_H */

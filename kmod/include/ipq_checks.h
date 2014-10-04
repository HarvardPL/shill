#ifndef IPQ_CHECKS_H
#define IPQ_CHECKS_H

void
shill_ipq_create(struct mbuf *m, struct label *mlabel,
                 struct ipq *q, struct label *qlabel);
void
shill_ipq_destroy_label(struct label *label);
int
shill_ipq_init_label(struct label *label, int flag);
int
shill_ipq_match(struct mbuf *m, struct label *mlabel,
                struct ipq *q, struct label *qlabel);
void
shill_ipq_reassemble(struct ipq *q, struct label *qlabel,
		    struct mbuf *m, struct label *mlabel);
void
shill_ipq_update(struct mbuf *m, struct label *mlabel,
                 struct ipq *q, struct label *qlabel);

#endif // IPQ_CHECKS_H

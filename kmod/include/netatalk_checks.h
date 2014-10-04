#ifndef NETATALK_CHECKS_H
#define NETATALK_CHECKS_H

void
shill_netatalk_aarp_send(struct ifnet *ifp,
		    struct label *ifplabel, struct mbuf *m,
		    struct label *mlabel);

#endif // NETATALK_CHECKS_H

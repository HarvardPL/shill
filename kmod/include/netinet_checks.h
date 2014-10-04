#ifndef NETINET_CHECKS_H
#define NETINET_CHECKS_H

void
shill_netinet_arp_send(struct ifnet *ifp,
                       struct label *ifplabel, struct mbuf *m,
                       struct label *mlabel);
void
shill_netinet_firewall_reply(struct mbuf *mrecv,
                             struct label *mrecvlabel, struct mbuf *msend,
                             struct label *msendlabel);
void
shill_netinet_firewall_send(struct mbuf *m,
                            struct label *mlabel);
void
shill_netinet_fragment(struct mbuf *m,
                       struct label *mlabel, struct mbuf *frag,
                       struct label *fraglabel);
void
shill_netinet_icmp_reply(struct mbuf *mrecv,
                         struct label *mrecvlabel, struct mbuf *msend,
                         struct label *msendlabel);
void
shill_netinet_icmp_replyinplace(struct mbuf *m,
                                struct label *mlabel);
void
shill_netinet_igmp_send(struct ifnet *ifp,
                        struct label *ifplabel, struct mbuf *m,
                        struct label *mlabel);
void
shill_netinet_tcp_reply(struct mbuf *m,
                        struct label *mlabel);
void
shill_netinet6_nd6_send(struct ifnet *ifp,
                        struct label *ifplabel, struct mbuf *m,
                        struct label *mlabel);

#endif // NETINET_CHECKS_H

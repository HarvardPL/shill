#ifndef SHILL_SOCKET_CAP_H
#define SHILL_SOCKET_CAP_H

#include "mem-manage.h"
#include "shill-data.h"
#include "shill-socket-permissions.h"

/* Memory Management */

DECLARE_ALLOC(shill_network_cap, network_cap_zone, network_cap_count)
DECLARE_FREE(shill_network_cap, network_cap_zone, network_cap_count)
DECLARE_ALLOC(shill_network_cap_list, network_cap_list_zone, network_cap_list_count)
DECLARE_FREE(shill_network_cap_list, network_cap_list_zone, network_cap_list_count)

void
shill_clear_network_cap_session_list(struct shill_network_cap_list *list);

void
shill_socket_module_init(void);

void
shill_socket_module_destroy(void);

/* System Calls */

int
shill_socket_module_syscall(struct ucred *cred,
                            int call,
                            void *user_arg);

/* Other */
/*
struct shill_network_cap *
shill_network_cap_list_get_af(struct shill_network_cap_list *list,
                              int address_family);

char
shill_network_cap_lte(struct shill_network_cap *a, struct shill_network_cap *b);

struct shill_network_cap *
shill_make_network_cap(int af,
                       int socket_type,
                       uint64_t permissions,
                       struct shill_session *session);

struct shill_network_cap *
shill_get_network_cap_for_session_from_socket_label(struct label *solabel,
                                                    struct shill_session *session);

struct shill_network_cap *
shill_get_network_cap_from_session_by_af(struct shill_session *session,
                                         int address_family);

void
shill_network_cap_add_permissions(struct shill_network_cap * network_cap,
                                  uint64_t permissions);
*/
/*
 * MAC Procedures
 */

int
shill_socket_check_accept(struct ucred *cred,
                          struct socket *so,
                          struct label *solabel);

int
shill_socket_check_bind(struct ucred *cred,
                        struct socket *so,
                        struct label *solabel,
                        struct sockaddr *sa);

int
shill_socket_check_connect(struct ucred *cred,
                           struct socket *so,
                           struct label *solabel,
                           struct sockaddr *sa);

int
shill_socket_check_create(struct ucred *cred,
                          int domain,
                          int type,
                          int protocol);

int
shill_socket_check_deliver(struct socket *so,
                           struct label *solabel,
                           struct mbuf *m,
                           struct label *mlabel);

int
shill_socket_check_listen(struct ucred *cred,
                          struct socket *so,
                          struct label *solabel);

int
shill_socket_check_poll(struct ucred *cred,
                        struct socket *so,
                        struct label *solabel);

int
shill_socket_check_receive(struct ucred *cred,
                           struct socket *so,
                           struct label *solabel);

int
shill_socket_check_send(struct ucred *cred,
                        struct socket *so,
                        struct label *solabel);

int
shill_socket_check_stat(struct ucred *cred,
                        struct socket *so,
                        struct label *solabel);

int
shill_socket_check_visible(struct ucred *cred,
                           struct socket *so,
                           struct label *solabel);


void
shill_socket_copy_label(struct label *src,
                        struct label *dest);

void
shill_socket_create(struct ucred *cred, struct socket *so,
                    struct label *solabel);

void
shill_socket_create_mbuf(struct socket *so,
                         struct label *solabel, struct mbuf *m,
                         struct label *mlabel);

void
shill_socket_destroy_label(struct label *label);

int
shill_socket_externalize_label(struct label *label,
                               char *element_name,
                               struct sbuf *sb,
                               int *claimed);

void
shill_socket_newconn(struct socket *oldso,
                     struct label *oldsolabel,
                     struct socket *newso,
                     struct label *newsolabel);

int
shill_socket_init_label(struct label *label, int flag);

int
shill_socket_internalize_label(struct label *label,
                               char *element_name,
                               char *element_data,
                               int *claimed);

int
shill_socket_check_relabel(struct ucred *cred,
                           struct socket *so,
                           struct label *solabel,
                           struct label *newlabel);

void
shill_socket_relabel(struct ucred *cred,
                     struct socket *so,
                     struct label *oldlabel,
                     struct label *newlabel);

#endif /* SHILL_SOCKET_CAP_H */

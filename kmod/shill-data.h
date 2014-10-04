#ifndef SHILL_DATA_H
#define SHILL_DATA_H

/* cap data */

struct shill_session;

struct shill_cap {
  uint32_t sc_flags;
  struct shill_cap *sc_lookup;
  struct shill_cap *sc_createfile;
  struct shill_cap *sc_createdir;
  LIST_ENTRY(shill_cap) sc_session_list;
};

struct shill_label {
  struct shill_session *sl_session;
  struct shill_cap *sl_cap;
  LIST_ENTRY(shill_label) sl_label_list;
  LIST_ENTRY(shill_label) sl_session_list;
};

LIST_HEAD(shill_cap_list, shill_cap);
LIST_HEAD(shill_label_list, shill_label);

/* network cap data */

struct shill_network_cap {
  int snc_af_family;
  int snc_socket_type;
  uint64_t snc_permissions;
  /* a back pointer to the session, used when checking permissions for a given
   * thread
   */
  struct shill_session *snc_session;
  LIST_ENTRY(shill_network_cap) snc_session_list;
  LIST_ENTRY(shill_network_cap) snc_socket_list;
};

LIST_HEAD(shill_network_cap_list, shill_network_cap);

/* session data */

struct shill_session {
  uint64_t ss_refcount;
  struct shill_cap_list ss_cap_root;
  struct shill_label_list ss_label_root;
  uint8_t ss_state;
  struct shill_session *ss_parent;
  struct shill_network_cap_list ss_network_cap_list;
  struct shill_cap *ss_pipefactory_cap;
};

#define SESSIONPTR(session) ((((uintptr_t)(session)) & 1) ? NULL : (session))
#define SESSION_INIT 1
#define SESSION_START 3
#define SESSION_DEBUG 5

#endif /* SHILL_DATA_H */

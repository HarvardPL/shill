#include <sys/param.h>
#include <sys/lock.h>
#include <sys/mutex.h>
#include <sys/kernel.h>
#include <sys/module.h>
#include <sys/mman.h>
#include <sys/namei.h>
#include <sys/queue.h>
#include <sys/systm.h>
#include <sys/ucred.h>
#include <sys/queue.h>
#include <sys/sysctl.h>
#include <sys/syslog.h>
#include <vm/uma.h>

#include <sys/proc.h> /* for thread struct accessors: td_ucred */

#include <security/mac/mac_policy.h>
#include <sys/mac.h>

#include <machine/stdarg.h>

#include "mem-manage.h"
#include "shilldev.h"
#include "cflags.h"
#include "shill.h"
#include "syscalls.h"

#include "bpfdesc_checks.h"
#include "cred_checks.h"
#include "inpcb_checks.h"
#include "ip6q_checks.h"
#include "ipq_checks.h"
#include "kenv_checks.h"
#include "kld_checks.h"
#include "mbuf_checks.h"
#include "mount_checks.h"
#include "netatalk_checks.h"
#include "netinet_checks.h"
#include "pipe_checks.h"
#include "proc_checks.h"
#include "posixsem_checks.h"
#include "posixshm_checks.h"
#include "priv_checks.h"
#include "proc_checks.h"
#include "syncache_checks.h"
#include "system_checks.h"
#include "sysvmsg_checks.h"
#include "sysvmsq_checks.h"
#include "sysvsem_checks.h"
#include "sysvshm_checks.h"
#include "vnode_checks.h"
#include "shill-socket-caps.h"

static int shill_slot;
static uma_zone_t cap_zone;
static uma_zone_t label_zone;
static uma_zone_t label_list_zone;
static uma_zone_t session_zone;

SYSCTL_DECL(_security_mac);
static SYSCTL_NODE(_security_mac, OID_AUTO, shill, CTLFLAG_RW, 0,
		   "Shill TrustedBSD MAC policy controls");

#define DEFINE_SYSCTL_INT(name_id, ctlflag, initial_value, description) \
  static int name_id = initial_value;                                   \
  SYSCTL_INT(_security_mac_shill, OID_AUTO, name_id, ctlflag,           \
             &name_id, initial_value, description);                     \

#define DEFINE_EXTERN_SYSCTL_INT(name_id, ctlflag, initial_value, description) \
  int name_id = initial_value;                                          \
  SYSCTL_INT(_security_mac_shill, OID_AUTO, name_id, ctlflag,           \
             &name_id, initial_value, description);                     \

DEFINE_SYSCTL_INT(cap_count, CTLFLAG_RD, 0,
                  "Number of allocated capabilities\n");
DEFINE_SYSCTL_INT(label_count, CTLFLAG_RD, 0,
                  "Number of allocated shill labels\n");
DEFINE_SYSCTL_INT(list_count, CTLFLAG_RD, 0,
                  "Number of allocated label lists\n");
DEFINE_SYSCTL_INT(session_count, CTLFLAG_RD, 0,
                  "Number of allocated sessions\n");
DEFINE_EXTERN_SYSCTL_INT(shill_debug, CTLFLAG_RW, 0,
                         "Enable debug output\n");
DEFINE_EXTERN_SYSCTL_INT(network_cap_count, CTLFLAG_RD, 0,
                         "Number of allocated network capabilities\n")
DEFINE_EXTERN_SYSCTL_INT(network_cap_list_count, CTLFLAG_RD, 0,
                         "Number of allocated network capability lists\n")

struct mtx globalmtx;

/*
 * Policy module operations
 */
static void
shill_init(struct mac_policy_conf *conf) {
  mtx_init(&globalmtx, "shill_global_mtx", NULL, MTX_DEF | MTX_RECURSE);
  cap_zone = uma_zcreate("mac_shill_cap", sizeof(struct shill_cap),
			 NULL, NULL, NULL, NULL, UMA_ALIGN_PTR, 0);
  label_zone = uma_zcreate("mac_shill_label", sizeof(struct shill_label),
			   NULL, NULL, NULL, NULL, UMA_ALIGN_PTR, 0);
  label_list_zone = uma_zcreate("mac_shill_label_list", sizeof(struct shill_label_list),
			   NULL, NULL, NULL, NULL, UMA_ALIGN_PTR, 0);
  session_zone = uma_zcreate("mac_shill_session", sizeof(struct shill_session),
			     NULL, NULL, NULL, NULL, UMA_ALIGN_PTR, 0);
  shill_socket_module_init();
}

static void
shill_destroy(struct mac_policy_conf *conf) {
  mtx_destroy(&globalmtx);
  uma_zdestroy(cap_zone);
  uma_zdestroy(label_zone);
  uma_zdestroy(label_list_zone);
  uma_zdestroy(session_zone);
  shill_socket_module_destroy();
}

/*
 * Label mux/demux operations
 */

struct shill_session *
extract_shill_session(struct label *label) {
  KASSERT(label != NULL, ("extract_shill_session: null label"));
  return (struct shill_session *) mac_label_get(label, shill_slot);
}

inline void
set_shill_session(struct label *label, struct shill_session *session) {
  KASSERT(label != NULL, ("set_shill_session: null label"));
  mac_label_set(label, shill_slot, (uintptr_t) session);
}

static inline struct shill_label_list *
extract_shill_labels(struct label *label) {
  KASSERT(label != NULL, ("extract_shill_labels: null label"));
  return (struct shill_label_list *) mac_label_get(label, shill_slot);
}

static inline void
set_shill_labels(struct label *label, struct shill_label_list *llist) {
  KASSERT(label != NULL, ("set_shill_labels: null label"));
  mac_label_set(label, shill_slot, (uintptr_t) llist);
}

/*
 * Memory management
 */

void
shill_lock_session(struct shill_session *session) {
  mtx_lock(&globalmtx);
}

void
shill_unlock_session(struct shill_session *session) {
  mtx_unlock(&globalmtx);
}

static inline DEFINE_ALLOC(shill_cap, cap_zone, cap_count, M_WAITOK);
static inline DEFINE_FREE(shill_cap, cap_zone, cap_count);

static inline DEFINE_ALLOC(shill_label, label_zone, label_count, M_NOWAIT);
static inline DEFINE_FREE(shill_label, label_zone, label_count);

static inline DEFINE_ALLOC(shill_label_list, label_list_zone, list_count, M_NOWAIT);
static inline DEFINE_FREE(shill_label_list, label_list_zone, list_count);

static inline void
shill_clear_cap_list(struct shill_cap_list *list) {
  struct shill_cap *cur, *cur_temp;
  LIST_FOREACH_SAFE(cur, list, sc_session_list, cur_temp) {
    LIST_REMOVE(cur, sc_session_list);
    shill_cap_free(cur);
  }
}

static inline void
shill_clear_session_label_list(struct shill_label_list *list) {
  KASSERT(list != NULL, ("shill_clear_label_list: null list"));
  mtx_lock(&globalmtx);
  struct shill_label *cur, *cur_temp;
  LIST_FOREACH_SAFE(cur, list, sl_session_list, cur_temp) {
    // Remove the item from the label list
    LIST_REMOVE(cur, sl_label_list);
    LIST_REMOVE(cur, sl_session_list);
    shill_label_free(cur);
  }
  mtx_unlock(&globalmtx);
}

static inline struct shill_session *
shill_session_alloc(void) {
  struct shill_session *session = uma_zalloc(session_zone, M_ZERO | M_NOWAIT);
  KASSERT(session != NULL, ("shill_session_alloc: allocation failed"));
  atomic_add_rel_int(&session_count, 1);
  session->ss_refcount = 0;
  session->ss_state = 0;
  session->ss_parent = NULL;
  LIST_INIT(&session->ss_cap_root);
  LIST_INIT(&session->ss_label_root);
  LIST_INIT(&session->ss_network_cap_list);
  session->ss_pipefactory_cap = NULL;
  return session;
}

static inline void
release_shill_session(struct shill_session *);

static inline void
shill_session_free(struct shill_session *session) {
  if (session != NULL) {
    atomic_add_rel_int(&session_count, -1);
    if (session->ss_parent != NULL)
      release_shill_session(session->ss_parent);
    shill_clear_cap_list(&session->ss_cap_root);
    shill_clear_session_label_list(&session->ss_label_root);
    shill_clear_network_cap_session_list(&session->ss_network_cap_list);
  }
  uma_zfree(session_zone, session);
}

static inline void
acquire_shill_session(struct shill_session *session) {
  shill_lock_session(session);
  session->ss_refcount++;
  shill_unlock_session(session);
}

static inline void
release_shill_session(struct shill_session *session) {
  shill_lock_session(session);
  uint64_t count = --(session->ss_refcount);
  shill_unlock_session(session);
  if (count == 0) {
    shill_session_free(session);
  }
}

/*
 * Session management
 */ 

void
shill_cred_init_label(struct label *label) {
  set_shill_session(label, NULL);
}

void
shill_cred_destroy_label(struct label *label) {
  struct shill_session *session = extract_shill_session(label);
  if (SESSIONPTR(session) != NULL) {
    release_shill_session(session);
  }
  set_shill_session(label, NULL);
}

void
shill_cred_copy_label(struct label *src, struct label *dst) {
  struct shill_session *dstsession = extract_shill_session(dst);
  struct shill_session *srcsession = extract_shill_session(src);

  if (dstsession != NULL)
    release_shill_session(dstsession);

  if (srcsession != NULL)
    acquire_shill_session(srcsession);

  set_shill_session(dst, srcsession);
}
 
int
shill_cred_internalize_label(struct label *label,
			     char *element_name, char *element_data,
			     int *claimed) {
  if (strcmp(MAC_SHILL_LABEL_NAME, element_name) != 0)
    return 0;

  (*claimed)++;
  
  KASSERT(extract_shill_session(label) == NULL,
	  ("shill_cred_internalize_label: session already assigned"));

  if (strcmp("init", element_data) == 0) {
    set_shill_session(label, (struct shill_session *) SESSION_INIT);
  } else if (strcmp("start", element_data) == 0) {
    set_shill_session(label, (struct shill_session *) SESSION_START);
  } else if (strcmp("debug", element_data) == 0) {
    set_shill_session(label, (struct shill_session *) SESSION_DEBUG);
  } else {
    return EINVAL;
  }

  return 0;
}

int
shill_cred_check_relabel(struct ucred *cred, struct label *newlabel) {
  uintptr_t status = (uintptr_t) extract_shill_session(newlabel);
  struct shill_session *session = extract_shill_session(cred->cr_label);
  switch (status) {
  case SESSION_INIT:
    if (session != NULL && session->ss_state != SESSION_START && session->ss_state != SESSION_DEBUG) {
      shilld_logall("Can't create a nested sandbox until current sandbox entered");
      return EINVAL;
    }
    return 0;
  case SESSION_START:
    /* FALLTHROUGH */
  case SESSION_DEBUG:
    if (session == NULL) {
      shilld_logall("Sandbox not initialized");
      return EINVAL;
    }
    shill_lock_session(session);
    if (session->ss_state != SESSION_INIT) {
      shill_unlock_session(session);
      return EINVAL;
    }
    shill_unlock_session(session);
    return 0;
  default:
    shilld_logall("Unknown sandbox command");
    return EINVAL;
  }
}

void
shill_cred_relabel(struct ucred *cred, struct label *newlabel) {
  uintptr_t status = (uintptr_t) extract_shill_session(newlabel);
  struct shill_session *cur_session = extract_shill_session(cred->cr_label);

  if (status == SESSION_INIT) {
    struct shill_session *session = shill_session_alloc();
    session->ss_state = SESSION_INIT;

    if (cur_session != NULL) {
      /* Don't acquire cur_session again here since we would also
	 be releasing cur_session from the current cred---net effect
	 on refcount is 0. */
      session->ss_parent = cur_session;
    }

    acquire_shill_session(session);
    shilld_log((uintptr_t)session, "Initializing new session\n");
    set_shill_session(cred->cr_label, session);
  }

  if (status == SESSION_START || status == SESSION_DEBUG) {
    shill_lock_session(cur_session);
    if (cur_session->ss_parent != NULL) {
      cur_session->ss_parent = NULL;
      release_shill_session(cur_session->ss_parent);
    }
    cur_session->ss_state = status;
    shilld_log((uintptr_t)cur_session, 
               (status == SESSION_START ? "Starting session\n"
                : "Starting session in debug mode\n"));
    shill_unlock_session(cur_session);
  }
}

/*
 * Find a label with the appropriate session type in a label list.
 */
static inline struct shill_label *
shill_find_label(struct shill_label_list *list,
		 struct shill_session *session) {
  mtx_lock(&globalmtx);
  struct shill_label *cur, *found = NULL;
  KASSERT(list != NULL, ("shill_find_label: null list"));
  LIST_FOREACH(cur, list, sl_label_list) {
    if (cur->sl_session == session) {
      found = cur;
      break;
    } 
  }
  mtx_unlock(&globalmtx);
  return found;
}

/*
 * Find (or create) a capability in a particular session space.
 *
 * Preconditions:
 *   1. cap->sc_lookup is a member of list
 *   2. cap->sc_createfile is a member of list
 *   3. cap->sc_createdir is a member of list
 */
static struct shill_cap *
shill_get_or_create_cap(struct shill_session *session, struct shill_cap_list *list, struct shill_cap *cap) {
  KASSERT(cap != NULL, ("shill_get_or_create_cap: NULL capability"));

  struct shill_cap *cur, *found = NULL;
  LIST_FOREACH(cur, list, sc_session_list) {
    if (cur->sc_flags != cap->sc_flags)
      continue;
    
    if (cur->sc_lookup != cap->sc_lookup &&
	(cur != cur->sc_lookup || cap != cap->sc_lookup))
      continue;

    if (cur->sc_createfile != cap->sc_createfile &&
	(cur != cur->sc_createfile || cap != cap->sc_createfile))
      continue;

    if (cur->sc_createdir != cap->sc_createdir &&
	(cur != cur->sc_createdir || cap != cap->sc_createdir))
      continue;

    found = cur;
    break;
  }

  if (found == NULL) {
    found = shill_cap_alloc();
    found->sc_flags = cap->sc_flags;
    found->sc_lookup = (cap->sc_lookup == cap) ? found : cap->sc_lookup;
    found->sc_createfile = (cap->sc_createfile == cap) ? found : cap->sc_createfile;
    found->sc_createdir = (cap->sc_createdir == cap) ? found : cap->sc_createdir;
    shill_lock_session(session);
    LIST_INSERT_HEAD(list, found, sc_session_list);
    shill_unlock_session(session);
    if (session != NULL)
      shilld_logcap((uintptr_t)session, found);
  }
  
  return found;
}

/*
 * Decode a capability from user space.
 */
static struct shill_cap *
shill_decode_cap(struct shill_session *session, struct shill_cap_list *list, char *encoded, size_t len, size_t i) {
  struct shill_cap_enc cur;
  memcpy(&cur, &encoded[i], sizeof(struct shill_cap_enc));

  size_t lookupi = (size_t)((cur.ce_lookup & ~0x8000) - 1) * sizeof(struct shill_cap_enc);
  size_t createfilei = (size_t)((cur.ce_createfile & ~0x8000) - 1) * sizeof(struct shill_cap_enc);
  size_t creatediri  = (size_t)((cur.ce_createdir & ~0x8000) - 1) * sizeof(struct shill_cap_enc);
  if (lookupi != 0 && (lookupi >= len || lookupi <= i)) {
    shilld_logall("Decoding sandbox capability failed, lookup reference out of bounds");
    return (void *)-1;
  }
  if (createfilei != 0 && (createfilei >= len || createfilei <= i)) {
    shilld_logall("Decoding sandbox capability failed, create reference out of bounds");
    return (void *)-1;
  }
  if (creatediri != 0 && (creatediri >= len || creatediri <= i)) {
    shilld_logall("Decoding sandbox capability failed, create reference out of bounds");
    return (void *)-1;
  }
  
  struct shill_cap temp = {
    .sc_flags      = cur.ce_flags & ~(C_MASKS),
    .sc_lookup     = lookupi == 0 ? &temp : shill_decode_cap(session, list, encoded, len, lookupi),
    .sc_createfile = createfilei == 0 ? &temp : shill_decode_cap(session, list, encoded, len, createfilei),
    .sc_createdir  = creatediri == 0 ? &temp : shill_decode_cap(session, list, encoded, len, creatediri),
  };

  if (temp.sc_lookup == (void *)-1 || temp.sc_createfile == (void *)-1 || temp.sc_createdir == (void *)-1)
    return (void *)-1;

  mtx_lock(&globalmtx);
  struct shill_cap *ret = shill_get_or_create_cap(session, list, &temp);
  mtx_unlock(&globalmtx);
  KASSERT(ret != NULL, ("shill_decode_cap: shill_get_or_create_cap failed"));
  return ret;
}

/*
 * Import a capability tree from one session key to another.
 */
static struct shill_cap *
shill_import_cap(struct shill_session *session, struct shill_cap_list *list, struct shill_cap *cap) {
  KASSERT(cap != NULL, ("shill_import_cap: NULL capability"));
  
  struct shill_cap temp = {
    .sc_flags = cap->sc_flags,
    .sc_lookup = cap->sc_lookup,
    .sc_createfile = cap->sc_createfile,
    .sc_createdir = cap->sc_createdir,
  };

  if (temp.sc_lookup != cap) {
    temp.sc_lookup = shill_import_cap(session, list, temp.sc_lookup);
  } else {
    temp.sc_lookup = &temp;
  }
  if (temp.sc_createfile != cap) {
    temp.sc_createfile = shill_import_cap(session, list, temp.sc_createfile);
  } else {
    temp.sc_createfile = &temp;
  }
  if (temp.sc_createdir != cap) {
    temp.sc_createdir = shill_import_cap(session, list, temp.sc_createdir);
  } else {
    temp.sc_createdir = &temp;
  }

  mtx_lock(&globalmtx);
  struct shill_cap *result = shill_get_or_create_cap(session, list, &temp);
  mtx_unlock(&globalmtx);
  return result;
}

static int shill_cap_lte(struct shill_cap *, struct shill_cap *);

/*
 * Merge two capability trees
 *
 * Capabilities must be from the same list
 */
struct shill_cap *
shill_merge_cap(struct shill_session *session, struct shill_cap *dst, struct shill_cap *src) {
  struct shill_cap *lookup = NULL;
  struct shill_cap *createfile = NULL;
  struct shill_cap *createdir = NULL;

  if (dst == NULL)
    return src;

  if (dst == src)
    return dst;

  int dstlookup = dst->sc_flags & C_LOOKUP;
  int srclookup = src->sc_flags & C_LOOKUP;
  if (dstlookup && !srclookup) {
    lookup = dst->sc_lookup;
  } else if (!dstlookup && srclookup) {
    lookup = src->sc_lookup;
  } else if (dstlookup && srclookup) {
    if (dst->sc_lookup != dst || src->sc_lookup != src)
      lookup = shill_merge_cap(session,dst->sc_lookup,src->sc_lookup);
  }
  
  int dstcreatefile = dst->sc_flags & C_CREATEFILE;
  int srccreatefile = src->sc_flags & C_CREATEFILE;
  if (dstcreatefile && !srccreatefile) {
    createfile = dst->sc_createfile;
  } else if (!dstcreatefile && srccreatefile) {
    createfile = src->sc_createfile;
  } else if (dstcreatefile && srccreatefile) {
    if (dst->sc_createfile != dst || src->sc_createfile != src) {
      if (shill_cap_lte(dst->sc_createfile,src->sc_createfile) ||
          shill_cap_lte(src->sc_createfile,dst->sc_createfile)) {
        createfile = shill_merge_cap(session,dst->sc_createfile,src->sc_createfile);
      } else {
        shilld_log((uintptr_t)session, "Capability %lx not compatible with %lx, not merging",
                   (uintptr_t)dst->sc_createfile, (uintptr_t)src->sc_createfile);
        createfile = dst;
      }
    }
  }

  int dstcreatedir = dst->sc_flags & C_CREATEDIR;
  int srccreatedir = src->sc_flags & C_CREATEDIR;
  if (dstcreatedir && !srccreatedir) {
    createdir = dst->sc_createdir;
  } else if (!dstcreatedir && srccreatedir) {
    createdir = src->sc_createdir;
  } else if (dstcreatedir && srccreatedir) {
    if (dst->sc_createdir != dst || src->sc_createdir != src) {
      if (shill_cap_lte(dst->sc_createdir,src->sc_createdir) ||
          shill_cap_lte(src->sc_createdir,dst->sc_createdir)) {
        createdir = shill_merge_cap(session,dst->sc_createdir,src->sc_createdir);
      } else {
        shilld_log((uintptr_t)session, "Capability %lx not compatible with %lx, not merging",
                   (uintptr_t)dst->sc_createdir, (uintptr_t)src->sc_createdir);
        createdir = dst;
      }
    }
  }

  struct shill_cap temp = {
    .sc_lookup = (lookup == NULL) ? &temp : lookup,
    .sc_createfile = (createfile == NULL) ? &temp : createfile,
    .sc_createdir = (createdir == NULL) ? &temp : createdir,
    .sc_flags = dst->sc_flags | src->sc_flags,
  };

  mtx_lock(&globalmtx);
  struct shill_cap *result = shill_get_or_create_cap(session, &session->ss_cap_root, &temp);
  mtx_unlock(&globalmtx);
  return result;
}

struct shill_cap *
shill_create_pipe_cap(struct shill_session *session) {
  struct shill_cap *cap = session->ss_pipefactory_cap;
  if (cap == NULL) {
    struct shill_cap temp = {
      .sc_lookup = &temp,
      .sc_createfile = &temp,
      .sc_createdir = &temp,
      .sc_flags = 0,
    };

    mtx_lock(&globalmtx);
    cap = shill_get_or_create_cap(session, &session->ss_cap_root, &temp);
    mtx_unlock(&globalmtx);
  }

  return cap;
}

static int
shill_cap_lte_children(struct shill_cap *left, struct shill_cap *lchild,
			   struct shill_cap *right, struct shill_cap *rchild) {
  if (left == lchild && right == rchild)
    return true;

  return shill_cap_lte(lchild,rchild);
}

static int
shill_cap_lte(struct shill_cap *left, struct shill_cap *right) {
  KASSERT(left != NULL, ("shill_cap_lte: NULL left capability"));
  KASSERT(right != NULL, ("shill_cap_lte: NULL right capability"));

  if (left == right)
    return true;

  if ((right->sc_flags | left->sc_flags) != right->sc_flags)
    return false;

  if (left->sc_flags & C_LOOKUP &&
      !shill_cap_lte_children(left, left->sc_lookup, right, right->sc_lookup))
    return false;

  if (left->sc_flags & C_CREATEFILE &&
      !shill_cap_lte_children(left, left->sc_createfile, right, right->sc_createfile))
    return false;

  if (left->sc_flags & C_CREATEDIR &&
      !shill_cap_lte_children(left, left->sc_createdir, right, right->sc_createdir))
    return false;
  
  return true;
}

static inline struct shill_cap *
shill_grant(struct shill_session *session, struct shill_cap_list *list, struct shill_cap *cap, uint32_t rights) {
  struct shill_cap zero = {
      .sc_flags = 0,
      .sc_lookup = &zero,
      .sc_createfile = &zero,
      .sc_createdir = &zero,
  };

  struct shill_cap temp = {
    .sc_flags = 0,
    .sc_lookup = &temp,
    .sc_createfile = &temp,
    .sc_createdir = &temp,
  };

  uint32_t cur = 0;

  if (cap != NULL) {
    temp.sc_flags = cap->sc_flags;
    temp.sc_lookup = cap->sc_lookup;
    temp.sc_createfile = cap->sc_createfile;
    temp.sc_createdir = cap->sc_createdir;
    cur = cap->sc_flags;
  }

  temp.sc_flags |= rights;

  if (((rights & C_LOOKUP) != 0) && ((cur & C_LOOKUP) == 0))
    temp.sc_lookup = shill_get_or_create_cap(session,list,&zero);

  if (((rights & C_CREATEFILE) != 0) && ((cur & C_CREATEFILE) == 0))
    temp.sc_createfile = shill_get_or_create_cap(session,list,&zero);

  if (((rights & C_CREATEDIR) != 0) && ((cur & C_CREATEDIR) == 0))
    temp.sc_createdir = shill_get_or_create_cap(session,list,&zero);

  mtx_lock(&globalmtx);
  struct shill_cap * result = shill_get_or_create_cap(session,list,&temp);
  mtx_unlock(&globalmtx);
  return result;
}



/*
 * cap operations
 */

static void
shill_cap_list_add(struct shill_cap_list *list, struct shill_cap *cap) {
  if (cap->sc_lookup != cap)
    shill_cap_list_add(list,cap->sc_lookup);
  if (cap->sc_createfile != cap)
    shill_cap_list_add(list,cap->sc_createfile);
  if (cap->sc_createdir != cap)
    shill_cap_list_add(list,cap->sc_createdir);

  struct shill_cap *cur;
  LIST_FOREACH(cur, list, sc_session_list) {
    if (cur == cap)
      return;
  }

  LIST_INSERT_HEAD(list, cap, sc_session_list);
  return;
}

void
shill_cap_free_rec(struct shill_cap *cap) {
  struct shill_cap_list temp;
  LIST_INIT(&temp);
  shill_cap_list_add(&temp, cap);
  shill_clear_cap_list(&temp);
}

inline struct shill_session *
shill_get_session(struct ucred *cred) {
  return extract_current_shill_session(cred->cr_label);
}

inline struct shill_session *
extract_current_shill_session(struct label *label) {
  struct shill_session *session = extract_shill_session(label);
  if (session == NULL)
    return NULL;

  struct shill_session *ret;
  shill_lock_session(session);
  switch (session->ss_state) {
  case SESSION_INIT:
    ret = session->ss_parent;
    break;
  case SESSION_START:
    /* FALLTHROUGH */
  case SESSION_DEBUG:
    ret = session;
    break;
  default:
    ret = NULL;
  }
  shill_unlock_session(session);
  
  KASSERT(ret != NULL, ("shill_get_session: unknown session state"));
  return ret;
}

inline struct shill_session *
shill_session_parent(struct shill_session *session) {
  KASSERT(session != NULL, ("shill_session_parent: null session"));
  shill_lock_session(session);
  struct shill_session *ret = session->ss_parent;
  shill_unlock_session(session);
  return ret;
}

inline bool
shill_session_debug(struct shill_session *session) {
  KASSERT(session != NULL, ("shill_session_debug: null session"));
  shill_lock_session(session);
  bool ret = (session->ss_state == SESSION_DEBUG);
  shill_unlock_session(session);
  return ret;  
}

inline struct shill_cap *
shill_get_cap(struct label *label, struct shill_session *session) {
  struct shill_label_list *list = extract_shill_labels(label);
  KASSERT(list != NULL, ("shill_get_cap: NULL label list"));

  struct shill_label *sl = shill_find_label(list, session);
  if (sl == NULL) {
    sl = shill_label_alloc();
    mtx_lock(&globalmtx);
    sl->sl_session = session;
    sl->sl_cap = NULL;
    LIST_INSERT_HEAD(&session->ss_label_root, sl, sl_session_list);
    LIST_INSERT_HEAD(list, sl, sl_label_list);
    mtx_unlock(&globalmtx);
  }
  return sl->sl_cap;
}

inline void
shill_set_cap(struct label *label, struct shill_session *session, struct shill_cap *cap) {
  struct shill_label_list *list = extract_shill_labels(label);
  KASSERT(list != NULL, ("shill_set_cap: NULL label list"));
  
  mtx_lock(&globalmtx);
  struct shill_label *sl = shill_find_label(list, session);
  if (sl == NULL) {
    sl = shill_label_alloc();
    sl->sl_cap = cap;
    shill_lock_session(session);
    sl->sl_session = session;
    LIST_INSERT_HEAD(&session->ss_label_root,sl,sl_session_list);
    shill_unlock_session(session);
    LIST_INSERT_HEAD(list,sl,sl_label_list);
  } else {
    sl->sl_cap = cap;
  }
  mtx_unlock(&globalmtx);
}

inline void
shill_cap_init_label(struct label *label) {
  struct shill_label_list *list = shill_label_list_alloc();
  LIST_INIT(list);
  set_shill_labels(label, list);
}

inline void
shill_cap_copy_label(struct label *src, struct label *dst) {
  // XXX Unimplemented
}

inline int
shill_deny_all(const char *check, struct ucred *cred, void *obj) {
  struct shill_session *session = shill_get_session(cred);
  if (session != NULL) {
    if (session->ss_state == SESSION_DEBUG) {
      shilld_log((uintptr_t)session, "Permitting %s on %lx for debugging", check, (uintptr_t)obj);
      return 0;
    } else {
      shilld_log((uintptr_t)session, "Denied %s on %lx", check, (uintptr_t)obj);
      return EACCES;
    }
  }
  else 
    return 0;
}

inline int
shill_allow_all(const char *check, struct ucred *cred, void *obj) {
  struct shill_session *session = shill_get_session(cred);

  if (session != NULL) {
    shilld_log((uintptr_t)session, "Allowing %s on %lx", check, (uintptr_t)obj);
  }

  return 0;
}

inline int
shill_cap_check_any(const char *check, struct ucred *cred, void *obj, struct label *label) {
  struct shill_session *session = shill_get_session(cred);
  if (session == NULL)
    return 0;

  struct shill_cap *cap = shill_get_cap(label, session);
  if (cap == NULL || cap->sc_flags == 0) {
    if (session->ss_state == SESSION_DEBUG) {
      shilld_log((uintptr_t)session, "Permitting %s on %lx with no capabilities", check, (uintptr_t)obj);
    } else {
      shilld_log((uintptr_t)session, "Insufficient (no) capabilities for %s on %lx", check, (uintptr_t)obj);
      return EACCES;
    }
  }

  return 0;
}

inline int
shill_cap_check_rights(const char *check,
                       struct ucred *cred, void *obj,
                       struct label *label, uint32_t rights) {
  struct shill_session *session = shill_get_session(cred);
  if (session == NULL)
    return 0;

  struct shill_cap *cap = shill_get_cap(label, session);
  if (cap == NULL || (cap->sc_flags & rights) != rights) {
    if (session->ss_state == SESSION_DEBUG) {
      shilld_log((uintptr_t)session, "Insufficient capabilities for %s on %lx",
		 check, (uintptr_t) obj);
      struct shill_cap *new = shill_grant(session,&session->ss_cap_root,cap,rights);
      shill_set_cap(label,session,new);
      shilld_log((uintptr_t)session, "Granted additional privileges on %lx: %lx", (uintptr_t)obj, (uintptr_t)new);
    } else {
      shilld_log((uintptr_t)session, "Insufficient capabilities for %s on %lx: %lx",
		 check, (uintptr_t) obj, (uintptr_t) cap);
      return EACCES;
    }
  }

  return 0;
}

int
shill_cap_internalize_label(struct label *label,
                            char *element_name, char *element_data,
                            int *claimed) {
  if (strcmp(MAC_SHILL_LABEL_NAME, element_name) != 0)
    return 0;

  (*claimed)++;
  struct shill_label_list *list = extract_shill_labels(label);
  KASSERT(list != NULL, ("shill_cap_internalize_label: NULL label list"));

  size_t len = strnlen(element_data, MAC_MAX_LABEL_ELEMENT_DATA);
  struct shill_cap_list tmp_cap_list;
  LIST_INIT(&tmp_cap_list);
  struct shill_cap *cap = shill_decode_cap(NULL,&tmp_cap_list, element_data, len, 0);
  if (cap == (void *)-1) {
    shill_clear_cap_list(&tmp_cap_list);
    return EINVAL;
  } else {
    struct shill_cap *cur, *cur_temp;
    LIST_FOREACH_SAFE(cur, &tmp_cap_list, sc_session_list, cur_temp) {
      LIST_REMOVE(cur, sc_session_list);
    }
  }

  struct shill_label *new = shill_label_alloc();
  new->sl_session = NULL;
  new->sl_cap = cap;

  LIST_INSERT_HEAD(list, new, sl_label_list);

  return 0;
}

int
shill_cred_externalize_label(struct label *label,
                             char *element_name,
                             struct sbuf *sb, int *claimed) {
  // XXX implement this
  return 0;
}

int
shill_cap_check_relabel(struct ucred *cred, void *obj, struct label *objlabel, struct label *newlabel) {
  struct shill_session *session = extract_shill_session(cred->cr_label);
  if (session == NULL) {
    shilld_logall("Granting capabilities to %lx not prermitted: no current sandbox",
		  (uintptr_t)obj);
    return EACCES;
  }

  struct shill_session *parent = NULL;
  shill_lock_session(session);
  if (session->ss_state != SESSION_INIT) {
    shill_unlock_session(session);
    shilld_log((uintptr_t)session,
	      "Granting capabilities to %lx not permitted: sandbox already entered",
	      (uintptr_t)obj);
    return EACCES;
  } else if (session->ss_parent != NULL) {
    parent = session->ss_parent;
  }
  shill_unlock_session(session);

  if (parent == NULL)
    return 0;

  struct shill_cap *curcap = shill_get_cap(objlabel, parent);
  struct shill_cap *newcap = shill_get_cap(newlabel, NULL);

  KASSERT(newcap != NULL, ("shill_vnode_check_relabel: new capability is NULL"));

  if (curcap == NULL || !shill_cap_lte(newcap,curcap)) {
    shilld_log((uintptr_t)session,
	      "Nested sandbox may not grant additional authority on %lx!",
	      (uintptr_t)obj);
    return EACCES;
  }

  return 0;
}

void
shill_cap_relabel(struct ucred *cred, void *obj, struct label *objlabel, struct label *newlabel) {
  struct shill_session *session = extract_shill_session(cred->cr_label);
  if (SESSIONPTR(session) == NULL)
    return;

  shill_lock_session(session);
  if (session->ss_state != SESSION_INIT) {
    shill_unlock_session(session);
    return;
  }
  shill_unlock_session(session);  

  struct shill_label_list *objlist = extract_shill_labels(objlabel);
  if (objlist == NULL) {
    return;
  }

  struct shill_label_list *newlist = extract_shill_labels(newlabel);
  if (newlist == NULL) {
    return;
  }

  struct shill_label *objl = shill_find_label(objlist, session);
  if (objl == NULL) {
    objl = shill_label_alloc();
    mtx_lock(&globalmtx);
    objl->sl_session = session;
    objl->sl_cap = NULL;
    LIST_INSERT_HEAD(&session->ss_label_root, objl, sl_session_list);
    LIST_INSERT_HEAD(objlist, objl, sl_label_list);
    mtx_unlock(&globalmtx);
  }

  struct shill_label *npl = shill_find_label(newlist, NULL);
  if (npl != NULL) {
    struct shill_cap *newcap = shill_import_cap(session, &session->ss_cap_root, npl->sl_cap);
    newcap = shill_merge_cap(session, objl->sl_cap, newcap);
    objl->sl_cap = newcap;
  }

  shilld_log((uintptr_t)session, "Granted capabilities on %lx: %lx", (uintptr_t)obj, (uintptr_t)objl->sl_cap);
}

void
shill_cap_destroy_label(struct label *label) {
  struct shill_label_list *list = extract_shill_labels(label);
  if (list == NULL)
    return;

  mtx_lock(&globalmtx);
  struct shill_label *cur, *cur_temp;
  LIST_FOREACH_SAFE(cur, list, sl_label_list, cur_temp) {
    if (cur->sl_session != NULL) {
      LIST_REMOVE(cur, sl_session_list);
    } else {
      // This wasn't a real session label, free the caps too
      if (cur->sl_cap != NULL)
        shill_cap_free_rec(cur->sl_cap);
    }
    LIST_REMOVE(cur, sl_label_list);
    shill_label_free(cur);
  }
  mtx_unlock(&globalmtx);
  shill_label_list_free(list);
}

static int
shill_add_pipe_factory_syscall(struct ucred *cred, void *arg) {
  int err;

  struct shill_session *session = extract_shill_session(cred->cr_label);
  struct shill_session *parent = NULL;
 
  if (session == NULL) {
    shilld_logall("Granting pipe factory caps not permitted: no current sandbox");
    return EACCES;
  }
 
  shill_lock_session(session);
  if (session->ss_state != SESSION_INIT) {
    shill_unlock_session(session);
    shilld_log((uintptr_t)session,
               "Granting pipe factory caps not permitted: sandbox already entered");
    return EACCES;
  } else if (session->ss_parent != NULL) {
    parent = session->ss_parent;
  }
 
  /* Copy in the capability string */
  char capstr[MAC_MAX_LABEL_ELEMENT_DATA];
  size_t len;
  err = copyinstr((const char *)arg, capstr, MAC_MAX_LABEL_ELEMENT_DATA, &len);
  if (err) {
    shill_unlock_session(session);
    return err;
  }

  struct shill_cap_list tmp_cap_list;
  LIST_INIT(&tmp_cap_list);
  struct shill_cap *cap = shill_decode_cap(NULL,&tmp_cap_list,capstr,len,0);
  if (cap == (void *)-1) {
    shill_clear_cap_list(&tmp_cap_list);
    shill_unlock_session(session);
    return EINVAL;
  }

  if (parent != NULL) {
    shill_lock_session(parent);
    if ((parent->ss_pipefactory_cap == NULL) || !shill_cap_lte(cap,parent->ss_pipefactory_cap)) {
      shill_unlock_session(parent);
      shill_cap_free_rec(cap);
      shilld_log((uintptr_t)session,
                 "Cannot grant pipe factory capability because parent session "
                 "doesn't have a super-capability.");
      shill_unlock_session(session);
      return EACCES;
    }
    shill_unlock_session(parent);
  }

  if (session->ss_pipefactory_cap != NULL &&
      !(shill_cap_lte(cap,session->ss_pipefactory_cap) ||
        shill_cap_lte(cap,session->ss_pipefactory_cap))) {
    shill_cap_free_rec(cap);
    shilld_log((uintptr_t)session,
               "Cannot grant pipe factory capability because it is incompatible with existing capability");
    shill_unlock_session(session);
    return EACCES;
  }
 
  // Everything checks out, add the capability
  struct shill_cap *newcap = shill_import_cap(session, &session->ss_cap_root, cap);
  session->ss_pipefactory_cap = newcap;
  shilld_log((uintptr_t)session,
             "Granted new pipe factory capbility %lx", (uintptr_t)newcap);
  shill_cap_free_rec(cap);
  shill_unlock_session(session);
  return 0;
}

static int
shill_syscall(struct thread *td, int call, void *arg) {
  struct ucred *cred = td->td_ucred;
  struct shill_session *session = shill_get_session(cred);

  switch (call) {
  case 1:
    shilld_log((uintptr_t)session,
               "Dispatching to socket module for shill system call 1");
    return shill_socket_module_syscall(cred, call, arg);
    break;
  case 2:
    shilld_log((uintptr_t)session,
               "Dispatching to pipe factory checks for shill system call 2");
    return shill_add_pipe_factory_syscall(cred, arg);
  case MKOPENDIRAT:
	return (mkopendirat(td, arg));
  default:
    shilld_log((uintptr_t)session,
               "Unsupported system call number, given: %d",
               call);
    return EDOOFUS;
  }
}

static struct mac_policy_ops ops = {
  .mpo_init = shill_init,
  .mpo_destroy = shill_destroy,

  .mpo_syscall = shill_syscall,

  /* XXX
  .mpo_bpfdesc_check_receive = shill_bpfdesc_check_receive,
  .mpo_bpfdesc_create = shill_bpfdesc_create,
  .mpo_bpfdesc_create_mbuf = shill_bpfdesc_create_mbuf,
  .mpo_bpfdesc_destroy_label = shill_bpfdesc_destroy_label,
  .mpo_bpfdesc_init_label = shill_bpfdesc_init_label,
  */

  .mpo_cred_associate_nfsd = shill_cred_associate_nfsd,
  .mpo_cred_check_relabel = shill_cred_check_relabel,
  .mpo_cred_check_setaudit = shill_cred_check_setaudit,
  .mpo_cred_check_setaudit_addr = shill_cred_check_setaudit_addr,
  .mpo_cred_check_setauid = shill_cred_check_setauid,
  .mpo_cred_check_setuid = shill_cred_check_setuid,
  .mpo_cred_check_seteuid = shill_cred_check_seteuid,
  .mpo_cred_check_setgid = shill_cred_check_setgid,
  .mpo_cred_check_setegid = shill_cred_check_setegid,
  .mpo_cred_check_setgroups = shill_cred_check_setgroups,
  .mpo_cred_check_setreuid = shill_cred_check_setreuid,
  .mpo_cred_check_setregid = shill_cred_check_setregid,
  .mpo_cred_check_setresuid = shill_cred_check_setresuid,
  .mpo_cred_check_setresgid = shill_cred_check_setresgid,
  .mpo_cred_check_visible = shill_cred_check_visible,
  .mpo_cred_copy_label = shill_cred_copy_label,

  /* No special treatment of swapper or int */
  /*
  .mpo_cred_create_swapper = shill_cred_create_swapper,
  .mpo_cred_create_init = shill_cred_create_init,
  */
  .mpo_cred_destroy_label = shill_cred_destroy_label,
  .mpo_cred_externalize_label = shill_cred_externalize_label,
  .mpo_cred_init_label = shill_cred_init_label,
  .mpo_cred_internalize_label = shill_cred_internalize_label,
  .mpo_cred_relabel = shill_cred_relabel,

  /* Not needed by shill? */
  /*
  .mpo_devfs_create_device = shill_devfs_create_device,
  .mpo_devfs_create_directory = shill_devfs_create_directory,
  .mpo_devfs_create_symlink = shill_devfs_create_symlink,
  .mpo_devfs_destroy_label = shill_devfs_destroy_label,
  .mpo_devfs_init_label = shill_devfs_init_label,
  .mpo_devfs_update = shill_devfs_update,
  .mpo_devfs_vnode_associate = shill_devfs_vnode_associate,
  */

  /* Not needed by shill */
  /*
  .mpo_ifnet_check_relabel = shill_ifnet_check_relabel,
  .mpo_ifnet_check_transmit = shill_ifnet_check_transmit,
  .mpo_ifnet_copy_label = shill_ifnet_copy_label,
  .mpo_ifnet_create = shill_ifnet_create,
  .mpo_ifnet_create_mbuf = shill_ifnet_create_mbuf,
  .mpo_ifnet_destroy_label = shill_ifnet_destroy_label,
  .mpo_ifnet_externalize_label = shill_ifnet_externalize_label,
  .mpo_ifnet_init_label = shill_ifnet_init_label,
  .mpo_ifnet_internalize_label = shill_ifnet_internalize_label,
  .mpo_ifnet_relabel = shill_ifnet_relabel,
  */

  /* XXX
  .mpo_inpcb_check_deliver = shill_inpcb_check_deliver,
  .mpo_inpcb_check_visible = shill_inpcb_check_visible,
  .mpo_inpcb_create = shill_inpcb_create,
  .mpo_inpcb_create_mbuf = shill_inpcb_create_mbuf,
  .mpo_inpcb_destroy_label = shill_inpcb_destroy_label,
  .mpo_inpcb_init_label = shill_inpcb_init_label,
  .mpo_inpcb_sosetlabel = shill_inpcb_sosetlabel,

  .mpo_ip6q_create = shill_ip6q_create,
  .mpo_ip6q_destroy_label = shill_ip6q_destroy_label,
  .mpo_ip6q_init_label = shill_ip6q_init_label,
  .mpo_ip6q_match = shill_ip6q_match,
  .mpo_ip6q_reassemble = shill_ip6q_reassemble,
  .mpo_ip6q_update = shill_ip6q_update,

  .mpo_ipq_create = shill_ipq_create,
  .mpo_ipq_destroy_label = shill_ipq_destroy_label,
  .mpo_ipq_init_label = shill_ipq_init_label,
  .mpo_ipq_match = shill_ipq_match,
  .mpo_ipq_reassemble = shill_ipq_reassemble,
  .mpo_ipq_update = shill_ipq_update,
  */

  .mpo_kenv_check_dump = shill_kenv_check_dump,
  .mpo_kenv_check_get = shill_kenv_check_get,
  .mpo_kenv_check_set = shill_kenv_check_set,
  .mpo_kenv_check_unset = shill_kenv_check_unset,

  .mpo_kld_check_load = shill_kld_check_load,
  .mpo_kld_check_stat = shill_kld_check_stat,

  /* XXX
  .mpo_mbuf_copy_label = shill_mbuf_copy_label,
  .mpo_mbuf_destroy_label = shill_mbuf_destroy_label,
  .mpo_mbuf_init_label = shill_mbuf_init_label,
  */

  .mpo_mount_check_stat = shill_mount_check_stat,
  /* Not needed by shill */ 
  /*
  .mpo_mount_create = shill_mount_create,
  .mpo_mount_destroy_label = shill_mount_destroy_label,
  .mpo_mount_init_label = shill_mount_init_label,
  */

  /* XXX
  .mpo_netatalk_aarp_send = shill_netatalk_aarp_send,

  .mpo_netinet_arp_send = shill_netinet_arp_send,
  .mpo_netinet_firewall_reply = shill_netinet_firewall_reply,
  .mpo_netinet_firewall_send = shill_netinet_firewall_send,
  .mpo_netinet_fragment = shill_netinet_fragment,
  .mpo_netinet_icmp_reply = shill_netinet_icmp_reply,
  .mpo_netinet_icmp_replyinplace = shill_netinet_icmp_replyinplace,
  .mpo_netinet_igmp_send = shill_netinet_igmp_send,
  .mpo_netinet_tcp_reply = shill_netinet_tcp_reply,

  .mpo_netinet6_nd6_send = shill_netinet6_nd6_send,
  */

  .mpo_pipe_check_ioctl = shill_pipe_check_ioctl,
  .mpo_pipe_check_poll = shill_pipe_check_poll,
  .mpo_pipe_check_read = shill_pipe_check_read,
  .mpo_pipe_check_relabel = shill_pipe_check_relabel,
  .mpo_pipe_check_stat = shill_pipe_check_stat,
  .mpo_pipe_check_write = shill_pipe_check_write,
  .mpo_pipe_copy_label = shill_pipe_copy_label,
  .mpo_pipe_create = shill_pipe_create,
  .mpo_pipe_destroy_label = shill_pipe_destroy_label,
  .mpo_pipe_externalize_label = shill_pipe_externalize_label,
  .mpo_pipe_init_label = shill_pipe_init_label,
  .mpo_pipe_internalize_label = shill_pipe_internalize_label,
  .mpo_pipe_relabel = shill_pipe_relabel,

  .mpo_posixsem_check_getvalue = shill_posixsem_check_getvalue,
  .mpo_posixsem_check_open = shill_posixsem_check_open,
  .mpo_posixsem_check_post = shill_posixsem_check_post,
  .mpo_posixsem_check_setmode = shill_posixsem_check_setmode,
  .mpo_posixsem_check_setowner = shill_posixsem_check_setowner,
  .mpo_posixsem_check_stat = shill_posixsem_check_stat,
  .mpo_posixsem_check_unlink = shill_posixsem_check_unlink,
  .mpo_posixsem_check_wait = shill_posixsem_check_wait,
  /* Not needed by shill */
  /*
  .mpo_posixsem_create = shill_posixsem_create,
  .mpo_posixsem_destroy_label = shill_posixsem_destroy_label,
  .mpo_posixsem_init_label = shill_posixsem_init_label,
  */

  .mpo_posixshm_check_create = shill_posixshm_check_create,
  .mpo_posixshm_check_mmap = shill_posixshm_check_mmap,
  .mpo_posixshm_check_open = shill_posixshm_check_open,
  .mpo_posixshm_check_setmode = shill_posixshm_check_setmode,
  .mpo_posixshm_check_setowner = shill_posixshm_check_setowner,
  .mpo_posixshm_check_stat = shill_posixshm_check_stat,
  .mpo_posixshm_check_truncate = shill_posixshm_check_truncate,
  .mpo_posixshm_check_unlink = shill_posixshm_check_unlink,
  /* Not needed by shill */
  /*
  .mpo_posixshm_create = shill_posixshm_create,
  .mpo_posixshm_destroy_label = shill_posixshm_destroy_label,
  .mpo_posixshm_init_label = shill_posixshm_init_label,
  */

  /* XXX Unimplemented
  .mpo_priv_check = shill_priv_check,
  .mpo_priv_grant = shill_priv_grant,
  */

  .mpo_proc_check_debug = shill_proc_check_debug,
  .mpo_proc_check_sched = shill_proc_check_sched,
  .mpo_proc_check_signal = shill_proc_check_signal,
  .mpo_proc_check_wait = shill_proc_check_wait,
  .mpo_proc_destroy_label = shill_proc_destroy_label,
  .mpo_proc_init_label = shill_proc_init_label,

  .mpo_socket_check_accept = shill_socket_check_accept,
  .mpo_socket_check_bind = shill_socket_check_bind,
  .mpo_socket_check_connect = shill_socket_check_connect,
  .mpo_socket_check_create = shill_socket_check_create,
  .mpo_socket_check_deliver = shill_socket_check_deliver,
  .mpo_socket_check_listen = shill_socket_check_listen,
  .mpo_socket_check_poll = shill_socket_check_poll,
  .mpo_socket_check_receive = shill_socket_check_receive,
  .mpo_socket_check_relabel = shill_socket_check_relabel,
  .mpo_socket_check_send = shill_socket_check_send,
  .mpo_socket_check_stat = shill_socket_check_stat,
  .mpo_socket_check_visible = shill_socket_check_visible,
  .mpo_socket_copy_label = shill_socket_copy_label,
  .mpo_socket_create = shill_socket_create,
  .mpo_socket_create_mbuf = shill_socket_create_mbuf,
  .mpo_socket_destroy_label = shill_socket_destroy_label,
  .mpo_socket_externalize_label = shill_socket_externalize_label,
  .mpo_socket_init_label = shill_socket_init_label,
  .mpo_socket_internalize_label = shill_socket_internalize_label,
  .mpo_socket_newconn = shill_socket_newconn,
  .mpo_socket_relabel = shill_socket_relabel,
  
  /* shill does not need socketpeer labels */
  /*
  .mpo_socketpeer_destroy_label = shill_socketpeer_destroy_label,
  .mpo_socketpeer_externalize_label = shill_socketpeer_externalize_label,
  .mpo_socketpeer_init_label = shill_socketpeer_init_label,
  .mpo_socketpeer_set_from_mbuf = shill_socketpeer_set_from_mbuf,
  .mpo_socketpeer_set_from_socket = shill_socketpeer_set_from_socket,
  */

  /* XXX
  .mpo_syncache_init_label = shill_syncache_init_label,
  .mpo_syncache_destroy_label = shill_syncache_destroy_label,
  .mpo_syncache_create = shill_syncache_create,
  .mpo_syncache_create_mbuf = shill_syncache_create_mbuf,
  */

  .mpo_system_check_acct = shill_system_check_acct,
  .mpo_system_check_audit = shill_system_check_audit,
  .mpo_system_check_auditctl = shill_system_check_auditctl,
  .mpo_system_check_auditon = shill_system_check_auditon,
  .mpo_system_check_reboot = shill_system_check_reboot,
  .mpo_system_check_swapon = shill_system_check_swapon,
  .mpo_system_check_swapoff = shill_system_check_swapoff,
  .mpo_system_check_sysctl = shill_system_check_sysctl,

  /* Not needed by shill */
  /*
  .mpo_sysvmsg_cleanup = shill_sysvmsg_cleanup,
  .mpo_sysvmsg_create = shill_sysvmsg_create,
  .mpo_sysvmsg_destroy_label = shill_sysvmsg_destroy_label,
  .mpo_sysvmsg_init_label = shill_sysvmsg_init_label,
  */

  .mpo_sysvmsq_check_msgmsq = shill_sysvmsq_check_msgmsq,
  .mpo_sysvmsq_check_msgrcv = shill_sysvmsq_check_msgrcv,
  .mpo_sysvmsq_check_msgrmid = shill_sysvmsq_check_msgrmid,
  .mpo_sysvmsq_check_msqctl = shill_sysvmsq_check_msqctl,
  .mpo_sysvmsq_check_msqget = shill_sysvmsq_check_msqget,
  .mpo_sysvmsq_check_msqrcv = shill_sysvmsq_check_msqrcv,
  .mpo_sysvmsq_check_msqsnd = shill_sysvmsq_check_msqsnd,
  /* Not needed by shill */
  /*
  .mpo_sysvmsq_cleanup = shill_sysvmsq_cleanup,
  .mpo_sysvmsq_create = shill_sysvmsq_create,
  .mpo_sysvmsq_destroy_label = shill_sysvmsq_destroy_label,
  .mpo_sysvmsq_init_label = shill_sysvmsq_init_label,
  */

  .mpo_sysvsem_check_semctl = shill_sysvsem_check_semctl,
  .mpo_sysvsem_check_semget = shill_sysvsem_check_semget,
  .mpo_sysvsem_check_semop = shill_sysvsem_check_semop,
  /* Not needed by shill */
  /*
  .mpo_sysvsem_cleanup = shill_sysvsem_cleanup,
  .mpo_sysvsem_create = shill_sysvsem_create,
  .mpo_sysvsem_destroy_label = shill_sysvsem_destroy_label,
  .mpo_sysvsem_init_label = shill_sysvsem_init_label,
  */

  .mpo_sysvshm_check_shmat = shill_sysvshm_check_shmat,
  .mpo_sysvshm_check_shmctl = shill_sysvshm_check_shmctl,
  .mpo_sysvshm_check_shmdt = shill_sysvshm_check_shmdt,
  .mpo_sysvshm_check_shmget = shill_sysvshm_check_shmget,
  /* Not needed by shill */
  /*
  .mpo_sysvshm_cleanup = shill_sysvshm_cleanup,
  .mpo_sysvshm_create = shill_sysvshm_create,
  .mpo_sysvshm_destroy_label = shill_sysvshm_destroy_label,
  .mpo_sysvshm_init_label = shill_sysvshm_init_label,
  */

  /* Not needed by shill */
  /*
  .mpo_thread_userret = shill_thread_userret,
  */

  .mpo_vnode_check_access = shill_vnode_check_access,
  .mpo_vnode_check_chdir = shill_vnode_check_chdir,
  .mpo_vnode_check_chroot = shill_vnode_check_chroot,
  .mpo_vnode_check_create = shill_vnode_check_create,
  .mpo_vnode_post_create = shill_vnode_post_create,
  .mpo_vnode_check_deleteacl = shill_vnode_check_deleteacl,
  .mpo_vnode_check_deleteextattr = shill_vnode_check_deleteextattr,
  .mpo_vnode_check_exec = shill_vnode_check_exec,
  .mpo_vnode_check_getacl = shill_vnode_check_getacl,
  .mpo_vnode_check_getextattr = shill_vnode_check_getextattr,
  .mpo_vnode_check_link = shill_vnode_check_link,
  .mpo_vnode_check_listextattr = shill_vnode_check_listextattr,
  .mpo_vnode_check_lookup = shill_vnode_check_lookup,
  .mpo_vnode_post_lookup = shill_vnode_post_lookup,
  .mpo_vnode_check_mmap = shill_vnode_check_mmap,
  .mpo_vnode_check_mmap_downgrade = shill_vnode_check_mmap_downgrade,
  .mpo_vnode_check_mprotect = shill_vnode_check_mprotect,
  .mpo_vnode_check_open = shill_vnode_check_open,
  .mpo_vnode_check_poll = shill_vnode_check_poll,
  .mpo_vnode_check_read = shill_vnode_check_read,
  .mpo_vnode_check_readdir = shill_vnode_check_readdir,
  .mpo_vnode_check_readlink = shill_vnode_check_readlink,
  .mpo_vnode_check_relabel = shill_vnode_check_relabel,
  .mpo_vnode_check_rename_from = shill_vnode_check_rename_from,
  .mpo_vnode_check_rename_to = shill_vnode_check_rename_to,
  .mpo_vnode_check_revoke = shill_vnode_check_revoke,
  .mpo_vnode_check_setacl = shill_vnode_check_setacl,
  .mpo_vnode_check_setextattr = shill_vnode_check_setextattr,
  .mpo_vnode_check_setflags = shill_vnode_check_setflags,
  .mpo_vnode_check_setmode = shill_vnode_check_setmode,
  .mpo_vnode_check_setowner = shill_vnode_check_setowner,
  .mpo_vnode_check_setutimes = shill_vnode_check_setutimes,
  .mpo_vnode_check_stat = shill_vnode_check_stat,
  .mpo_vnode_check_unlink = shill_vnode_check_unlink,
  .mpo_vnode_check_write = shill_vnode_check_write,
  /* XXX Unimplemented
  .mpo_vnode_associate_extattr = shill_vnode_associate_extattr,
  .mpo_vnode_associate_singlelabel = shill_vnode_associate_singlelabel,
  .mpo_vnode_create_extattr = shill_vnode_create_extattr,
  .mpo_vnode_setlabel_extattr = shill_vnode_setlabel_extattr,
  */
  .mpo_vnode_destroy_label = shill_vnode_destroy_label,
  .mpo_vnode_copy_label = shill_vnode_copy_label,
  .mpo_vnode_execve_transition = shill_vnode_execve_transition,
  .mpo_vnode_execve_will_transition = shill_vnode_execve_will_transition,
  .mpo_vnode_externalize_label = shill_vnode_externalize_label,
  .mpo_vnode_init_label = shill_vnode_init_label,
  .mpo_vnode_internalize_label = shill_vnode_internalize_label,
  .mpo_vnode_relabel = shill_vnode_relabel,
};

MAC_POLICY_SET(&ops, shill, "Shill Sandbox Module",
	       MPC_LOADTIME_FLAG_NOTLATE, &shill_slot);

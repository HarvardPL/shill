#include <sys/param.h>
#include <sys/kernel.h>
#include <sys/lock.h>
#include <sys/mac.h>
#include <sys/queue.h>
#include <sys/systm.h>
#include <sys/ucred.h>
#include <sys/proc.h>

#include "shill.h"
#include "shilldev.h"
#include "proc_checks.h"

static inline int
allow_same_session(const char *check, struct ucred *cred, struct proc *p) {
  struct shill_session *subject = shill_get_session(cred);

  if (subject == NULL) {
    // Don't enforce restrictions on un-sandboxed processes
    return 0;
  }

  // A process can interact with a process in the same session
  // or in a descendent session.
  struct shill_session *object = shill_get_session(p->p_ucred);
  while (object != NULL) {
    if (subject == object) { return 0; }
    object = shill_session_parent(object);
  }

  if (shill_session_debug(subject)) {
    shilld_log((uintptr_t)subject, "Permitting %s on %lx for debugging",
               check, (uintptr_t)p);
    return 0;
  } else {
    shilld_log((uintptr_t)subject, "Denied %s on %lx",
               check, (uintptr_t)p);
    return EACCES;
  }
}

int
shill_proc_check_debug(struct ucred *cred,
                       struct proc *proc) {
  return allow_same_session("process debug", cred, proc);
}

int
shill_proc_check_sched(struct ucred *cred,
                       struct proc *proc) {
  return allow_same_session("process sched", cred, proc);
}

int
shill_proc_check_signal(struct ucred *cred,
                        struct proc *proc, int signum) {
  return allow_same_session("process signal", cred, proc);
}

int
shill_proc_check_wait(struct ucred *cred,
                      struct proc *proc) {
  return allow_same_session("process wait", cred, proc);
}

void
shill_proc_destroy_label(struct label *label) {
  // Intentionally left blank. Shill relies on
  // the process's credential's label.
}

void
shill_proc_init_label(struct label *label) {
  // Intentionally left blank. Shill relies on
  // the process's credential's label.
}

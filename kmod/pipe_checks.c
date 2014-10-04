#include <sys/param.h>
#include <sys/kernel.h>
#include <sys/lock.h>
#include <sys/mac.h>
#include <sys/mman.h>
#include <sys/mutex.h>
#include <sys/namei.h>
#include <sys/queue.h>
#include <sys/systm.h>
#include <sys/ucred.h>
#include <sys/selinfo.h>
#include <sys/pipe.h>

#include "cflags.h"
#include "shill.h"
#include "shilldev.h"
#include "pipe_checks.h"

int
shill_pipe_check_ioctl(struct ucred *cred,
                       struct pipepair *pp, struct label *pplabel,
                       unsigned long cmd, void *data) {
  return shill_cap_check_any("pipe_ioctl", cred, pp, pplabel);
    //  return shill_deny_all("pipe_ioctl", cred, pp);
}

int
shill_pipe_check_poll(struct ucred *cred,
                      struct pipepair *pp, struct label *pplabel) {
  return shill_cap_check_rights("pipe_poll", cred, pp, pplabel, C_READ);
}

int
shill_pipe_check_read(struct ucred *cred,
                      struct pipepair *pp, struct label *pplabel) {
  return shill_cap_check_rights("pipe_read", cred, pp, pplabel, C_READ);
}

int
shill_pipe_check_relabel(struct ucred *cred,
                         struct pipepair *pp, struct label *pplabel,
                         struct label *newlabel) {
  return shill_cap_check_relabel(cred, pp, pplabel, newlabel);
}

int
shill_pipe_check_stat(struct ucred *cred,
                      struct pipepair *pp, struct label *pplabel) {
  return shill_cap_check_rights("pipe_cap", cred, pp, pplabel, C_STAT);
}

int
shill_pipe_check_write(struct ucred *cred,
                       struct pipepair *pp, struct label *pplabel) {
  // XXX Don't need to require both here?
  return shill_cap_check_rights("pipe_write", cred, pp, pplabel, C_WRITE | C_APPEND);
}

void
shill_pipe_create(struct ucred *cred, struct pipepair *pp,
                  struct label *pplabel) {
  struct shill_session *session = shill_get_session(cred);
  if (session == NULL)
    return;

  struct shill_cap *cap = shill_create_pipe_cap(session);
  shill_set_cap(pplabel, session, cap);
}

void
shill_pipe_destroy_label(struct label *label) {
  shill_cap_destroy_label(label);
}

void
shill_pipe_init_label(struct label *label) {
  shill_cap_init_label(label);
}

void
shill_pipe_copy_label(struct label *src, struct label *dst) {
  shill_cap_copy_label(src,dst);
}

int
shill_pipe_internalize_label(struct label *label,
                             char *element_name,
                             char *element_data,
                             int *claimed) {
  return shill_cap_internalize_label(label,element_name,element_data,claimed);
}

int
shill_pipe_externalize_label(struct label *label,
                             char *element_name,
                             struct sbuf *sb, int *claimed) {
  // XXX implement this
  return 0;
}

void
shill_pipe_relabel(struct ucred *cred, struct pipepair *pp,
                   struct label *oldlabel, struct label *newlabel) {
  shill_cap_relabel(cred, pp, oldlabel, newlabel);
}

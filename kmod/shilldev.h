#ifndef SHILLDEV_H
#define SHILLDEV_H

#include "message.h"

struct shill_cap;

struct shill_message_entry {
  STAILQ_ENTRY(shill_message_entry) entries; /* message queue */
  struct shill_message msg;
};

void shilld_logcap(uintptr_t _session, const struct shill_cap *_cap);
void shilld_log(uintptr_t _session, const char *_fmt, ...) __printflike(2,3);
void shilld_logall(const char *_fmt, ...) __printflike(1,2);

#endif /* SHILLDEV_H */

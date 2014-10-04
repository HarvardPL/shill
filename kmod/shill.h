#ifndef SHILL_H
#define SHILL_H

#define MAC_SHILL_LABEL_NAME "shill"

#include "shill-data.h"

void shill_cap_free_rec(struct shill_cap *);

struct label;

void shill_lock_session(struct shill_session *session);
void shill_unlock_session(struct shill_session *session);

struct shill_session *shill_get_session(struct ucred *);
struct shill_session *extract_shill_session(struct label *label);
struct shill_session *extract_current_shill_session(struct label *label);
struct shill_session *shill_session_parent(struct shill_session *);
void set_shill_session(struct label *label, struct shill_session *session);
bool shill_session_debug(struct shill_session *);

struct shill_cap *shill_get_cap(struct label *, struct shill_session *);
void shill_set_cap(struct label *, struct shill_session *, struct shill_cap *);
struct shill_cap *shill_merge_cap(struct shill_session *, struct shill_cap *, struct shill_cap *);
struct shill_cap *shill_create_pipe_cap(struct shill_session *);

void shill_cap_init_label(struct label *);
void shill_cap_copy_label(struct label *, struct label *);
int shill_cap_check_any(const char *, struct ucred *, void *, struct label *);
int shill_deny_all(const char *, struct ucred *, void *);
int shill_allow_all(const char *, struct ucred *, void *);
int shill_cap_check_rights(const char *, struct ucred *, void *, struct label *, uint32_t);
int shill_cap_internalize_label(struct label *, char *, char *, int *);
int shill_cap_check_relabel(struct ucred *, void *, struct label *, struct label *);
void shill_cap_relabel(struct ucred *, void *, struct label *, struct label *);
void shill_cap_destroy_label(struct label *);

#endif /* SHILL_H */

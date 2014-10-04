#ifndef SHILL_MEM_MANAGE_H
#define SHILL_MEM_MANAGE_H

#include <sys/param.h> /* for KASSERT */
#include <sys/systm.h> /* for KASSERT */

/* in all the macros below, the arguments should be identifiers */

#define DECLARE_ALLOC(type_id, zone_id, count_id)                       \
  struct type_id *                                                      \
  type_id ## _alloc(void);

#define DEFINE_ALLOC(type_id, zone_id, count_id, flags_exp)             \
  struct type_id *                                                      \
  type_id ## _alloc(void) {                                             \
    struct type_id *_shill_temp = uma_zalloc(zone_id, M_ZERO | (flags_exp)); \
    KASSERT(_shill_temp != NULL, (#type_id "_alloc: allocation failed")); \
    atomic_add_rel_int(&count_id, 1);                                   \
    return _shill_temp;                                                 \
  }

#define DECLARE_FREE(type_id, zone_id, count_id)                        \
  void                                                                  \
  type_id ## _free(struct type_id *_shill_temp);

#define DEFINE_FREE(type_id, zone_id, count_id)                         \
  void                                                                  \
  type_id ## _free(struct type_id *_shill_temp) {                       \
    if (_shill_temp != NULL)                                            \
      atomic_add_rel_int(&count_id, -1);                                \
    uma_zfree(zone_id, _shill_temp);                                    \
  }

#endif /* SHILL_MEM_MANAGE_H */

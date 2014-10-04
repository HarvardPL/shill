#ifndef SHILLMESSAGE_H
#define SHILLMESSAGE_H

#define SHILLMSG_CAP 0
#define SHILLMSG_STR 1
#define SHILLMSG_LEN 512

struct shill_cap_enc {
  uint32_t ce_flags;
  uint16_t ce_lookup;
  uint16_t ce_createfile;
  uint16_t ce_createdir;
  uint16_t ce_fill;
};

struct shill_message_cap {
  uintptr_t mc_id;
  uint32_t mc_flags;
  uintptr_t mc_lookup;
  uintptr_t mc_createfile;
  uintptr_t mc_createdir;
};

struct shill_message {
  uintptr_t m_session;
  int m_type;
  union {
    struct shill_message_cap d_cap;
    char d_str[SHILLMSG_LEN];
  } m_data;
};

#endif /* SHILLMESSAGE_H */

#include <sys/types.h>
#include <sys/uio.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "cflags.h"
#include "message.h"

inline void
print_right(uint32_t flags, uint32_t right, const char *name) {
  if ((flags & right) != 0)
    printf("%s ", name);
}

void
print_cap(uintptr_t session, struct shill_message_cap *cap) {
  printf("Session: %lx Capability: %lx\n", session, cap->mc_id);
  print_right(cap->mc_flags,C_CHDIR,"+chdir");
  print_right(cap->mc_flags,C_CHROOT,"+chroot");
  print_right(cap->mc_flags,C_CREATEFILE,"+create-file");
  if ((cap->mc_flags & C_CREATEFILE) && (cap->mc_createfile != (uintptr_t)NULL)) {
	printf("{ %lx } ", cap->mc_createfile);
  }
  print_right(cap->mc_flags,C_CREATEDIR,"+create-dir");
  if ((cap->mc_flags & C_CREATEDIR) && (cap->mc_createdir != (uintptr_t)NULL)) {
	printf("{ %lx } ", cap->mc_createdir);
  }
  print_right(cap->mc_flags,C_EXEC,"+exec");
  print_right(cap->mc_flags,C_LOOKUP,"+lookup");
  if ((cap->mc_flags & C_LOOKUP) && (cap->mc_lookup != (uintptr_t)NULL)) {
	printf("{ %lx } ", cap->mc_lookup);
  }
  print_right(cap->mc_flags,C_CONTENT,"+content");
  print_right(cap->mc_flags,C_ADDLNK,"+addlink");
  print_right(cap->mc_flags,C_MAKELNK,"+makelink");
  print_right(cap->mc_flags,C_UNLINKFILE,"+unlink-file");
  print_right(cap->mc_flags,C_UNLINKDIR,"+unlink-dir");
  print_right(cap->mc_flags,C_READ,"+read");
  print_right(cap->mc_flags,C_WRITE,"+write");
  print_right(cap->mc_flags,C_APPEND,"+append");
  print_right(cap->mc_flags,C_CHMODE,"+chmode");
  print_right(cap->mc_flags,C_CHOWN,"+chown");
  print_right(cap->mc_flags,C_CHFLAGS,"+chflags");
  print_right(cap->mc_flags,C_CHTIMES,"+chtimes");
  print_right(cap->mc_flags,C_STAT,"+stat");
  printf("\n");
}

void
print_message(uintptr_t session, char msg[]) {
  char buf[SHILLMSG_LEN];
  snprintf(buf, SHILLMSG_LEN, "%s", msg);
  if (session == (uintptr_t)NULL) {
	printf("Message: %s\n", buf);
  } else {
	printf("Session: %lx Message: %s\n", session, buf);
  }
}

int main(int argc, char *argv[]) {
  struct shill_message msg;
  ssize_t n;

  int fd = open("/dev/shilld", O_RDONLY);
  if (fd == -1) {
    perror("Error occured opening /dev/shilld");
    exit(-1);
  }

  while (1) {
    n = read(fd, &msg, sizeof(struct shill_message));
    if (n == sizeof(struct shill_message)) {
      switch (msg.m_type) {
      case SHILLMSG_CAP:
	print_cap(msg.m_session, &msg.m_data.d_cap);
	break;
      case SHILLMSG_STR:
	print_message(msg.m_session, msg.m_data.d_str);
	break;
      default:
	printf("Unknown message type!\n");
      }
    }
    fflush(stdout);
  }
}

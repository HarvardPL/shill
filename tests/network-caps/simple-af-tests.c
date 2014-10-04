#include <sys/param.h>
#include <sys/lock.h>
#include <sys/mutex.h>
#include <sys/kernel.h>
#include <sys/module.h>
#include <sys/mman.h>
#include <sys/namei.h>
#include <sys/ucred.h>
#include <sys/queue.h>
#include <sys/vnode.h>
#include <sys/sysctl.h>
#include <sys/syslog.h>
#include <vm/uma.h>

#include <sys/types.h>
#include <sys/socket.h> /* for AF_INET, AF_LOCAL */

#include <sys/mac.h>

#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>

int finish_and_print_results(int failures);

int main() {
  int err;
  int failures = 0;
  mac_t mac_cmd;
  uint64_t ipc_no_permissions[3] = { AF_UNIX,
                                     SOCK_STREAM,
                                     0 }; /* no permissions */

  /* TEST: make syscall before session */
  {
    err = mac_syscall("shill", 1, ipc_no_permissions);

    if (0 == err) {
      printf("test failed: mac_syscall before session is started should fail\n");
      printf("  line number: %d\n", __LINE__);
      printf("  real error num %d\n", errno);
      perror("  mac_syscall:");
      ++failures;
    }
  }
  /* ENDTEST: make syscall before session */


  if (0 != (err = mac_from_text(&mac_cmd, "shill/init"))) {
    printf("mac_from_text returned %d\n", err);
    printf("** failed to initialize shill, this is a fundamental problem **\n");
    return -1;
  }
  if (0 != (err = mac_set_proc(mac_cmd))) {
    printf("mac_set_proc returned %d\n", err);
    printf("** failed to initialize shill, this is a fundamental problem **\n");
    return -1;
  }

  /* TEST: make syscall while session is in init state */
  {
    err = mac_syscall("shill", 1, ipc_no_permissions);

    if (0 != err) {
      printf("test failed: mac_syscall after session started should succeed\n");
      printf("  line number: %d\n", __LINE__);
      printf("  real error num %d\n", errno);
      perror("  mac_syscall:");

      printf("** terminating early because later tests depend on previous "
             "failed test **\n");
      return finish_and_print_results(++failures);
    }
  }
  /* ENDTEST: make syscall while session is in init state */

  if (0 != (err = mac_from_text(&mac_cmd, "shill/start"))) {
    printf("mac_from_text returned %d\n", err);
    printf("** failed to initialize shill, this is a fundamental problem **\n");
    return -1;
  }
  if (0 != (err = mac_set_proc(mac_cmd))) {
    printf("mac_set_proc returned %d\n", err);
    printf("** failed to initialize shill, this is a fundamental problem **\n");
    return -1;
  }

  /* TEST: create socket outside of acceptable address family */
  {
    int sock;
    sock = socket(AF_INET, SOCK_STREAM, 0); /* a TCP/IP socket */

    if (sock != -1) {
      printf("test failed: creating a socket with an address family to which "
             "the session has no capabilities should fail.\n");
      printf("  line number: %d\n", __LINE__);
      printf("  real error num %d\n", errno);
      perror("  socket:");
      ++failures;
    }
  }
  /* ENDTEST: create socket outside of acceptable address family */

  /* TEST: create socket in acceptable address family */
  {
    int sock;
    sock = socket(AF_UNIX, SOCK_STREAM, 0); /* an IPC socket */

    if (sock == -1) {
      printf("test failed: creating a socket with an address family to which "
             "the session has a capability should succeed.\n");
      printf("  line number: %d\n", __LINE__);
      printf("  real error num %d\n", errno);
      perror("  socket:");
      ++failures;
    }
  }
  /* ENDTEST: create socket in acceptable address family */

  /* TEST: create socket with unacceptable socket type */
  {
    int current_errno;
    int sock;
    sock = socket(AF_UNIX, SOCK_DGRAM, 0); /* an IPC socket */

    if (sock != -1) {
      printf("test failed: creating a socket with a socket type for which "
             "the session doesn't hold a capability should fail.\n");
      printf("  line number: %d\n", __LINE__);
      printf("  real error num %d\n", errno);
      perror("  socket:");
      ++failures;
    }

    current_errno = errno;

    if (sock == -1 && current_errno != EACCES) {
      printf("test failed: creating a socket with a socket type for which "
             "the session doesn't hold a capability should fail with "
             "EACCES (%d), but actually failed with %d.\n",
             EACCES,
             current_errno);
      printf("  line number: %d\n", __LINE__);
      printf("  real error num %d\n", current_errno);
      perror("  socket:");
      ++failures;
    }
  }
  /* ENDTEST: create socket with unacceptable socket type */

  return finish_and_print_results(failures);
}


int finish_and_print_results(int failures) {
  /* test results, don't place tests below this */
  if (failures == 0) {
    printf("All tests passed!\n");
  } else {
    printf("%d tests failed.\n", failures);
  }

  return failures;
}

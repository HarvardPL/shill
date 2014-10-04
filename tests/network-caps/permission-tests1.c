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
#include <netinet/in.h>

#include <strings.h> /* for bzero */

#include "../../kmod/shill-socket-permissions.h" /* for SHILL_SOCKET_CAP_* macros */

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
  uint64_t inet_client_w[3] = { AF_INET,
                                SOCK_STREAM,
                                SHILL_SOCKET_CAP_CONNECT |
                                SHILL_SOCKET_CAP_SEND }; /* no permissions */
  uint64_t inet_server_r[3] = { AF_INET,
                                SOCK_STREAM,
                                SHILL_SOCKET_CAP_BIND |
                                SHILL_SOCKET_CAP_LISTEN |
                                SHILL_SOCKET_CAP_ACCEPT |
                                SHILL_SOCKET_CAP_RECV }; /* no permissions */

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

  err = mac_syscall("shill", 1, inet_client_w);
  if (0 != err) {
    printf("couldn't add inet_client_w cap, cannot continue with test\n");
    printf("  line number: %d\n", __LINE__);
    printf("  mac_syscall returned %d\n", err);
    printf("  errno %d\n", errno);
    perror("  errmsg:");
    return -1;
  }
  err = mac_syscall("shill", 1, inet_server_r);
  if (0 != err) {
    printf("couldn't add inet_server_r cap, cannot continue with test\n");
    printf("  line number: %d\n", __LINE__);
    printf("  mac_syscall returned %d\n", err);
    printf("  errno %d\n", errno);
    perror("  errmsg:");
    return -1;
  }

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
    sock = socket(AF_UNIX, SOCK_STREAM, 0); /* an IPC socket */

    if (sock != -1) {
      printf("test failed: creating a socket with an address family to which "
             "the session has no capabilities should fail.\n");
      printf("  line number: %d\n", __LINE__);
      printf("  real error num %d\n", errno);
      perror("  socket");
      ++failures;
    }
  }
  /* ENDTEST: create socket outside of acceptable address family */

  {
    /* TEST: create socket in acceptable address family */
    int sock;
    sock = socket(AF_INET, SOCK_STREAM, 0); /* a TCP/IP socket */

    if (sock == -1) {
      printf("test failed: creating a socket with an address family to which "
             "the session has a capability should succeed.\n");
      printf("  line number: %d\n", __LINE__);
      printf("  real error num %d\n", errno);
      perror("  socket");

      printf("** terminating early because later tests depend on previous "
             "failed test **\n");
      return finish_and_print_results(++failures);
    }
    /* ENDTEST: create socket in acceptable address family */

    /* TEST: try to bind */
    struct sockaddr_in sa;

    bzero(&sa, sizeof sa);

    sa.sin_family = AF_INET;
    sa.sin_port = htons(4242);
    sa.sin_addr.s_addr = htonl((((((127 << 8) | 0) << 8) | 0) << 8) | 1);

    err = bind(sock, (struct sockaddr *)&sa, sizeof sa);

    if (err == -1) {
      printf("test failed: binding an INET socket with appropriate capability "
             "should have succeeded.\n");
      printf("  line number: %d\n", __LINE__);
      printf("  errno %d\n", errno);
      perror("  socket:");

      close(sock);
      printf("** terminating early because later tests depend on previous "
             "failed test **\n");
      return finish_and_print_results(++failures);
    }
    /* ENDTEST: try to bind */
    /* TEST: try to listen */
    err = listen(sock, 1);

    if (err == -1) {
      printf("test failed: listening on an INET socket with appropriate "
             "capability should have succeeded.\n");
      printf("  line number: %d\n", __LINE__);
      printf("  errno %d\n", errno);
      perror("  socket:");
      ++failures;
    }
    /* ENDTEST: try to listen */
    /* TEST: try to accept */
    /* how do I do this without another socket? */
    /* ENDTEST: try to accept */
    close(sock);
  }

  {
    /* TEST: create socket in acceptable address family */
    int sock;
    sock = socket(AF_INET, SOCK_STREAM, 0); /* a TCP/IP socket */

    if (sock == -1) {
      printf("test failed: creating a socket with an address family to which "
             "the session has a capability should succeed.\n");
      printf("  line number: %d\n", __LINE__);
      printf("  real error num %d\n", errno);
      perror("  socket:");

      printf("** terminating early because later tests depend on previous "
             "failed test **\n");
      return finish_and_print_results(++failures);
    }
    /* ENDTEST: create socket in acceptable address family */
    /* TEST: try to connect */
    struct sockaddr_in sa;

    bzero(&sa, sizeof sa);

    sa.sin_family = AF_INET;
    sa.sin_port = htons(22); /* hopefully sshd is running ... */
    sa.sin_addr.s_addr = htonl((((((127 << 8) | 0) << 8) | 0) << 8) | 1);

    err = connect(sock, (struct sockaddr *)&sa, sizeof sa);

    if (err == -1) {
      printf("test failed: connecting on an INET socket with appropriate "
             "capability should have succeeded.\n");
      printf("  line number: %d\n", __LINE__);
      printf("  errno %d\n", errno);
      perror("  socket:");

      printf("** terminating early because later tests depend on previous "
             "failed test **\n");
      return finish_and_print_results(++failures);
    }
    /* ENDTEST: try to connect */
    /* TEST: try to write */
    {
      int buf[100];
      err = write(sock, &buf, 100);

      if (err == -1) {
        printf("test failed: connecting on an INET socket with appropriate "
               "capability should have succeeded.\n");
        printf("  line number: %d\n", __LINE__);
        printf("  errno %d\n", errno);
        perror("  socket:");
        ++failures;
      }
    }
    /* ENDTEST: try to write */
    close(sock);
  }

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

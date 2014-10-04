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

#include <sys/socket.h> /* for AF_INET, AF_LOCAL */
#include <netinet/in.h>

#include <strings.h> /* for bzero */

#include "../../kmod/shill-socket-permissions.h" /* for SHILL_SOCKET_CAP_* macros */

#include <sys/mac.h>

#include <errno.h>

#include <sys/types.h> /* pid_t */
#include <sys/wait.h>  /* waitpid */
#include <stdio.h>     /* printf, perror */
#include <stdlib.h>    /* exit */
#include <unistd.h>    /* _exit, fork */

#include <sys/uio.h> /* for read */

int finish_and_print_results(int failures);
int client();
int server(int pid);


#define TEST_VALUE1 42


int main() {
  int err;
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

  pid_t pid = fork();

  if (pid == -1) {
    perror("fork failed");
    return -1;
  }
  else if (pid == 0) {
    return client();
  }
  else {
    return server(pid);
  }
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

int client() {
  int err;
  int failures = 0;
  int client;
  struct sockaddr_in sa;
  bzero(&sa, sizeof sa);
  sa.sin_family = AF_INET;
  sa.sin_port = htons(4242);
  sa.sin_addr.s_addr = htonl((((((127 << 8) | 0) << 8) | 0) << 8) | 1);

  usleep(100000); /* give the server time to initialize */

  client = socket(AF_INET, SOCK_STREAM, 0);
  if (client == -1) {
    printf("Could not create a client socket, errno: %d\n", errno);
    perror("  socket:");
    return -1;
  }

  err = connect(client, (struct sockaddr *)&sa, sizeof sa);

  if (err == -1) {
    printf("Could not connect on client socket, errno %d\n", errno);
    perror("  connect:");
    return -1;
  }

  /* TEST: write to a write-only client socket */
  {
    int buf[1] = {TEST_VALUE1};
    int err = write(client, &buf, sizeof(int));

    if (err == -1) {
      printf("test failed: write to a write-only client socket should "
             "succeed.\n");
      printf("  line number: %d\n", __LINE__);
      printf("  real error num %d\n", errno);
      perror("  write");
      ++failures;
    }
  }
  /* ENDTEST: write to a write-only client socket */

  {
    int buf[1] = {TEST_VALUE1};
    int current_errno;

    /* TEST: read on a write-only client socket should fail */
    err = read(client, &buf, sizeof(int));
    if (err != -1) {
      printf("test failed: read on a write-only client socket should fail.\n");
      printf("  line number: %d\n", __LINE__);
      ++failures;
    }

    current_errno = errno;

    if (err == -1 && current_errno != EACCES) {
      printf("test failed: read on a write-only client socket should fail with "
             "EACCES (%u), actually failed with %u.\n",
             EACCES,
             current_errno);
      printf("  line number: %d\n", __LINE__);
      printf("  real error num %d\n", current_errno);
      perror("  write");
      ++failures;
    }
    /* ENDTEST: read on a write-only client socket should fail */
  }

  close(client);
  return failures;
}

int server(int pid) {
  int err;
  int failures = 0;
  int server;
  struct sockaddr_in sa;
  bzero(&sa, sizeof sa);
  sa.sin_family = AF_INET;
  sa.sin_port = htons(4242);
  sa.sin_addr.s_addr = htonl((((((127 << 8) | 0) << 8) | 0) << 8) | 1);

  server = socket(AF_INET, SOCK_STREAM, 0);
  if (server == -1) {
    printf("Could not create a server socket, errno: %d\n", errno);
    perror("  socket:");
    return -1;
  }

  err = bind(server, (struct sockaddr *)&sa, sizeof sa);

  if (err == -1) {
    printf("Could not bind server socket, errno %d\n", errno);
    perror("  bind:");
    return -1;
  }

  err = listen(server, 10);

  if (err == -1) {
    printf("Could not listen on server socket, errno %d\n", errno);
    perror("  listen:");
    return -1;
  }

  int remote = accept(server, NULL, NULL);

  if (remote == -1) {
    printf("Could not accept a connect on the server socket, errno %d\n", errno);
    perror("  accept:");
    return -1;
  }

  {
    int buf[1] = {0};

    /* TEST: read on a read-only server socket should succeed */
    err = read(remote, &buf, sizeof(int));
    if (err == -1) {
      printf("test failed: read on a read-only server socket should "
             "succeed.\n");
      printf("  line number: %d\n", __LINE__);
      printf("  real error num %d\n", errno);
      perror("  write");
      ++failures;
    }
    /* ENDTEST: read on a read-only server socket should succeed */

    /* TEST: verify that the read received the correct value */
    if (buf[0] != TEST_VALUE1) {
      printf("test failed: read should have recieved %d, but actually "
             "received %d.\n",
             TEST_VALUE1,
             buf[0]);
      printf("  line number: %d\n", __LINE__);
      printf("  real error num %d\n", errno);
      perror("  write");
      ++failures;
    }
    /* ENDTEST: verify that the read received the correct value */
  }

  {
    int buf[1] = {TEST_VALUE1};
    int current_errno;

    /* TEST: write on a read-only server socket should fail */
    err = write(remote, &buf, sizeof(int));
    if (err != -1) {
      printf("test failed: write on a read-only server socket should "
             "fail.\n");
      printf("  line number: %d\n", __LINE__);
      ++failures;
    }

    current_errno = errno;

    if (err == -1 && current_errno != EACCES) {
      printf("test failed: write on a read-only server socket should "
             "fail with EACCES (%u), actually failed with %u.\n",
             EACCES,
             current_errno);
      printf("  line number: %d\n", __LINE__);
      printf("  real error num %d\n", current_errno);
      perror("  write");
      ++failures;
    }
    /* ENDTEST: write on a read-only server socket should fail */
  }

  int client_failures = -1;
  waitpid(pid, &client_failures, 0);
  close(server);

  if (client_failures == -1) {
    return -1;
  } else {
    return finish_and_print_results(failures + WEXITSTATUS(client_failures));
  }
}

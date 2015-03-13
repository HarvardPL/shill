#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mac.h>
#include <sys/queue.h> /* for shill.h */
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/module.h>
#include <sys/syscall.h>
#include <sys/wait.h>

#include "message.h"
#include "sandbox.h"

#define SHILL_MAX_CAP_LEN ((MAC_MAX_LABEL_ELEMENT_DATA-6) / sizeof(struct shill_cap_enc))

int shill_cap_encode(struct shill_cap_enc buf[], struct shill_cap *cap, int i, int free) {
  if (i >= SHILL_MAX_CAP_LEN)
    return -1;

  buf[i].ce_flags = cap->sc_flags | C_MASKS;

  uint16_t lookup = (cap->sc_lookup != NULL) ? (free++) : 0;
  uint16_t createfile = (cap->sc_createfile != NULL) ? (free++) : 0;
  uint16_t createdir = (cap->sc_createdir != NULL) ? (free++) : 0;

  buf[i].ce_lookup = (lookup+1) | 0x8000;
  buf[i].ce_createfile = (createfile+1) | 0x8000;
  buf[i].ce_createdir = (createdir+1) | 0x8000;
  buf[i].ce_fill = 0x8888;

  if (lookup != 0)
    free = shill_cap_encode(buf, cap->sc_lookup, lookup, free);

  if (createfile != 0)
    free = shill_cap_encode(buf, cap->sc_createfile, createfile, free);

  if (createdir != 0)
    free = shill_cap_encode(buf, cap->sc_createdir, createdir, free);

  return free;
}

int shill_grant(int fd, struct shill_cap *cap) {
  char buf[MAC_MAX_LABEL_ELEMENT_DATA];
  strcpy(buf, "shill/");

  struct shill_cap_enc caps[SHILL_MAX_CAP_LEN];
  memset(caps, 0, SHILL_MAX_CAP_LEN);
  if (-1 == shill_cap_encode(caps, cap, 0, 1)) {
    errno = EINVAL;
    return -1;
  }
  memcpy(&buf[6], caps, SHILL_MAX_CAP_LEN * sizeof(struct shill_cap_enc));

  mac_t label;
  int ret;
  if (0 != (ret = mac_from_text(&label, (const char *)buf))) {
    return ret;
  }

  return mac_set_fd(fd, label);
}

int shill_grant_pipefactory(struct shill_cap *cap) {
  struct shill_cap_enc caps[SHILL_MAX_CAP_LEN];
  memset(caps, 0, SHILL_MAX_CAP_LEN);
  if (-1 == shill_cap_encode(caps, cap, 0, 1)) {
    errno = EINVAL;
    return -1;
  }

  return mac_syscall("shill", 2, &caps);
}

static int shill_logpaths_num = -1;

int shill_logpaths() {
  int modid;
  struct module_stat stat;

  if (shill_logpaths_num == -1) {
	stat.version = sizeof(stat);
	modid = modfind("sys/shill_logpaths");
	if (modid == -1) {
	  errno = ENOSYS;
	  return -1;
	}
	if (modstat(modid, &stat) == -1) {
	  return -1;
	}
	shill_logpaths_num = stat.data.intval;
  }
  
  return syscall(shill_logpaths_num);
}

int shill_init() {
  int err;
  mac_t init;
  if (0 != (err = mac_from_text(&init, "shill/init")))
    return err;

  return mac_set_proc(init);
}

int shill_enter() {
  int err;
  mac_t start;
  if (0 != (err = mac_from_text(&start, "shill/start")))
    return err;

  shill_logpaths();
  return mac_set_proc(start);
}

int shill_debug() {
  int err;
  mac_t start;
  if (0 != (err = mac_from_text(&start, "shill/debug")))
    return err;

  shill_logpaths();
  return mac_set_proc(start);
}

extern char **environ;

int shill_sandbox(int execfd, int stdin, int stdout, int stderr,
                  int capfds[], struct shill_cap *caps[], int lim,
                  rlim_t timeout, int debug, char *const argv[],
                  uint64_t *netcaps, int netcapcount,
                  struct shill_cap *pipefactory) {
  pid_t cpid;

  if (lim < 0) {
    errno = EINVAL;
    return -1;
  }

  cpid = fork();
  if (-1 == cpid)
    return -1;

  if (0 == cpid) {
    // child
    int err;
    if (0 != (err = shill_init()))
      exit(err);

    for (int i=0; i < lim; i++) {
      err = shill_grant(capfds[i], caps[i]);
      if (0 != err) {
        perror("shill_grant");
	exit(err);
      }
    }

    for (int j=0; j < netcapcount; j=j+3) {
      uint64_t netcap[3] = { netcaps[j],
                             netcaps[j+1],
                             netcaps[j+2] };
      err = mac_syscall("shill", 1, netcap);
      if (0 != err) {
        printf("couldn't add netcap { address_family := %lu, socket_type := %lu, "
               "permissions := %lx }, cannot continue with test\n",
               netcaps[j],
               netcaps[j+1],
               netcaps[j+2]);
        printf("  mac_syscall returned %d\n", err);
        printf("  errno %d\n", errno);
        perror("  mac_syscall:");
        exit(err);
      }
    }

    if (pipefactory != NULL) {
      err = shill_grant_pipefactory(pipefactory);
      if (0 != err) {
        perror("shill_grant_pipefactory");
        exit(err);
      }
    }

    err = debug ? shill_debug() : shill_enter();
    if (0 != err) {
      perror("shill_enter");
      exit(err);
    }

    // Set up stdin/out/err
    if (-1 != stdin) {
      if (-1 == dup2(stdin,0)) {
        perror("dup2(stdin,0)");
        exit(-1);
      }
    } else {
      close(0);
    }

    if (-1 != stdout) {
      if (-1 == dup2(stdout,1)) {
        perror("dup2(stdout,1)");
        exit(-1);
      }
    } else {
      close(1);
    }

    if (-1 != stderr) {
      if (-1 == dup2(stderr,2)) {
        perror("dup2(stderr,2)");
        exit(-1);
      }
    } else {
      close(2);
    }

    // Need the executable's file descriptor for exec...
    if (-1 == dup2(execfd,3)) {
      perror("dup2(execfd,3)");
      exit(-1);
    }

    // No other file descriptors remain open after we call
    // fexecve, but close them just to be sure
    closefrom(4);

    // Set the timeout if it was requested
    if (timeout != -1) {
      struct rlimit rlim = {
        .rlim_cur = timeout,
        .rlim_max = timeout,
      };
      setrlimit(RLIMIT_CPU, &rlim);
    }

    // returns only on failure
    err = fexecve(3, argv, environ);
    perror("fexecve");
    exit(err);
  } else {
    // parent
    int status;
    pid_t ret;

    do {
      ret = waitpid(cpid, &status, WEXITED);
    } while (ret != cpid && errno == EINTR);

    if (ret == cpid) {
      return WEXITSTATUS(status);
    } else {
      return -1;
    }
  }
}

#include <sys/types.h>
#include <sys/module.h>
#include <sys/systm.h>  /* uprintf */
#include <sys/param.h>  /* defines used in kernel.h */
#include <sys/kernel.h> /* types used in module initialization */
#include <sys/conf.h>   /* cdevsw struct */
#include <sys/lock.h>
#include <sys/mutex.h>
#include <sys/uio.h>    /* uio struct */
#include <sys/queue.h>
#include <sys/sysctl.h>
#include <vm/uma.h>

#include <machine/stdarg.h>

#include "shill.h"
#include "message.h"
#include "shilldev.h"

/* Function prototypes */
static d_open_t      shill_open;
static d_close_t     shill_close;
static d_read_t      shill_read;
static d_write_t     shill_write;

/* Character device entry points */
static struct cdevsw shill_cdevsw = {
  .d_version = D_VERSION,
  .d_open = shill_open,
  .d_close = shill_close,
  .d_read = shill_read,
  .d_write = shill_write,
  .d_name = "shilld",
};

static struct cdev *shill_dev;

static struct shill_message queue[1024];
static int start, end, count;

static struct mtx mtx;

MALLOC_DECLARE(M_SHILLBUF);
MALLOC_DEFINE(M_SHILLBUF, "shillbuffer", "buffer for shill module");

extern int shill_debug;

/*
 * This function is called by the kld[un]load(2) system calls to
 * determine what actions to take when a module is loaded or unloaded.
 */
static int
shill_loader(struct module *m __unused, int what, void *arg __unused)
{
  int error = 0;

  switch (what) {
  case MOD_LOAD:                /* kldload */
    error = make_dev_p(MAKEDEV_CHECKNAME | MAKEDEV_WAITOK,
		       &shill_dev,
		       &shill_cdevsw,
		       0,
		       UID_ROOT,
		       GID_WHEEL,
		       0600,
		       "shilld");
    if (error != 0)
      break;

    mtx_init(&mtx, "shilldev_mtx", NULL, MTX_SPIN);
    start = 0;
    end = 0;
    count = 0;
    break;
  case MOD_UNLOAD:
    destroy_dev(shill_dev);
    mtx_destroy(&mtx);
    break;
  default:
    error = EOPNOTSUPP;
    break;
  }
  return (error);
}

static int
shill_open(struct cdev *dev __unused, int oflags __unused, int devtype __unused,
	  struct thread *td __unused)
{
  int error = 0;
  return (error);
}

static int
shill_close(struct cdev *dev __unused, int fflag __unused, int devtype __unused,
	   struct thread *td __unused)
{
  return (0);
}

static int
shill_read(struct cdev *dev __unused, struct uio *uio, int ioflag __unused)
{
  mtx_lock_spin(&mtx);
  struct shill_message *msg = (count != 0) ? &queue[start] : NULL;
  mtx_unlock_spin(&mtx);

  if (msg == NULL)
    return 0;

  int amt = MIN(uio->uio_resid, sizeof(struct shill_message));
  int error = uiomove(msg, amt, uio);
  if (error == 0) {
    mtx_lock_spin(&mtx);
    start = (start + 1) % 1024;
    count--;
    mtx_unlock_spin(&mtx);
  }

  return (error);
}

/*
 * shill_write returns an error because writes are not supported
 */
static int
shill_write(struct cdev *dev __unused, struct uio *uio, int ioflag __unused)
{
  return (ENODEV);
}

void
shilld_logcap(uintptr_t session, const struct shill_cap *cap) {
  if (!shill_debug)
    return;

  mtx_lock_spin(&mtx);
  if (start == end && count != 0) {
    mtx_unlock_spin(&mtx);
    return;
  }

  struct shill_message *msg = &queue[end];

  msg->m_session = session;
  msg->m_type = SHILLMSG_CAP;
  msg->m_data.d_cap.mc_id = (uintptr_t)cap;
  msg->m_data.d_cap.mc_flags = cap->sc_flags;
  msg->m_data.d_cap.mc_lookup = (uintptr_t)cap->sc_lookup;
  msg->m_data.d_cap.mc_createfile = (uintptr_t)cap->sc_createfile;
  msg->m_data.d_cap.mc_createdir = (uintptr_t)cap->sc_createdir;

  end = (end+1) % 1024;
  count++;
  mtx_unlock_spin(&mtx);
}

void
shilld_log(uintptr_t session, const char *fmt, ...) {
  if (!shill_debug)
    return;

  mtx_lock_spin(&mtx);
  if (start == end && count != 0) {
    mtx_unlock_spin(&mtx);
    return;
  }

  struct shill_message *msg = &queue[end];

  msg->m_session = session;
  msg->m_type = SHILLMSG_STR;

  va_list ap;
  va_start(ap, fmt);
  vsnprintf(msg->m_data.d_str, SHILLMSG_LEN, fmt, ap);
  va_end(ap);

  end = (end+1) % 1024;
  count++;
  mtx_unlock_spin(&mtx);
}

void
shilld_logall(const char *fmt, ...) {
  if (!shill_debug)
    return;

  mtx_lock_spin(&mtx);
  if (start == end && count != 0) {
    mtx_unlock_spin(&mtx);
    return;
  }

  struct shill_message *msg = &queue[end];

  msg->m_session = 0;
  msg->m_type = SHILLMSG_STR;

  va_list ap;
  va_start(ap, fmt);
  vsnprintf(msg->m_data.d_str, SHILLMSG_LEN, fmt, ap);
  va_end(ap);

  end = (end+1) % 1024;
  count++;
  mtx_unlock_spin(&mtx);
}

DEV_MODULE(shilld, shill_loader, NULL);

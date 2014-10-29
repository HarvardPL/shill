#include <sys/param.h>
#include <sys/systm.h>
#include <sys/proc.h>
#include <sys/module.h>
#include <sys/sysproto.h>
#include <sys/sysent.h>
#include <sys/kernel.h>
#include <sys/systm.h>
#include <sys/uio.h>
#include <sys/namei.h>
#include <sys/param.h>
#include <sys/buf.h>
#include <sys/fcntl.h>
#include <sys/filedesc.h>
#include <sys/file.h>
#include <sys/mount.h>
#include <sys/proc.h>
#include <sys/vnode.h>
#include <sys/priv.h>

struct pathargs {
  int fd;
  char *buf;
  int len;
};

static int
shill_path(struct thread *td, void *args) {
  struct pathargs *pargs;
  struct filedesc *fdp;
  struct file *fp;
  char *rpath, *fbuf;
  int error;

  pargs = (struct pathargs *)args;

  fdp = td->td_proc->p_fd;
  FILEDESC_SLOCK(fdp);
  if ((unsigned)pargs->fd >= fdp->fd_nfiles) {
    FILEDESC_SUNLOCK(fdp);
    return EBADF;
  }
  if (((fp = fdp->fd_ofiles[pargs->fd].fde_file) != NULL)
      && (fp->f_type == DTYPE_VNODE)) {
    struct vnode *vp = fp->f_vnode;
    vref(vp);

    error = vn_fullpath_global(td, vp, &rpath, &fbuf);
    if (error != 0) {
      vrele(vp);
      FILEDESC_SUNLOCK(fdp);
      return (error);
    }

    int len = strlen(rpath);
    if (len >= pargs->len) {
      vrele(vp);
      free(fbuf, M_TEMP);
      FILEDESC_SUNLOCK(fdp);
      return (ENAMETOOLONG);
    }

    error = copyout(rpath,pargs->buf,len);
    vrele(vp);
    free(fbuf, M_TEMP);
  } else {
    error = (EINVAL);
  }
  FILEDESC_SUNLOCK(fdp);

  return error;
}

static struct sysent path_sysent = {
  .sy_narg = 3,
  .sy_call = shill_path,
};

static int offset = NO_SYSCALL;

static int
load(struct module *module, int cmd, void *arg) {
  int error = 0;

  switch (cmd) {
  case MOD_LOAD:
        printf("path syscall loaded at %d\n", offset);
        break;
  case MOD_UNLOAD:
        printf("path syscall unloaded from %d\n", offset);
        break;
  default:
        error = EOPNOTSUPP;
        break;
  }
  return (error);
}

SYSCALL_MODULE(shill_path, &offset, &path_sysent, load, NULL);

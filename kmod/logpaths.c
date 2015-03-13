#include <sys/param.h>
#include <sys/proc.h>
#include <sys/module.h>
#include <sys/sysproto.h>
#include <sys/sysent.h>
#include <sys/kernel.h>
#include <sys/systm.h>
#include <sys/uio.h>
#include <sys/namei.h>
#include <sys/buf.h>
#include <sys/filedesc.h>
#include <sys/file.h>
#include <sys/mount.h>
#include <sys/proc.h>
#include <sys/vnode.h>
#include <sys/priv.h>

#include "shilldev.h"

static void
logspecial(struct thread *td, struct vnode *vp, char *msg) {
  char *rbuf, *fbuf;
  int error;

  if (vp == NULL)
	return;
  
  error = vn_fullpath_global(td, vp, &rbuf, &fbuf);
  vrele(vp);
  
  if (error) {
	shilld_logall("vnode %lx (%s) could not be resolved to a path", (uintptr_t)vp, msg);
  } else {
	shilld_logall("vnode %lx (%s) resolves to %s", (uintptr_t)vp, msg, rbuf);
	free(fbuf, M_TEMP);
  }
}

static void
logfd(struct thread *td, struct vnode *vp, int fd) {
  char *rbuf, *fbuf;
  int error;

  if (vp == NULL)
	return;
  
  error = vn_fullpath_global(td, vp, &rbuf, &fbuf);
  vrele(vp);
  
  if (error) {
	shilld_logall("vnode %lx (fd %d) could not be resolved to a path", (uintptr_t)vp, fd);
  } else {
	shilld_logall("vnode %lx (fd %d) resolves to %s", (uintptr_t)vp, fd, rbuf);
	free(fbuf, M_TEMP);
  }
}

// See kern_proc_filedesc_out from kern_descrip.c
// XXX lock order reversal issues with the FILEDESC and devfs locks?
static int
logpaths(struct thread *td)
{
  struct proc *p;
  struct filedesc *fdp;
  struct file *fp;
  struct vnode *cttyvp, *textvp, *tracevp;

  p = td->td_proc;
  fdp = p->p_fd;

  PROC_LOCK(p);
  tracevp = p->p_tracevp;
  if (tracevp != NULL)
	vref(tracevp);

  textvp = p->p_textvp;
  if (textvp != NULL)
	vref(textvp);

  cttyvp = NULL;
  if (p->p_pgrp != NULL && p->p_pgrp->pg_session != NULL) {
	cttyvp = p->p_pgrp->pg_session->s_ttyvp;
	if (cttyvp != NULL)
	  vref(cttyvp);
  }
  PROC_UNLOCK(p);

  logspecial(td,tracevp,"ktrace");
  logspecial(td,textvp,"text");
  logspecial(td,cttyvp,"ctty");

  FILEDESC_SLOCK(fdp);
  if (fdp->fd_cdir != NULL) {
	vref(fdp->fd_cdir);
	logspecial(td,fdp->fd_cdir,"cwd");
  }
  if (fdp->fd_rdir != NULL) {
	vref(fdp->fd_rdir);
	logspecial(td,fdp->fd_rdir,"root");
  }
  if (fdp->fd_jdir != NULL) {
	vref(fdp->fd_jdir);
	logspecial(td,fdp->fd_jdir,"jail");
  }
  for (int i = 0; i < fdp->fd_nfiles; i++) {
	if ((fp = fdp->fd_ofiles[i]) == NULL)
	  continue;
	if (fp->f_type != DTYPE_VNODE)
	  continue;

	vref(fp->f_vnode);
	logfd(td,fp->f_vnode,i);
  }
  FILEDESC_SUNLOCK(fdp);

  return (0);
}

static int
shill_logpaths(struct thread *td, void *arg) {
  return logpaths(td);
}

static struct sysent logpaths_sysent = {
  .sy_narg = 0,
  .sy_call = shill_logpaths
};

static int offset = NO_SYSCALL;

static int
load(struct module *module, int cmd, void *arg) {
  int error = 0;

  switch (cmd) {
  case MOD_LOAD:
	printf("syscall loaded at %d\n", offset);
	break;
  case MOD_UNLOAD:
	printf("syscall unloaded from %d\n", offset);
	break;
  default:
	error = EOPNOTSUPP;
	break;
  }
  return (error);
}

SYSCALL_MODULE(shill_logpaths, &offset, &logpaths_sysent, load, NULL);

/*-
 * Copyright (c) 1989, 1993
 *      The Regents of the University of California.  All rights reserved.
 * (c) UNIX System Laboratories, Inc.
 * All or some portions of this file are derived from material licensed
 * to the University of California by American Telephone and Telegraph
 * Co. or Unix System Laboratories, Inc. and are reproduced herein with
 * the permission of UNIX System Laboratories, Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
*/

#include <sys/capability.h>
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
#include <sys/fcntl.h>
#include <sys/filedesc.h>
#include <sys/file.h>
#include <sys/mount.h>
#include <sys/vnode.h>
#include <sys/priv.h>
#include <sys/stat.h>

static int
kern_frmdirat(struct thread *td, int fd, char *path, int target, enum uio_seg pathseg)
{
	struct filedesc *fdp;
        struct file *fp;
	struct mount *mp;
	struct vnode *vp, *tvp;
	int error;
	struct nameidata nd;
	int vfslocked;

	fdp = td->td_proc->p_fd;
        FILEDESC_SLOCK(fdp);
        if (((unsigned)target >= fdp->fd_nfiles)
            || ((fp = fdp->fd_ofiles[target]) == NULL)) {
          FILEDESC_SUNLOCK(fdp);
          return EBADF;
        } else {
          tvp = fp->f_vnode;
          vref(tvp);
        }
	FILEDESC_SUNLOCK(fdp);

restart:
	bwillwrite();
	NDINIT_ATRIGHTS(&nd, DELETE, LOCKPARENT | LOCKLEAF | MPSAFE |
	    AUDITVNODE1, pathseg, path, fd, CAP_RMDIR, td);
	if ((error = namei(&nd)) != 0) {
		vrele(tvp);
		return (error);
        }
	vfslocked = NDHASGIANT(&nd);
	vp = nd.ni_vp;
	if (vp->v_type != VDIR) {
		error = ENOTDIR;
		goto out;
	}

        /*
         * Check that the target matches
         */
        if (tvp != vp) {
		error = EINVAL;
		goto out;
        }

	/*
	 * No rmdir "." please.
	 */
	if (nd.ni_dvp == vp) {
		error = EINVAL;
		goto out;
	}
	/*
	 * The root of a mounted filesystem cannot be deleted.
	 */
	if (vp->v_vflag & VV_ROOT) {
		error = EBUSY;
		goto out;
	}
#ifdef MAC
	error = mac_vnode_check_unlink(td->td_ucred, nd.ni_dvp, vp,
	    &nd.ni_cnd);
	if (error)
		goto out;
#endif
	if (vn_start_write(nd.ni_dvp, &mp, V_NOWAIT) != 0) {
		NDFREE(&nd, NDF_ONLY_PNBUF);
		vput(vp);
		if (nd.ni_dvp == vp)
			vrele(nd.ni_dvp);
		else
			vput(nd.ni_dvp);
		VFS_UNLOCK_GIANT(vfslocked);
		if ((error = vn_start_write(NULL, &mp, V_XSLEEP | PCATCH)) != 0) {
			vrele(tvp);
			return (error);
                }
		goto restart;
	}
	vfs_notify_upper(vp, VFS_NOTIFY_UPPER_UNLINK);
	error = VOP_RMDIR(nd.ni_dvp, nd.ni_vp, &nd.ni_cnd);
	vn_finished_write(mp);
out:
	NDFREE(&nd, NDF_ONLY_PNBUF);
	vput(vp);
	if (nd.ni_dvp == vp)
		vrele(nd.ni_dvp);
	else
		vput(nd.ni_dvp);
	VFS_UNLOCK_GIANT(vfslocked);
        vrele(tvp);
	return (error);
}

static int
kern_funlinkat(struct thread *td, int fd, char *path, int target, enum uio_seg pathseg,
    ino_t oldinum)
{
	struct filedesc *fdp;
        struct file *fp;
	struct mount *mp;
	struct vnode *vp, *tvp;
	int error;
	struct nameidata nd;
	struct stat sb;
	int vfslocked;

        fdp = td->td_proc->p_fd;
        FILEDESC_SLOCK(fdp);
        if (((unsigned)target >= fdp->fd_nfiles)
            || ((fp = fdp->fd_ofiles[target]) == NULL)) {
          FILEDESC_SUNLOCK(fdp);
          return EBADF;
        } else {
          tvp = fp->f_vnode;
          vref(tvp);
        }
	FILEDESC_SUNLOCK(fdp);


restart:
	bwillwrite();
	NDINIT_AT(&nd, DELETE, LOCKPARENT | LOCKLEAF | MPSAFE | AUDITVNODE1,
	    pathseg, path, fd, td);
	if ((error = namei(&nd)) != 0) {
		vrele(tvp);
		return (error == EINVAL ? EPERM : error);
        }
	vfslocked = NDHASGIANT(&nd);
	vp = nd.ni_vp;
        if (tvp != vp) {
		error = EINVAL;	/* File to unlink does not match name */
        } else if (vp->v_type == VDIR && oldinum == 0) {
		error = EPERM;		/* POSIX */
	} else if (oldinum != 0 &&
		  ((error = vn_stat(vp, &sb, td->td_ucred, NOCRED, td)) == 0) &&
		  sb.st_ino != oldinum) {
			error = EIDRM;	/* Identifier removed */
	} else {
		/*
		 * The root of a mounted filesystem cannot be deleted.
		 *
		 * XXX: can this only be a VDIR case?
		 */
		if (vp->v_vflag & VV_ROOT)
			error = EBUSY;
	}
	if (error == 0) {
		if (vn_start_write(nd.ni_dvp, &mp, V_NOWAIT) != 0) {
			NDFREE(&nd, NDF_ONLY_PNBUF);
			vput(nd.ni_dvp);
			if (vp == nd.ni_dvp)
				vrele(vp);
			else
				vput(vp);
			VFS_UNLOCK_GIANT(vfslocked);
			if ((error = vn_start_write(NULL, &mp,
			    V_XSLEEP | PCATCH)) != 0) {
				vrele(tvp);
				return (error);                          
                        }
			goto restart;
		}
#ifdef MAC
		error = mac_vnode_check_unlink(td->td_ucred, nd.ni_dvp, vp,
		    &nd.ni_cnd);
		if (error)
			goto out;
#endif
		vfs_notify_upper(vp, VFS_NOTIFY_UPPER_UNLINK);
		error = VOP_REMOVE(nd.ni_dvp, vp, &nd.ni_cnd);
#ifdef MAC
out:
#endif
		vn_finished_write(mp);
	}
	NDFREE(&nd, NDF_ONLY_PNBUF);
	vput(nd.ni_dvp);
	if (vp == nd.ni_dvp)
		vrele(vp);
	else
		vput(vp);
	VFS_UNLOCK_GIANT(vfslocked);
        vrele(tvp);
	return (error);
}

#define	PAD_(t)	(sizeof(register_t) <= sizeof(t) ? \
		0 : sizeof(register_t) - sizeof(t))

#if BYTE_ORDER == LITTLE_ENDIAN
#define	PADL_(t)	0
#define	PADR_(t)	PAD_(t)
#else
#define	PADL_(t)	PAD_(t)
#define	PADR_(t)	0
#endif

struct funlinkat_args {
  char dfd_l_[PADL_(int)];     int dfd;    char dfd_r_[PADR_(int)];
  char path_l_[PADL_(char *)]; char *path; char path_r_[PADR_(char *)];
  char flag_l_[PADL_(int)];    int flag;   char flag_r_[PADR_(int)];
  char fd_l_[PADL_(int)];      int fd;     char fd_r_[PADR_(int)];
};

#undef PAD_
#undef PADL_
#undef PADR_

static int
sys_funlinkat(struct thread *td, void *arg) {
  struct funlinkat_args *uap = arg;

  if (uap->flag & ~AT_REMOVEDIR)
    return EINVAL;

  if (uap->flag & AT_REMOVEDIR)
    return kern_frmdirat(td, uap->dfd, uap->path, uap->fd, UIO_USERSPACE);
  else
    return kern_funlinkat(td, uap->dfd, uap->path, uap->fd, UIO_USERSPACE, 0);
}

static struct sysent funlinkat_sysent = {
  .sy_narg = 4,
  .sy_call = sys_funlinkat
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

SYSCALL_MODULE(funlinkat, &offset, &funlinkat_sysent, load, NULL);

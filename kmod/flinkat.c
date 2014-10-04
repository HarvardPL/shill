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
#include <sys/vnode.h>
#include <sys/priv.h>

/*
 * This function is duplicated from sys/kern/vfs_syscalls.c.
 * Should alter kernel to expose that functionality, or move
 * this system call into the kernel. In the kernel, the security
 * checks in this function are controlled by two sysctl's,
 * hardlink_check_uid and hardlink_check_gid. Since these can't
 * be accessed without a kernel modification, assume they are
 * set to the most permissive setting...
 */
static int
can_hardlink(struct vnode *vp, struct ucred *cred)
{
	struct vattr va;
	int error;

	error = VOP_GETATTR(vp, &va, cred);
	if (error != 0)
		return (error);

	if (cred->cr_uid != va.va_uid) {
		error = priv_check_cred(cred, PRIV_VFS_LINK, 0);
		if (error)
			return (error);
	}

	if (!groupmember(va.va_gid, cred)) {
		error = priv_check_cred(cred, PRIV_VFS_LINK, 0);
		if (error)
			return (error);
	}

	return (0);
}

static int
kern_flinkat(struct thread *td, int fd, int dfd, char *path,
	     enum uio_seg segflg)
{
	struct filedesc *fdp;
	struct vnode *vp;
	struct mount *mp;
	struct nameidata nd;
	int vfslocked;
	int lvfslocked;
	int error;

	bwillwrite();

	fdp = td->td_proc->p_fd;
	FILEDESC_SLOCK(fdp);
	error = fgetvp(td, fd, 0, &vp);
	FILEDESC_SUNLOCK(fdp);
	if (error != 0)
	  return (error);

	vfslocked = VFS_LOCK_GIANT(vp->v_mount);
	if (vp->v_type == VDIR) {
	  vrele(vp);
	  VFS_UNLOCK_GIANT(vfslocked);
	  return (EPERM); /* POSIX */
	}

	// Prepare to start writing (the write is to add the link)
	if ((error = vn_start_write(vp, &mp, V_WAIT | PCATCH)) != 0) {
		vrele(vp);
		VFS_UNLOCK_GIANT(vfslocked);
		return (error);
	}
	// Attempt to create the link
	NDINIT_AT(&nd, CREATE, LOCKPARENT | SAVENAME | MPSAFE | AUDITVNODE2,
	    segflg, path, dfd, td);
	if ((error = namei(&nd)) == 0) {
		lvfslocked = NDHASGIANT(&nd);
		// SM: Check if a file already exists at the link site
		if (nd.ni_vp != NULL) {
			if (nd.ni_dvp == nd.ni_vp)
				vrele(nd.ni_dvp);
			else
				vput(nd.ni_dvp);
			vrele(nd.ni_vp);
			error = EEXIST;
		} else if ((error = vn_lock(vp, LK_EXCLUSIVE | LK_RETRY))
		    == 0) {
		  // ^ SM: Lock the vnode we are linking to
		  error = can_hardlink(vp, td->td_ucred);
		  if (error == 0)
#ifdef MAC
				error = mac_vnode_check_link(td->td_ucred,
				    nd.ni_dvp, vp, &nd.ni_cnd);
			if (error == 0)
#endif
				error = VOP_LINK(nd.ni_dvp, vp, &nd.ni_cnd);
			// ^ SM: Create the link in nd.ni_dvp at nd.ni_cnd to vp
			VOP_UNLOCK(vp, 0);
			vput(nd.ni_dvp);
		}
		NDFREE(&nd, NDF_ONLY_PNBUF);
		VFS_UNLOCK_GIANT(lvfslocked);
	}
	// SM: cleanup
	vrele(vp);
	vn_finished_write(mp);
	VFS_UNLOCK_GIANT(vfslocked);
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

struct flinkat_args {
  char fd_l_[PADL_(int)];      int fd;     char fd_r_[PADR_(int)];
  char dfd_l_[PADL_(int)];     int dfd;    char dfd_r_[PADR_(int)];
  char path_l_[PADL_(char *)]; char *path; char path_r_[PADR_(char *)];
};

#undef PAD_
#undef PADL_
#undef PADR_

static int
foo_flinkat(struct thread *td, void *arg) {
  struct flinkat_args *uap = arg;
  return kern_flinkat(td, uap->fd, uap->dfd, uap->path, UIO_USERSPACE);
}

static struct sysent flinkat_sysent = {
  .sy_narg = 3,
  .sy_call = foo_flinkat
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

SYSCALL_MODULE(flinkat, &offset, &flinkat_sysent, load, NULL);

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
#include <sys/systm.h>
#include <sys/buf.h>
#include <sys/capsicum.h>
#include <sys/mount.h>
#include <sys/mutex.h>
#include <sys/namei.h>
#include <sys/filedesc.h>
#include <sys/fcntl.h>
#include <sys/stat.h>
#include <sys/vnode.h>
#include <sys/proc.h>

#include <security/audit/audit.h>

#include "syscalls.h"

static
int kern_mkopendirat(struct thread *td, int fd,
		     const char *path, enum uio_seg segflg,
		     int mode, int flags) {
	struct mount *mp;
	struct vnode *vp;
	struct vattr vattr;
	int error;
	struct nameidata nd;
	struct file *fp, *nfp;
	int indx = -1;
	cap_rights_t rights;

	AUDIT_ARG_FFLAGS(flags);
	AUDIT_ARG_MODE(mode);
	if (flags & ~O_CLOEXEC) {
	  return (EINVAL);
	} else {
	  flags = flags | O_RDONLY;
	  flags = flags | O_DIRECTORY;
	  flags = FFLAGS(flags);
	}

	/* allocate the file descriptor, but don't install yet */
	error = falloc_noinstall(td, &nfp);
	if (error)
	  return (error);

	/* An extra reference on `nfp' has been held for us by falloc_noinstall(). */
	fp = nfp;
	/* Set the flags early so the finit in devfs can pick them up. */
	fp->f_flag = flags & FMASK;
	
restart:
	bwillwrite();
	NDINIT_ATRIGHTS(&nd, CREATE, LOCKPARENT | SAVENAME | AUDITVNODE1,
		segflg, path, fd, cap_rights_init(&rights, CAP_MKDIRAT), td);
	nd.ni_cnd.cn_flags |= WILLBEDIR;
	if ((error = namei(&nd)) != 0) {
	  if (indx != -1)
		fdclose(td, fp, indx);
	  fdrop(fp, td);
	  return (error);
	}
	vp = nd.ni_vp;
	if (vp != NULL) {
		NDFREE(&nd, NDF_ONLY_PNBUF);

		/*
		 * XXX namei called with LOCKPARENT but not LOCKLEAF has
		 * the strange behaviour of leaving the vnode unlocked
		 * if the target is the same vnode as the parent.
		 */
		if (vp == nd.ni_dvp)
			vrele(nd.ni_dvp);
		else
			vput(nd.ni_dvp);
		vrele(vp);
		if (indx != -1)
		  fdclose(td, fp, indx);
		fdrop(fp, td);
		return (EEXIST);
	}
	if (vn_start_write(nd.ni_dvp, &mp, V_NOWAIT) != 0) {
		NDFREE(&nd, NDF_ONLY_PNBUF);
		vput(nd.ni_dvp);
		if ((error = vn_start_write(NULL, &mp, V_XSLEEP | PCATCH)) != 0) {
		  if (indx != -1)
			fdclose(td, fp, indx);
		  fdrop(fp, td);
		  return (error);
		}
		goto restart;
	}
	VATTR_NULL(&vattr);
	vattr.va_type = VDIR;
	vattr.va_mode = (mode & ACCESSPERMS) &~ td->td_proc->p_fd->fd_cmask;
#ifdef MAC
	error = mac_vnode_check_create(td->td_ucred, nd.ni_dvp, &nd.ni_cnd,
	    &vattr);
	if (error)
		goto out;
#endif
	error = VOP_MKDIR(nd.ni_dvp, &nd.ni_vp, &nd.ni_cnd, &vattr);
	if (error)
	  goto out;
#ifdef MAC
	mac_vnode_post_create(td->td_ucred,
						  nd.ni_dvp, nd.ni_vp,
						  &nd.ni_cnd, &vattr);
#endif
	error = VOP_OPEN(nd.ni_vp, flags, td->td_ucred, td, fp);
	if (error)
	  goto out;
	fp->f_vnode = nd.ni_vp;
	fp->f_seqcount = 1;
	finit(fp, flags & FMASK, DTYPE_VNODE, nd.ni_vp, &vnops);
#ifdef CAPABILITIES
	if (nd.ni_strictrelative == 1) {
		/*
		 * We are doing a strict relative lookup; wrap the
		 * result in a capability.
		 */
		if ((error = kern_capwrap(td, fp, nd.ni_baserights, &indx)) != 0)
		  goto out;
	} else
#endif
	  error = finstall(td, fp, &indx, flags, NULL);
out:
	NDFREE(&nd, NDF_ONLY_PNBUF);
	vput(nd.ni_dvp);
	if (!error) {
	  VOP_UNLOCK(nd.ni_vp, 0);
	  td->td_retval[0] = indx;
	} else {
	  vput(nd.ni_vp);
	  if (indx != -1)
		fdclose(td, fp, indx);
	  td->td_retval[0] = -1;
	}
	fdrop(fp, td);
	vn_finished_write(mp);
	return (error);
}

int mkopendirat(struct thread *td, void *usr_arg) {
  struct mkopendirat_args args;
  
  if (0 != copyin(usr_arg, &args, sizeof(struct mkopendirat_args)))
	return EFAULT;

  return (kern_mkopendirat(td, args.fd, args.path, UIO_USERSPACE,
			   args.mode, args.flags));
}

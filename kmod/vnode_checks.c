#include <sys/param.h>
#include <sys/kernel.h>
#include <sys/mac.h>
#include <sys/mman.h>
#include <sys/namei.h>
#include <sys/queue.h>
#include <sys/systm.h>
#include <sys/ucred.h>
#include <sys/vnode.h>

#include "cflags.h"
#include "shill.h"
#include "shilldev.h"
#include "vnode_checks.h"

void
shill_vnode_init_label(struct label *label) {
  shill_cap_init_label(label);
}

void
shill_vnode_copy_label(struct label *src, struct label *dst) {
  shill_cap_copy_label(src,dst);
}

int
shill_vnode_internalize_label(struct label *label,
                              char *element_name, char *element_data,
                              int *claimed) {
  return shill_cap_internalize_label(label,element_name,element_data,claimed);
}

int
shill_vnode_externalize_label(struct label *label, char *element_name,
                              struct sbuf *sb, int *claimed) {
  // XXX implement
  return 0;
}


int
shill_vnode_check_relabel(struct ucred *cred, struct vnode *vp,
			  struct label *vplabel, struct label *newlabel) {
  return shill_cap_check_relabel(cred, vp, vplabel, newlabel);
}

void
shill_vnode_relabel(struct ucred *cred, struct vnode *vp,
		    struct label *vplabel, struct label *newlabel) {
  shill_cap_relabel(cred, vp, vplabel, newlabel);
}

void
shill_vnode_destroy_label(struct label *label) {
  shill_cap_destroy_label(label);
}		    

int
shill_vnode_check_open(struct ucred *cred, struct vnode *vp,
		       struct label *vplabel, accmode_t accmode) {
  return shill_cap_check_any("open", cred, vp, vplabel);
}

int
shill_vnode_check_lookup(struct ucred *cred,
			 struct vnode *dvp, struct label *dvplabel,
			 struct componentname *cnp) {
  return shill_cap_check_rights("lookup", cred, dvp, dvplabel, C_LOOKUP);
}

void
shill_vnode_post_lookup(struct ucred *cred,
			struct vnode *dvp, struct label *dvplabel,
			struct componentname *cnp,
			struct vnode *vp, struct label *vplabel) {
  struct shill_session *session = shill_get_session(cred);
  if (session == NULL)
    return;

  char name[NAME_MAX+1];
  strncpy(name, cnp->cn_nameptr, cnp->cn_namelen);
  name[cnp->cn_namelen] = 0;
  shilld_log((uintptr_t)session, "%lx/%s -> %lx", (uintptr_t)dvp,
	     name, (uintptr_t)vp);

  struct shill_cap *dcap = shill_get_cap(dvplabel, session);
  if (dcap == NULL || (dcap->sc_flags & C_LOOKUP) == 0)
    return;

  KASSERT(dcap->sc_lookup != NULL, ("shill_vnode_post_lookup: NULL lookup capability"));
  
  if (cnp->cn_namelen == 1 && cnp->cn_nameptr[0] == '.')
    return;
  if (cnp->cn_namelen == 2 &&
      cnp->cn_nameptr[0] == '.' && cnp->cn_nameptr[1] == '.')
    return;

  shilld_log((uintptr_t)session,
	     "About to transfer capability to %lx after lookup from %lx",
	     (uintptr_t)vp, (uintptr_t)dvp);

  struct shill_cap *cur = shill_get_cap(vplabel, session);
  struct shill_cap *merged = shill_merge_cap(session, cur, dcap->sc_lookup);

  if (cur != merged) {
    shilld_log((uintptr_t)session,
	       "Increased capabilities to %lx after lookup from %lx",
	       (uintptr_t)vp, (uintptr_t)dvp);
    shilld_log((uintptr_t)session,
	       "Previous: %lx Lookup: %lx Merged: %lx",
	       (uintptr_t)cur,
	       (uintptr_t)dcap->sc_lookup,
	       (uintptr_t)merged);
  }
  shill_set_cap(vplabel, session, merged);

  return;
}

int
shill_vnode_check_access(struct ucred *cred, struct vnode *vp,
			 struct label *vplabel, accmode_t accmode) {  
  // XXX Allow with any caps for now...
  // return shill_deny_all("access", cred, vp);
  return shill_cap_check_any("access", cred, vp, vplabel);
}

int
shill_vnode_check_chdir(struct ucred *cred, struct vnode *dvp,
			struct label *dvplabel) {
  return shill_cap_check_rights("chdir",cred,dvp,dvplabel,C_CHDIR);
}

int
shill_vnode_check_chroot(struct ucred *cred, struct vnode *dvp,
			 struct label *dvplabel) {
  return shill_cap_check_rights("chroot",cred,dvp,dvplabel,C_CHROOT);
}

int
shill_vnode_check_create(struct ucred *cred, struct vnode *dvp,
			 struct label *dvplabel, struct componentname *cnp,
			 struct vattr *vap) {
  struct shill_session *session = shill_get_session(cred);
  if (session == NULL)
    return 0;

  switch (vap->va_type) {
  case VLNK:
    return shill_cap_check_rights("create symlink",cred,dvp,dvplabel,C_ADDSYMLNK);
  case VDIR:
    return shill_cap_check_rights("create directory",cred,dvp,dvplabel,C_CREATEDIR);
  case VREG:
    return shill_cap_check_rights("create file",cred,dvp,dvplabel,C_CREATEFILE);
  default:
    return shill_deny_all("create for unknown type",cred,dvp);
  }
}

void
shill_vnode_post_create(struct ucred *cred,
			struct vnode *dvp, struct label *dvplabel,
			struct vnode *vp, struct label *vplabel,
			struct componentname *cnp, struct vattr *vap) {
  struct shill_session *session = shill_get_session(cred);
  if (session == NULL)
    return;

  char name[NAME_MAX+1];
  strncpy(name, cnp->cn_nameptr, cnp->cn_namelen);
  name[cnp->cn_namelen] = 0;
  shilld_log((uintptr_t)session, "%lx/%s := %lx", (uintptr_t)dvp,
	     name, (uintptr_t)vp);

  struct shill_cap *dcap = shill_get_cap(dvplabel, session);
  if (dcap == NULL)
    return;

  struct shill_cap *createcap = NULL;

  switch (vap->va_type) {
  case VDIR:
    if ((dcap->sc_flags & C_CREATEDIR) == 0) {
      return;
    } else {
      createcap = dcap->sc_createdir;
      break;
    }
  case VREG:
    if ((dcap->sc_flags & C_CREATEFILE) == 0) {
      return;
    } else {
      createcap = dcap->sc_createfile;
      break;
    }
  default:
    return;
  }

  KASSERT(createcap != NULL, ("shill_vnode_post_create: NULL capability"));
  
  if (cnp->cn_namelen == 1 && cnp->cn_nameptr[0] == '.')
    return;
  if (cnp->cn_namelen == 2 &&
      cnp->cn_nameptr[0] == '.' && cnp->cn_nameptr[1] == '.')
    return;

  shill_set_cap(vplabel, session, createcap);

  return;
}

int
shill_vnode_check_deleteacl(struct ucred *cred, struct vnode *vp,
			    struct label *vplabel, acl_type_t type) {
  return shill_deny_all("deleteacl", cred, vp);
}

int
shill_vnode_check_deleteextattr(struct ucred *cred, struct vnode *vp,
				struct label *vplabel, int attrnamespace,
				const char *name) {
  return shill_cap_check_rights("deleteextattr", cred, vp, vplabel, C_WREXTATTR);
}

int
shill_vnode_check_exec(struct ucred *cred, struct vnode *vp,
		       struct label *vplabel, struct image_params *imgp,
		       struct label *execlabel) {
  return shill_cap_check_rights("exec", cred,vp, vplabel, C_EXEC);
}

int
shill_vnode_check_getacl(struct ucred *cred, struct vnode *vp,
			 struct label *vplabel, acl_type_t type) {
  return shill_deny_all("getacl", cred, vp);
}

int
shill_vnode_check_getextattr(struct ucred *cred, struct vnode *vp,
			     struct label *vplabel, int attrnamespace,
			     const char *name) {
  return shill_cap_check_rights("getextattr", cred, vp, vplabel, C_RDEXTATTR);
}

int
shill_vnode_check_link(struct ucred *cred, struct vnode *dvp,
		       struct label *dvplabel, 
		       struct vnode *vp, struct label *vplabel,
		       struct componentname *cnp) {
  int ret;
  if (0 != (ret = shill_cap_check_rights("link",cred,vp,vplabel,C_MAKELNK))) {
    return ret;
  }

  return shill_cap_check_rights("link", cred,dvp,dvplabel,C_ADDLNK);
}

int
shill_vnode_check_listextattr(struct ucred *cred, struct vnode *vp,
			      struct label *vplabel, int attrnamespace) {
  return shill_cap_check_rights("listextattr", cred, vp, vplabel, C_RDEXTATTR);
}

int
shill_vnode_check_mmap(struct ucred *cred, struct vnode *vp,
    struct label *vplabel, int prot, int flags) {
  uint32_t rights = 0;
  rights |= ((prot & PROT_READ) ? C_READ : 0);
  rights |= (((prot & PROT_WRITE) && !(flags & MAP_PRIVATE)) ? C_WRITE : 0);
  rights |= ((prot & PROT_EXEC) ? C_EXEC : 0);
  return shill_cap_check_rights("mmap", cred,vp,vplabel,rights);
}

void
shill_vnode_check_mmap_downgrade(struct ucred *cred, struct vnode *vp,
				 struct label *vplabel, int *prot) {
  // XXX double check that this is the right thing to do here
  if (0 != shill_cap_check_rights("mmap add read", cred,vp,vplabel,*prot & PROT_READ))
    *prot &= ~PROT_READ;

  if (0 != shill_cap_check_rights("mmap add write", cred,vp,vplabel,*prot & PROT_WRITE))
    *prot &= ~PROT_WRITE;

  if (0 != shill_cap_check_rights("mmap add exec", cred,vp,vplabel,*prot & PROT_EXEC))
    *prot &= ~PROT_EXEC;
}

int
shill_vnode_check_mprotect(struct ucred *cred, struct vnode *vp,
			   struct label *vplabel, int prot) {
  return shill_deny_all("mprotect", cred, vp);
}

int
shill_vnode_check_poll(struct ucred *active_cred, struct ucred *file_cred,
		       struct vnode *vp, struct label *vplabel) {
  return shill_cap_check_rights("poll", active_cred, vp, vplabel, C_READ);
}

int
shill_vnode_check_read(struct ucred *active_cred, struct ucred *file_cred,
		       struct vnode *vp, struct label *vplabel) {
  return shill_cap_check_rights("read", active_cred, vp, vplabel, C_READ);
}

int
shill_vnode_check_readdir(struct ucred *cred, struct vnode *dvp,
			  struct label *dvplabel) {
  return shill_cap_check_rights("readdir", cred, dvp, dvplabel, C_CONTENT);
}

int
shill_vnode_check_readlink(struct ucred *cred, struct vnode *vp,
			   struct label *vplabel) {
  return shill_cap_check_rights("read-symlink", cred, vp, vplabel, C_READLINK);
}

int
shill_vnode_check_rename_from(struct ucred *cred,
			      struct vnode *dvp, struct label *dvplabel,
			      struct vnode *vp, struct label *vplabel,
			      struct componentname *cnp) {
  int ret;
  switch (vp->v_type) {
  case VDIR:
    if (0 != (ret = shill_cap_check_rights("rename_from", cred, dvp, dvplabel, C_UNLINKDIR))) {
      return ret;
    }
    break;
  case VLNK:
    if (0 != (ret = shill_cap_check_rights("rename_from", cred, dvp, dvplabel, C_UNLINKSYM))) {
      return ret;
    }
    break;
  case VREG:
    if (0 != (ret = shill_cap_check_rights("rename_from", cred, dvp, dvplabel, C_UNLINKFILE))) {
      return ret;
    }
    break;
  default:
    return shill_deny_all("rename_from", cred, dvp);
  }

  return shill_cap_check_rights("rename_from", cred, vp, vplabel, C_MAKELNK);
}

int
shill_vnode_check_rename_to(struct ucred *cred,
			    struct vnode *dvp, struct label *dvplabel,
			    struct vnode *vp, struct label *vplabel,
			    int samedir, struct componentname *cnp) {
  int ret;
  if (0 != (ret = shill_cap_check_rights("rename_to", cred, dvp, dvplabel, C_ADDLNK))) {
    return ret;
  }

  if (vp != NULL) {
    // the rename would unlink the object vp, check if this is permitted
    switch (vp->v_type) {
    case VDIR:
      if (0 != (ret = shill_cap_check_rights("rename_to", cred, dvp, dvplabel, C_UNLINKDIR))) {
	return ret;
      }
      break;
    case VLNK:
      if (0 != (ret = shill_cap_check_rights("rename_to", cred, dvp, dvplabel, C_UNLINKSYM))) {
	return ret;
      }
      break;
    case VREG:
      if (0 != (ret = shill_cap_check_rights("rename_to", cred, dvp, dvplabel, C_UNLINKFILE))) {
	return ret;
      }
      break;
    default:
      return shill_deny_all("rename_to", cred, dvp);
    }
  }

  return ret;
}

int
shill_vnode_check_revoke(struct ucred *cred, struct vnode *vp,
			 struct label *vplabel) {
  return shill_deny_all("revoke", cred, vp);
}

int
shill_vnode_check_setacl(struct ucred *cred,
			 struct vnode *vp, struct label *vplabel,
			 acl_type_t type, struct acl *acl) {
  return shill_deny_all("setacl", cred, vp);
}

int
shill_vnode_check_setextattr(struct ucred *cred,
			     struct vnode *vp, struct label *vplabel,
			     int attrnamespace, const char *name) {
  return shill_cap_check_rights("setextattr", cred, vp, vplabel, C_WREXTATTR);
}

int
shill_vnode_check_setflags(struct ucred *cred,
			   struct vnode *vp, struct label *vplabel,
			   u_long flags) {
  return shill_cap_check_rights("setflags", cred, vp, vplabel, C_CHFLAGS);
}

int
shill_vnode_check_setmode(struct ucred *cred,
			  struct vnode *vp, struct label *vplabel,
			  mode_t mode) {
  return shill_cap_check_rights("setmode", cred, vp, vplabel, C_CHMODE);
}

int
shill_vnode_check_setowner(struct ucred *cred,
			   struct vnode *vp, struct label *vplabel,
			   uid_t uid, gid_t gid) {
  return shill_cap_check_rights("setowner", cred, vp, vplabel, C_CHOWN);
}

int
shill_vnode_check_setutimes(struct ucred *cred,
			    struct vnode *vp, struct label *vplabel,
			    struct timespec atime, struct timespec mtime) {
  return shill_cap_check_rights("setutimes", cred, vp, vplabel, C_CHTIMES);
}

int
shill_vnode_check_stat(struct ucred *active_cred, struct ucred *file_cred,
		       struct vnode *vp, struct label *vplabel) {
  return shill_cap_check_rights("stat", active_cred, vp, vplabel, C_STAT);
}

int
shill_vnode_check_unlink(struct ucred *cred,
			 struct vnode *dvp, struct label *dvplabel,
			 struct vnode *vp, struct label *vplabel,
			 struct componentname *cnp) {
  int err;
  switch (vp->v_type) {
  case VDIR:
    err = shill_cap_check_rights("unlink-dir", cred, dvp, dvplabel, C_UNLINKDIR);
    return (err != 0) ? err : shill_cap_check_rights("unlink", cred, vp, vplabel, C_UNLINK);
  case VLNK:
    return shill_cap_check_rights("unlink-symlink", cred, dvp, dvplabel, C_UNLINKSYM);
  case VREG:
    err = shill_cap_check_rights("unlink-file", cred, dvp, dvplabel, C_UNLINKFILE);
    return (err != 0) ? err : shill_cap_check_rights("unlink", cred, vp, vplabel, C_UNLINK);
  default:
    return shill_deny_all("unlink", cred, dvp);
  }
}

int
shill_vnode_check_write(struct ucred *active_cred, struct ucred *file_cred,
			struct vnode *vp, struct label *vplabel) {
  return shill_cap_check_rights("write", active_cred, vp, vplabel, C_WRITE | C_APPEND);
}

void
shill_vnode_execve_transition(struct ucred *old, struct ucred *new,
    struct vnode *vp, struct label *vplabel, struct label *interpvplabel,
    struct image_params *imgp, struct label *execlabel) {
  // XXX
  shilld_logall("Uh oh! in a vnode_execve_transition!");
}

int
shill_vnode_execve_will_transition(struct ucred *old,
				   struct vnode *vp, struct label *vplabel,
				   struct label *interpvplabel,
				   struct image_params *imgp, struct label *execlabel) {
  // XXX
  return 0;
}

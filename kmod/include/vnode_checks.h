#ifndef VNODE_CHECKS_H
#define VNODE_CHECKS_H

struct image_params;
struct sbuf;

int	shill_vnode_associate_extattr(struct mount *mp, struct label *mplabel,
				      struct vnode *vp, struct label *vplabel);
void	shill_vnode_associate_singlelabel(struct mount *mp,
					  struct label *mplabel,
					  struct vnode *vp,
					  struct label *vplabel);
int	shill_vnode_check_access(struct ucred *cred, struct vnode *vp,
				 struct label *vplabel, accmode_t accmode);
int	shill_vnode_check_chdir(struct ucred *cred, struct vnode *dvp,
				struct label *dvplabel);
int	shill_vnode_check_chroot(struct ucred *cred, struct vnode *dvp,
				 struct label *dvplabel);
int	shill_vnode_check_create(struct ucred *cred,
				 struct vnode *dvp, struct label *dvplabel,
				 struct componentname *cnp, struct vattr *vap);
void	 shill_vnode_post_create(struct ucred *cred, struct vnode *dvp,
				 struct label *dvplabel, struct vnode *vp,
				 struct label *vplabel,
				 struct componentname *cnp, struct vattr *vap);
int	shill_vnode_check_deleteacl(struct ucred *cred, struct vnode *vp,
				    struct label *vplabel, acl_type_t type);
int	shill_vnode_check_deleteextattr(struct ucred *cred, struct vnode *vp,
					struct label *vplabel,
					int attrnamespace, const char *name);
int	shill_vnode_check_exec(struct ucred *cred, struct vnode *vp,
			       struct label *vplabel, struct image_params *imgp,
			       struct label *execlabel);
int	shill_vnode_check_getacl(struct ucred *cred, struct vnode *vp,
				 struct label *vplabel, acl_type_t type);
int	shill_vnode_check_getextattr(struct ucred *cred, struct vnode *vp,
				     struct label *vplabel, int attrnamespace,
				     const char *name);
int	shill_vnode_check_link(struct ucred *cred, struct vnode *dvp,
			       struct label *dvplabel, struct vnode *vp,
			       struct label *vplabel, struct componentname *cnp);
int	shill_vnode_check_listextattr(struct ucred *cred, struct vnode *vp,
				      struct label *vplabel, int attrnamespace);
int	shill_vnode_check_lookup(struct ucred *cred, struct vnode *dvp,
				 struct label *dvplabel,
				 struct componentname *cnp);
void	shill_vnode_post_lookup(struct ucred *cred, struct vnode *dvp,
				struct label *dvplabel,
				struct componentname *cnp,
				struct vnode *vp, struct label *vplabel);
int	shill_vnode_check_mmap(struct ucred *cred, struct vnode *vp,
			       struct label *label, int prot, int flags);
void	shill_vnode_check_mmap_downgrade(struct ucred *cred,struct vnode *vp,
					 struct label *vplabel, int *prot);
int	shill_vnode_check_mprotect(struct ucred *cred, struct vnode *vp,
				   struct label *vplabel, int prot);
int	shill_vnode_check_open(struct ucred *cred, struct vnode *vp,
			       struct label *vplabel, accmode_t accmode);
int	shill_vnode_check_poll(struct ucred *active_cred, struct ucred *file_cred,
			       struct vnode *vp, struct label *vplabel);
int	shill_vnode_check_read(struct ucred *active_cred, struct ucred *file_cred,
			       struct vnode *vp, struct label *vplabel);
int	shill_vnode_check_readdir(struct ucred *cred,struct vnode *dvp,
				  struct label *dvplabel);
int	shill_vnode_check_readlink(struct ucred *cred, struct vnode *vp,
				   struct label *vplabel);
int	shill_vnode_check_relabel(struct ucred *cred,struct vnode *vp,
				  struct label *vplabel,struct label *newlabel);
int	shill_vnode_check_rename_from(struct ucred *cred, struct vnode *dvp,
				      struct label *dvplabel, struct vnode *vp,
				      struct label *vplabel,
				      struct componentname *cnp);
int	shill_vnode_check_rename_to(struct ucred *cred, struct vnode *dvp,
				    struct label *dvplabel, struct vnode *vp,
				    struct label *vplabel, int samedir,
				    struct componentname *cnp);
int	shill_vnode_check_revoke(struct ucred *cred, struct vnode *vp,
				 struct label *vplabel);
int	shill_vnode_check_setacl(struct ucred *cred, struct vnode *vp,
				 struct label *vplabel, acl_type_t type,
				 struct acl *acl);
int	shill_vnode_check_setextattr(struct ucred *cred, struct vnode *vp,
				     struct label *vplabel, int attrnamespace,
				     const char *name);
int	shill_vnode_check_setflags(struct ucred *cred, struct vnode *vp,
				   struct label *vplabel, u_long flags);
int	shill_vnode_check_setmode(struct ucred *cred,struct vnode *vp,
				  struct label *vplabel, mode_t mode);
int	shill_vnode_check_setowner(struct ucred *cred, struct vnode *vp,
				   struct label *vplabel, uid_t uid, gid_t gid);
int	shill_vnode_check_setutimes(struct ucred *cred, struct vnode *vp,
				    struct label *vplabel,
				    struct timespec atime,
				    struct timespec mtime);
int	shill_vnode_check_stat(struct ucred *active_cred,
			       struct ucred *file_cred,
			       struct vnode *vp, struct label *vplabel);
int	shill_vnode_check_unlink(struct ucred *cred, struct vnode *dvp,
				 struct label *dvplabel, struct vnode *vp,
				 struct label *vplabel,
				 struct componentname *cnp);
int	shill_vnode_check_write(struct ucred *active_cred,
				struct ucred *file_cred,
				struct vnode *vp, struct label *vplabel);
int	shill_vnode_create_extattr(struct ucred *cred, struct mount *mp,
				   struct label *mplabel, struct vnode *dvp,
				   struct label *dvplabel, struct vnode *vp,
				   struct label *vplabel,
				   struct componentname *cnp);
void	shill_vnode_destroy_label(struct label *label);
void	shill_vnode_execve_transition(struct ucred *old, struct ucred *new,
				    struct vnode *vp, struct label *vplabel,
				    struct label *interpvplabel,
				    struct image_params *imgp,
				    struct label *execlabel);
int	shill_vnode_execve_will_transition(struct ucred *old, struct vnode *vp,
					 struct label *vplabel,
					 struct label *interpvplabel,
					 struct image_params *imgp,
					 struct label *execlabel);
int	shill_vnode_externalize_label(struct label *label, char *element_name,
				      struct sbuf *sb, int *claimed);
void	shill_vnode_init_label(struct label *label);
void	shill_vnode_copy_label(struct label *src, struct label *dst);
int	shill_vnode_internalize_label(struct label *label, char *element_name,
				      char *element_data, int *claimed);
void	shill_vnode_relabel(struct ucred *cred, struct vnode *vp,
			    struct label *vplabel, struct label *label);
int	shill_vnode_setlabel_extattr(struct ucred *cred, struct vnode *vp,
				     struct label *vplabel,
				     struct label *intlabel);

#endif /* VNODE_CHECKS_H */

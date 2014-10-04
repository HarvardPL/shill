#ifndef MOUNT_CHECKS_H
#define MOUNT_CHECKS_H

int
shill_mount_check_stat(struct ucred *cred,
                       struct mount *mp, struct label *mplabel);

#endif // MOUNT_CHECKS_H

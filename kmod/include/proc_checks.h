#ifndef PROC_CHECKS_H
#define PROC_CHECKS_H

int
shill_proc_check_debug(struct ucred *cred,
                       struct proc *p);
int
shill_proc_check_sched(struct ucred *cred,
                       struct proc *p);
int
shill_proc_check_signal(struct ucred *cred,
                        struct proc *proc, int signum);
int
shill_proc_check_wait(struct ucred *cred,
                      struct proc *proc);
void
shill_proc_destroy_label(struct label *label);
void
shill_proc_init_label(struct label *label);

#endif // PROC_CHECKS_H

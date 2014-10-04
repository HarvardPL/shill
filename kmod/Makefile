SRCS!=ls *.c *.h include/*.h
SRCS+=vnode_if.h  ../include/cflags.h
KMOD=shill
CFLAGS=-I../include -Iinclude -Wall -Werror -g

.include <bsd.kmod.mk>

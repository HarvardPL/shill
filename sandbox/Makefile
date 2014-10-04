PROG=sandbox

PREFIX?=        /usr/local
BINDIR=         ${PREFIX}/bin
LIBDIR=         ${PREFIX}/lib
MANDIR=         ${PREFIX}/man
FILESDIR=       ${PREFIX}/share/misc

SRCS=yacc.y lex.l
YHEADER=1

CFLAGS+=-I../include -Wall -Werror
LDADD+=-lshillrt
LDFLAGS+=-L../rt
NO_MAN=no_man

.include <bsd.prog.mk>

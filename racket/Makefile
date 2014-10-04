RACKETLOC!=which racket

all:
	find . -name '*.rkt' -exec raco make '{}' \;

clean:
	find . -name 'compiled' -print0 | xargs -0 rm -rf

install:
	raco link -n shill .
	ln -sf $(RACKETLOC) /usr/local/bin/shill

all:
	find . -name '*.cap' -exec raco make '{}' \;

clean:
	find . -name 'compiled' -print0 | xargs -0 rm -rf

install:
	raco link -n shill .

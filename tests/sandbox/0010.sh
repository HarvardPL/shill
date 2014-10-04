#!/usr/local/bin/bash

script=`basename $0`
echo "${script}: attempt to create a pipe in a sandbox with privilege"
tmp="tmp-$$"

mkdir ${tmp}

if [ $? -ne 0 ] ; then
    echo "Could not create test directory"
    exit $?
fi

cat > ${tmp}/policy.txt <<EOF
{ +lookup }
/
.

{ +exec }
/bin/sh
/bin/cat

{ +lookup, +stat }
/etc
/usr

{ +read, +stat }
/etc/libmap.conf

{ +read }
/var/run/ld-elf.so.hints

{ +read, +exec }
/libexec/ld-elf.so.1

{ +read, +exec, +stat }
/lib/libc.so.7
/lib/libedit.so.7
/lib/libncurses.so.8

{ +write, +append, +stat }
&stdout
&stderr

{ +read, +write, +append, +stat }
&pipefactory
EOF

mkdir ${tmp}/foo
mkdir ${tmp}/bar

cat > ${tmp}/bar/hello.txt <<EOF
hello
EOF

eval "../../sandbox/sandbox ${tmp}/policy.txt sh -c 'echo hello | cat' &> ${tmp}/out.txt"

diff ${tmp}/out.txt ${tmp}/bar/hello.txt >/dev/null

result=$?

if [ $result -eq 0 ] ; then
    echo "SUCCESS"
else
    echo "FAILED"
fi

rm -Rf ${tmp}

exit $result

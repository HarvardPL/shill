#!/usr/local/bin/bash

script=`basename $0`
echo "${script}: read through a symlink with permission"
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

{ +lookup { +read, +read-symlink }, +add-symlink, +create-file { +read } }
${tmp}/foo

{ +lookup { +read } }
${tmp}/bar

{ +exec }
/bin/sh
/bin/ln
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
/lib/libncursesw.so.8

{ +write, +append, +stat }
&stdout
&stderr
EOF

mkdir ${tmp}/foo
mkdir ${tmp}/bar

cat > ${tmp}/bar/hello.txt <<EOF
Hello world!
EOF

eval "../../sandbox/sandbox ${tmp}/policy.txt sh -c 'ln -s ../bar/hello.txt ${tmp}/foo/hello.txt && cat ${tmp}/foo/hello.txt' > ${tmp}/out.txt"

diff ${tmp}/out.txt ${tmp}/bar/hello.txt >/dev/null

result=$?

if [ $result -eq 0 ] ; then
    echo "SUCCESS"
else
    echo "FAILED"
fi

rm -Rf ${tmp}

exit $result

#!/usr/local/bin/bash

script=`basename $0`
echo "${script}: kill a process in the same sandbox"
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
/dev
{ +exec }
/bin/sh
/bin/kill
bin/shill-loop-test
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
/dev/null
&stderr
EOF

if [ -e bin/shill-loop-test ]; then
    eval "../../sandbox/sandbox ${tmp}/policy.txt sh -c 'bin/shill-loop-test & kill -9 \$!'" &> /dev/null
    killall shill-loop-test &> /dev/null
    result=$?
else
    echo "Compile bin/shill-loop-test first"
    result=0
fi

if [ $result -eq 0 ] ; then
    echo "FAILED"
    result=1
else
    echo "SUCCESS"
    result=0
fi

rm -Rf ${tmp}

exit $result

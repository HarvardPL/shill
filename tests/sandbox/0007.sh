#!/usr/local/bin/bash

script=`basename $0`
echo "${script}: attempt to kill process outside of sandbox"
tmp="`pwd`/tmp-$$"

mkdir ${tmp}

if [ $? -ne 0 ] ; then
    echo "Could not create test directory"
    exit $?
fi

cat > ${tmp}/policy.txt <<EOF
{ +exec }
/bin/kill
{ +stat }
/etc
{ +lookup, +stat }
/usr
{ +stat, +read }
/etc/libmap.conf
{ +read, +exec }
/libexec/ld-elf.so.1
{ +stat, +read, +exec}
/lib/libc.so.7
{ +lookup, +read }
/usr/share/locale/en_US.UTF-8
/usr/share/locale/UTF-8/LC_CTYPE
{ +read }
/var/run/ld-elf.so.hints
{ +write, +append, +stat }
&stdout
EOF

if [ -e bin/shill-loop-test ]; then
    bin/shill-loop-test > /dev/null &
    pid=$!
    disown
    ../../sandbox/sandbox ${tmp}/policy.txt kill -9 ${pid} &> /dev/null
    killall shill-loop-test
    result=$?
else
    echo "Compile bin/shill-loop-test first"
    result=1
fi

if [ $result -eq 0 ] ; then
    echo "SUCCESS"
else
    echo "FAILED"
fi

rm -Rf ${tmp}

exit $result

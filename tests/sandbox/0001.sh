#!/usr/local/bin/bash

script=`basename $0`
echo "${script}: cat a file"
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
/bin/cat
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
${tmp}/hello.txt
{ +write, +append, +stat }
&stdout
EOF

cat > ${tmp}/hello.txt <<EOF
Hello world!
EOF

../../sandbox/sandbox ${tmp}/policy.txt cat ${tmp}/hello.txt > ${tmp}/out.txt

diff ${tmp}/out.txt ${tmp}/hello.txt >/dev/null

result=$?

if [ $result -eq 0 ] ; then
    echo "SUCCESS"
else
    echo "FAILED"
fi

rm -Rf ${tmp}

exit $result

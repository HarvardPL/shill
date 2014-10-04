#!/usr/local/bin/bash

script=`basename $0`
echo "${script}: move a file"
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

{ +lookup { +makelink }, +unlink-file, +addlink }
${tmp}

{ +exec }
/bin/mv

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

{ +write, +append, +stat }
&stdout
&stderr
EOF

cat > ${tmp}/hello.txt <<EOF
Hello world!
EOF

cp ${tmp}/hello.txt ${tmp}/foo.txt

../../sandbox/sandbox ${tmp}/policy.txt mv ${tmp}/foo.txt ${tmp}/bar.txt >/dev/null

diff ${tmp}/bar.txt ${tmp}/hello.txt >/dev/null

result=$?

if [ $result -eq 0 ] ; then
    echo "SUCCESS"
else
    echo "FAILED"
fi

rm -Rf ${tmp}

exit $result

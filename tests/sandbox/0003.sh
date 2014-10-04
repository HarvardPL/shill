#!/usr/local/bin/bash

script=`basename $0`
echo "${script}: cat a file in debug mode"
tmp="tmp-$$"

mkdir ${tmp}

if [ $? -ne 0 ] ; then
    echo "Could not create test directory"
    exit $?
fi

cat > ${tmp}/policy.txt <<EOF

EOF

cat > ${tmp}/hello.txt <<EOF
Hello world!
EOF

../../sandbox/sandbox -d ${tmp}/policy.txt cat ${tmp}/hello.txt > ${tmp}/out.txt

diff ${tmp}/out.txt ${tmp}/hello.txt >/dev/null

result=$?

if [ $result -eq 0 ] ; then
    echo "SUCCESS"
else
    echo "FAILED"
fi

rm -Rf ${tmp}

exit $result

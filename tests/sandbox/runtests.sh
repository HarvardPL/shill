#!/usr/local/bin/bash

success=0
total=0

for test in 0*.sh
do
    total=$(($total+1))
    ./${test}
    if [ $? -eq 0 ]; then
	success=$(($success+1))
    fi
done

echo "==================================="
echo "${success} / ${total} tests passed."

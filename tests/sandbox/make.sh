#!/usr/local/bin/bash

function doit {
    for i in {1..50}
    do
	sleep 2
	{ time $CMD ; } 2>&1 | awk '/real/ { print $2 }'
    done
}

CMD="make clean all -DINVARIANTS"
echo None
doit

#CMD="sandbox -d empty.txt make clean all -DINVARIANTS"
#echo Sandbox
#doit

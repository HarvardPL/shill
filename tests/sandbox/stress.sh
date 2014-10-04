#!/usr/local/bin/bash

while [ 1 ]; do
    ./runtests.sh
    sysctl security.mac.shill
    sleep 1
done

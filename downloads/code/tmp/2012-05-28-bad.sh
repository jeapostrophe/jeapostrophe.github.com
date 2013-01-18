#!/bin/bash

x=${1:-10}

if [ $x -eq 0 ] ; then
    read
    echo done
else
    bash bad.sh $(expr $x - 1)
fi

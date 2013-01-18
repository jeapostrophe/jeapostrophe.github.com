#!/bin/bash

x=${1:-10}

if [ $x -eq 0 ] ; then
    read
    echo done
else
    exec ./2012-05-28-good.sh $(expr $x - 1)
fi

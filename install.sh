#!/bin/bash

os='U'

if [ -z "$1" ];
    then
        read -p "(M)ac or (L)inux? " os
else
    os=${1//-/}
fi

if [ $os == 'L' ];
    then
        echo You are running Linux
elif [ $os == 'M' ];
    then
        echo You are running Mac
else
    echo "OS not set... Exiting with no change."
fi

#!/bin/bash

if [ $OSTYPE == 'linux-gnu' ];
    then
        echo You are running Linux
elif [ $OSTYPE == 'darwin' ];
    then
        echo You are running Mac
elif [ $OSTYPE == 'msys' ]; 
    then
        echo You are using Git Bash on Windows
else
    echo "OS not set... Exiting with no change."
fi

#!/bin/bash
clear;
make;

echo " "
echo "###################"
echo "Compiled"
echo "###################"
echo " "


./main $1 res.out

echo " "
echo "###################"
echo "lisp compiled"
echo "###################"
echo " "

./vm res.out

echo " "
echo "###################"
echo "end"
echo "###################"
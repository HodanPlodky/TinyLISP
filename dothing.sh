#!/bin/bash
clear;
make;

echo " "
echo "###################"
echo "Compiled"
echo "###################"
echo " "


./main basic.lisp res.out

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
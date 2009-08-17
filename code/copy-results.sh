#!/bin/bash
if [ $# -eq 0 ]
    then
    echo "Pas assez d'arguments"
    exit
fi

mkdir "res/$1"
cp Makefile *.F90 *.dat *.plt "res/$1"
cp out.txt "res/$1/ecran.txt"
echo genere le $(date) > "res/$1/readme"
if [ $# -eq 2 ]
then
    echo "$2" >> "res/$1/readme"
fi

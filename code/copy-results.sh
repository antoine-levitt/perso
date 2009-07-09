#!/bin/bash
if [ $# -eq 0 ]
    then
    echo "Pas assez d'arguments"
    exit
fi

mkdir "res/$1"
cp Makefile *.f90 *.dat *.plt "res/$1"
cp out.txt "res/$1/ecran.txt"
if [ $# -eq 2 ]
then
    echo Commentaire "$2" dans "res/$1"
    echo "$2" > "res/$1/readme"
fi

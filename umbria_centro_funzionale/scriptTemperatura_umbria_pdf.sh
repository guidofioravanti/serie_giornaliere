#!/bin/bash
#
#Pulisce il file di testo della precipitazione (generato dal pdf mediante pdftotext -layout) in modo di avere un file più semplice da elaborare
#Il file temp.txt va elaborato con lo script R
#
#I file pdf sono stati acquisiti da: centro funzionale Umbria (sono disponibili i pdf a cominciare dal 2016)

pdfseparate ${1} temp%d.pdf

for dd in $(ls temp*.pdf); do

pdftotext -layout ${dd}

grep -A35 -B4 -E "^ +Gen +Feb + Mar.+$" ${dd%.pdf}.txt |
grep -v "^--$" | 
sed -e 's/^\(.\)\( \+.\+\)Termometro aria .\+$/\2/g' |
grep -v -E "^$" > clean_${dd%.pdf}.txt

rm -rf $dd
rm -rf ${dd%.pdf}.txt

done

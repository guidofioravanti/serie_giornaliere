#!/bin/bash
#per acquisire la lista dei pdf da scaricare mediante "wget -i" è stata scaricata la pagina html qui sotto e quindi usato questo script.
#I file pdf sono quindi stati trasformati in testo e rielaborati con R.
grep -E "http:/.+/Klima.+pdf" http\:_meteo.provincia.bz.it_dati-storici.asp.html  | sed -n -e 's/\(.\+\)\(http:\/\/.\+pdf\)\(<.\+\)$/\2/p'

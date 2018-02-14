rm(list=objects())
library("tidyverse")
library("stringr")
library("stringdist")
source("lettura_help.R")
options(error=recover)

tipoSensore<-c("Pluviometro","Termometro")

purrr::map(tipoSensore,.f=function(sensore){
  list.files(pattern=paste0("^Stampa.+",sensore,".+csv$"))  
})->listaFile

names(listaFile)<-tipoSensore
purrr::map(listaFile,length)->numeroFile
if(all(numeroFile==0)) stop("Nessun file trovato!")

names(which.max(numeroFile))->sensore
print(sprintf("Dati %s",sensore))
listaFile[[sensore]]->ffile

purrr::walk(ffile,.f =leggiPrecipitazione)
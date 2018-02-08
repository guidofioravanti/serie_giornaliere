#8 febbraio 2018
#Elaborazione delle serie del Centro Funzionale Marche

#Dati di input: serie del Centro Funzionale (dati giornalieri)
#Dati di output: 
#- serie giornaliere in formato HisCentral (per Piero); 
#- file unico per Tmax/Tmin/Prec per gli anni in esame (quelli dei file passati dal Centro Funzionale)
#- file unico per Tmax/Tmin/Prec da passare alle routine per i controlli di qualità dato dall'unione dei dati elaborati con i dati storici. Quest'ultimo
#  file viene prodotto solo se esiste il file Tmax.csv/Tmin.csv/Precipitation.csv

#I dati di output sono prodotti anche in formato HisCentral per passarli a Piero

#Si utilizza un file anagrafica che è l'unione delle serie in HisCentral più le serie passate dal Centro Funzionale
rm(list=objects())
library("readr")
library("tidyr")
library("magrittr")
library("dplyr")
library("lubridate")
library("stringr")
library("purrr")
options(warn=2,stringsAsFactors=FALSE,error=recover)

#elaborare un parametro per volta, scegliere il parametro 1 ,2 ,3 <----------------------- DA FISSARE
PARAMETRO<-c("Tmax","Tmin","Precipitation")[c(1)]

#anno inizio e fine delle serie: fissare anno inizio e fine <----------------------- DA FISSARE
ANNOI<-"2017" #<----------------------- DA FISSARE
ANNOF<-"2017" #<----------------------- DA FISSARE

# INIZIO PROGRAMMA

#file a cui unire i dati
nome.file.storici<-paste0(PARAMETRO,".csv")
if(file.exists(nome.file.storici)){
  print(sprintf("TROVATO file %s, i dati elaborati verranno uniti alle serie degli anni precedenti ",nome.file.storici))
}else{
  print(sprintf("File %s NON TROVATO, i dati elaborati NON verranno uniti alle serie degli anni precedenti ",nome.file.storici))  
}

giornoI<-as.Date(paste0(ANNOI,"-01-01"))
giornoF<-as.Date(paste0(ANNOI,"-12-31"))
calendario<-data.frame(time=as.character(seq.Date(from=giornoI,to=giornoF,by="day")))

if(PARAMETRO=="Tmax"){
  stringa<-"TX"
}else if(PARAMETRO=="Tmin"){
  stringa<-"TN"  
}else if(PARAMETRO=="Precipitation"){
  stringa<-"PP"  
}else{
  stop("Parametro da elaborare non riconosciuto")
}

list.files(pattern=paste0("^[0-9]+-",stringa,".txt$"))->ffile
stopifnot(length(ffile)!=0)

#creazione directory
try(dir.create("mancanti"))
try(dir.create("nuovoFormato"))

#anagrafica che contiene sia le info di HisCentral che le info che appaiono solo nel set di dati del Centro Funzionale
tryCatch({
  read.csv(file="reg.marche.info.csv",sep=";",head=TRUE)
},error=function(e){
  stop("Non trovo anagrafica reg.marche")
})->ana

purrr::map(ffile,.f=function(nome.file){

  #print(sprintf("--> Leggo file di dati: %s",nome.file))

  tryCatch({
    read.csv(nome.file,sep="|",head=FALSE)[,c(1,2)]
  },error=function(e){
    stop("Errore lettura file %s",nome.file)
  })->dati
    
  names(dati)<-c("time","DataValue")
  dati$time<-as.character(dati$time)
  dati$DataValue<-as.double(dati$DataValue)

  #scaliamo le date di 1
  dati %<>% mutate(time=as.Date((time),format="%Y%m%d",tz="UTC") ) %>%
    mutate(UTCOffset=NA,Qualifier=NA,CensorCode="nc",DateTimeUTC=time,MethodCode=NA,SourceCode=NA,QualityControlLevelCode=NA) %>%
    filter(time>giornoI & time<giornoF) %>% arrange(time)

  if(length(grep("PP",nome.file))==1) PARAMETRO<-"Precipitation"
  if(length(grep("TX",nome.file))==1) PARAMETRO<-"Tmax"
  if(length(grep("TN",nome.file))==1) PARAMETRO<-"Tmin"

  #cerca codice in anagrafica
  unlist(str_split(nome.file,"-"))[1]->codice
  which(ana$SiteID==codice)->riga

  #trovo più di un codice
  if(length(riga)>1) stop(sprintf("Impossibile: codice SiteID %s duplicato in anagrafica",codice))
  
  #non ho trovato corrispondenza
  if(length(riga)==0){

    sink("_log_codiciNonInAnagrafica.txt",append=TRUE)
    cat(paste0("Codice non trovato nel file di anagrafica: ",codice,"\n"))
    sink()
    
    write_delim(dati,path=paste0("./mancanti/",nomeOut),col_names=TRUE,delim=";")
    return(NULL)        
    
  }
  
  #ok ho trovato il codice
  nomeOut<-paste0("serie_",codice,"-reg.marche",PARAMETRO,".csv")

  dati %>% write_delim(.,path=paste0("./nuovoFormato/",nomeOut),col_names=TRUE,delim=";")
  
  #assegna il codice a DataValue
  names(dati)[2]<-codice
  
  dati %>% mutate(time=as.character(time)) %>% select(1:2)

}) %>% compact %>% 
  reduce(left_join,by=c("time"="time"),.init=calendario) %>%
  separate(col=time,into=c("yy","mm","dd"),sep="-")->finale

  write_delim(finale,path=paste0(PARAMETRO,"_",ANNOI,"_",ANNOF,".csv"),delim=",",col_names=TRUE)

if(file.exists(nome.file.storici)){
  
  read_delim(nome.file.storici,delim=",",col_names = TRUE) %>% 
    mutate(yy=as.character(yy),mm=as.character(mm),dd=as.character(dd))->storici
  
  #conversione delle colonne in numeric
  ncol(storici)->numeroColonne
  purrr::map(4:numeroColonne,.f=~(as.numeric(storici[[.]]))) %>% reduce(cbind) %>% as.data.frame()->tmp
  names(tmp)<-names(storici)[4:numeroColonne]
  data.frame(storici[,c(1,2,3)],tmp,check.names = FALSE)->storici
  
  bind_rows(storici,finale) %>% 
    write_delim(.,path=paste0(PARAMETRO,"_storici_fino_a_",ANNOF,".csv"),delim=",",col_names=TRUE)
  
}#fine if



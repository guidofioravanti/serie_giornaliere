#12 Febbraio 2018, revisione programma per stazioni Valle d'Aosta
#
#I dati di input (dati orari) sono divisi in due cartelle: pluviometro (dati di pioggia in formato Pluviometro_.+csv) e
# termometro (dati di temperatura Termometro_.+csv)
rm(list=objects())
library("dplyr")
library("magrittr")
library("readr")
library("tidyr")
library("lubridate")
library("stringr")
library("purrr")
source("indexTime_maggio2017.R")
options(error=recover,warn = 2)

PARAMETRO<-c("Tmax","Tmin","Precipitation")[c(1)] #<------------- selezionare parametro, il programma va fatto girare per un parametro alla volta
ANNOI<-2017 #<------------- selezionare anno inizio e fine dei dati
ANNOF<-2017 #<------------- selezionare anno inizio e fine dei dati

#file a cui unire i dati
nome.file.storici<-paste0(PARAMETRO,".csv")
if(file.exists(nome.file.storici)){
  print(sprintf("TROVATO file %s, i dati elaborati verranno uniti alle serie degli anni precedenti ",nome.file.storici))
}else{
  print(sprintf("File %s NON TROVATO, i dati elaborati NON verranno uniti alle serie degli anni precedenti ",nome.file.storici))  
}


# Inizio programma --------------------------------------------------------
giornoI<-as.Date(paste0(ANNOI,"-01-01"))
giornoF<-as.Date(paste0(ANNOF,"-12-31"))
yymmdd<-as.character(seq.Date(from=giornoI,to=giornoF,by="day"))

data.frame(yymmdd=yymmdd) %>% separate(yymmdd,c("yy","mm","dd"),sep="-")->calendario

Sys.setenv(TZ="UTC")
#################################################
ripulisci<-function(myDF){
  

  #per essere sicuri dell'ordine convertiamo in POSIXct (supportato da dplyr)
  myDF %<>% ungroup %>% select(-val,-interi) %>%
    mutate(UTCOffset=NA,Qualifier=NA,CensorCode=NA,DateTimeUTC=NA,MethodCode=NA,SourceCode=NA,QualityControlLevelCode=NA)

  return(myDF)
  
}#fine funzione ripulisci
#################################################

if(PARAMETRO=="Precipitation"){
  if(!dir.exists("pluviometro")) stop("Non trovo la directory con i dati di pioggia")
  list.files(pattern="Pluviometro_[0-9]+\\.csv",path="./pluviometro")->ffile
  directory<-"pluviometro"
}else if(PARAMETRO=="Tmax" || PARAMETRO=="Tmin"){
  if(!dir.exists("termometro")) stop("Non trovo la directory con i dati di temperatura")  
   list.files(pattern="Termometro_[0-9]+\\.csv",path="./termometro")->ffile  
   directory<-"termometro"   
}else{
  stop("PARAMETRO NON RICONOSCIUTO")
}

stopifnot(length(ffile)!=0)
paste(directory,ffile,sep="/")->ffile

#Lettura anagrafica regione Valle d'Aosta
tryCatch({
  read.csv("reg.aosta.info.csv",sep=";",head=TRUE,stringsAsFactors=FALSE,check.names=FALSE)
},error=function(e){
  stop("File anagrafica Valle d'Aosta non trovato")
})->ana

purrr::map(ffile,.f=function(nome.file){
  
  #Prima di tutto: estraiamo il codice della stazione. Questo codice viene ricavato dal programma selenium
  #che al momento dello scarico del file zip individua il codice stazione, fa unzip del file e rinomina
  #il file di dati in Pluviometro/Termometro_codice.csv
  if(is.null(nome.file)) browser()
  str_extract(nome.file,pattern="[0-9]+")->codiceStazione
  tryCatch({
    as.integer(codiceStazione)
  },error=function(e){
    stop(sprintf("Errore di conversione del codice stazione, verificare il codice %s",codiceStazione))
  })
    
  #lettura come readLines, il formato sembra essere cambiato da un anno all'altro con l'aggiunta di un'intestazione.
  #Leggiamo i file in modo generico
  tryCatch({
    readLines(nome.file)
  },error=function(e){
    stop(sprintf("File %s non trovato",nome.file))
  })->tmp  
  
  #La riga numero due deve contenere il nome della stazione. Va fissato il corretto encoding ovvero latin1
  tmp[2]->stringaStazione
  Encoding(stringaStazione)<-"latin1"
  #ora convertiamo in UTF-8
  iconv(tmp[2],from="latin1",to="utf-8")->stringaStazione
  
  #verifichiamo che contenga la stringa Stazione
  if(!grepl("Stazione:",stringaStazione)) stop(sprintf("Verificare la stringa %s nel file %s"),stringaStazione,nome.file)
  #estraiamo il nome della stazione
  str_replace(str_extract(stringaStazione,": .+$"),": ","")->nomeStazione
  rm(stringaStazione)
  
  #elaboriamo la stringa
  str_split(str_replace(str_extract(tmp,"[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}.+$"),",","."),";")->tmp2
  
  #le righe dell'intestazione restituiscono in tmp2 solo NA, eliminiamo questa parte
  which(purrr::map_int(tmp2,length)==1)->intestazione
  tmp2[-intestazione]->tmp2
  
  #valori e ore
  str_replace(purrr::map_chr(tmp2,1),"\"","")->yymmdd
  as.numeric(purrr::map_chr(tmp2,2))->valori
  
  #data.frame da elaborare
  dati<-data.frame(time=yymmdd,val=valori)
  
  #names(dati)<-c("time","val","flag") 
  parse_date_time(dati$time, orders="ymd HMS")->dati$time

  order(dati$time)->indice
  dati[indice,]->dati	

  #le serie storiche vanno cumulate dalle 00 alle 24 passando a indexTime oraRiferimento=0. 
  #Le uniche serie storiche di cui effetticamente si hanno aggiornamenti sul sito del Centro Funzionale VDA sono la 1270, 3060, 4090
  if(codiceStazione %in% c("1270","3060","4090","G003","G006","G007","G008") ){
    indexTime(myTime=dati$time,oraRiferimento = 0)->vettoreInteri
  }else{
    indexTime(myTime=dati$time,oraRiferimento = 9)->vettoreInteri
  }
    
  dati$interi<-vettoreInteri

  dati %<>% group_by(interi)

  #Aggreghiamo i dati orari: somma per precipitazione, max e min per temperatura
  if(PARAMETRO=="Precipitation"){

    dati %>% summarise(DataValue=cumulaPrec(val))->prcp
    left_join(dati,prcp,by=c("interi"))->jprcp
    #ora da tutti i dati orari dobbiamo estrarre il giorno. Il giorno di ciascun valore è quello alle 
    #ore 9.00 ovvero quando il vettore interi incrementa
    which(diff(jprcp$interi)!=0)->posizionePrcp
    #attenzione: mai slice con il risultato di un join! non funziona
    ripulisci(jprcp[posizionePrcp,])->datiFinale

  }else if(PARAMETRO=="Tmax"){

   dati %>% summarise(DataValue=massimoTemp(val))->tmax   
   left_join(dati,tmax,by=c("interi"))->jtmax
   which(diff(jtmax$interi)!=0)->posizioneTmax
   ripulisci(jtmax[posizioneTmax,])->datiFinale
    
  }else if(PARAMETRO=="Tmin"){    

    dati %>% summarise(DataValue=minimoTemp(val))-> tmin
    left_join(dati,tmin,by=c("interi"))->jtmin
    which(diff(jtmin$interi)!=0)->posizioneTmin
    ripulisci(jtmin[posizioneTmin,])->datiFinale
    
  }else{
    
    stop("Errore: parametro non riconosciuto")
    
  }#fine if

  #dati in formato HisCentral
  datiFinale %>% separate(time,c("yymmdd","hhmmss"),sep=" ") %>%
    separate(yymmdd,c("yy","mm","dd"),sep="-") %>%
    select(yy,mm,dd,DataValue)->datiFinale
  
  #Cerchiamo la stazione in anagrafica: due controlli per nome stazione e codice
  agrep(nomeStazione,ana$SiteName)->rigaAgrep
  which(ana$SiteCode==codiceStazione)->rigaWhich

  if(length(rigaWhich)>1) stop(sprintf("Errore codice %s,corrispondenza multipla nel file di anagrafica",codiceStazione))  
  if(length(rigaWhich)==0){
  
    sink("_stazioni_non_in_anagrafica.txt",append=TRUE)
    cat(sprintf("Stazione %s con codice %s non in anagrafica\n",nomeStazione,codiceStazione))
    sink()
    
    #la stazione NON viene prodotta in output
    return(NULL)
      
  }#fine if
  
  
  if(length(rigaWhich)==1 && length(rigaAgrep)>1){
    
    sink("_stazioni_con_associazione_multipla_per_nome.txt",append=TRUE)
    cat("################################################\n")    
    for(zz in rigaAgrep){
      cat(sprintf("Stazione %s con codice %s di riga %s associato a stazione %s \n",nomeStazione,codiceStazione,zz,ana[zz,]$SiteName))    
    }  
    cat("################################################\n")    
    sink()
    
  }
  
  if(length(rigaWhich)==1 && length(rigaAgrep)==0){
    
    sink("_stazioni_senza_associazione_in_anagrafica.txt",append=TRUE)
    cat("################################################\n")        
    cat(sprintf("Codice %s non associato in anagrafica\n",codiceStazione)) 
    cat(sprintf("Codice %s ha nome %s, in anagrafica il  suo nome è %s\n",codiceStazione,nomeStazione,ana[rigaWhich,]$SiteName))     
    cat("Anche in assenza di associazione la stazione viene prodotta nel file di output\n")
    cat("################################################\n")    
    sink()
    
  }  
  
  names(datiFinale)[4]<-ana[rigaWhich,]$SiteID
  
  datiFinale
 
}) %>% compact-> listaOut
  
listaOut %>% reduce(left_join,by=c("yy"="yy","mm"="mm","dd"="dd"),.init=calendario)->finale

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

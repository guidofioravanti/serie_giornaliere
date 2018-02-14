#13 Febbraio 2018, revisione programma per stazioni Abruzzo
#
#I dati di input giornalieri sono divisi in due cartelle: pluviometro (dati di pioggia in formato Pluviometro_.+csv) e termometro (dati di temperatura Termometro_.+csv)
#
#I dati di input riportano in intestazione il nome della serie ma nessun codice. Questo programma crea un file 
#in cui mediante "stringdist" vengono create le possibili associazioni tra i nomi che compaiononell'intestazione del file
#e i nomi che compaiono nel file di anagrafica inviato dal Centro Funzionale. Da questa associazione è quindi possibile ricavare il codice
#HisCentral per accodare i dati alle serie storiche

#Output del programma: 
# - un file csv con la stazione nell'header file e il nome in anagrafica sulla base di stringdist (diversi algoritmi di distanza)
# - un file csv con le stazioni che non è stato possibile associare
#
#I due file vanno risistemati manualmente (ad esempio: l'Aquila Meteo è impossibile associarla ai nomi in anagrafica, va capito manualmente a chi corrisponde)
#e quindi fatto girare il programma che crea i file per i controlli di qualità, accodando i nuovi dati alle serie storiche

#Per correggere eventuali stazioni non trovate o stazioni associate male si può utilizzare il file ottenuto dalla mappa delle stazioni dell'abruzzo
#
#In sintesi il problema è che i file riportano un nome stazione. Questo nome stazione non sempre ha un analogo univoco (o nessuna corrispondenza)
#nel file di anagrafica inviato dal Centro Funzionale (colonna nome stazione). La mappa (leaflet) delle stazioni dell'Abruzzo contiene per ogni stazione
#il nome e il codice. Questo nome nella mappa "leaflet" aiuta a risolvere gli eventuali dubbi in quanto corrisponde al nome delle stazioni nel file header.
rm(list=objects())
library("dplyr")
library("magrittr")
library("readr")
library("tidyr")
library("lubridate")
library("stringr")
library("purrr")
library("stringdist")
options(error=recover,warn = 2)

PARAMETRO<-c("Tmax","Tmin","Precipitation")[c(1)] #<------------- selezionare parametro, il programma va fatto girare per un parametro alla volta


if(PARAMETRO=="Precipitation"){
  if(!dir.exists("pluviometro")) stop("Non trovo la directory con i dati di pioggia")
  list.files(pattern="^.+P\\.csv",path="./pluviometro")->ffile
  directory<-"pluviometro"
}else if(PARAMETRO=="Tmax" || PARAMETRO=="Tmin"){
  if(!dir.exists("termometro")) stop("Non trovo la directory con i dati di temperatura")  
  list.files(pattern="^.+T.*max.*min.*\\.csv",path="./termometro")->ffile  
  directory<-"termometro"   
}else{
  stop("PARAMETRO NON RICONOSCIUTO")
}

stopifnot(length(ffile)!=0)
paste(directory,ffile,sep="/")->ffile

#Lettura anagrafica stazioni inviata dal Centro Funzionale
tryCatch({
  read.csv("anagrafica_stazioni_idrope.csv",sep=";",head=TRUE,stringsAsFactors=FALSE,check.names=FALSE)
},error=function(e){
  stop("File anagrafica Valle d'Aosta non trovato")
})->anaCF



# Funzione per individuare la stazione nell'anagrafica del Centro  --------
trovaRiga<-function(nomiStazioni,testoIntestazione,metodo){
  
  purrr::map_dbl(nomiStazioni,.f=function(.x){
    
    stringdist(.x,testoIntestazione,method=metodo, weight = c(d = 1,i = 0.1, s = 1, t = 0.5))
    
  })->distanze
  
  which.min(distanze)
  
} #fine funzione trovaRiga


#elaboraIntestazione
elaboraIntestazione<-function(fileHeader,param){
  
  stopifnot(param %in% c("Precipitation","Tmax","Tmin"))
  
  if(param=="Precipitation"){
    fileHeader[grep("Stazione",fileHeader)]->ris
    str_replace(str_replace(ris,"^Stazione +",""),"\\(LAT.+\\)","")->ris
  }else{
    fileHeader[grep("Massime",fileHeader)]->ris
    str_replace(str_replace(ris,"^Data;Ora;","")," * - *T.? - *Massime *Giornaliere.+","")->ris 
    
    #se length(ris)==0, prova con un'intestazione analoga a quella della precipitazione
    if(length(ris)==0){
      fileHeader[grep("Stazione",fileHeader)]->ris
      str_replace(str_replace(ris,"^Stazione +",""),"\\(LAT.+\\)","")->ris      
    }
    
  }#fine if su PARAMETRO  
  
  stopifnot(length(ris)!=0)
  
  #ris contiene il nome della stazione
  ris
  
}#fine elaboraIntestazione


purrr::walk(ffile,.f=function(nome.file){
  
  #lettura come readLines, il formato sembra essere cambiato da un anno all'altro con l'aggiunta di un'intestazione.
  #Leggiamo i file in modo generico
  tryCatch({
    readLines(nome.file)
  },error=function(e){
    stop(sprintf("File %s non trovato",nome.file))
  })->tmp  
  
  Encoding(tmp)<-"latin1"
  
  #elaboriamo la stringa
  str_split(str_replace(str_extract(tmp,"^[0-9]{2}/[0-9]{2}/[0-9]{4};.+$"),",","."),";")->tmp_splitted
  
  #almeno una riga di intestazione ce la aspettiamo
  which(is.na(tmp_splitted))->righeIntestazione
  stopifnot(length(righeIntestazione)!=0)
  
  #troviamo ora la righe che contiene il nome della stazione
  tmp[righeIntestazione]->fileHeader
  
  elaboraIntestazione(fileHeader,param=PARAMETRO)->nomeStazioneInHeader
  
  #metodi per il calcolo delle distanze tra stringhe
  c("osa", "lv", "dl", "hamming", "lcs", "qgram","cosine", "jaccard", "jw")->metodiDistanze
  
  purrr::map_dbl(metodiDistanze,.f=~(trovaRiga(nomiStazioni=anaCF$nomeStazione,testoIntestazione=nomeStazioneInHeader,metodo=.)))->listaRighe
  
  #trova la riga più frequente, ovvero la riga che corrisponde all'associazione più probabile in base ai vari metodi per il calcolo delle
  #distanze tra stringhe
  as.integer(names(which.max(table(listaRighe))))->rigaInAnagrafica
  
  
  if(length(rigaInAnagrafica)==1){
    
    sink(paste0(PARAMETRO,"_stazioni_trovate_associazioni.txt"),append=TRUE)
    cat(sprintf("%s;%s\n",nomeStazioneInHeader,anaCF[rigaInAnagrafica,]$nomeStazione))
    sink()        
    
  }else{ #nessuna riga
    
    sink(paste0(PARAMETRO,"_stazioni_non_trovate.txt"),append=TRUE)
    cat(sprintf("Stazione %s non trovata\n",nomeStazioneInHeader))
    sink()
    
  }#fine if
  
}) #fine walk, fine programma
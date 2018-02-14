#13 Febbraio 2018, revisione programma per stazioni Abruzzo
#
#I dati di input (dati orari) sono divisi in due cartelle: pluviometro (dati di pioggia in formato Pluviometro_.+csv) e
# termometro (dati di temperatura Termometro_.+csv)

#Per far girare il programma è necessario aver prima fatto girare il programma che associa ai nomi delle stazioni (che compare nell'intestazione di ciascun file di dati)
#il corrispettivo nome in anagrafica del Centro Funzionale (questa associazione è quasi perfetta, i casi associati male vanno aggiustati a mano).
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

#Lettura anagrafica regione Abruzzo
tryCatch({
  read.csv("reg.abruzzo.info.csv",sep=";",head=TRUE,stringsAsFactors=FALSE,check.names=FALSE)
},error=function(e){
  stop("File anagrafica Abruzzo HC non trovato")
})->anaHC


#Lettura anagrafica stazioni inviata dal Centro Funzionale
tryCatch({
  read.csv("anagrafica_stazioni_idrope.csv",sep=";",head=TRUE,stringsAsFactors=FALSE,check.names=FALSE)
},error=function(e){
  stop("File anagrafica Abruzzo CF non trovato")
})->anaCF

str_trim(anaCF$nomeStazione,side="both")->anaCF$nomeStazione


#lettura del file delle associazioni nomi
tryCatch({
  read_delim("associazioni_nomi_stazioni.txt",delim=";",col_names=FALSE,comment="#")
},error=function(e){
  stop("File asssociazioni nomi non trovato")
})->associazioni

#la prima colonna è il nome come compare nell'header del file di input, il secondo è il nome come compare in anagrafica
names(associazioni)<-c("nomeInFile","nomeInAna")

str_trim(associazioni$nomeInFile,side="both")->associazioni$nomeInFile
str_trim(associazioni$nomeInAna,side="both")->associazioni$nomeInAna


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





purrr::map(ffile,.f=function(nome.file){
  
  print(nome.file)
  
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
  
  #erroneamente ho scritto i dati di pioggia con il formato yy/mm/dd invece di dd/mm/yy, prevediamo
  #quindi un controllo quando tmp_splitted è tutto NA 
  if(all(is.na(purrr::map(tmp_splitted,1)))){
    str_split(str_replace(str_extract(tmp,"^[0-9]{4}/[0-9]{2}/[0-9]{2};.+$"),",","."),";")->tmp_splitted
    PARSE_FORMAT<-"ymd"
  }else{
    PARSE_FORMAT<-"dmy"    
  }  

  if(all(is.na(purrr::map(tmp_splitted,1)))) stop("str_extract ha fallito, non riconosco l'espressione regolare")
  
  #almeno una riga di intestazione ce la aspettiamo
  which(is.na(tmp_splitted))->righeIntestazione
  stopifnot(length(righeIntestazione)!=0)
  
  #troviamo ora la righe che contiene il nome della stazione
  tmp[righeIntestazione]->fileHeader
  
  elaboraIntestazione(fileHeader,param=PARAMETRO)->nomeStazioneInHeader
  str_trim(nomeStazioneInHeader,side="both")->nomeStazioneInHeader
  print(nomeStazioneInHeader)
  
  #questa stazione non compare in anagrafica e non se ne conosce il codice, compare però nella mappa leaflet dell'Abruzzo
  if(grepl("Villa Vallucci",nomeStazioneInHeader)) return(NULL) #stazione sconosciuta
  
  #queste stazioni compaiono sia nel file da leaflet mappa che nell'anagrafica del CF ma i codici non corrispondono
  if(grepl("^Bellante$",nomeStazioneInHeader)) return(NULL) #stazione sconosciuta
  if(grepl("^Citta' S.Angelo$",nomeStazioneInHeader)) return(NULL) #stazione sconosciuta  
  if(grepl("^Isola del Gran Sasso$",nomeStazioneInHeader)) return(NULL) #stazione sconosciuta  
  if(grepl("^Ortucchio$",nomeStazioneInHeader)) return(NULL) #stazione sconosciuta  
  
    
  which(associazioni$nomeInFile==nomeStazioneInHeader)->rigaAssociazione
  #non può fallire il contrololo che segue perchè il file con le associazioni è stato creato utilizzando gli header dei file
  stopifnot(length(rigaAssociazione)==1)

  which(anaCF$nomeStazione==associazioni[rigaAssociazione,]$nomeInAna)->rigaInCF
  #non può fallire il contrololo che segue perchè il file con le associazioni è stato creato utilizzando i nomi in anaCF
  stopifnot(length(rigaInCF)==1)
  
  #questo è il codice di HisCentral
  anaCF[rigaInCF,]$codIdro->codice
  
  #Il codice trovato compare effettivamente nella nostra anagrafica di HisCentral?
  which(anaHC$SiteCode==codice)->rigaInHC
  
  if(length(rigaInHC)==0){
    
    sink(paste0(PARAMETRO,"_stazioni_non_in_HC.txt"),append=TRUE)
    cat(sprintf("%s;%s\n",nomeStazioneInHeader,codice))
    sink()     
    return(NULL)
  }#fine if
  
  #ora ricaviamo il codice che assoceremo alla stazione nel file finale
  anaHC[rigaInHC,]$SiteID->codiceID
  
  
  tmp_splitted[-righeIntestazione]->tmp2
  #valori e ore
  str_replace(purrr::map_chr(tmp2,1),"\"","")->yymmdd
  
  #nel trasformare le stringhe in numeri dobbiamo evitare i warnings causati dalla presenza di valori mancanti
  #espressi come NA o come "-". Infatto options(warn=2) trasforma i warnings in errori--> trasformiamo NA e "-" in ""
  
  if(PARAMETRO=="Precipitation"){

    as.numeric(str_replace(str_replace(str_replace(purrr::map_chr(tmp2,2),"^-$",""),"^NA$",""),",","."))->valori        
    
  }else if(PARAMETRO=="Tmax"){
    
    purrr::map_chr(tmp2,2)->ore
    #la seconda colonna me la aspetto tutta di ore, ma può non accadere. Il file di Campo Imperatore ad esempio va scartato
    #Ha un header come la precipitazione, ha solo due colonne (data e tmin???)..quindi i suoi dati vanno scartati, non affidabili
    if(!all(grep("^[0-9]{2}:[0-9]{2}$",ore))){

      sink(paste0(PARAMETRO,"_stazioni_con_dati_anomali.txt"),append=TRUE)
      cat(sprintf("%s;%s\n",nomeStazioneInHeader,codice))
      sink()            
      
      return(NULL)
    }
    
    #if(nomeStazioneInHeader=="Aterno a Fagnano") browser()
    purrr::map_chr(tmp2,3)->valori
    as.numeric(str_replace(str_replace(str_replace(valori,"^-$",""),"^NA$",""),",","."))->valori
    
  }else if(PARAMETRO=="Tmin"){
    
    purrr::map_chr(tmp2,2)->ore
    if(!all(grep("^[0-9]{2}:[0-9]{2}$",ore))){
      
      sink(paste0(PARAMETRO,"_stazioni_con_dati_anomali.txt"),append=TRUE)
      cat(sprintf("%s;%s\n",nomeStazioneInHeader,codice))
      sink()            
      
      return(NULL)
    }    

    purrr::map_chr(tmp2,4)->valori
    as.numeric(str_replace(str_replace(str_replace(valori,"^-$",""),"^NA$",""),",","."))->valori
                   
    
  }#fine if
  
  
  #data.frame da elaborare
  dati<-data.frame(time=yymmdd,val=valori)

  #il formato 2015/2016 della precipitazione prevede 31 giorni per tutti i mesi, quindi
  #nel fare le conversioni dei warnings ci saranno sempre perche (ad esempio i 30 febbraio)
  #verranno trasformati in NA. 
  #Questi NA spariranno quando alla fine unisco tutte le serie mediante reduce, left_join usando come .init=calendario
  suppressWarnings(parse_date_time(dati$time, orders=PARSE_FORMAT)->dati$time)

  #ovviamente dati$time non deve essere tutto NA
  which(!is.na(dati$time))->conversioniRiuscite
  
  #il controllo che segue però non ha senso se i dati passati hanno giorni mancanti
  #In questo caso length(conversioniRiuscite) differisce dal numero di giorni nel calendario
  #I dati dell'Abruzzo però sembrano non soffrire della presenza di date mancanti
  if(length(conversioniRiuscite)!=nrow(calendario)) stop("Il numero di giorni validi in dati$time != dalla lunghezza del calendario")
    
  order(dati$time)->indice
  dati[indice,]->dati	

  str_replace(as.character(dati$time)," *UTM","")->dati$time
  
  dati %<>% separate(time,c("yy","mm","dd"),sep="-")
  names(dati)[4]<-codiceID
  
  dati
 
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

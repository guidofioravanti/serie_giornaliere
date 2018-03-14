rm(list=objects())
library("stringr")
library("readr")
library("dplyr")
library("purrr")
library("tidyr")
library("magrittr")

ANNO<-"2017"
parametro<-c("Tmax","Tmin")[1]

ifelse(parametro=="Tmax","Valori massimi","Valori minimi")->stringaTemp

seq.Date(from=as.Date(paste0(ANNO,"-01-01")),to=as.Date(paste0(ANNO,"-12-31")),by="day")->calendario
data.frame(calendario) %>% separate(col=calendario,into=c("yy","mm","dd"),sep="-")->calendario

try(file.remove("log.txt"))

#mesi
stringr::str_pad(1:12,pad="0",side="left",width=2)->mesi

purrr::imap(list.files(pattern="^clean_temp.+txt$"),.f=function(nomeFile,.y){

  readLines(nomeFile,n=2)->intestazione
  stringr::str_trim(intestazione[1],side="both")->nomeStazione
  print(sprintf("Elaboro stazione %s",nomeStazione))

  stringr::str_trim(intestazione[2],side="both")->qualeParametro  
  if(qualeParametro!=stringaTemp) return(NULL)
  
  #if(nomiStazioni[.y]!="Castelluccio di Norcia") return()
  read_table(nomeFile,col_names = TRUE,skip=2,na=c("","*"))->dati_full
  names(dati_full)[1]<-"dd"
  
  if(ncol(dati_full)!=13){ 
    sink("log.txt",append=TRUE)
    cat(sprintf("File %s numero di colonne %s\n", nomeFile,ncol(dati_full)))
    sink()
    dati_full %<>% select(-contains("X"))
  }  
  

  #stopifnot(ncol(dati_full)==13)  
  stopifnot(grep("MIN",dati_full[[1]])==33)  
  stopifnot(grep("MAX",dati_full[[1]])==34)
  stopifnot(grep("MED",dati_full[[1]])==35)  
  
  dati_full %>% slice(2:32)->dati
  print(nomeFile)
  #media
  apply(dati %>% select(-dd),2,FUN=function(x){ 
    
    if(all(is.na(x))) return(NA)
    as.integer(floor(mean(x,na.rm=TRUE)))
    
  })->meanCalcolata

  apply(dati %>% select(-dd),2,FUN=function(x){ 
    
    if(all(is.na(x))) return(NA)
    as.integer(floor(min(x,na.rm=TRUE)))
    
  })->minCalcolata
  
  apply(dati %>% select(-dd),2,FUN=function(x){ 
    
    if(all(is.na(x))) return(NA)
    as.integer(floor(max(x,na.rm=TRUE)))
    
  })->maxCalcolata
  
  
  as.integer(floor(dati_full[33,2:13]))->minLetta
  as.integer(floor(dati_full[34,2:13]))->maxLetta
  as.integer(floor(dati_full[35,2:13]))->meanLetta
  
  if(!all(is.na(minCalcolata))) if(! all(minCalcolata==minLetta,na.rm=TRUE)) stop("Errore temperatura minima")
  if(!all(is.na(maxCalcolata))) if(! all(maxCalcolata==maxLetta,na.rm=TRUE)) stop("Errore temperatura massima")
  if(!all(is.na(meanCalcolata))) if(! all((meanCalcolata-meanLetta >= -1) & (meanCalcolata-meanLetta <= 1),na.rm=TRUE)) browser() #stop("Errore temperatura media")  
  
  stopifnot(all(as.integer(dati_full[1,2:13])==ANNO ))
  
  #dati è una tabella in cui la prima colonna sono i giorni, poi segue la colonna di gennaio, poi febbraio etc etc
  names(dati)<-c(as.character(c("dd",mesi)))

  dati %<>%
    gather(key ="mm",value="val",-dd) %>%
      mutate(yy=ANNO,dd=stringr::str_pad(dd,pad="0",side="left",width=2)) %>%
        select(yy,mm,dd,everything())
  
  names(dati)[4]<-nomeStazione
  
  dati
  
}) %>% compact %>% reduce(left_join,by=c("yy"="yy","mm"="mm","dd"="dd"),.init=calendario)->listaOut

write_delim(listaOut,paste0(parametro,ANNO,"_",ANNO,".csv"),delim=",",col_names=TRUE)

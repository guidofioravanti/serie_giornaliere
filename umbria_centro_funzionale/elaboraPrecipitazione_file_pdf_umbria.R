rm(list=objects())
library("stringr")
library("readr")
library("dplyr")
library("purrr")
library("tidyr")
library("magrittr")

ANNO<-"2017"

seq.Date(from=as.Date(paste0(ANNO,"-01-01")),to=as.Date(paste0(ANNO,"-12-31")),by="day")->calendario
data.frame(calendario) %>% separate(col=calendario,into=c("yy","mm","dd"),sep="-")->calendario

try(file.remove("log.txt"))

#mesi
stringr::str_pad(1:12,pad="0",side="left",width=2)->mesi

purrr::imap(list.files(pattern="^clean_temp.+txt$"),.f=function(nomeFile,.y){

  stringr::str_trim(readLines(nomeFile,n=1),side="both")->nomeStazione
  print(sprintf("Elaboro stazione %s",nomeStazione))

  #if(nomiStazioni[.y]!="Castelluccio di Norcia") return()
  read_table(nomeFile,col_names = TRUE,skip=1,na=c("","*"))->dati_full
  names(dati_full)[1]<-"dd"
  
  if(ncol(dati_full)!=13){
    sink("log.txt",append=TRUE)
    cat(sprintf("File %s numero di colonne %s\n", nomeFile,ncol(dati_full)))
    sink()
    dati_full %<>% select(-contains("X"))
  }  
  #stopifnot(ncol(dati_full)==13)  
  stopifnot(grep("MAX",dati_full[[1]])==33)
  stopifnot(grep("TOT",dati_full[[1]])==34)  
  
  dati_full %>% slice(2:32)->dati
  print(nomeFile)
  apply(dati %>% select(-dd),2,FUN=function(x){ 
    
    if(all(is.na(x))) return(NA)
    as.integer(round(sum(x,na.rm=TRUE),0))
    
  })->totaleCalcolata

  as.integer(round(dati_full[34,2:13],0))->totaleLetta

  if(!all(is.na(totaleCalcolata))) if(! all(totaleCalcolata==totaleLetta,na.rm=TRUE)) stop("Errore precipitazione totale")
  stopifnot(all(as.integer(dati_full[1,2:13])==ANNO ))
  
  #dati è una tabella in cui la prima colonna sono i giorni, poi segue la colonna di gennaio, poi febbraio etc etc
  names(dati)<-c(as.character(c("dd",mesi)))

  dati %<>%
    gather(key ="mm",value="val",-dd) %>%
      mutate(yy=ANNO,dd=stringr::str_pad(dd,pad="0",side="left",width=2)) %>%
        select(yy,mm,dd,everything())
  
  names(dati)[4]<-nomeStazione
  
  dati
  
}) %>% reduce(left_join,by=c("yy"="yy","mm"="mm","dd"="dd"),.init=calendario)->listaOut

write_delim(listaOut,paste0("Precipitation_",ANNO,"_",ANNO,".csv"),delim=",",col_names=TRUE)

library("dplyr")
library("magrittr")
library("readr")
library("tidyr")
library("lubridate")
library("stringr")
library("purrr")
options(error=recover,warn = 2)

PARAMETRO<-c("Tmax","Tmin","Precipitation")[c(3)] #<------------- selezionare parametro, il programma va fatto girare per un parametro alla volta
ANNOI<-2000 #<------------- selezionare anno inizio e fine dei dati #sono stati scaricati file dal 2000 per verificare la sovrapposizione delle serie
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
  if(!dir.exists("prcp")) stop("Non trovo la directory con i dati di pioggia")
  list.files(pattern="^.+\\.csv",path="./prcp")->ffile
  directory<-"prcp"
  
  stringaRegex<-"^.+ Cumulata -" 
  
  
}else if(PARAMETRO=="Tmax"){
  if(!dir.exists("tmax")) stop("Non trovo la directory con i dati di temperatura")  
  list.files(pattern="^.+\\.csv",path="./tmax")->ffile  
  directory<-"tmax"   
  
  stringaRegex<-"^.+ - Temperatura Aria -"
  
}else if(PARAMETRO=="Tmax" || PARAMETRO=="Tmin"){
  if(!dir.exists("tmin")) stop("Non trovo la directory con i dati di temperatura")  
  list.files(pattern="^.+\\.csv",path="./tmin")->ffile  
  directory<-"tmin" 
  
  stringaRegex<-"^.+ - Temperatura Aria -"
  
}else{
  stop("PARAMETRO NON RICONOSCIUTO")
}

stopifnot(length(ffile)!=0)
paste(directory,ffile,sep="/")->ffile

#estrai nomi stazioni
unique(str_replace(str_replace(str_extract(ffile,stringaRegex)," *- *.+",""),"^.+/",""))->nomiStazioni


purrr::map(nomiStazioni,.f=function(stazione){
  
  ffile[grepl(stazione,ffile)]->daLeggere
  
  purrr::map(daLeggere,.f=function(nomeFile){

    tryCatch({
      read_delim(nomeFile,delim=";",col_names = TRUE,skip = 1,locale = locale(decimal_mark = ","))->dati
      names(dati)[c(1)]<-"yymmdd"
      
      str_extract(dati$yymmdd,"^[^ ]+")->dati$yymmdd
      
      dati %>% separate(yymmdd,c("dd","mm","yy"),sep="/")
      
    },error=function(e){
      NULL
    })
    
  }) %>% compact %>% reduce(rbind)->mydf  #fine map interno  
  
  if(length(mydf)==0) return(NULL)
  
  left_join(calendario,mydf,by=c("yy"="yy","mm"="mm","dd"="dd"))->ris
  
  
  if(stazione=="Matera") codiceID<-"42"
  if(stazione=="Potenza") codiceID<-"48"  
  if(stazione=="Albano di Lucania") codiceID<-"104"
  if(stazione=="Grassano SP") codiceID<-"15" 

  names(ris)[4]<-codiceID
  
  ris %>% select(yy,mm,dd,matches("^[0-9]+$"))->ris

  #verifichiamo presenza di date duplicate e nel caso le eliminiamo
  ris %>% mutate(mm=str_pad(mm,pad="0",side="left",width=2),dd=str_pad(dd,pad="0",side="left",width=2)) %>% unite("time",yy,mm,dd,sep="-") %>% select(time)->tempoUnione

  ris[!duplicated(tempoUnione[[1]]),]
  
  
}) %>% compact %>% reduce(left_join) %>% filter(yy>=2014)->finale

write_delim(finale,path=paste0(PARAMETRO,"_",ANNOI,"_",ANNOF,".csv"),delim=",",col_names=TRUE)

if(file.exists(nome.file.storici)){
  
read.csv(nome.file.storici,sep=",",header = TRUE,stringsAsFactors = FALSE,check.names = FALSE) %>%  
    mutate(yy=as.character(yy),mm=as.character(mm),dd=as.character(dd)) %>% filter(yy<=2013) ->storici

  #conversione delle colonne in numeric
  ncol(storici)->numeroColonne
  purrr::map(4:numeroColonne,.f=~(as.numeric(storici[[.]]))) %>% reduce(cbind) %>% as.data.frame()->tmp
  names(tmp)<-names(storici)[4:numeroColonne]
  data.frame(storici[,c(1,2,3)],tmp,check.names = FALSE)->storici
  
  bind_rows(storici,finale)->unione
  
  #controlliamo che non vi siano date ripetute e che le lunghezze del calendario in unione
  #sia uguale alla lunghezza del calendario generato da seq.Date
  range(unione$yy)->anniIF
  seq.Date(from=as.Date(paste0(anniIF[1],"-01-01")),to=as.Date(paste0(anniIF[2],"-12-31")),by="day")->sequenzaGiorni
  
  stopifnot(length(sequenzaGiorni)==nrow(unione))
  unione %>% mutate(mm=str_pad(mm,pad="0",side="left",width=2),dd=str_pad(dd,pad="0",side="left",width=2)) %>% unite("time",yy,mm,dd,sep="-") %>% select(time)->tempoUnione
  stopifnot(all(as.character(sequenzaGiorni) %in% tempoUnione[[1]]))
  
  write_delim(unione,path=paste0(PARAMETRO,"_storici_fino_a_",ANNOF,".csv"),delim=",",col_names=TRUE)
  
  
}#fine if

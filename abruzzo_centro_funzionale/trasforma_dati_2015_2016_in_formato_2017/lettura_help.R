individuaMetaDati<-function(nomeFile){

  str_extract(nomeFile,"\\(.+\\)")->codice
  str_sub(codice,2,nchar(codice)-1)->codice #codice stazione
  
  #anno
  str_extract(nomeFile,"201[56]")->anno
  
  #nome serie
  str_locate_all(nomeFile,"\\)")->parentesiPos

  if(length(parentesiPos)[[1]]!=1) stop(sprintf("Verificare parentesi in file: %s",nomeFile))
  #il nome della stazione si trova tra pos1 (ovvero dove si chiude la parentesi tonda del codice)
  #e pos2 (ovvero dove inizia l'anno dei dati)
  parentesiPos[[1]][1,1]->pos1
  str_locate(nomeFile,anno)[1,1]->pos2

  #nome stazione
  str_sub(nomeFile,pos1+1,pos2-1) %>% str_replace("^-","") %>% str_replace("-$","")->nomeStazione
  
  list(nome=nomeStazione,anno=anno,codice=codice)
  
}#individuaMetaDati


leggiFileComeStringa<-purrr::partial(...f=readr:::read_file,locale=locale(encoding="UTF-8"))

leggiPrecipitazione<-function(nomeFile){
  
  print(nomeFile)

  individuaMetaDati(nomeFile)->metaDati
  leggiFileComeStringa(nomeFile)->datiStringa
  str_count(datiStringa,metaDati$nome)->conteggio
  if(conteggio!=2) stop(sprintf("Precipitazione %s, conteggio nome stazione in datiStringa errato",nomeFile))

  
  ###################################
  str_locate_all(datiStringa,metaDati$nome)->posizioneDati
  
  purrr::map(1:2,.f=function(riga){
    
    if(riga==2){
      secondaPos<-10000 #posizione fittizia
    }else{
      posizioneDati[[1]][riga+1,1]->secondaPos
    }
    
    posSub<-c(posizioneDati[[1]][riga,1],secondaPos)    
    str_sub(datiStringa,posSub[1]-1,posSub[2])
  })->listaStringhe

  purrr::map_lgl(listaStringhe,~(grepl("dalle 09:00 alle 09:00",.)))->quale
  if(all(!quale)) stop(sprintf("Cumulati dalle 9 alle 9 non trovati, file %s",metaDati$nome))
  listaStringhe[quale]->pluvio99
  
  purrr::map_lgl(listaStringhe,~(grepl("dalle 00:00 alle 24:00",.)))->quale
  if(all(!quale)) stop(sprintf("Cumulati dalle 0 alle 24 non trovati, file %s",metaDati$nome))
  listaStringhe[quale]->pluvio24 
  
  purrr::map(list(pluvio24,pluvio99),.f=function(stringaDati){
    
    ifelse(grepl("dalle 09:00 alle 09:00",stringaDati),"pluvio99",ifelse(grepl("dalle 00:00 alle 24:00",stringaDati),"pluvio24","problema"))->nomeParametro
    stopifnot(nomeParametro!="problema")

    tryCatch({
      read.csv(textConnection(stringaDati[[1]]),head=,skip=2,sep=";",stringsAsFactors = FALSE)
    },error=function(e){
      NULL
    })->mydf
    
    
    if(is.null(mydf) || !nrow(mydf)) return(NULL)
    
    names(mydf)->nomi
    stopifnot(grep("Gen",nomi)==2)
    stopifnot(grep("Dic",nomi)==13)   
    names(mydf)[1]<-"dd"
    names(mydf)[2:13]<-str_pad(1:12,pad = "0",width = 2,side="left")

    mydf %>% 
      select(1:13) %>%
      filter(dd %in% 1:31) %>%
      mutate(dd=str_pad(dd,pad="0",side="left",width=2)) %>%
      mutate(yy=metaDati$anno) %>%
      gather(key=mm,value=DataValue,-dd,-yy) %>%
      mutate(DataValue=as.numeric(str_replace_all(DataValue,",","."))) %>%
      select(yy,mm,dd,DataValue)->newDF
    names(newDF)[4]<-nomeParametro
    
    newDF
    
  }) %>% compact->piogge
  
  if(!length(piogge)){print(sprintf("File vuoto? %s",nomeFile)); browser()}
  
  if(length(piogge)==2){
    reduce(piogge,left_join)->piogge
  }else{
    piogge[[1]]->piogge
    #quale delle due piogge è mancanti nel file?
    ifelse(grepl("99",names(piogge)[4]),"pluvio24","pluvio99")->mancante
    piogge$x<-NA
    names(piogge)[5]<-mancante
  }
  
  as.character(seq.Date(from=as.Date(paste(metaDati$anno,"01","01",sep="-")),to=as.Date(paste(metaDati$anno,"12","31",sep="-")),by="day"))->calendario
  piogge %>% transmute(time=paste(yy,mm,dd,sep="-"))->calendarioDF
  
  #eliminiamo le date fittizie che si trovano in calendarioDF
  which(!(calendarioDF$time %in% calendario))->indice
  stopifnot(length(calendarioDF$time[-indice]) %in% c(365,366))

  paste0(metaDati$anno," ",metaDati$nome," P.csv")->nomeFinale

  sink(nomeFinale,append=FALSE)
  cat(sprintf("Dati sensore dal 01/01/%s al 31/12/%s\n",metaDati$anno,metaDati$anno))
  cat(sprintf("Stazione %s (LAT = x, LON = x, ALT = 3)\n",metaDati$nome))
  cat("Sensore Pluviometro (DTM = 900, DTR = 900, DTC = 900)\n")
  cat("Data;mm;\n")
  sink()
  
  piogge %>%unite(col="Data",yy,mm,dd,sep="/") %>% select(Data,pluvio99)->daScrivere
  
  write_delim(daScrivere,path=nomeFinale,delim=";",col_names=FALSE,append=TRUE)
  
}#fine leggiPrecipitazione
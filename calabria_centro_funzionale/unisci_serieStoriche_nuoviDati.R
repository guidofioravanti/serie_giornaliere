#19 febbraio 2018
#Unisci serie storiche con serie nuove scaricate da Centro Funzionale Calabria
rm(list=objects())
library("tidyverse")
library("stringr")
library("readr")
library("purrr")
options(error=recover,warn = 2)

ANNO<-as.integer(2017)

#Se il file delle nuove serie contiene i codici HisCentral OD (in anagrafica, SiteCode)
#vanno riconvertiti in SiteID
CONVERTI_NOMI<-TRUE

tryCatch({
  read_delim("reg.calabria.info.csv",delim=";",col_names=TRUE)
},error=function(e){
  stop("Anagrafica regione Calabria, file non trovato")
})->ana

c("Precipitation","Tmax","Tmin")->parametri

leggiDati<-function(nomeFile,DELIM=","){
  
  if(missing(nomeFile)) stop("Manca il nome del file da leggere")
  stopifnot(DELIM %in% c(",",";"))
  
  #leggiamo solo intestazione per sapere quante colonne ho e quindi scrivere il formato di lettura
  tryCatch({
    
    read_delim(nomeFile,delim=DELIM,col_names=TRUE,n_max=1)
    
  },error=function(e){
    
    stop(sprintf("errore lettura file %s",nomeFile))            
      

  })->dati
  
  stopifnot(all(names(dati)[1:3] %in% c("yy","mm","dd")))
  
  #numero ci colonne
  (ncol(dati)-3) ->numCol

  #lettura dati storici utilizzando il formato double 
  read_delim(nomeFile,delim=DELIM,col_names=TRUE,col_types=paste("iii",paste(rep("d",numCol),collapse=""),sep =""))
  

}#fine leggiDati

  
purrr::walk(parametri,.f=function(parametro){
  
    paste0(parametro,"_fino",ANNO-1,".csv")->nomeFile
  
    leggiDati(nomeFile,DELIM=",")->dati
    
    paste0(parametro,"_",ANNO,".csv")->nomeFileDatiNuovi

    #leggiamo solo intestazione per sapere quante colonne ho e quindi scrivere il formato di lettura
    leggiDati(nomeFileDatiNuovi,DELIM=";")->datiNuovi 
    

    if(CONVERTI_NOMI){
      
      match(names(datiNuovi)[4:ncol(datiNuovi)],ana$SiteCode)->posizioni
      stopifnot(all(!is.na(posizioni)))
      names(datiNuovi)<-c("yy","mm","dd",ana[posizioni,]$SiteID)
      
    }#fine CONVERTI_NOMI
    
    
    #unione e scrittura  
    bind_rows(dati,datiNuovi)->finale
    
    print(sprintf("PARAMETRO %s",parametro))
    range(finale$yy)->anni
    print(sprintf("Range anni da %s a %s",anni[1],anni[2]))    
    
    write_delim(finale,path=paste0(parametro,"_storici_fino",ANNO,".csv"),delim=",",col_names=TRUE)
    
    #quale serie manca nel nuovo anno ANNO?
    setdiff(names(dati),names(datiNuovi))->missingIn
    
    #viceversa: quali serie sono presenti nel ANNO e non ho nel file fino ad ANNO-1?
    setdiff(names(datiNuovi),names(dati))->missingInUp
    
    sink(paste0("log_bindSerie",ANNO,"ConSerieFino",ANNO-1,".txt"),append=TRUE)
    print("*************")
    print(parametro)
    print(sprintf("Serie Centro Funzionale Calabria nel %s ma che non hanno corrispondenza nel %s:",ANNO,ANNO-1))
    print(missingInUp)
    print(sprintf("Serie fino a %s senza dati nel %s:",ANNO-1,ANNO))
    print(missingIn)
    print("*************")
    sink()
    
})#fine lapply
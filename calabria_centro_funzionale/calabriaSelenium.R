#revisione del 19 febbraio 2018
rm(list=objects())
library("RSelenium")
library("dplyr")
library("readr")
library("stringr")
library("magrittr")
library("rvest")
library("tidyr")
library("purrr")
options(warn=2,error=recover)

PARAMETRO<-c("Tmax","Tmin","Precipitation")[c(2)] #<---fissare parametro
ANNO<-2017 #<----fissare anno



urlLoginCalabria<-"http://www.cfd.calabria.it/Login/login.php"

###########################################################################
# Inizio programma --------------------------------------------------------
###########################################################################

giornoI<-as.Date(paste0(ANNO,"-01-01"))
giornoF<-as.Date(paste0(ANNO,"-12-31"))
yymmdd<-as.character(seq.Date(from=giornoI,to=giornoF,by="day"))
data.frame(yymmdd=yymmdd) %>% 
  separate(yymmdd,c("yy","mm","dd"),sep="-") %>%
  mutate(yy=as.character(yy),mm=as.character(mm),dd=as.character(dd))->calendario

#ok abbiamo il codice stazione,  parametroStringa serve per la richiesta al server della Calabria
ifelse(PARAMETRO=="Precipitation","PG",PARAMETRO)->ParametroStringa

#Lettura anagrafica regione Valle d'Aosta
tryCatch({
  read.csv("reg.calabria.info.csv",sep=";",head=TRUE,stringsAsFactors=FALSE,check.names=FALSE)
},error=function(e){
  stop("File anagrafica Calabria non trovato")
})->ana


#funzione per convertire
converti<-function(x){
  
  #ci sono dati con asterisco? Precipitazione nevosa
  grep("\\*",x)->indexAst
  if(length(indexAst)) x[indexAst]<-"0.0" #trasformiamo in 0
  #stringhe del tipo "-" o "- " sono 0.0 mm
  str_replace(x,"^-[^0-9]?$","0.0")->x
  str_replace(x,"[^0-9]+$","")->x
  
  return(as.double(x))
  
}#fine funzione converti

rsDriver(port =4444L)->rD
remDr <- rD[["client"]]
remDr$open()
remDr$navigate(urlLoginCalabria)
remDr$findElement("name", "utente")$sendKeysToElement(list("guido.fioravanti@isprambiente.it"))
remDr$findElement("name", "password")$sendKeysToElement(list("gu1do$Fior3v"))

#seleziona il sensore
remDr$findElement(using="xpath",value='//input[@type="submit"]')$clickElement()
remDr$findElement(using="tag",value="a")$clickElement()

#piogge
if(PARAMETRO=="Precipitation"){
  remDr$findElement(using="link text",value="Piogge")$clickElement()
  #clicca su piogge giornaliere
  remDr$findElement(using="xpath",value='//input[@alt="Piogge giornaliere"]')$clickElement()
}else{
  remDr$findElement(using="link text",value="Temperature")$clickElement()
  #clicca su massime o minime
  
  ifelse(PARAMETRO=="Tmax",'//input[@alt="Temperature massime giornaliere"]','//input[@alt="Temperature minime giornaliere"]')->xpathTemp
  remDr$findElement(using="xpath",value=xpathTemp)$clickElement()     
  
}#fine if sul titpo di parametro


#passa l'anno di interesse
remDr$findElement(using="xpath",'//input[@type="radio" and @value="anno_testo"]')$clickElement()
remDr$findElement(using="xpath",'//input[@name="anno_testo"]')$sendKeysToElement(list(as.character(ANNO)))

#estrai elenco stazioni
remDr$findElement(using="xpath",'//input[@type="submit" and @value="Estrai elenco stazioni"]')$clickElement()

#aspettiamo tre secondi
Sys.sleep(3) #necessario per essere sicuro che si carichi la tabella
remDr$getPageSource()[[1]]->sorgente

#parsing dell'html: tabella contiene la lista delle stazioni con il codice associato
read_html(sorgente) %>% 
  html_node(xpath='//table[@border="1" and @width="600"]') %>% 
    html_table(header=FALSE,dec=",",fill=TRUE,trim=TRUE)->tabella

#Qui inizia il ciclo suule stazioni
unlist(tabella)->tabellaUnlisted

sink(sprintf("_log_calabria_selenium_%s.txt",PARAMETRO),append=FALSE)

purrr::map(tabellaUnlisted,.f=function(stringa){
  
  if(!nchar(stringa) || !grepl("-",stringa) || is.na(stringa)) return()

  #estrai codice della stazione
  unlist(str_split(stringa,"-"))[1]->codiceC
  
  #togliamo uno o più spazi
  str_replace(codiceC," +","")->codiceStaz
  
  #verifichiamo che si tratti di un codice numerico: se fallisce la conversione in integer
  #allora return  
  
  tryCatch({
    as.integer(codiceStaz)
  },error=function(e){
    NA
  })->codInt
  
  if(is.na(codInt)){
    message(sprintf("Qualcosa è fallito nella conversione a intero del codice stazione: %s\n",stringa))
    return()
  }#fine if
  
  #ora cerchiamo la stringa all'interno dell'anagrafica
  str_trim(str_replace(str_split(stringa,"-")[[1]][[2]],"\\(.+\\)",""))->nomeStazione
  which(ana$SiteCode==codInt)->rigaInAnagrafica
  if(length(rigaInAnagrafica)==0){
    print(sprintf("Stazion %s con codice %s non in anagrafica",nomeStazione,codInt))
    return()
  }

  print(sprintf("Associo la stazione %s con la stazione %s in anagrafica\n",nomeStazione,ana[rigaInAnagrafica,]$SiteName))  

  stringaRichiesta<-paste0("http://www.cfd.calabria.it/DatiVari/midmar/banca_dati.php?p=visualizza_dati_giornalieri&codice=",
         codiceStaz,"&anno=",ANNO,"&par=",ParametroStringa)
  
  remDr$navigate(stringaRichiesta)
  
  #acquisisce la tabella con i dati
  Sys.sleep(3) #necessario per essere sicuro che si carichi la tabella
  remDr$getPageSource()[[1]]->sorgente
  
  #parsing dell'html
  read_html(sorgente) %>% html_node(xpath='//table[@width="600"]') %>% html_table(header=TRUE,dec=".",fill=TRUE,trim=TRUE)->tabellaValori

  #verifichiamo di aver acquisito i dati giusti
  names(tabellaValori)[1]->colonnaGiorni
  grep("giorno",tolower(colonnaGiorni))->indexGrep
  if(length(indexGrep)!=1){
    message(paste0("Qualcosa è fallito, colonna GIORNO non trovata: ",stringa))
    remDr$goBack()
    return()    
  } #fine if
  
  #Troviamo la riga GRAFICI, indipendentemente dal parametro
  grep("grafici",tolower(tabellaValori[,c(1)]))->indexGrafici  
  
  if(length(indexGrafici)!=1){
    message(paste0("Qualcosa è fallito, riga GRAFICI non trovata nella tabella valori: ",stringa))
    remDr$goBack()
    return()           
  }#if su indexGrafici
  
  #ok indexGrafici contiene una riga che corrisponde alla riga "GRAFICI", aspettiamo a toglierla
  
  #cerchiamo righe specifiche per ciascun parametro: la riga TOT per la precipitazione   
  if(PARAMETRO=="Precipitation"){
    #cerchiamo le righe con le voci "TOT" e "GRAFICI" se si tratta di dati di pioggia
    grep("tot",tolower(tabellaValori[,c(1)]))->indexTot

    if(length(indexTot)!=1){
      message(paste0("Qualcosa è fallito, riga TOT non trovata nella tabella della precipitazione: ",stringa))
      remDr$goBack()
      return()        
    }#fine if

    #sembra che la colonna GIORNO vada bene
    tbl_df(tabellaValori[-c(indexTot,indexGrafici),])->tabellaValori        
    
  }else{
  
    if(PARAMETRO=="Tmax"){

      #cerchiamo le righe con la voce MAX
      grep("max",tolower(tabellaValori[,c(1)]))->indexMinMax

    }else{
      
      #cerchiamo le righe con le MIN
      grep("min",tolower(tabellaValori[,c(1)]))->indexMinMax      
      
    }
    
    if(length(indexMinMax)!=1){
      print(sprintf("Qualcosa è fallito, riga MIN/MAX non trovata: %s\n",stringa))
      remDr$goBack()
      return(NULL)        
    }#fine if    
    
    #sembra che la colonna GIORNO vada bene
    tbl_df(tabellaValori[-c(indexMinMax,indexGrafici),])->tabellaValori   
    
  }#fine su grep
  
    
  #verifichiamo i nomi delle colonne restanti: devono essere i mesi
  
  if(ncol(tabellaValori)!=13){
    print(sprintf("Qualcosa è fallito, numero colonne errato: %s\n",stringa))
    remDr$goBack()
    return(NULL)    
  }#controllo su numero colonne fallito
  
  which(names(tabellaValori)[2:13] %in% c("Gen","Feb","Mar","Apr","Mag","Giu","Lug","Ago","Set","Ott","Nov","Dic"))->indexMesi
  if(length(indexMesi)!=12){
    print(sprintf("Qualcosa è fallito, colonne mesi in: %s\n",stringa))
    remDr$goBack()
    return(NULL)    
  }#controllo su numero colonne fallito  
  

  names(tabellaValori)<-c("dd",str_pad(seq(1,12,by=1),2,pad="0",side="left"))
  tabellaValori$dd<-str_pad(str_replace(tabellaValori$dd,"[^0-9]",""),2,pad="0",side="left")  
  
  #ok proviamo a convertire i dati
  tryCatch({
    tabellaValori %>% mutate(yy=ANNO) %>% select(yy,dd,everything()) %>% gather(.,mm,tempVar,-yy,-dd) %>% mutate(DataValue=converti(tempVar)) %>% select(-tempVar)
  },error=function(e){
    NULL
  })->mDati
  
  if(is.null(mDati)) return(NULL)

  #teniamo la colonna time
  mDati %<>% mutate(mm=str_pad(mm,2,pad="0",side="left"),dd=str_pad(dd,2,pad="0",side="left")) %>% unite(.,col=time,yy,mm,dd,sep="-",remove=FALSE)   

  #eliminiamo giorni fasulli: 31 o 29 e 30 (per febbraio)
  as.Date(mDati$time)->myDate
  which(is.na(myDate))->indexFasulli
  
  #ora time possiamo toglierlo, c'è servito solo per eliminare le date fasulle
  mDati %<>% slice(-indexFasulli) %>% select(yy,mm,dd,DataValue)
  names(mDati)[4]<-codInt

  remDr$goBack()
  
  mDati %>% mutate(yy=as.character(yy),mm=as.character(mm),dd=as.character(dd))
  
}) %>% compact ->listaOut #fine purrr::map

if(!length(listaOut)){
  
  print(sprintf("Nessun dato trovato per il parametro %s!!!\n",PARAMETRO))
  
}else{
  
  listaOut %>% 
    reduce(left_join,by=c("yy"="yy","mm"="mm","dd"="dd"),.init=calendario)->finale   
  
  finale %>% write_delim(path=paste0(PARAMETRO,"_",ANNO,".csv"),delim=";",col_names=TRUE)
  
  print(sprintf("Numero di stazioni per parametro %s: %s\n",PARAMETRO,length(tabellaUnlisted)))
  print(sprintf("Numero di stazioni per nel data frame finale: %s\n",ncol(finale)))  
  
}#fine if

sink()


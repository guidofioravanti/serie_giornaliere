#rivisto il 9 gennaio 2020
#Il programma è stato aggiustato in modo di funzionare con il nuovo sito del Centro funzionale Lazio.
#Quello che sembra essere cambiato e' il main frame, non più identificato da id "main". Per accedere
#all'iframe in cui c'è la possibilita di scaricare i dati utilizzare siwtchtoframe(1) [i frame sono numerati partendo da 0]
#utilizzare rsDriver per scaricare (se necessario) il driver e lanciarlo
rm(list=objects())
library("rvest")
library("RSelenium")
library("dplyr")
library("tidyr")
library("readr")
library("magrittr")
library("stringr")
library("purrr")
options(warn=-2)


# Funzioni ----------------------------------------------------------------
converti<-function(x){
  
  #vingole in punti decimali
  str_replace(x,",",".")->x
  
  grep("---",x)->indexNA
  if(length(indexNA)) {x[indexNA]<-NA}  
  
  grep("^.?[^0-9]+$",x)->index
  if(length(index)) {browser(); x[index]<-NA}
  
  as.double(x)
  
}#fine converti


# INIZIO PROGRAMMA --------------------------------------------------------
annoI<-2019
annoF<-2019

rsDriver(port =4567L,version = "3.141.59",verbose=TRUE, browser = c("firefox"),check=F)->rD
rmDr <- rD[["client"]]
rmDr$open()

urlAnnali<-"http://www.idrografico.regione.lazio.it/annali/index.htm"
vPARAMETRO<-c("Precipitation","Tmax","Tmin")[1]

read.csv(file="lista.txt",head=TRUE,stringsAsFactors=FALSE,sep=";")->ana

purrr::walk(vPARAMETRO,.f=function(PARAMETRO){
  
  ifelse(grepl("precip",tolower(PARAMETRO)),"pluviometri","termometri")->nomeSensori

#  remoteDriver(browserName="firefox")->rmDr
#  rmDr$open()


  purrr::map(1:nrow(ana),.f=function(iii){
  
    ana$SiteName[iii]->nome.stazione
    ana$SiteCode[iii]->siteCode
    ana$SiteID[iii]->siteID
    
    purrr::map(annoI:annoF,.f=function(anno){
    
      print(nome.stazione)
      print(anno)
      
      rmDr$navigate(urlAnnali)
  
      stringaStazione<-paste0('//frame[@id="main"]')
      #rmDr$findElement(using="xpath",stringaStazione)$switchToFrame("main") <--- non esiste più nel nuovo sito il frame main
      #I frame sono numerati a partire da 0, si veda ad esempio la pagina: https://www.guru99.com/handling-iframes-selenium.html
      rmDr$switchToFrame(1)

      rmDr$findElement(using="xpath",paste0("//input[@value='",nomeSensori,"']"))$clickElement()  
      Sys.sleep(6)
      rmDr$findElement(using="name","anno")$sendKeysToElement(list(as.character(anno),key="enter"))    
      Sys.sleep(3)
      rmDr$findElement(using="name","stazione")$sendKeysToElement(list(toupper(nome.stazione),key="enter")) 
      Sys.sleep(3)
      rmDr$findElement(using="name","anno")$sendKeysToElement(list(as.character(anno),key="enter"))   #ridare anno per una seconda volta, con un solo click seleziona anno sbagliato 

      rmDr$findElement(using="name","Submit")$clickElement()  
      Sys.sleep(3) #necessario per essere sicuro che si carichi la tabella
      rmDr$getPageSource()[[1]]->sorgente
   
      #tabella dentro tabella...uff
      tryCatch({
        read_html(sorgente[[1]]) %>% html_node(xpath="//table[@style='border: solid 2px #000000; padding: 3px;']/tbody/tr/td/table") %>% html_table(header=FALSE,dec=",",fill=TRUE,trim=TRUE)
      },error=function(e){
        NULL
      })->tabella

      if(nrow(tabella)<10 || is.null(tabella)) return(NULL)  
    
      #quante colonnee? dipende dal parametro
      if(nomeSensori=="pluviometri"){
        nColonneAtteso<-14
        rigaNomeStazione<-2
        stringaDaCercare<-"pluviometro"      
      }else{
        nColonneAtteso<-25
        rigaNomeStazione<-3     
        stringaDaCercare<-"termometro"
      }    
      
      if(ncol(tabella)!=nColonneAtteso) {browser(); stop(paste0("Numero colonne atteso, errore:",nome.stazione))}

      #la seconda o terza riga deve contenere il nome stazione
      grep(tolower(nome.stazione),tolower(tabella[rigaNomeStazione,c(1)]))->posNome
      #2017 if(length(posNome)!=1) stop(paste0("Riga nome stazione, errore:",nome.stazione))
      if(length(posNome)!=1) return(NULL)
      
      #la terza o quarta riga deve contenere termometro o pluviometro
      grep(tolower(stringaDaCercare),tolower(tabella[rigaNomeStazione+1,c(1)]))->posNome
      if(length(posNome)!=1) stop(paste0("Riga nome stazione, errore:",nome.stazione))
    
      #verifichiamo la prima riga: deve contenere i mesi. La seconda colonna contiene Gennaio?
      grep("giorno",tolower(tabella[1,]))->posGiorno
      if(posGiorno!=1) stop(paste0("Colonna giorno, errore:",nome.stazione))
      grep("gennaio",tolower(tabella[1,]))->posGennaio
  
      if(length(posGennaio)==2){
        if(posGennaio[1]!=2) stop(paste0("Colonna gennaio, errore:",nome.stazione))
        if(posGennaio[2]!=3) stop(paste0("Colonna gennaio, errore:",nome.stazione))
      }else if(length(posGennaio)==1){
        if(posGennaio!=2) stop(paste0("Colonna gennaio, errore:",nome.stazione))      
      }
        
      #se arrivo qui estraggo i dati
      rigaNomeStazione+2->primoDelMese
      ultimoDelMese<-primoDelMese+30
      if(nomeSensori=="pluviometri"){
        tabella[primoDelMese:ultimoDelMese,1:13]->subTabella
      }else{
        tabella[primoDelMese:ultimoDelMese,]->subTabella
      }
      
      names(subTabella)[c(1)]<-"dd"
  
      subTabella %>% select(-dd) %>% mutate_all(funs(converti) )->dati
    
      if(nomeSensori!="pluviometri"){
        
        if(length(grep("tmax",tolower(PARAMETRO)))==1){
          
          dati[,seq(2,24,by=2)]->dati  
          
        }else if(length(grep("tmin",tolower(PARAMETRO)))==1){
          
          dati[,seq(1,23,by=2)]->dati        
          
        }else{
          
          stop("PARAMETRO NON RICONOSCIUTO")
          
        }  
  
      } 
      
      names(dati)<-str_pad(seq(1,12),2,side="left",pad="0")    
      dati %<>% mutate(yy=anno,dd=str_pad(seq(1,31),2,side="left",pad="0"))
        
      dati
      
    }) %>% compact ->listaOut #fine lapply su anno

      #lista.out contiene i dati di un determinato parametro per una determinata stazione
      if(!length(listaOut)) return(NULL)
    
      listaOut %>%
        reduce(bind_rows) %>%
          gather(.,mm,DataValue,-yy,-dd) %>% 
            arrange(yy,mm,dd) %>% 
              unite(col=time,yy,mm,dd,sep="-",remove=FALSE)->dfDati
      
      #troviamo i giorni di febbraio da eliminari e  i 31 in eccesso
      as.Date(dfDati$time,format="%Y-%m-%d")->calendario
      which(is.na(calendario))->indexCalendario
      
      if(!length(indexCalendario)) stop("Impossibile") #ci sono sempre giorni fittizzi che diventano NA
      #tutti i dati corrispondenti a indexCalendario sono NA? Se non tutti uguali a NA fermati
      stopifnot(all(is.na(dfDati[indexCalendario,c("DataValue")])))
      
      dfDati %<>% select(yy,mm,dd,DataValue,-time) %>% slice(-indexCalendario)
      names(dfDati)[4]<-siteID
      
      dfDati 

    }) %>% compact %>% reduce(left_join,by=c("yy"="yy","mm"="mm","dd"="dd"))->listaDati #fine purrr su nomestazione

    write_delim(listaDati,path=paste0(PARAMETRO,"_",annoI,"_",annoF,".csv"),delim=",",col_names = TRUE)  
  
  
}) #fine purrr su parametro

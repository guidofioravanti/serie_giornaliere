#1 Marzo 2018: acquisizione dei dati dal Centro Funzionale Campania
#Sono fondamentali le pause, altrimenti il codice html non viene caricato interamente e lo script fallisce a trovare gli ementi nel DOM
rm(list=objects())
library("RSelenium")
library("dplyr")
library("readr")
library("purrr")
library("rvest")
library("stringr")
library("tidyr")
#options(warn=2,error=recover)

ANNOI<-2016 #<------- selezionare anno
ANNOF<-2016

SENSORE<-c("Termometro","Pluviometro")[c(1)] #<------ selezionare uno dei due


# INIZIO PROGRAMMA --------------------------------------------------------
giornoI<-as.Date(paste0(ANNOI,"-01-01"))
giornoF<-as.Date(paste0(ANNOF,"-12-31"))
yymmdd<-as.character(seq.Date(from=giornoI,to=giornoF,by="day"))

data.frame(yymmdd=yymmdd) %>% separate(yymmdd,c("yy","mm","dd"),sep="-")->calendario



if(SENSORE=="Termometro"){
  urlLogin<-"http://centrofunzionale.regione.campania.it/regionecampania/centrofunzionale/archiviosensori/termo.php"  
}else{
  urlLogin<-"http://centrofunzionale.regione.campania.it/regionecampania/centrofunzionale/archiviosensori/pluvio.php"
}

#FILE CON LA LISTA DELLE STAZIONI E CON CODICE ASSOCIATO AL SENSORE (non sono i codici HisCentral, ma dei codici usati qui dal CF)
FILECF<-"listaStazioni_CentroFunzionaleCampania.csv"

#serve per tenere il conto di quante volte "ritento" a leggere i dati di una medesima stazione.
#una stazione che non riesco a leggere non può generare una serie infinita di tentativi.
#Qui dopo tre tentativi falliti passo alla stazione successiva
indice<-0
conta<-0

repeat{

    tryCatch({
      read_delim(FILECF,delim=";",col_names=TRUE,comment = "#") %>% filter(sensore==SENSORE) 
    },error=function(e){
      stop(sprintf("File %s con elenco stazioni del Centro Funzionale Campania non trovato!",FILECF))
    })->stazioniCF
    
    #esci da repeat: ho letto tutte le stazioni e ho esaurito le righe
    if(!nrow(stazioniCF)){
      print(sprintf("Fine esecuzione programma, ho letto %s stazioni",conta)) 
      break
    }
      

    purrr::map(stazioniCF$codiceCF[c(1)],.f=function(codiceCF){
      
        print(sprintf("Stazione %s, tentativo numero %s",codiceCF,as.character(indice)))
        print(sprintf("Cerco codice stazione %s",codiceCF))

        purrr::map(seq(ANNOI,ANNOF),.f=function(anno){
          
	  try(remDr$closeServer())		

          rsDriver(port =4444L)->rD
          remDr <- rD[["client"]]
          remDr$open()
          
          remDr$navigate(urlLogin)
          
          #trova il menu con la motivazione
          remDr$setWindowSize(width=1600,1024)
          
          #selezioniamo la stazione
          tryCatch({
            remDr$findElement("css",paste0("option[value='",codiceCF,"']") )
          },error=function(e){
            NULL
          })->foundValue  
          
          if(is.null(foundValue)){
            remDr$close()
            Sys.sleep(6)
            return(NULL)
          }
            
          foundValue$clickElement()
          print(sprintf("Codice stazione %s trovato!",codiceCF))
          
          Sys.sleep(6)
 #         remDr$findElements(using="css","#select_anno > option")->elemAnni
 #          purrr::map(elemAnni,~(.$getElementText()[[1]])) %>% unlist %>% as.integer() %>% sort->vettoreAnni
          #vettoreAnni contiene tutti gli anni disponibili per la stazione selezionata, prendiamo solo gli anni tra ANNOI e ANNOF
          
 #         base::intersect(seq(ANNOI,ANNOF),vettoreAnni)->anniDaScaricare
          
#          if(!length(anniDaScaricare)){
#            print(sprintf("Nessun anno per il codice %s",codiceCF)) 
#            return(NULL)
#          }                    
          
    
          print(sprintf("Cerco anno %s per codice stazione %s",anno,codiceCF))
          Sys.sleep(6)
          tryCatch({
            remDr$findElement("css",paste0("#select_anno > option[value='",anno,"']") )
          },error=function(e){
            NULL
          })->foundYear
          
          if(is.null(foundYear)){
            print(sprintf("Anno %s per codice stazione %s NON trovato!",anno,codiceCF))
            remDr$close()
            Sys.sleep(6)
            return(NULL)
          }  
          
          foundYear$clickElement()
          print(sprintf("Anno %s per codice stazione %s trovato!",anno,codiceCF))
          
          Sys.sleep(6)  
          remDr$findElement("css","input[value='Cerca']")$clickElement()
          Sys.sleep(6)  
    
          tryCatch({
            remDr$findElement(using="id",value="table3")->tabella
          },error=function(e){
            NULL
          })->tabella
          
          if(is.null(tabella)){
            remDr$close() 
            Sys.sleep(6)
            return(NULL)
          }
            
          tabella$getElementAttribute("outerHTML")->stringaHtml
          
          #stringa nulla
          if(!nchar(stringaHtml)) return(NULL)
          
          if(SENSORE=="Pluviometro"){
            str_replace_all(str_replace_all(str_replace_all(stringaHtml,"</td><td>",";"),"</tr><tr>",""),"</td><td>","#")->stringaHtml
            str_split(str_replace(str_replace(stringaHtml,"<table.+<tbody><tr><td>",""),"</td></tr></tbody></table>",""),"#")->vettoreDati
          }else{
            
            #per la temperatura va gestito il fatto che si hanno dati di media, minima e massima
            str_replace(str_replace(stringaHtml,"<table id=\"table3\"><tbody><tr><td style=\"width: 25% !important;\">",""),"</tbody></table>","")->stringaHtml
            str_replace_all(stringaHtml,"<td style=\\\"width: 25% !important;\\\">","")->stringaHtml
            str_split(str_replace_all(stringaHtml,"</td></tr>","#"),"#")[[1]]->listaDati
            
            purrr::map(listaDati,.f=function(dd){
              
              str_replace(str_replace_all(str_replace_all(dd,"<td>",""),"</td>",";"),"<tr>","")->stringaOut
              
              if(!nchar(stringaOut)) return(NULL)
    
              stringaOut
              
            }) %>% unlist->vettoreDati
    
          }
          
          data.frame(temp=vettoreDati)->mydf
          names(mydf)<-"x"
          
          
          if(SENSORE=="Pluviometro"){
              
            #eliminare i messaggi ci serve perche abbiamo delle conversioni a NA che danno origine a un messaggio di allerta
              suppressWarnings({
                  mydf %>% 
                    separate(x,c("time","valori"),sep=";") %>% 
                      separate(time,c("dd","mm","yy"),sep="/") %>%
                        mutate(valori=as.double(valori)) %>%
                        select(yy,mm,dd,valori)->datiFinale
              })
                    #aggiungiamo prcp solo per un motivo, per rendere poi uniforme il processo di scrittura del data.frame finale,
                    #seguendo lo stesso schema applicato a tmax e tmin. Per la precipitazione non ci sarebbe bisogno del suffisso
            
                    names(datiFinale)[4]<-paste0(codiceCF,".","prcp")
            
          }else{
            
              suppressWarnings({
                mydf %>% 
                  separate(x,c("time",c("tmax","tmed","tmin")),sep=";") %>% 
                    separate(time,c("dd","mm","yy"),sep="/") %>%
                        mutate(tmax=as.double(tmax),tmin=as.double(tmin)) %>%
                          select(yy,mm,dd,tmax,tmin)->datiFinale
              })                  
                      
              #qui dobbiamo tener traccia della massima e della minima
              names(datiFinale)[c(4,5)]<-paste0(codiceCF,".", names(datiFinale)[c(4,5)])
            
          }  
    
          if(!nrow(datiFinale)) stop("Numero di righe 0, non me lo aspettavo")
            
          remDr$close()
          Sys.sleep(6)
          
          datiFinale      
    
          
        }) %>% compact->listaDatiPerUnaStazione
        
        #listaDatiPerUnaStazione è una lista i cui elementi sono i dati giornalieri dei vari anni per una stazione specifica
        if(!length(listaDatiPerUnaStazione)) return(NULL)
        
        listaDatiPerUnaStazione %>% reduce(rbind)
    
    }) %>% compact->listaOut #fine purrr::map su codiceStazione
    

    if(length(listaOut)){ 
      
        #annulliamo l'indice 
        indice<<-0
        conta<<-conta+1
      
        #prima di tutto: se ho letto i dati, riscriviamo il file FILECF eliminando da esso il codice della stazioni
        #di cui ho appena letto i dati
        write_delim(stazioniCF[-c(1),],path=FILECF,delim=";",col_names = TRUE)
      
        #ora procediamo con la scrittura dei dati  
        listaOut %>% reduce(left_join,by=c("yy"="yy","mm"="mm","dd"="dd"),.init=calendario)->finale
        
        #a questo punto, la precipitazione la posso scrivere cosi com'è mentre la
        #temperatura devo suddividierla in tmax e tmin
        
        purrr::walk(c("Prcp","Tmax","Tmin"),.f=function(param){
          
          finale %>% select(yy,mm,dd,contains(tolower(param) )) %>% mutate(yy=as.integer(yy),mm=as.integer(mm),dd=as.integer(dd))->subFinale
          
          #se sto lavorando i dati di precipitazione, quando passo tmax e tmin ottengo solo yy mm dd
          #se sto lavorando sulla temperatura quando passo prcp ottengo solo yy mm dd
          #In entrambi i casi la scrittura non va fatta
          if(all(names(subFinale) %in% c("yy","mm","dd")))  return(NULL)
            
          #togliamo l'etichetta che identifica il parametro e riassegnamo i nomi
          str_replace_all(names(subFinale),paste0(".",tolower(param)),"")->names(subFinale)
          
          #se trova il file di output lo leggee, gli aggiunge i nuovi dati (colonne) e lo sovrascrive
          if(file.exists(paste0(param,"_",ANNOI,"_",ANNOF,".csv"))) {
            
            	read_delim( paste0(param,"_",ANNOI,"_",ANNOF,".csv"),delim=",",col_names=TRUE)->temp
              temp %>% map_if(is.character,.f=~(as.double(.)))->temp
              as_data_frame(temp) %>% mutate(yy=as.integer(yy),mm=as.integer(mm),dd=as.integer(dd))->temp
            	left_join(subFinale,temp,by=c("yy"="yy","mm"="mm","dd"="dd"))->subFinale
            	
          }#if su file.exists		
        
          write_delim(subFinale,path=paste0(param,"_",ANNOI,"_",ANNOF,".csv"),delim=",",col_names=TRUE)
      
        })#fine purrr::walk

    }else{ #se non sono riuscito a leggere i dati delle stazione nel file FILECF, mettiamo da parte il codice 
      
      indice<<-indice+1
      
      if(indice==3){
        
        #scrivo il file FILECF ed elimino il codice della stazione, pur non avendone letto i dati
        write_delim(stazioniCF[-c(1),],path=FILECF,delim=";",col_names = TRUE)  
        
        sink("_log_stazioniFallite.txt",append=TRUE)
          cat(sprintf("%s\n",stazioniCF$codiceCF[c(1)]))
        sink()
        
      }#fine if su indice
      
    }#if su length(listaOut)    
    
    Sys.sleep(6)

} #fine ciclo repeat

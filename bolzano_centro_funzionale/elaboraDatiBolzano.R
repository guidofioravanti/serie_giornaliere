#Estrazione dei dati dai file pdf scaricati da:  http://meteo.provincia.bz.it/dati-storici.asp
#I file pdf sono stati scaricati salvando il sorgente della pagina html, utilizzando uno script sed per ricavare i link dei file Klima*.pdf
#
#I file pdf sono stati quindi convertiti in txt mediante: pdftotext -layout
#
#Questo programma legge i file di testo e li formatta in modo appropriato
rm(list=objects())
library("readr")
library("stringr")
library("purrr")
library("tidyr")
library("dplyr")
options(warn=2,error=recover)

tryCatch({
  read_delim("reg.bolzano.info.csv",delim=";",col_names=TRUE)->ana
},error=function(e){
  stop("Errore lettura file anagrafica")
})
  
list.files(pattern="^Klima.+txt$")->fileTXT
stopifnot(length(fileTXT)!=0)

sink("log.txt")
purrr::map(fileTXT,.f=function(klima){
  
  suppressWarnings(readLines(klima)->documento)

  #estrai date
  str_extract(documento,pattern="^ +[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}.+$")->righeConDati
  str_split(righeConDati[!is.na(righeConDati)]," +")->valori

  #valori è una lista dove ogni elemento è un vettore di caratteri con:
  #elemtno vuoto ""
  #primo elemento è la data
  #secondo la precipitazione
  #terzo la temperatura massima
  #quarto la temperatura minima
  
  purrr::map_chr(valori,2)->giorni
  
  tryCatch({
    as.Date(giorni,format="%d.%m.%Y")
  },error=function(e){
    stop("Non posso convertire in giorni le stringhe")
  })->giorni
  
  range(giorni)->estremi
  seq.Date(from = estremi[1],to=estremi[2],by="day")->calendario

  #se non hanno la stessa lunghezza vuol dire che nel file di Bolzano o manca qualche data o l'abbiamo scartata noi erroneamente con la regular expression sopra
  stopifnot(length(calendario)==length(giorni))
  
  as.character(giorni)->giorni
  
  str_split(giorni,"-")->giorni

  purrr::map_chr(giorni,1)->anno
  purrr::map_chr(giorni,2)->mese
  purrr::map_chr(giorni,3)->giorno  
  
  purrr::map_chr(valori,3)->pr
  purrr::map_chr(valori,4)->tmax
  purrr::map_chr(valori,5)->tmin

  purrr::map(list(pr,tmax,tmin),.f=function(x){
    
    purrr::map_dbl(x,.f=function(dato){
 
        #ora converti le virgole in "."
        tryCatch({
          as.numeric(str_replace_all(dato,",","."))
        },error=function(e){
          print(paste0("Impossibile convertire: ",dato))
          NA_real_
        })
         
    })    
      
  })%>% reduce(cbind)->df 

  data.frame(yy=anno,mm=mese,dd=giorno,df,stringsAsFactors = FALSE) ->newdf
  names(newdf)[c(4,5,6)]<-c("pr","tmax","tmin")

  str_split(str_replace(klima,"^.+daily_",""),"_")[[1]][[1]]->codiceKlima
  str_split(str_replace(klima,"^.+daily_",""),"_")[[1]][[2]]->nomeKlima
  
  which(ana$SiteCode==codiceKlima)->riga
  
  if(!length(riga)){
    print("******************* Stazione non trovata")
    print(codiceKlima)
    print(nomeKlima)
    print("*******************")
    
    pioggia<-NULL
    tmax<-NULL
    tmin<-NULL
    
  }else{
  
    print("******************* Stazione ok")
    print(codiceKlima)
    print(nomeKlima)
    print(ana[riga,]$SiteName)
    print("*******************")
    
    newdf %>% select(yy,mm,dd,pr)->pioggia
    newdf %>% select(yy,mm,dd,tmax)->tmax  
    newdf %>% select(yy,mm,dd,tmin)->tmin  
    
    names(pioggia)[4]<-ana[riga,]$SiteID
    names(tmax)[4]<-ana[riga,]$SiteID
    names(tmin)[4]<-ana[riga,]$SiteID
    
  }#fine if
    
  list(pioggia=pioggia,tmax=tmax,tmin=tmin)
  
})->listaOut

sink()

purrr::map(c("pioggia","tmax","tmin"),.f=function(pp){
  
  ifelse(pp=="pioggia","Prcp",ifelse(pp=="tmax","Tmax","Tmin"))->fileOut

  purrr::map(listaOut,pp) %>% 
    compact %>%
      reduce(left_join,by=c("yy"="yy","mm"="mm","dd"="dd")) %>%
        select(yy,dd,mm,everything()) %>%
          write_delim(.,paste0(fileOut,".csv"),delim=",",col_names=TRUE)
})

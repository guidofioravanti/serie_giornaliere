rm(list=objects())
library("tidyverse")
library("xml2")
library("stringr")
library("geosphere")
read_delim("metaDatiAbruzzo.csv",delim=";",col_names=TRUE)->metaDati

purrr::map(metaDati$codice,.f=function(codex){
  
  print(codex)
  paste0("http://www.himet.it/cgi-bin/meteo/gmaps/stazioni/new_anagrafica.cgi?code=",codex)->stringaRichiesta
  read_xml(stringaRichiesta,encoding="UTF-8")->xmlDoc

  xml_find_all(xmlDoc,"//stazione")->ris

  tryCatch({
    read_html(xml_text(ris)) %>% xml_text()
  },error=function(e){
    NULL
  })->stringaStazione
  
  if(is.null(stringaStazione)) return(NULL)
  
  str_replace(str_replace(stringaStazione,"^.+Comune: *",""),"Provincia.+","")->comune
  str_replace(str_replace(stringaStazione,"^.+Latitudine: *",""),"Longitudine.+","")->latitudine  
  str_replace(str_replace(stringaStazione,"^.+Longitudine: *",""),"Quota.+","")->longitudine 
  str_replace(str_replace(stringaStazione,"^.+Quota s.l.m.: *","")," m[A-Z].+$","")->quota 

  str_replace(quota,"m","")->quota
  str_replace(str_replace(stringaStazione,"^.+Codice Stazione: *",""),"Codice IDRO:.+","")->codiceStazione 
  str_replace(str_replace(stringaStazione,"^.+Codice IDRO: *",""),"Descrizione Stazione:.+","")->codiceIdro 
  str_replace(stringaStazione,"^.+Termometro aria *\\(codice *([:digit:]+).*\\)","\\1")->codiceTermo 
  if(nchar(codiceTermo)>10) codiceTermo<-NA
  str_replace(stringaStazione,"^.+Pluviometro *\\(codice *([:digit:]+).+\\)","\\1")->codicePluvio
  if(nchar(codicePluvio)>10) codicePluvio<-NA


  c(comune,codiceStazione,longitudine,latitudine,quota,codiceIdro,codiceTermo,codicePluvio)
  
}) %>% compact %>% reduce(rbind) %>% as.data.frame->ris
names(ris)<-c("comune","codiceStazione","longitudine","latitudine","quota","codiceIdro","codiceTermo","codicePluvio")


write_delim(ris,"infoSitoAbruzzo.csv",delim=";",col_names=TRUE)


read_delim("infoSitoAbruzzo.csv",delim=";",col_names = TRUE)->ris



read_delim("reg.abruzzo.info.csv",delim=";",col_names = TRUE)->anaHis
left_join(ris,anaHis,by=c("codiceIdro"="SiteCode"))->ldati
purrr::map_int(1:nrow(ldati),.f=function(riga){
  agrep(ldati$comune[riga],ldati$SiteName[riga],ignore.case = TRUE,max.distance = 0.1)->ris
  if(!length(ris)) ris<-0
  as.integer(ris)
})->ldati$distanzaLevenshtein

purrr::map_int(1:nrow(ldati),.f=function(riga){
  ldati$quota[riga]-ldati$Elevation[riga]->ris
  if(!length(ris)) ris<- 9999
  as.integer(abs(ris))
})->ldati$distanzaQuota

purrr::map_dbl(1:nrow(ldati),.f=function(riga){

  distm(c(ldati$longitudine[riga],ldati$latitudine[riga]), c(ldati$Longitude[riga], ldati$Latitude[riga]), fun = distHaversine)->ris

  if(!length(ris)) ris<- 9999
  ris
  
})->ldati$distanzaLAtLon

data.frame(ldati)->mydf
data.frame(ldati[!is.na(ldati$Longitude),])->mydf2
coordinates(mydf)=~longitudine+latitudine
coordinates(mydf2)=~Longitude+Latitude
write_delim(ldati,path="infoSitoAbruzzo_hisCentral_centroIdrografico.csv",delim=";",col_names=TRUE)

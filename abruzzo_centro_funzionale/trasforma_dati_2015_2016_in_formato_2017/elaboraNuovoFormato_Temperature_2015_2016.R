#Rielabora i dati del 2015/2016 di temperatura per creare dei file analoghi a quelli ricevuti nel 2017 in modo di poter utilizzare i programmi per le serie 2017
rm(list=objects())
library("stringr")
library("tidyverse")
options(error=recover,warn = -1)

ANNO<-2015


list.files(pattern=paste0("^.+",ANNO,".*\\.csv$"))->ffile
stopifnot(length(ffile)!=0)


elaboraFile<-function(nomeFile){
  
  read_delim(nomeFile,delim=";",col_names = TRUE,locale = locale(decimal_mark = ","),na = c("", "NA","-")) %>% 
    select(Data,contains("Giornaliere")) %>%  select(-contains("Medie")) ->dati

  names(dati)->nomi
  nomi[grep("Massime",nomi)]->nomiMassime
  str_replace(nomiMassime," * - *T.? - *Massime *Giornaliere.*","")->nomiMassime
  nomi[grep("Minime",nomi)]->nomiMinime
  str_replace(nomiMinime," * - *T.? - *Minime *Giornaliere.*","")->nomiMinime  
  stopifnot(length(nomiMassime)==length(nomiMinime))
  
  
  purrr::map(nomiMassime,.f=function(nomeStazione){
    
    dati %>% select(contains("Data"),contains(nomeStazione)) %>%
      mutate(Ora="00:00") %>% select(Data,Ora,everything()) %>%
      write_delim(.,paste0(ANNO," ",nomeStazione," T max min.csv"),delim=";",col_names=TRUE)
    
  })
  

}#fine elaboraFIle

purrr::walk(ffile,.f=elaboraFile)
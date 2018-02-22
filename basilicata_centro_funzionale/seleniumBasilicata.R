#15 febbraio 2018
#Attenzione: la ricerca degli elementi del DOM va fatta utilizzando il testo che compare nelle varie "options" dei "select"
#I valori "value" che accompagnano ciascuna "option" sono inaffidabili. Cambiano da sessione a sessione. L'unico punto di riferimento e' il
#testo di ciascuna opzione.
rm(list=objects())
library("RSelenium")
library("stringr")
options(warn=2,error=recover)

SENSORE<-c("Tmax","Tmin","Pluviometro")[c(3)] #<------ selezionare uno dei due
ANNOI<-2000 #<------- selezionare anno
ANNOF<-2017
giornoI<-as.Date(paste0("01-01-",ANNOI),format="%d-%m-%Y")
giornoF<-as.Date(paste0("01-12-",ANNOF),format="%d-%m-%Y")

seq.Date(from=giornoI,to=giornoF,by="month")->calendario

#VALORI PER SENSORE TERMPOMETRO E PLUVIOMETRO
if(SENSORE=="Tmax" || SENSORE=="Tmin"){
  
  SENSORE_VALUE<-"Temperatura Aria"
  
  ifelse(SENSORE=="Tmax","ElabDailyMaximum_Date","ElabDailyMinimum_Date")->METODO_VALUE
  str_replace("//select[@id='scegli_metodo']/option[@value='YYY']","YYY",METODO_VALUE)->stringaMetodo  
  
  PERIODO_VALUE<-"30"
  
  if(SENSORE=="Tmax") stazioni<-c("Potenza","Matera") #Potenza già scaricata la Tmax
  if(SENSORE=="Tmin") stazioni<-c("Potenza","Matera")
  
}else{
  
  SENSORE_VALUE<-"Pioggia Cumulata"  
  
  "ElabDailyIncrements99"->METODO_VALUE #Precipitazioni giornaliere (9-9)
  str_replace("//select[@id='scegli_metodo']/option[@value='YYY']","YYY",METODO_VALUE)->stringaMetodo  

  PERIODO_VALUE<-"30"  
  
  stazioni<-c("Potenza","Albano di Lucania","Grassano SP")  
}


"http://www.centrofunzionalebasilicata.it/it/scaricaDati.php"->urlBasilicata

rsDriver(port =4444L)->rD
remDr <- rD[["client"]]
remDr$open()

remDr$navigate(urlBasilicata)
Sys.sleep(3)

purrr::walk(stazioni,.f=function(STAZIONE_VALUE){


    
  #stazione  
  str_replace("//select[@id='scegli_stazione']/option[contains(text(),'YYY')]","YYY",STAZIONE_VALUE)->stringaStazione  
  remDr$findElement(using="xpath",stringaStazione)$clickElement()
        
  #sensore  
  str_replace("//select[@id='scegli_rilevamento']/option[contains(text(),'YYY')]","YYY",SENSORE_VALUE)->stringaSensore
  remDr$findElement(using="xpath",stringaSensore)$clickElement()
  
  #metodo: Massime giornaliere, minime etc etc
  remDr$findElement(using="xpath",stringaMetodo)->opzioneMetodo
  opzioneMetodo$clickElement()  

  purrr::walk(calendario,.f=function(DATA_VALUE){
    
    unlist(str_split(DATA_VALUE,"-"))->tmp
    # MESE GIORNO ANNO
    DATA_VALUE<-paste(tmp[2],tmp[3],tmp[1],sep="-")      
  
    #scegli la data
    #1) Inject Javascript per aggiungere la classe "user-success"
    "return document.querySelector(\"table tr td input[name='startDate']\").classList.add('user-success')"->stringaData
    remDr$executeScript(stringaData)
    
    remDr$findElement(using="xpath","//input[@name='startDate']")$sendKeysToElement(list(DATA_VALUE))
    
    #periodo da scaricare: 1 mese
    remDr$findElement(using="xpath","//select[@name='interval']/option[contains(text(),'1 mese')]")$clickElement()  
    
    Sys.sleep(5)
      
    #scarica
    remDr$findElement(using="xpath","//div[@class='scarica']/a[@href='#']")$clickElement()
    
    Sys.sleep(3)
  
  }) #fine walk su calendario

}) #fine walk
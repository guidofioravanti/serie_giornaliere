#16 Maggio 2017
#Il programma adesso permette di cumulare tra le 00 e le 00 di un determinato giorno: passare alla funzione
#indexTime il parametro oraRiferimento=0, mentre per cumulare tra le 9 e le 9 passare oraRiferimento=9.

#Cumulare tra le 00 e le 00 serve per allungare le serie storiche fornite dall'Arpa Valle d'Aosta mediante i dati
#orari scaricati dal sito web dell'Arpa

#La funzione prende (x) un vettore di dati giornalieri
#con ore minuti e secondi nel formato: yy-mm-dd hh-mm-ss
#e restituisce un vettore numerico lungo quanto x
#I valori uguali nel vettore numerico fanno riferimento a date comprese
#tra un giorno con ora pari a oraRiferimento e
#le 24 ore successive

#Ad esempio: per i dati di hisCentral gli accumuli di precipitazione
#vanno fatti dalle 9 alle 9. Aggregare con dplyr mediante summarise
#utilizzando group_by(giorno) non restituisce il risultato corretto
#perchè aggrega all'interno del giorno. L'aggregazione va fatta con group_by
#utilizzando una variabile (il vettore restituito dalla funzione che segue)
#che permette di distinguere tutti i dati che vanno dalle ore 9 di un giorno
#alle ore 9 del giorno precedente.

#Quindi: il risultato di questa funzione non è il valore aggregato ma
#semplicemente un vettore da passare a dplyr: group_by per poi procedere con
#summarise

#Attenzione: se cerco i dati tra le 9 e le 9 e i dati in x
#vanno dalle 00 del 1 Gennaio alle 23:30 del 31 dicembre
#risultera che i dati che vanno dalle 9:30 del 31 dicembre fino
#alle 23:30 rimarranno scoperti, cioè non gli sara ossiciato una
#interno placeHolder. Però poco male: a questi valori comunque sarà associato
#il valore 0 con cui viene riempito inizialmente il vettore tempVett
#Il valore 0 rimarrà associato univocamente a questi valori

indexTime<-function(myTime,oraRiferimento){

  if(oraRiferimento==0){
    print("*********************************************************************************************")
    print("ATTENZIONE: oraRiferimento=0, cumulo orari tra le 00 e le 23:30. Utile per serie storiche VdA")
    print("*********************************************************************************************")
    
  }else if(oraRiferimento==9){
    
    print("*********************************************************************************************")
    print("ATTENZIONE: oraRiferimento=9, cumulo orari tra le 09 e le 08:30. Utile per serie non storiche VdA")  
    print("*********************************************************************************************")
    
  }else{
    print("*********************************************************************************************")
    print("oraRiferimento: non so gestire i casi in cui oraRiferimento != 0/9. Verificare codice funzioni correttamente!")
    print("*********************************************************************************************")
    
  }

  placeHolder<-1
  #tempVett è il vettore di interi che viene restituito
  #viene via via riempito con i valori di placeHolder incrementati
  #di 1. La soluzione più rapida sarebbe stata cercare nel vettore x
  #le date con ora para a oraRiferimento. QUesto però funziona solo se 
  #non ci sono buchi. Ad esempio: un dato isoldato come 1971-04-04 08:30:00
  #in un vettore di dati che comprende valori fino al 1968 e che riparte dal 1973
  #(caso assurdo ma possibile) non verrebbe mai trovato. A quel punto gli indici
  #in tempVett non ricoprirebbero questa data. Con l'implementazione che segue invece siamo sicuri
  #che anche a una data isolata viene associato un intero. Sarà poi successivamente la
  #funzione di aggregazione a decidere se per avere un dato giornaliero
  #basta un dato o ne servono 24 o 48.
  tempVett<-vector(mode="integer",length=length(myTime))
  #floor date approssima al giorno più vicino, sequenzaGiorni
  #tiene i giorni "unici" che compaiono nel calendario
  #unique(floor_date(myTime,"day"))->sequenzaGiorni
  if(oraRiferimento==9){
    seq(min(myTime)-days(1),max(myTime)+days(1),by="day")->sequenzaGiorni	
  }else if(oraRiferimento==0){
    seq(min(myTime)-days(0),max(myTime)+days(0),by="day")->sequenzaGiorni	
  }else{
    print("Per oraRiferimento diverso da 0 e 9 rivedere codice")
    seq(min(myTime)-days(1),max(myTime)+days(1),by="day")->sequenzaGiorni	
  }
  
  lapply(sequenzaGiorni,FUN=function(giorno){

    update(giorno,hour=oraRiferimento,minute=0)->giornoOreNove
    giornoOreNove-hours(24)->giornoAntecedente

    if(oraRiferimento==9){
      which(myTime <=(giornoOreNove) & myTime >=(giornoAntecedente+minutes(30)))->indiceGiorniCompresi
      if(!length(indiceGiorniCompresi)) {print(paste0("giorni non trovati! tra:",giornoAntecedente+minutes(30)," e ",giornoOreNove));return()}
    }else if(oraRiferimento==0){
      giornoOreNove+hours(24)-minutes(30)->giornoSuccessivo
      which(myTime>=(giornoOreNove) & myTime <=(giornoSuccessivo))->indiceGiorniCompresi
      if(!length(indiceGiorniCompresi)) {print(paste0("giorni non trovati! tra:",giornoOreNove," e ",giornoSuccessivo));return()}
    }else{
      print("Per oraRiferimento diverso da 0 e 9 rivedere codice")
      which(myTime <=(giornoOreNove) & myTime >=(giornoAntecedente+minutes(30)))->indiceGiorniCompresi
      if(!length(indiceGiorniCompresi)) {print(paste0("giorni non trovati! tra:",giornoAntecedente+minutes(30)," e ",giornoOreNove));return()}
    }  
    #if(length(indiceGiorniCompresi) <48) browser()
    
    tempVett[indiceGiorniCompresi]<<-placeHolder
    placeHolder<<-placeHolder+1
  
    invisible()
      
  })#fine lapply

  return(tempVett)
  
} #fine funzione indexTime


#47 osservazioni tra le 9:00 e le 8.30
#se sono meno restituisci NA
cumulaPrec<-function(x=NULL,numObs=48){
  
  if(is.null(x)) stop("Vettore dati mancante")

  somma<-NA
  if(length(x)==numObs)	sum(x,na.rm=FALSE)->somma

  return(somma)
    
} #fine cumulaPrec



#47 osservazioni tra le 9:00 e le 8.30
#se sono meno restituisci NA
minimoTemp<-function(x=NULL,numObs=48){
  
  if(is.null(x)) stop("Vettore dati mancante")
  
  minimo<-NA
  if(length(x)==numObs) min(x,na.rm=FALSE)->minimo
  
  return(minimo)
  
} #fine cumulaPrec


#47 osservazioni tra le 9:00 e le 8.30
#se sono meno restituisci NA
massimoTemp<-function(x=NULL,numObs=48){
  
  if(is.null(x)) stop("Vettore dati mancante")
  
  massimo<-NA
  if(length(x)==numObs) max(x,na.rm=FALSE)->massimo
  
  return(massimo)
  
} #fine cumulaPrec

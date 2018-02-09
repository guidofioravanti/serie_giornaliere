#revisione 9 febbraio 2018
#ATTENZIONE: il programma potrebbe lamentaresi di non trovare il pulsante per fare il login. Questo succede (sembra)
#se davanti al pulsante di login compare l'avvertenza per i cookies, etc etc...Quindi: quando si apre la prima pagina
#cliccare manualmente sul pulsante di accettazione per quanto riguarda i termini della privacy in modo che il programma
#possa funzionare.
rm(list=objects())
library("RSelenium")
options(warn=2,error=recover)

urlLoginAosta<-"http://cf.regione.vda.it/richiesta_dati.php"
ANNO<-2017 #<------- selezionare anno

SENSORE<-c("Termometro","Pluviometro")[c(2)] #<------ selezionare uno dei due

rsDriver(port =4444L)->rD
remDr <- rD[["client"]]
remDr$open()

remDr$navigate(urlLoginAosta)
Sys.sleep(8)
remDr$findElement("id", "login")$clickElement()
Sys.sleep(4)
remDr$findElement("name", "email")$sendKeysToElement(list("guidofioravanti@yahoo.it"))
remDr$findElement("name", "pass")$sendKeysToElement(list("o1r6n5q1"))
remDr$findElement("name","login")$clickElement()

#trova il menu con la motivazione
remDr$setWindowSize(width=1600,1024)
Sys.sleep(4)
remDr$findElement("id","dropdown")$sendKeysToElement(list("Ricerca a scopo scientifico"))
remDr$findElement("id","datepicker_start")$sendKeysToElement(list(paste0("01-01-",ANNO) ))
remDr$findElement("id","datepicker_end")$sendKeysToElement(list(paste0("31-12-",ANNO) ))

Sys.sleep(4)
remDr$findElement("id","passo_temporale")$sendKeysToElement(list("Semiorario (GMT)"))
#clicca su termometri

if(SENSORE=="Termometro"){
  #per termometro
  remDr$executeScript(script="$('ul > li.par_tot > label> input[value=1]').click();")
}else if(SENSORE=="Pluviometro"){  
  #per pluviometro
  remDr$executeScript(script="$('ul > li.par_tot > label> input[value=7]').click();")
}else{
  stop("SENSORE NON RICONOSCIUTO")
}

Sys.sleep(4)
remDr$findElement("id","vendita_dati")$clickElement()

remDr$executeScript(script="var vettore=[]; var vetAttr=$('#list_target').children('option').each(function(opzione){

  vettore.push($(this).attr('value'));   
  //vettore.push($(this).text());    

}); return vettore;")->codiciStaz

unlist(codiciStaz)->codiciStaz
print(codiciStaz)
remDr$executeScript(script="var vettore=[]; var vetAttr=$('#list_target').children('option').each(function(opzione){

                    vettore.push($(this).text());   
                   
                    }); return vettore;")->nomiStaz
unlist(nomiStaz)->nomiStaz
print(nomiStaz)
write.table(data.frame(nomiStaz,codiciStaz),"codiciPrec.csv",row.names=F,col.names=T,quote=F,sep=";")


lapply(codiciStaz,FUN=function(codice){
  

  if(!nchar(codice)) return()
  Sys.sleep(4)
  remDr$findElement("xpath",paste0("//select[@id='list_target']/option[@value=",codice,"]") )$clickElement()
  Sys.sleep(4)
  
  if(SENSORE=="Termometro"){
      #Per termometro
        try({
          remDr$executeScript(script="$('#labid_1~input[value=1]').click();")
        })
  }else if(SENSORE=="Pluviometro"){
     #Per pluviometro
     try({
       remDr$executeScript(script="$('#labid_7~input[value=7]').click();")
     })
  }else{
    stop("SENSORE NON RICONOSCIUTO")  
  }
  
  Sys.sleep(4)  
  remDr$findElement("xpath","//a[@id='end_button']")$clickElement()
  remDr$executeScript(script="$('#end_button_r').click();")
  Sys.sleep(6)
  remDr$findElement("link text","Cancella")$clickElement()

})#fine lapply


remDr$findElement("id", "logout")$click()


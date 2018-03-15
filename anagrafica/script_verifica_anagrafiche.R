rm(list=objects())
library("dplyr")
library("sf")
library("readr")

list.files(pattern="^reg.+csv$")->regioni

purrr::map(regioni,.f=function(rr){
  
  read_delim(rr,delim=";",col_names=TRUE)->ana
  
  if(!all(c("Elevation","Elevation_DEM","Elevation_FonteDati") %in% names(ana))) browser()
  stopifnot(!all(is.na(ana$Elevation)))  
  
  which((ana$Latitude <35) | (ana$Latitude > 50) )->righeLat
  if(length(righeLat)){
    print(range(ana[righeLat,]$Latitude))
    browser()
    #ana[righeLat,]$Latitude<-ana[righeLat,]$Latitude/1000
  }  

  which((ana$Longitude < 5) | (ana$Longitude > 20) )->righeLon    
  if(length(righeLon)){
    print(range(ana[righeLon,]$Longitude))
    browser()
    #ana[righeLon,]$Longitude<-ana[righeLon,]$Longitude/1000
  }  
  
  if(any(ana$Latitude < 35,na.rm=TRUE)) browser()
  if(any(ana$Latitude > 50,na.rm=TRUE)) browser()
  if(any(ana$Longitude < 5,na.rm=TRUE)) browser()
  if(any(ana$Longitude > 20,na.rm=TRUE)) browser()
  
    
  ana %>%
    select(SiteID,SiteCode,SiteName,Longitude,Latitude,Elevation,Elevation_DEM,Elevation_FonteDati,Regione,everything()) %>%
    write_delim(.,paste0("./new/",rr),delim=";",col_names=TRUE)
  
})

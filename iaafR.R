#Cargar librerías
library(rvest)
library(xml2)
library(tidyverse)

###########
# Outdoor #
###########

# variables de categoria y pruebas
sexo<-c("men","women")
dist<-c("100-metres","200-metres","400-metres","800-metres","1500-metres","5000-metres","10000-metres","3000-metres",
        "3000-metres-steeplechase","10-kilometres","half-marathon","marathon","110-metres-hurdles","100-metres-hurdles","400-metres-hurdles",
        "high-jump","pole-vault","long-jump","triple-jump","shot-put","discus-throw","hammer-throw","javelin-throw","50-kilometres-race-walk",
        "20-kilometres-race-walk","10-kilometres-race-walk","4x100-metres-relay","4x400-metres-relay")

#Las URL iniciales para cada sector
url1<-"https://worldathletics.org/records/toplists/sprints/"
url2<-"https://worldathletics.org/records/toplists/middle-long/"
url3<-"https://worldathletics.org/records/toplists/road-running/"
url4<-"https://worldathletics.org/records/toplists/hurdles/"
url5<-"https://worldathletics.org/records/toplists/jumps/"
url6<-"https://worldathletics.org/records/toplists/throws/"
url61<-"https://worldathletics.org/records/toplists/race-walks/"
url62<-"https://worldathletics.org/records/toplists/relays/"


#Resto de partes de la URL común para todas

url7<-"/outdoor/" #men or women
url8<-"/senior/" #aquí año
url9<-"?regionType=world&timing=all&page=" #Aqui pagina
url10<-"&bestResultsOnly=false"


j=0
# Lo pongo como for por si alguno se anima a sacarlo en plan 2001:2020 pero son millones de registros
# merece la pena ir año a año
for(x in 2020:2020){
  print(x)
  for(k in sexo){
    print(k)
    for(h in dist){
      print(h)
      # Aquí calculo la página máxima que recorrer
      if(h %in% c("100-metres","200-metres","400-metres")){
        url_final<-paste(url1,h,url7,k,url8,x,url9,1,url10,sep="")}
      else if(h %in% c("800-metres","1500-metres","5000-metres","10000-metres",
                          "3000-metres-steeplechase","3000-metres")){
        url_final<-paste(url2,h,url7,k,url8,x,url9,1,url10,sep="")}
      else if(h %in% c("10-kilometres","half-marathon","marathon")){
        url_final<-paste(url3,h,url7,k,url8,x,url9,1,url10,sep="")}
      else if(h %in% c("400-metres-hurdles")){
        url_final<-paste(url4,h,url7,k,url8,x,url9,1,url10,sep="")}
      else if(h %in% c("high-jump","pole-vault","long-jump","triple-jump")){
        url_final<-paste(url5,h,url7,k,url8,x,url9,1,url10,sep="")}
      else if(h %in% c("shot-put","discus-throw","hammer-throw","javelin-throw")){
        url_final<-paste(url6,h,url7,k,url8,x,url9,1,url10,sep="")}
      else if(h %in% c("50-kilometres-race-walk","20-kilometres-race-walk","10-kilometres-race-walk")){
        url_final<-paste(url61,h,url7,k,url8,x,url9,1,url10,sep="")}
      else if(h %in% c("110-metres-hurdles") & k=='men'){
        url_final<-paste(url4,h,url7,k,url8,x,url9,1,url10,sep="")}
      else if(h %in% c("110-metres-hurdles") & k=='women'){
        next}
      else if(h %in% c("100-metres-hurdles") & k=='women'){
        url_final<-paste(url4,h,url7,k,url8,x,url9,1,url10,sep="")}
      else if(h %in% c("100-metres-hurdles") & k=='men'){
        next}
      else if(h %in% c("4x100-metres-relay","4x400-metres-relay")){
        url_final<-paste(url62,h,url7,k,url8,x,url9,1,url10,sep="")}


      #print(url_final)
      url <- read_html(url_final)

      num<-url %>%
        html_nodes("a") %>%
        html_attr("data-page")

      paginas<-data.frame((as.numeric(num)))
      paginas[is.na(paginas)] <- 0

      final_pag<-max(paginas)

      # Como ya está calculada recorremos todas páginas para obtener la tabla
      for(pa in 1:final_pag){

        if(h %in% c("100-metres","200-metres","400-metres")){
          url_final<-paste(url1,h,url7,k,url8,x,url9,pa,url10,sep="")}
        else if(h %in% c("800-metres","1500-metres","5000-metres","10000-metres","3000-metres-steeplechase","3000-metres")){
          url_final<-paste(url2,h,url7,k,url8,x,url9,pa,url10,sep="")}
        else if(h %in% c("10-kilometres","half-marathon","marathon")){
          url_final<-paste(url3,h,url7,k,url8,x,url9,pa,url10,sep="")}
        else if(h %in% c("400-metres-hurdles")){
          url_final<-paste(url4,h,url7,k,url8,x,url9,pa,url10,sep="")}
        else if(h %in% c("high-jump","pole-vault","long-jump","triple-jump")){
          url_final<-paste(url5,h,url7,k,url8,x,url9,pa,url10,sep="")}
        else if(h %in% c("shot-put","discus-throw","hammer-throw","javelin-throw")){
          url_final<-paste(url6,h,url7,k,url8,x,url9,pa,url10,sep="")}
        else if(h %in% c("50-kilometres-race-walk","20-kilometres-race-walk","10-kilometres-race-walk")){
          url_final<-paste(url61,h,url7,k,url8,x,url9,pa,url10,sep="")}
        else if(h %in% c("110-metres-hurdles") & k=='men'){
          url_final<-paste(url4,h,url7,k,url8,x,url9,pa,url10,sep="")}
        else if(h %in% c("100-metres-hurdles") & k=='women'){
          url_final<-paste(url4,h,url7,k,url8,x,url9,pa,url10,sep="")}
        else if(h %in% c("4x100-metres-relay","4x400-metres-relay")){
          url_final<-paste(url62,h,url7,k,url8,x,url9,pa,url10,sep="")}


        url <- read_html(url_final)

        lista <- url %>%
          html_node(xpath='//*[@id="toplists"]/div[3]/table')%>%
          html_table()

        lista$gender<-k
        lista$distancia<-h
        lista$year<-x
        lista$environment<-'outdoor'
        lista$Pos<-as.character(lista$Pos)
        lista$DOB<-as.character(lista$DOB)
        lista$Mark<-as.character(lista$Mark)

        if(h %in% c("100-metres","200-metres","long-jump","triple-jump","110-metres-hurdles","100-metres-hurdles")){
          columnas<-c("Rank","WIND","Mark","Competitor","DOB","Nat","Pos","Venue","Date","Results Score","gender","distancia","year","environment")}
        else{
          columnas<-c("Rank","Mark","Competitor","DOB","Nat","Pos","Venue","Date","Results Score","gender","distancia","year","environment")}


        lista<-lista[columnas]

        if(j==0){
          iaaf_out<-lista
        }else{
          iaaf_out<-bind_rows(iaaf_out,lista)
        }
        j=j+1
      }
    }
  }
}

##########
# Indoor #
##########

sexo<-c("men","women")
dist<-c("60-metres","400-metres","800-metres","1500-metres","3000-metres",
        "60-metres-hurdles","high-jump","pole-vault","long-jump","triple-jump","shot-put")

url1<-"https://worldathletics.org/records/toplists/sprints/"
url2<-"https://worldathletics.org/records/toplists/middle-long/"
url4<-"https://worldathletics.org/records/toplists/hurdles/"
url5<-"https://worldathletics.org/records/toplists/jumps/"
url6<-"https://worldathletics.org/records/toplists/throws/"


url7<-"/indoor/" #men or women
url8<-"/senior/" #aquí año
url9<-"?regionType=world&timing=all&page=" #Aqui pagina
url10<-"&bestResultsOnly=false"


j=0
for(x in 2020:2020){
  print(x)
  for(k in sexo){
    print(k)
    for(h in dist){
      print(h)
      # Aquí lo que hacemos es obtner la página máxima de resultados
      if(h %in% c("60-metres","400-metres")){
        url_final<-paste(url1,h,url7,k,url8,x,url9,1,url10,sep="")}
      else if(h %in% c("800-metres","1500-metres","3000-metres")){
        url_final<-paste(url2,h,url7,k,url8,x,url9,1,url10,sep="")}
      else if(h %in% c("60-metres-hurdles")){
        url_final<-paste(url4,h,url7,k,url8,x,url9,1,url10,sep="")}
      else if(h %in% c("high-jump","pole-vault","long-jump","triple-jump")){
        url_final<-paste(url5,h,url7,k,url8,x,url9,1,url10,sep="")}
      else if(h %in% c("shot-put")){
        url_final<-paste(url6,h,url7,k,url8,x,url9,1,url10,sep="")}


      #print(url_final)
      url <- read_html(url_final)

      num<-url %>%
        html_nodes("a") %>%
        html_attr("data-page")

      paginas<-data.frame((as.numeric(num)))
      paginas[is.na(paginas)] <- 0

      final_pag<-max(paginas)

      #ya podemos recorrer toda las páginas
      for(pa in 1:final_pag){

        if(h %in% c("60-metres","400-metres")){
          url_final<-paste(url1,h,url7,k,url8,x,url9,pa,url10,sep="")}
        else if(h %in% c("800-metres","1500-metres","3000-metres")){
          url_final<-paste(url2,h,url7,k,url8,x,url9,pa,url10,sep="")}
        else if(h %in% c("60-metres-hurdles")){
          url_final<-paste(url4,h,url7,k,url8,x,url9,pa,url10,sep="")}
        else if(h %in% c("high-jump","pole-vault","long-jump","triple-jump")){
          url_final<-paste(url5,h,url7,k,url8,x,url9,pa,url10,sep="")}
        else if(h %in% c("shot-put")){
          url_final<-paste(url6,h,url7,k,url8,x,url9,pa,url10,sep="")}


        url <- read_html(url_final)

        lista <- url %>%
          html_node(xpath='//*[@id="toplists"]/div[3]/table')%>%
          html_table()

        lista$gender<-k
        lista$distancia<-h
        lista$year<-x
        lista$environment<-'indoor'
        lista$Pos<-as.character(lista$Pos)
        lista$DOB<-as.character(lista$DOB)
        lista$Mark<-as.character(lista$Mark)

        columnas<-c("Rank","Mark","Competitor","DOB","Nat","Pos","Venue","Date","Results Score","gender","distancia","year","environment")


        lista<-lista[columnas]

        if(j==0){
          iaaf_ind<-lista
        }else{
          iaaf_ind<-bind_rows(iaaf_ind,lista)
        }
        j=j+1
      }
    }
  }

}


iaaf_total_año<-bind_rows(iaaf_out,iaaf_ind)

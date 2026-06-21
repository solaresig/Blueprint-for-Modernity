#### Blueprint for modernity####
## Alumni lists##
#Author: Israel G Solares#
#contact:isgarcia@colmex.mx#
#Licence: CC by 4#
#language: English#

#### I. defining, extracting and cleaning####
####I.0.1 installing packages####
install.packages("readr")
install.packages("grid")
install.packages("tidyverse")
install.packages("stringr")
install.packages("deeplr")
install.packages("ggmap")
install.packages("ggplot2")
install.packages("grid")
install.packages("rworldmap")
install.packages("RColorBrewer")
install.packages("sp")
install.packages("countrycode")
install.packages("dplyr")
install.packages("tidyr")
install.packages("tesseract")
install.packages("magick")
install.packages("magrittr")
install.packages("igraph")
install.packages("network")
install.packages("viridis")
install.packages("refinr")
install.packages("stringi")
install.packages("tibble")
install.packages("grid")
install.packages("gridExtra")
install.packages("networkD3")
####I.0. 2 defining working directory####
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#### I.0.3 loading packages####
library(network)
library(readr)
library(grid)
library(tidyverse)
library(stringr)
library(ggmap)
library(deeplr)
library(ggplot2)
library(grid)
library(rworldmap)
library(RColorBrewer)
library(sp)
library(rworldmap)
library(countrycode)
library(dplyr)
library(refinr)
library(plyr)
library(tesseract)
library(magick)
library(magrittr)
library(igraph)
library(viridis)
library(tibble)
library(grid)
library(gridExtra)
library(networkD3)
library(reshape2)
####I. 0. 4. bases for maps####
theme_black = function(base_size = 12, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = "black", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "white"),  
      panel.grid.major = element_line(color = "black"),  
      panel.grid.minor = element_line(color = "black"),  
      panel.margin = unit(0.5, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
      
    )
}
base<- getMap()
base<- ggplot() + geom_polygon(data = base, aes(x=long, y = lat, group = group),  fill = "white", color="black")
base2<- getMap()
base2<- ggplot() + geom_polygon(data = base2, aes(x=long, y = lat, group = group),  fill = "black", color="black")

####I.0.4 defining functions####
buscar<-function(x){
  register_google(key = )
  x<-geocode(as.character(x))
  x<-as.data.frame(x)
}
buscar2<-function(x){
  y<-subset(x[which(!is.na(x$loc)),], select=c("loc"))
  y<-unique(y$loc)
  y<-as.data.frame(y)
  colnames(y)<-"loc"
  z<-buscar(y$loc)
  y$lon<-z$lon
  y$lat<-z$lat
  z<-left_join(x, y, by="loc")
  return(z)
}
mapear<-function(df, titulo, color){
  df<-base+ geom_point(data=df, aes(x=lon, y=lat, fill=color), color = "white", shape=21, size=3.0, alpha=0.5)+
    coord_fixed(ratio = 1.1)+
    scale_colour_distiller(palette="Purples", name="year", guide = "colorbar")+
    ggtitle(titulo)+
    theme(plot.title = element_text(hjust = 0.5))
  df}
coords2country <- function(df){  
  ll<-subset(df, select=c("lon", "lat"))
  ll[is.na(ll)]<-FALSE
  countriesSP <- getMap(resolution='high')
  pointsSP <- SpatialPoints(ll, proj4string=CRS(proj4string(countriesSP)))  
  indices <- over(pointsSP, countriesSP, use="complete.obs")
  indices$ADMIN<-tolower(indices$ADMIN)
  indices$ADMIN
}
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
conv<-function(x, y) {
  #limpiando el texto
  #el argumento debe de tener la estructura abc$text
  x<- tesseract::ocr(x, engine = "eng")
  trimws(x, which=c("both"))
  write.csv(x, y)
}
rfin<-function(x){
  x<-key_collision_merge(as.character(x))
  x<-n_gram_merge(as.character(x), numgram=6)
  x<-key_collision_merge(as.character(x))
  x<-n_gram_merge(as.character(x), numgram=6)
  x<-key_collision_merge(as.character(x))
  x<-n_gram_merge(as.character(x), numgram=6)
  x<-key_collision_merge(as.character(x))
  x<-n_gram_merge(as.character(x), numgram=6)
  x<-key_collision_merge(as.character(x))
  x<-n_gram_merge(as.character(x), numgram=6)
}
llenar<-function(x){
  y<-ifelse(!is.na(x), x, lag(x, 1))
  return(y)
}
llenar2<-function(x, cond){
  y<-ifelse(!is.na(x), x, 
            ifelse(cond==lag(cond, 1), lag(x, 1), 
                   ifelse(cond==lead(cond, 1), lead(x, 1),   
                          x)))
  return(y)
}
cias<-function(x){
  y<-ifelse(str_extract(x, "^(\\w+)")==str_extract(lag(x,1), "^(\\w+)"), 
            lag(x, 1), x)
  y[1]<-x[1]
  return(y)
}
trimdigi<-function(x){
  y<-str_replace_all(x,  "C,", "0,")
  x<-str_replace_all(y,  "[(],", "0,")
  y<-str_replace_all(x,  "C(?=\\d)", "0")
  x<-str_replace_all(y,  "[(]0", "00")
  y<-str_replace_all(x,  "OO", "00")
  x<-str_replace_all(y,  "OC", "00")
  y<-str_replace_all(x,  "CO", "00")
  x<-str_replace_all(y,  "Q(?=\\d)", "0")
  y<-str_replace_all(x,  "(?<=\\d)Q", "0")
  x<-str_replace_all(y,  "O(?=\\d)", "0")
  y<-str_replace_all(x,  "(?<=\\d)O", "0")
  x<-str_replace_all(y,  "o(?=\\d)", "0")
  y<-str_replace_all(x,  "(?<=\\d)o", "0")
  x<-str_replace_all(y,  "u(?=\\d)", "0")
  y<-str_replace_all(x,  "(?<=\\d)u", "0")
  x<-str_replace_all(y,  "U(?=\\d)", "0")
  y<-str_replace_all(x,  "(?<=\\d)U", "0")
  x<-str_replace_all(y,  "D(?=\\d)", "0")
  y<-str_replace_all(x,  "(?<=\\d)D", "0")
  x<-str_replace_all(y,  "M(?=\\d)", "0")
  y<-str_replace_all(x,  "(?<=\\d)M", "0")
  x<-str_replace_all(y,  "G(?=\\d)", "0")
  y<-str_replace_all(x,  "E(?=\\d)", "0")
  x<-str_replace_all(y,  "(?<=\\d)G", "0")
  y<-str_replace_all(x,  "(?<=\\d)E", "0")
  x<-str_replace_all(y,  "I(?=\\d)", "1")
  y<-str_replace_all(x,  "X(?=\\d)", "0")
  x<-str_replace_all(y,  "(?<=\\d)I", "1")
  y<-str_replace_all(x,  "(?<=\\d)X", "0")
  x<-str_replace_all(y,  "T(?=\\d)", "0")
  y<-str_replace_all(x,  "(?<=\\d)T", "1")
  x<-str_replace_all(y,  "[|](?=\\d)", "1")
  y<-str_replace_all(x,  "(?<=\\d)[|]", "1")
  x<-str_replace_all(y,  "EO", "00")
  y<-str_replace_all(x,  "t(?=\\d)", "4")
  x<-str_replace_all(y,  "(?<=\\d)t", "4")
  y<-str_replace_all(x,  "L(?=\\d)", "1")
  x<-str_replace_all(y,  "(?<=\\d)L", "1")
  y<-str_replace_all(x,  "l(?=\\d)", "1")
  x<-trimws(str_replace_all(y,  "(?<=\\d)l", "1"))
  return(x)
}
repetir2<-function(x, funcion){
  y<-funcion(x)
  x<-funcion(y)
  return(x)
}
repetir10<-function(x, funcion){
  y<-repetir2(x, funcion)
  x<-repetir2(y, funcion)
  y<-repetir2(x, funcion)
  x<-repetir2(y, funcion)
  y<-repetir2(x, funcion)
  return(y)
}
repetir100<-function(x, funcion){
  y<-repetir10(x, funcion)
  x<-repetir10(y, funcion)
  y<-repetir10(x, funcion)
  x<-repetir10(y, funcion)
  y<-repetir10(x, funcion)
  x<-repetir10(y, funcion)
  y<-repetir10(x, funcion)
  x<-repetir10(y, funcion)
  y<-repetir10(x, funcion)
  x<-repetir10(y, funcion)
  return(x)
}
repetir1000<-function(x, funcion){
  y<-repetir100(x, funcion)
  x<-repetir100(y, funcion)
  y<-repetir100(x, funcion)
  x<-repetir100(y, funcion)
  y<-repetir100(x, funcion)
  x<-repetir100(y, funcion)
  y<-repetir100(x, funcion)
  x<-repetir100(y, funcion)
  y<-repetir100(x, funcion)
  x<-repetir100(y, funcion)
  return(x)
}
repetir<-function(x, funcion, n){
  if(n==2){
    y<-repetir2(x,funcion)
    return(y)
  }
  else if(n==10){
    y<-repetir10(x,funcion)
    return(y)
  }
  else if(n==100){
    y<-repetir100(x,funcion)
    return(y)
  }
  else if(n==1000){
    y<-repetir1000(x,funcion)
    return(y)
  }
  else{
    warning('n has to be 2, 10, 100 or 1000')
    return(x)
  }
}
fname<-function(x){ 
  y<-tolower(str_extract(x, "^(\\w+)"))
  return(y)
}
lname<-function(x){
  y<-tolower(ifelse(!is.na(str_extract(x, "^the(\\s)|(\\s)the(\\s)|^with(\\s)|(\\s)with|time|dr(\\s)|prof(\\s)|geo(\\s)|
                               progress|they(\\s)|mining|company|licenciado|coronel|general|lic(//s)")), NA, 
                    ifelse(!is.na(str_extract(x, "(\\w){3,}$")),str_extract(x, "(\\w){4,}$"), 
                           ifelse(!is.na(str_extract(x, "(\\w){3,}(?=(\\w+)$)")),str_extract(x, "(\\w){4,}(?=(\\w+)$)"),
                                  ifelse(!is.na(str_extract(x, "(\\w){3,}(?=(\\w+)(\\s)(\\w+)$)")),str_extract(x, "(\\w){4,}(?=(\\w+)(\\s)(\\w+)$)"), 
                                         NA)))))
}
mname<-function(x){
  y<-ifelse(!is.na(str_extract(x, "^(\\w+)(\\s)(\\w+)")), 
            str_extract(x, "(\\w+)$|(\\w+)[.]$"), NA)
  return(y)
}
sname<-function(x){
  y<-ifelse(!is.na(x$lastname), 
            paste(x$lastname, ", ", 
                  ifelse(!is.na(x$firstname), substr(x$firstname, 1, 1), ""), " " ,
                  ifelse(!is.na(x$middlename), substr(x$middlename, 1, 1),""), sep=""), 
            NA)
  y<-tolower(trimws(y))
  return(y)
}
listocol<-function(x){
  ifelse(!is.na(x[1]), x[1], 
         ifelse(!is.na(x[2]), x[2], 
                ifelse(!is.na(x[3]), x[3], 
                       ifelse(!is.na(x[4]), x[4], 
                              ifelse(!is.na(x[5]), x[5], 
                                     ifelse(!is.na(x[6]), x[6], 
                                            ifelse(!is.na(x[7]), x[7], 
                                                   ifelse(!is.na(x[8]), x[8], 
                                                          NA))))))))
}
ids<-function(x){
  
}
muestra<-function(df){
  n<-ceiling((1.96^2)*((0.5*(1-0.5)/(0.05^2)))/(1+(0.5*(1-0.5)/((0.05^2)*nrow(df)))))
  y<-df[sample(nrow(df), n, replace=T), ]
  y<-y[order(y$id),]
  return(y)
}
pegar<-function(texto, nombre){
  ifelse(!is.na(nombre)&is.na(lead(nombre))&is.na(lead(nombre, 2)) &is.na(lead(nombre, 3)) &is.na(lead(nombre, 4)) &is.na(lead(nombre, 5)), 
         paste(texto, 
               ifelse((!is.na(str_extract(lead(texto), "^[A-Z]{1}")))|(!is.na(str_extract(lead(texto), "^[[:digit:]]{1}"))), " ", ""),
               lead(texto), 
               ifelse((!is.na(str_extract(lead(texto,2), "^[A-Z]{1}")))|(!is.na(str_extract(lead(texto,2), "^[[:digit:]]{1}"))) , " ", ""),
               lead(texto,2), 
               ifelse((!is.na(str_extract(lead(texto, 3), "^[A-Z]{1}")))|(!is.na(str_extract(lead(texto,3), "^[[:digit:]]{1}"))) , " ", ""),
               lead(texto,3),
               ifelse((!is.na(str_extract(lead(texto, 4), "^[A-Z]{1}")))|(!is.na(str_extract(lead(texto,4), "^[[:digit:]]{1}"))) , " ", ""),
               lead(texto,4),
               ifelse((!is.na(str_extract(lead(texto, 5), "^[A-Z]{1}")))|(!is.na(str_extract(lead(texto,5), "^[[:digit:]]{1}"))) , " ", ""),
               lead(texto,5),
               sep=""), 
         ifelse(!is.na(nombre)&is.na(lead(nombre))&is.na(lead(nombre, 2)) &is.na(lead(nombre, 3)) &is.na(lead(nombre, 4)), 
                paste(texto, 
                      ifelse((!is.na(str_extract(lead(texto, 1), "^[A-Z]{1}")))|(!is.na(str_extract(lead(texto), "^[[:digit:]]{1}"))), " ", ""),
                      lead(texto), 
                      ifelse((!is.na(str_extract(lead(texto, 2), "^[A-Z]{1}")))|(!is.na(str_extract(lead(texto,2), "^[[:digit:]]{1}"))) , " ", ""),
                      lead(texto,2), 
                      ifelse((!is.na(str_extract(lead(texto, 3), "^[A-Z]{1}")))|(!is.na(str_extract(lead(texto,3), "^[[:digit:]]{1}"))) , " ", ""),
                      lead(texto,3),
                      ifelse((!is.na(str_extract(lead(texto, 4), "^[A-Z]{1}")))|(!is.na(str_extract(lead(texto,4), "^[[:digit:]]{1}"))) , " ", ""),
                      lead(texto,4),
                      sep=""), 
                ifelse(!is.na(nombre)&is.na(lead(nombre))&is.na(lead(nombre, 2)) &is.na(lead(nombre, 3)), 
                       paste(texto, 
                             ifelse((!is.na(str_extract(lead(texto, 1), "^[A-Z]{1}")))|(!is.na(str_extract(lead(texto), "^[[:digit:]]{1}"))), " ", ""),
                             lead(texto), 
                             ifelse((!is.na(str_extract(lead(texto, 2), "^[A-Z]{1}")))|(!is.na(str_extract(lead(texto,2), "^[[:digit:]]{1}"))) , " ", ""),
                             lead(texto,2), 
                             ifelse((!is.na(str_extract(lead(texto, 3), "^[A-Z]{1}")))|(!is.na(str_extract(lead(texto,3), "^[[:digit:]]{1}"))) , " ", ""),
                             lead(texto,3),
                             sep=""), 
                       ifelse(!is.na(nombre)&is.na(lead(nombre))&is.na(lead(nombre, 2)), 
                              paste(texto, 
                                    ifelse((!is.na(str_extract(lead(texto, 1), "^[A-Z]{1}")))|(!is.na(str_extract(lead(texto), "^[[:digit:]]{1}"))), " ", ""),
                                    lead(texto), 
                                    ifelse((!is.na(str_extract(lead(texto, 2), "^[A-Z]{1}")))|(!is.na(str_extract(lead(texto,2), "^[[:digit:]]{1}"))) , " ", ""),
                                    lead(texto,2), sep=""), 
                              ifelse(!is.na(nombre)&is.na(lead(nombre)), 
                                     paste(texto, 
                                           ifelse((!is.na(str_extract(lead(texto), "^[A-Z]{1}")))|(!is.na(str_extract(lead(texto), "^[[:digit:]]{1}"))), " ", ""),
                                           lead(texto), sep=""), 
                                     ifelse(!is.na(nombre), texto, NA))))))
  
}
apellido<-function(df, apellido){
  df$apellido<-apellido
  temp<-df[which(!is.na(apellido)),]
  temp$apellidoF<- ifelse(is.na(lag(temp$apellido))|is.na(lag(temp$apellido, 2))|is.na(lead(temp$apellido))|is.na(lead(temp$apellido, 2)), temp$apellido,  
                          ifelse(str_extract(temp$apellido, "^(\\w){1}")==str_extract(lag(temp$apellido), "^(\\w){1}")|
                                   str_extract(temp$apellido, "^(\\w){1}")==str_extract(lead(temp$apellido), "^(\\w){1}")|
                                   str_extract(temp$apellido, "^(\\w){1}")==str_extract(lag(temp$apellido,2), "^(\\w){1}")|
                                   str_extract(temp$apellido, "^(\\w){1}")==str_extract(lead(temp$apellido,2), "^(\\w){1}"),
                                 temp$apellido, #this pretty long ifelse tries to situate apellidos that begin with the same letter than other contextual apellidos (lags and lead with to rows above)
                                 ifelse((match(str_extract(temp$apellido, "^(\\w){1}"), LETTERS[1:26])-match(str_extract(lag(temp$apellido), "^(\\w){1}"), LETTERS[1:26])>=0)&
                                          (match(str_extract(temp$apellido, "^(\\w){1}"), LETTERS[1:26])-match(str_extract(lag(temp$apellido), "^(\\w){1}"), LETTERS[1:26])<=5), 
                                        temp$apellido, #this pretty hard rule applies for all other apellidos that do not belong to the previous patterns, breaking the alphabetical order
                                        NA)))
  df<-left_join(df, subset(temp, select=c("id", "apellidoF")))
  return(df)
}

####II. MIT ####
####II. 1. 1903####
mit3<-read.delim("mit03.txt", na.strings = c("", " "))
colnames(mit3)<-"text"
eliminar<-c("^#.*#$","^(\\d){2,}$",
            "(.*)REGISTER OF GRADUATES(.*)", 
            "(.*)AND OCCUPATION(.*)", 
            "(.*)MASSACHUSETTS INSTITUTE(.*)")
eliminar<-paste(eliminar, collapse="|")
mit3$text<-trimws(str_replace(mit3$text, eliminar, ""), which="both")
mit3$text<-trimws(str_replace(mit3$text, "^(\\s+)$", ""), which="both")
mit3$text<-ifelse(mit3$text=="", NA, mit3$text)
mit3<-subset(mit3,!is.na(mit3$text))
mit3$year<-str_extract(mit3$text, "^18(\\d){2}(?=[.]$)|^19(\\d){2}(?=[.]$)")
degree<-c("(I[.])","(II[.])","(III[.])","(IV[.])", "(V[.])","(VI[.])","(VII[.])",
          "(VIII[.])","(IX[.])","(X[.])","(XI[.])","(XII[.])","(XIII[.])","(Sci[.](.*?))","(Phil[.])")
degree<-paste(degree, collapse = "|")
  mit3$|<-str_extract(mit3$text, degree)
mit3$name<-str_extract(mit3$text, "(.*?)(?=\\(I)|(.*?)(?=\\(X)|(.*?)(?=\\(V)|(.*?)(?=\\(Sci)")
mit3$loc<-ifelse(!is.na(mit3$name), str_extract(mit3$text, "(?<=\\)).*"), NA)
for(i in 1:10){
mit3$loc<-str_replace_all(mit3$loc, "^[[:punct:]]|^(\\s+)", "")
}
mit3<-fill(mit3, year,.direction="down")
mit3$text<-ifelse(is.na(str_extract(mit3$text, "^18(\\d){2}(?=[.]$)|^19(\\d){2}(?=[.]$)|Continued")), mit3$text, NA)
mit3<-subset(mit3, !is.na(mit3$text))
mit3$details<-ifelse(!is.na(mit3$name), NA, 
                     ifelse(!is.na(lead(mit3$name)), mit3$text, 
                            ifelse(!is.na(str_extract(mit3$text, "-$")), 
                                   paste(mit3$text, lead(mit3$text), sep=""), paste(mit3$text, lead(mit3$text), sep=" "))))
mit3$details<-ifelse(!is.na(lag(mit3$details)), NA, mit3$details)
mit3$details2<-ifelse(!is.na(mit3$name)&!is.na(lead(mit3$details)), lead(mit3$details), NA)
mit3$details2<-str_replace(mit3$details2, "\\(Mrs.*?\\)(\\s)", "")
mit3<-subset(mit3, !is.na(mit3$details2))
mit3$org<-str_extract(mit3$details2, "(?<=[,]).*")
mit3$prof<-ifelse(is.na(mit3$org),str_extract(mit3$details2, ".*?(?=[.])"), 
                  str_extract(mit3$details2, ".*?(?=[,])"))
mit3$org<-trimws(mit3$org, which="both")
mit3$prof<-trimws(mit3$prof, which="both")
mit3<-fill(mit3, c("name", "loc", "degree"), .direction="down")
mit3<-separate_rows(mit3, prof, sep="(\\s)and(\\s)|[;](\\s)")
mit03<-buscar2(mit3)
degree<-read.csv("keymit.csv")
mit03$degree<-str_replace(mit03$degree, "[.]", "")
mit03$key<-mit03$degree
mit03$degree<-NULL
mit03<-left_join(mit03, degree, by=("key"))
write.csv(mit03, "mit03.csv")

####II. 2. 1915. list#########
##it is a lot of f#$%#$% information about the MIT##
####II. 2.a first, making a list matching formation with location####
mit1 <- read_csv("mit1.csv")
mit1$loc<-paste( mit1$att17, mit1$att18, sep=" ")
mit1$loc<-trimws(str_replace_all(mit1$loc, "[^[:alpha:]]", " "), which=c("both"))
write.csv(mit1, "mit2.csv")
#cleaning the loc column visually, then reloading
mit2 <- read_csv("mit2b.csv")
mit2$loc3<-word(mit2$loc, -1)
mit2$loc2<-word(mit2$loc, -2)
mit2$loc1<-word(mit2$loc, -3)
mit2$loc0<-paste(mit2$loc1, mit2$loc2, mit2$loc3, sep=" ")
write.csv(mit2, "mit3.csv")
#cleaning the loc column visually, then reloading
mit3 <- read_csv("mit3b.csv")
mit3[is.na(mit3)]<-0
mit3$rest<-paste(mit3$att4, mit3$att5, mit3$att6, mit3$att7, mit3$att8, mit3$att9, mit3$att10, mit3$att11, mit3$att12, mit3$att13, mit3$att14, mit3$att15, mit3$att16, mit3$att17, mit3$att18, sep=",")
mit3$att4	<-	NULL
mit3$att5	<-	NULL
mit3$att6	<-	NULL
mit3$att7	<-	NULL
mit3$att8	<-	NULL
mit3$att9	<-	NULL
mit3$att10	<-	NULL
mit3$att11	<-	NULL
mit3$att12	<-	NULL
mit3$att13	<-	NULL
mit3$att14	<-	NULL
mit3$att15	<-	NULL
mit3$att16	<-	NULL
mit3$att17	<-	NULL
mit3$att18	<-	NULL
mitt3$loc2 <- mit3$Location
mit3$Location <- NULL
mit3$rest <- trimws(str_replace_all(mit3$rest, "[0,]", " "), which=c("both"))
write.csv(mit3, "mit4.csv")
##going to different approach, list of locations 
mitgeo <- read_csv("mitgeo.csv")
#count number of words
mitgeo$words<-sapply(strsplit(mitgeo$name, " "), length)
write.csv(mitgeo, "mitgeo2.csv")
mitgeo2 <- read_csv("mitgeo2b.csv")
mitgeo2$class<-trimws(substrRight(mitgeo2$name, 3))
mitgeo2$name<-trimws(str_replace_all(mitgeo2$name, "[^[:alpha:]]", " "), which=c("both"))
write.csv(mitgeo2, "mitgeo3.csv")
##integrate the last versions of two analysis, mit and mitgeo, in order to obtain maximum information
mitt <- read_csv("mitt.csv")
mitt$name<-tolower(trimws(str_replace_all(mit$name, "[^[:alpha:]]", " "), which=c("both")))
mitt$loc<-tolower(trimws(str_replace_all(mit$loc, "[^[:alpha:]]", " "), which=c("both")))
mitt$class<-trimws(str_replace_all(mit$class, "[^[:alpha:]]", " "), which=c("both"))
mitt$year<-trimws(str_replace_all(mit$year, "[^[:xdigit:]]", " "), which=c("both"))
write.csv(mitt, "mitt2.csv")
##process through openrefine
mitt2 <- read_csv("mitt2b.csv")
write.csv(mitt2, "mitt3.csv")
#rearrange names into initials
mitt3 <- read_csv("mitt3b.csv")
write.csv(mitt3, "mitt4.csv")
#recleaning 
mitt4 <- read_csv("mitt4b.csv")
mitll<-buscar(mitt4$loc)
mitt4$lon<-mitll$lon
mitt4$lat<-mitll$lat
write.csv(mitt4, "mitt5.csv")
mittNA<-rbind(mitt4[is.na(mitt4$lon), ], mitt4[which(mitt4$loc==FALSE), ])
write.csv(mittNA, "mittNA.csv")
mittNAb <- read_csv("mittNAb.csv")
mittFALSE <- read_csv("mittFALSE.csv")
mittnall<-buscar(mittNAb$loc)
mittNAb$lon<-mitnall$lon
mittNAb$lat<-mitnall$lat
mitt5<-rbind(mitt4[!is.na(mitt4$lon), ], mittNAb)
mitt5<-rbind(mittFALSE, mitt5[which(!mitt5$loc==FALSE),])
write.csv(mitt5, "mitt5.csv")
#eliminate duplicates. use excel to avoid the repetition of loc even with different grammar
mitt6<- read_csv("mitt5b.csv")
mitt6$`original name`<-NULL
mitt6$id<-NULL
mitt6$source<-"MIT"
mitt6$simpname<-mitt6$`simplified name`
mitt6$`simplified name`<-NULL
write.csv(mitt6, "mitt6.csv" )
#cleaning the class and inserting the key.
mitt7<- read_csv("mitt6b.csv")
#### II. 2.b extracting the employment and organization####
mitp<-read_csv("mitp.csv")
mitp$rest2<-trimws(str_replace_all(mitp$rest, "[^[:alpha:].,]", " "), which=c("both"))
mitp$rest2<-str_replace_all(mitp$rest2, "[[:space:]]{2,}", " ")
mitp$rest2<-str_replace_all(mitp$rest2, "[[:space:]]{2,}", " ")
mitp$rest2<-trimws(str_replace_all(mitp$rest2, "engi neer", "engineer"), which=c("both"))
mitp$rest2<-trimws(str_replace_all(mitp$rest2, "super intendent", "superintendent"), which=c("both"))
mitp$rest2<-trimws(str_replace_all(mitp$rest2, "vice president", "vicepresident"), which=c("both"))
mitp$rest2<-tolower(mitp$rest2)
professions<-c("manager", "vicepresident", "superintendent", "president","engineer", "accountant", 
               "agent",  "merchant", "professor", "adjuster", "analyst", "assistant", "abstractor", 
               "official", "artist", "assayer", "associate", "attorney", "author", "bacteriologist", 
               "banker", "bookkeeper", "broker", "builder", "captain", "cashier", "cartoonist", 
               "chairman", "chemist", "clerk", "clergy", "colonel", "constructor", "contractor", 
               "counselor", "custodian", "designer", "dentist", "director", "draftsman", "drawer", 
               "editor", "estimator", "exporter", "farmer", "fellow", "geologist", "geographer", 
               "graduate", "student", "hydrographer", "importer", "inspector", "journalist", 
               "judge", "lawyer", "librarian", "lecturer", "instructor", "lieutenant", "major", 
               "mechanic", "machinist", "manufacturer", "major", "soldier", "master", "metallurgist", 
               "miner", "oculist", "teacher", "physicist", "owner", "petrologist", "pathfinder", 
               "pastor", "expert", "pharmacist", "photographer", "physician", "planter", "producer", 
               "proprietor", "psychologist", "publisher", "pyrologist", "register", "salesman", "specialist", 
               "statistician", "supervisor", "surveyor", "timekeeper", "transitman", "treasurer", "trustee",
               "musician", "researcher", "deputy", "secretary", "architect", "dealer", "electrician", "optician",
               "storekeeper", "inventor", "decorator", "computer", "buyer", "controller", "drugist",
               "foreman", "erector", "investigator", "calculator", "scholar", "died", "deceased", "retired")
profes1<-paste(professions, collapse="|")
mitp$profession<-str_extract( mitp$rest2, profes1)
mitptable<-as.data.frame(table(mitp$profession))
#reorganizing the search terms according to frequency
mitptable<-mitptable[rev(order(mitptable$Freq)), ]
profes2<-as.vector(mitptable$Var1)
profes2<-paste(profes2, collapse="|")
mitp$profession<-str_extract( mitp$rest2, profes1)
##extracting organization
mitp$company<-str_extract( mitp$rest2, "(\\w+)(\\s)(\\w+)(\\s)(\\w+)(\\s)company")
mitp$corpor<-str_extract( mitp$rest2, "(\\w+)(\\s)(\\w+)(\\s)(\\w+)(\\s)corpor")
mitp$co<-str_extract( mitp$rest2, "(\\w+)(\\s)(\\w+)(\\s)(\\w+)(\\s)co[.]")
mitp$inc<-str_extract( mitp$rest2, "(\\w+)(\\s)(\\w+)(\\s)inc")
mitp$ltd<-str_extract( mitp$rest2, "(\\w+)(\\s)(\\w+)(\\s)(\\w+)(\\s)ltd")
mitp$dept<-str_extract( mitp$rest2, "(\\w+)(\\s)(\\w+)(\\s)depart")
mitp$dept2<-str_extract( mitp$rest2, "(\\w+)(\\s)(\\w+)(\\s)dept")
mitp$factory<-str_extract( mitp$rest2, "(\\w+)(\\s)(\\w+)(\\s)factory")
mitp$works<-str_extract( mitp$rest2, "(\\w+)(\\s)(\\w+)(\\s)works")
mitp$mine<-str_extract( mitp$rest2, "(\\w+)(\\s)(\\w+)(\\s)mine")
mitp$rail<-str_extract( mitp$rest2, "(\\w+)(\\s)(\\w+)(\\s)railroad")
mitp$shops<-str_extract( mitp$rest2, "(\\w+)(\\s)(\\w+)(\\s)shops")
mitp$comm<-str_extract( mitp$rest2, "(\\w+)(\\s)(\\w+)(\\s)commission")
mitp$university<-str_extract( mitp$rest2, "(\\w+)(\\s)(\\w+)(\\s)(\\w+)(\\s)university")
mitp$corps<-str_extract( mitp$rest2, "(\\w+)(\\s)(\\w+)(\\s)corps")
mitp$divis<-str_extract( mitp$rest2, "(\\w+)(\\s)(\\w+)(\\s)division")
mitp$us<-str_extract( mitp$rest2, "united states(\\s)(\\w+)(\\s)(\\w+)")
mitp$school<-str_extract( mitp$rest2, "(\\w+)(\\s)(\\w+)(\\s)school")
mitp$bank<-str_extract( mitp$rest2, "(\\w+)(\\s)(\\w+)(\\s)(\\w+)(\\s)bank")
mitp$with<-str_extract( mitp$rest2, "with(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)")
mitp$care<-str_extract( mitp$rest2, "(\\s)care(\\s)of(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)")
mitp$of<-str_extract( mitp$rest2, "(\\s)of(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)")
mitp$organization<-ifelse(!is.na(mitp$company), mitp$company, 
                          ifelse(!is.na(mitp$corpor), mitp$corpor, 
                                 ifelse(!is.na(mitp$co), mitp$co, 
                                        ifelse(!is.na(mitp$inc), mitp$inc,
                                               ifelse(!is.na(mitp$ltd), mitp$ltd,
                                                      ifelse(!is.na(mitp$dept), mitp$dept,
                                                             ifelse(!is.na(mitp$dept2), mitp$dept2,
                                                                    ifelse(!is.na(mitp$factory), mitp$factory,
                                                                           ifelse(!is.na(mitp$works), mitp$works,
                                                                                  ifelse(!is.na(mitp$mine), mitp$mine,
                                                                                         ifelse(!is.na(mitp$rail), mitp$rail,
                                                                                                ifelse(!is.na(mitp$shops), mitp$shops,
                                                                                                       ifelse(!is.na(mitp$comm), mitp$comm,
                                                                                                              ifelse(!is.na(mitp$university), mitp$university,
                                                                                                                     ifelse(!is.na(mitp$corps), mitp$corps,
                                                                                                                            ifelse(!is.na(mitp$divis), mitp$divis,
                                                                                                                                   ifelse(!is.na(mitp$us), mitp$us,
                                                                                                                                          ifelse(!is.na(mitp$school), mitp$school,
                                                                                                                                                 ifelse(!is.na(mitp$bank), mitp$bank,
                                                                                                                                                        ifelse(!is.na(mitp$with), mitp$with,
                                                                                                                                                               ifelse(!is.na(mitp$care), mitp$care,
                                                                                                                                                                      ifelse(!is.na(mitp$of), mitp$of, NA)
                                                                                                                                                               )
                                                                                                                                                        )
                                                                                                                                                 )
                                                                                                                                          )
                                                                                                                                   )
                                                                                                                            )
                                                                                                                     )
                                                                                                              )
                                                                                                       )
                                                                                                )
                                                                                         )
                                                                                  )
                                                                           )
                                                                    )
                                                             )
                                                      )
                                               )
                                        )
                                 )
                          )
)
write.csv(mitp, "mitpbackup2.csv")
#separate the non classified results
mitpa<-mitp[which(!is.na(mitp$organization)), ]
mitpb<-mitp[which(is.na(mitp$organization)), ]
#reprocessing the unclssified without ponction
mitpb$rest2<-str_replace_all(mitpb$rest2, "[^[:alpha:]]", " ")
mitpb$rest2<-trimws( str_replace_all(mitpb$rest2, "\\s+", " "), which=c("both"))
mitpb$rest2<-trimws( str_replace_all(mitpb$rest2, "\\s+", " "), which=c("both"))
mitpb$company<-str_extract( mitpb$rest2, "(\\w+)(\\s)(\\w+)(\\s)(\\w+)(\\s)company")
mitpb$corpor<-str_extract( mitpb$rest2, "(\\w+)(\\s)(\\w+)(\\s)(\\w+)(\\s)corpor")
mitpb$co<-str_extract( mitpb$rest2, "(\\w+)(\\s)(\\w+)(\\s)(\\w+)(\\s)co[.]")
mitpb$inc<-str_extract( mitpb$rest2, "(\\w+)(\\s)(\\w+)(\\s)inc")
mitpb$ltd<-str_extract( mitpb$rest2, "(\\w+)(\\s)(\\w+)(\\s)(\\w+)(\\s)ltd")
mitpb$dept<-str_extract( mitpb$rest2, "(\\w+)(\\s)(\\w+)(\\s)depart")
mitpb$dept2<-str_extract( mitpb$rest2, "(\\w+)(\\s)(\\w+)(\\s)dept")
mitpb$factory<-str_extract( mitpb$rest2, "(\\w+)(\\s)(\\w+)(\\s)factory")
mitpb$works<-str_extract( mitpb$rest2, "(\\w+)(\\s)(\\w+)(\\s)works")
mitpb$mine<-str_extract( mitpb$rest2, "(\\w+)(\\s)(\\w+)(\\s)mine")
mitpb$rail<-str_extract( mitpb$rest2, "(\\w+)(\\s)(\\w+)(\\s)railroad")
mitpb$shops<-str_extract( mitpb$rest2, "(\\w+)(\\s)(\\w+)(\\s)shops")
mitpb$comm<-str_extract( mitpb$rest2, "(\\w+)(\\s)(\\w+)(\\s)commission")
mitpb$university<-str_extract( mitpb$rest2, "(\\w+)(\\s)(\\w+)(\\s)(\\w+)(\\s)university")
mitpb$corps<-str_extract( mitpb$rest2, "(\\w+)(\\s)(\\w+)(\\s)corps")
mitpb$divis<-str_extract( mitpb$rest2, "(\\w+)(\\s)(\\w+)(\\s)division")
mitpb$us<-str_extract( mitpb$rest2, "united states(\\s)(\\w+)(\\s)(\\w+)")
mitpb$school<-str_extract( mitpb$rest2, "(\\w+)(\\s)(\\w+)(\\s)school")
mitpb$bank<-str_extract( mitpb$rest2, "(\\w+)(\\s)(\\w+)(\\s)(\\w+)(\\s)bank")
mitpb$with<-str_extract( mitpb$rest2, "with(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)")
mitpb$care<-str_extract( mitpb$rest2, "(\\s)care(\\s)of(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)")
mitpb$of<-str_extract( mitpb$rest2, "(\\s)of(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)")
mitpb$organization<-ifelse(!is.na(mitpb$company), mitpb$company, 
                           ifelse(!is.na(mitpb$corpor), mitpb$corpor, 
                                  ifelse(!is.na(mitpb$co), mitpb$co, 
                                         ifelse(!is.na(mitpb$inc), mitpb$inc,
                                                ifelse(!is.na(mitpb$ltd), mitpb$ltd,
                                                       ifelse(!is.na(mitpb$dept), mitpb$dept,
                                                              ifelse(!is.na(mitpb$dept2), mitpb$dept2,
                                                                     ifelse(!is.na(mitpb$factory), mitpb$factory,
                                                                            ifelse(!is.na(mitpb$works), mitpb$works,
                                                                                   ifelse(!is.na(mitpb$mine), mitpb$mine,
                                                                                          ifelse(!is.na(mitpb$rail), mitpb$rail,
                                                                                                 ifelse(!is.na(mitpb$shops), mitpb$shops,
                                                                                                        ifelse(!is.na(mitpb$comm), mitpb$comm,
                                                                                                               ifelse(!is.na(mitpb$university), mitpb$university,
                                                                                                                      ifelse(!is.na(mitpb$corps), mitpb$corps,
                                                                                                                             ifelse(!is.na(mitpb$divis), mitpb$divis,
                                                                                                                                    ifelse(!is.na(mitpb$us), mitpb$us,
                                                                                                                                           ifelse(!is.na(mitpb$school), mitpb$school,
                                                                                                                                                  ifelse(!is.na(mitpb$bank), mitpb$bank,
                                                                                                                                                         ifelse(!is.na(mitpb$with), mitpb$with,
                                                                                                                                                                ifelse(!is.na(mitpb$care), mitpb$care,
                                                                                                                                                                       ifelse(!is.na(mitpb$of), mitpb$of, NA)
                                                                                                                                                                )
                                                                                                                                                         )
                                                                                                                                                  )
                                                                                                                                           )
                                                                                                                                    )
                                                                                                                             )
                                                                                                                      )
                                                                                                               )
                                                                                                        )
                                                                                                 )
                                                                                          )
                                                                                   )
                                                                            )
                                                                     )
                                                              )
                                                       )
                                                )
                                         )
                                  )
                           )
)


mitp<-rbind(mitpa, mitpb)
mitp2a<-mitp[which(mitp$loc!=0),]
mitp2b<-mitp[which(mitp$loc==0),]
mitp2b$lon<-NA
mitp2b$lat<-NA
mitpll<-buscar(mitp2a$loc)
mitp2a$lon<-mitpll$lon
mitp2a$lat<-mitpll$lat
mitp2<-rbind(mitp2a, mitp2b)
write.csv(mitp2, "mitp2.csv")
#here I insert manually mitt4 as I had ommited the classificiation of education on this analysis
mitp2<-read.csv("mitp2.csv")
##making again the list, now with occupation and organization
mitemploy<-as.data.frame(mitp2$year)
colnames(mitemploy)[1]<-"year"
mitemploy$lastname<-mitp2$name
mitemploy$firstname<-word(mitp2$surname, 1)
mitemploy$middlename<-word(mitp2$surname, 1)
mitemploy$simpname<-tolower(trimws(paste(mitemploy$lastname, ", ", substr(mitemploy$firstname, 1, 1), " " ,substr(mitemploy$middlename, 1, 1), sep="")))
mitemploy$edu<-mitp2$class
mitemploy$loc<-mitp2$loc
mitemploy$prof<-mitp2$profession
mitemploy$org<-trimws(mitp2$organization)
mitemploy$lon<-mitp2$lon
mitemploy$lat<-mitp2$lat
mitemperr<-mitemploy[which(is.na(mitemploy$lon) & mitemploy$loc!=0),]
mitempvcorr<-mitemploy[which(!is.na(mitemploy$lon)| mitemploy$loc==0), ]
write.csv(mitemperr,"mitemployerrors.csv" )
#look manually the mistakes
mitemperr2<-read.csv("mitemployerrorb.csv")
mitempvcorr<-rbind(mitempvcorr, mitemperr2[which(is.na(mitemperr2$loc)), ])
mitemperr2<- mitemperr2[which(!is.na(mitemperr2$loc)), ]
mitemperrll<-buscar(as.character(mitemperr2$loc))
mitemperr2$lon<-mitemperrll$lon
mitemperr2$lat<-mitemperrll$lat
mitemploy2<-rbind(mitemperr2, mitempvcorr)
write.csv(mitemploy2,"mitemploy2.csv")
##integrating with geo mitt5
mitemploy3<-read.csv("mitemploy2b.csv")
mitemploy3<-mitemploy3[order(mitemploy3$simpname), ]
mitemploy3$edu<-toupper(mitemploy3$edu)
mitemploy3$edu<-gsub("^$", NA, mitemploy3$edu)
mitemploy3$edu<-gsub("0", NA, mitemploy3$edu)
mitemploy3$middlename<-gsub("^$", NA, mitemploy3$middlename)
mitemploy3$middlename<-gsub("0", NA, mitemploy3$middlename)
mitemploy3$prof<-gsub("^$", NA, mitemploy3$prof)
mitemploy3$prof<-gsub("0", NA, mitemploy3$prof)
mitemploy3$loc<-gsub("^$", NA, mitemploy3$loc)
mitemploy3$loc<-gsub("0", NA, mitemploy3$loc)
#mitemploy3$lastname<-repetir(mitemploy3$lastname, rfin, 100)
mitemploy3$firstname<-ifelse(
  (nchar(mitemploy3$firstname))>1, 
  mitemploy3$firstname, 
  ifelse((mitemploy3$lastname==lag(mitemploy3$lastname, 1))&(substr(mitemploy3$firstname, 1, 1)==lag(substr(mitemploy3$firstname, 1, 1), 1)), 
         lag(mitemploy3$firstname, 1),                                                     
         ifelse((mitemploy3$lastname==lead(mitemploy3$lastname, 1))&(substr(mitemploy3$firstname, 1, 1)==lead(substr(mitemploy3$firstname, 1, 1), 1)), 
                lead(mitemploy3$firstname, 1), NA)))
mitemploy3$middlename<-ifelse(
  !is.na(mitemploy3$middlename), mitemploy3$middlename, 
  ifelse((mitemploy3$lastname==lag(mitemploy3$lastname, 1))&(mitemploy3$firstname==lag(mitemploy3$firstname, 1)), 
         lag(mitemploy3$middlename, 1),  NA))
mitemploy3$middlename<-ifelse(
  !is.na(mitemploy3$middlename), mitemploy3$middlename, 
  ifelse((mitemploy3$lastname==lead(mitemploy3$lastname, 1))&(mitemploy3$firstname==lead(mitemploy3$firstname, 1)), 
         lead(mitemploy3$middlename, 1),  NA))
#mitemploy3$firstname<-repetir(mitemploy3$firstname, rfin, 100)
mitemploy3$simpname<-tolower(trimws(paste(mitemploy3$lastname, ", ", substr(mitemploy3$firstname, 1, 1), " " ,
                                          ifelse(!is.na(substr(mitemploy3$middlename, 1, 1)), substr(mitemploy3$middlename, 1, 1), ""), sep="")))      
mitemploy3<-mitemploy3[order(mitemploy3$simpname), ]
mitemploy3$firstname<-ifelse(
  (nchar(mitemploy3$firstname))>1, 
  mitemploy3$firstname, 
  ifelse((mitemploy3$lastname==lag(mitemploy3$lastname, 1))&(substr(mitemploy3$firstname, 1, 1)==lag(substr(mitemploy3$firstname, 1, 1), 1)), 
         lag(mitemploy3$firstname, 1),                                                     
         ifelse((mitemploy3$lastname==lead(mitemploy3$lastname, 1))&(substr(mitemploy3$firstname, 1, 1)==lead(substr(mitemploy3$firstname, 1, 1), 1)), 
                lead(mitemploy3$firstname, 1), NA)))
mitemploy3$middlename<-ifelse(
  !is.na(mitemploy3$middlename), mitemploy3$middlename, 
  ifelse((mitemploy3$lastname==lag(mitemploy3$lastname, 1))&(mitemploy3$firstname==lag(mitemploy3$firstname, 1)), 
         lag(mitemploy3$middlename, 1),  NA))
mitemploy3$middlename<-ifelse(
  !is.na(mitemploy3$middlename), mitemploy3$middlename, 
  ifelse((mitemploy3$lastname==lead(mitemploy3$lastname, 1))&(mitemploy3$firstname==lead(mitemploy3$firstname, 1)), 
         lead(mitemploy3$middlename, 1),  NA))
mitemploy3$loc<-tolower(mitemploy3$loc)
mitemploy3$edu<-str_replace_all(mitemploy3$edu, "(\\s)", "")
mitemploy3$edu<-str_replace_all(mitemploy3$edu, "^IDS$", "III")
mitemploy3$edu<-str_replace_all(mitemploy3$edu, "^VU$", "VII")
mitemploy3$edu<-str_replace_all(mitemploy3$edu, "^VOL$", "VII")
mitemploy3$edu<-str_replace_all(mitemploy3$edu, "^IR$", "IX")
mitemploy3$edu<-str_replace_all(mitemploy3$edu, "^ISS$", "IX")
mitemploy3$edu<-str_replace_all(mitemploy3$edu, "^IIX$", "IX")
mitemploy3$edu<-str_replace_all(mitemploy3$edu, "^VV$", "V")
mitemploy3$edu<-str_replace_all(mitemploy3$edu, "^VIV$", "VII")
mitemploy3$edu<-str_replace_all(mitemploy3$edu, "VV", "V")
mitemploy3$edu<-str_replace_all(mitemploy3$edu, "H", "II")
mitemploy3$edu<-str_replace_all(mitemploy3$edu, "N", "II")
mitemploy3$edu<-str_replace_all(mitemploy3$edu, "M", "II")
mitemploy3$edu<-str_replace_all(mitemploy3$edu, "L", "I")
mitemploy3$edu<-str_replace_all(mitemploy3$edu, "T", "I")
mitemploy3$edu<-str_replace_all(mitemploy3$edu, "U", "V")
mitemploy3$edu<-str_replace_all(mitemploy3$edu, "O", "X")
mitemploy3$edu<-str_replace_all(mitemploy3$edu, "IIII", "III")
edu<-c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII", "XIII", "XIV", "XV", "XVI")
edu2<-c("civil", "mechanical", "mining", "architecture", "chemistry", "electrical", "biology", "physics", "general", "chemical", "sanitary", "geology", "marine", "economics", "management", "aeronautics")
tablemit<-data.frame(edu, edu2)
mitemploy3<-left_join(mitemploy3, tablemit, by="edu")
mitemploy3$edu<-mitemploy3$edu2
mitemploy3$edu2<-NULL
#mitfinal2<-mitemploy3[which((mitemploy3$year==1915)), ]
#mitfinal3<-mitemploy3[which((mitemploy3$year!=1915)), ]
#mitfinal3$year<-1915
#mitfinal3$prof<-ifelse((mitfinal3$prof=="student"), NA, mitfinal3$prof)
#mitfinal3$org<-ifelse((mitfinal3$org=="MIT"), NA, mitfinal3$org)
#mitfinal4<-mitemploy3[which((mitemploy3$year!=1915)), ]
#mitfinal<-distinct(rbind( mitfinal2, mitfinal3, mitfinal4))
mitfinal<-mitemploy3
mitfinal$nationality<-NA
mitfinal$source<-"MIT"
mitfinal$type<-"alumni"
mitfinal<-distinct(mitfinal)
mitfinal<-mitfinal[order(mitfinal$simpname), ]
write.csv(mitfinal,"mitfinal.csv")
####II. 3. MIT list 1940####
mit40<-read.delim("livingmit.txt", na.strings = c("", " ", "MASSACHUSETTS INSTITUTE OF TECHNOLOGY", "LIVING FORMER STUDENTS"))
colnames(mit40)<-"text"
mit40$text<-ifelse(!is.na(str_extract(mit40$text, "######|^$")), NA, mit40$text)
mit40<-subset(mit40, !is.na(mit40$text))
mit40$id<-rownames(mit40)
mit40$text<-str_replace_all(mit40$text, "(\\s+)", " ")
mit40$text<-str_replace_all(mit40$text, "^[-]{1,}", "-")
mit40$apellido<-str_extract(mit40$text, "(?<=^)[A-Z]{3,}[-][A-Z]{3,}(?=(\\s)[A-Z][a-z])|(?<=^)Mc[A-Z]{3,}(\\s+)[A-Z]{3,}(?=(\\s)[A-Z][a-z])|(?<=^)Mc[A-Z]{3,}(?=(\\s)[A-Z][a-z])|(?<=^)[A-Z]{3,}(\\s+)[A-Z]{3,}(?=(\\s)[A-Z][a-z])|(?<=^)[A-Z]{3,}(?=(\\s)[A-Z][a-z])")
mit40$apellido<-ifelse(!is.na(str_extract(mit40$text, "^NYC|^USA|^USN|^II")), NA, mit40$apellido)
mit40<-separate_rows(mit40, text,sep="(?<=[A-Z]{3})(\\s)(?=[A-Z][a-z])")
mit40$text<-trimws(str_replace_all(mit40$text, "^(\\s)", ""), which="both")
mit40$nombre<-str_extract(mit40$text, "(?<=^[-])(\\w+)(.*?)(?=(\\d){2}(\\b))|(?<=^[-](\\s))(\\w+)(.*?)(?=(\\d){2}(\\b))")
mit40$nombre<-ifelse(!is.na(lag(mit40$apellido))&(lag(mit40$apellido)==lag(mit40$text)), str_extract(mit40$text, "^(.*?)(?=(\\d){2}(\\b))"), mit40$nombre)
#mit40$apellido<-ifelse(!is.na(mit40$apellido)&(!is.na(lead(mit40$apellido)))&(mit40$apellido==lead(mit40$apellido))&!is.na(mit40$nombre),NA, mit40$apellido)
mit40$apellido<-ifelse(!is.na(mit40$apellido)&is.na(mit40$nombre), NA, mit40$apellido)
temp<-mit40[which(!is.na(mit40$apellido)),]
temp$apellidoF<- ifelse(is.na(lag(temp$apellido))|is.na(lag(temp$apellido, 2))|is.na(lead(temp$apellido))|is.na(lead(temp$apellido, 2)), temp$apellido,  
                        ifelse(str_extract(temp$apellido, "^(\\w){1}")==str_extract(lag(temp$apellido), "^(\\w){1}")|
                                 str_extract(temp$apellido, "^(\\w){1}")==str_extract(lead(temp$apellido), "^(\\w){1}")|
                                 str_extract(temp$apellido, "^(\\w){1}")==str_extract(lag(temp$apellido,2), "^(\\w){1}")|
                                 str_extract(temp$apellido, "^(\\w){1}")==str_extract(lead(temp$apellido,2), "^(\\w){1}"),
                               temp$apellido, #this pretty long ifelse tries to situate apellidos that begin with the same letter than other contextual apellidos (lags and lead with to rows above)
                               ifelse((match(str_extract(temp$apellido, "^(\\w){1}"), LETTERS[1:26])-match(str_extract(lag(temp$apellido), "^(\\w){1}"), LETTERS[1:26])>=0)&
                                        (match(str_extract(temp$apellido, "^(\\w){1}"), LETTERS[1:26])-match(str_extract(lag(temp$apellido), "^(\\w){1}"), LETTERS[1:26])<=2), 
                                      temp$apellido, #this pretty hard rule applies for all other apellidos that do not belong to the previous patterns, breaking the alphabetical order
                                      NA)))
temp$apellidoF<-str_replace_all(temp$apellidoF, "(?<=[A-Z]{1})(\\s)?(?=[a-z])", "")
temp$len<-nchar(temp$apellidoF)
mit40<-left_join(mit40, subset(temp, select=c("id", "apellidoF")), by="id")
mit40<-fill(mit40, apellidoF, .direction = "down")
key<-c("(?<=(\\d){2}(\\s))AO",
       "(?<=(\\d){2}(\\s))Ceramics",
       "(?<=(\\d){2}(\\s))Ec(\\s)[&]",
       "(?<=(\\d){2}(\\s))F(\\s)[&]", 
       "(?<=(\\d){2}(\\s))Meteor", 
       "(?<=(\\d){2}(\\s))Sci", 
       "(?<=(\\d){2}(\\s))SMA",
       "(?<=(\\d){2}(\\s))XIX", 
       "(?<=(\\d){2}(\\s))XVIII","
       (?<=(\\d){2}(\\s))XVII",
       "(?<=(\\d){2}(\\s))XVI",
       "(?<=(\\d){2}(\\s))XV",
       "(?<=(\\d){2}(\\s))XIV",
       "(?<=(\\d){2}(\\s))XIII",
       "(?<=(\\d){2}(\\s))XII",
       "(?<=(\\d){2}(\\s))XI",
       "(?<=(\\d){2}(\\s))IX",
       "(?<=(\\d){2}(\\s))X",
       "(?<=(\\d){2}(\\s))VIII",
       "(?<=(\\d){2}(\\s))VII",
       "(?<=(\\d){2}(\\s))VI",
       "(?<=(\\d){2}(\\s))IV",
       "(?<=(\\d){2}(\\s))V",
       "(?<=(\\d){2}(\\s))III",
       "(?<=(\\d){2}(\\s))II",
       "(?<=(\\d){2}(\\s))I")
key2<-paste(key, collapse="|")
mit40$key<-str_extract(mit40$text, key2)
degree<-read.csv("keymit.csv")
degree2<-c("PhD", "ScD", "SB", "DrPH", "EngD", "BArch", "CPH", "SM", "MArch", "MCP")
degree2<-paste(degree2, collapse="|")
mit40<-left_join(mit40, degree, by="key")
mit40$nombre<-ifelse(!is.na(mit40$key)&is.na(mit40$nombre), str_extract(mit40$text, "(?<=^)[A-Z][a-z](.*?)(?=(\\d){2}(\\b))|(?<=^(\\s))[A-Z][a-z](.*?)(?=(\\d){2}(\\b))"), mit40$nombre)
mit40$gradyear<-ifelse(!is.na(mit40$nombre), str_extract(mit40$text, "(?<=[a-z](\\s))(\\d){2}(\\b)"), NA)
mit40$text<-str_replace(mit40$text, "[-]$", "")
mit42<-mit40[which(mit40$text!=mit40$apellidoF),]
mit42$details<-pegar(mit42$text, mit42$nombre)
mit42<-separate(mit42, details,into=c("temp", "details"), sep=key2)
mit42$temp<-NULL
mit42$details<-str_replace(mit42$details, "^(\\s+)", "")
mit42$degree2<-str_extract(mit42$details, degree2)
mit42<-separate(mit42, details,into=c("temp", "details"), sep=degree2)
mit42$temp<-NULL
mit42$details<-str_replace(mit42$details, "^(\\s+)", "")
mit42<-mit42[which(!is.na(mit42$details)),]
mit42$details<-trimws(mit42$details, which="both")
mit42<-separate(mit42, details, into=c("profes", "location"), sep="(\\s)(?=(\\d))|(?=<(\\b)Co(\\b))(\\s)|(?=<(\\b)Corp(\\b))(\\s)|(?=<(\\b)Inc(\\b))(\\s)")
mit42$location<-ifelse(!is.na(str_extract(mit42$location, "^(\\d+)$|^(\\d+)(\\s+)$")), NA, mit42$location)
mit42$profes<-ifelse(mit42$profes==mit42$key, NA, mit42$profes)
mit42$loc<-mit42$location
mit42<-buscar2(mit42)
profesiones<-c("Engr", "Engineer", "Supt", "Superintendent", "Mgr", "Manager", "Surveyor", "Geologist", "Agent",
               "Boss", "Chemist", "Assayer", "Foreman", "Metallurgist", "Dept.", "Technic", "Experimental Work", 
               "manager", "vicepresident", "superintendent", "president","engineer", "accountant", "agent",  "merchant", 
               "professor", "adjuster", "analyst", "assistant", "abstractor", "official", "artist", "assayer", 
               "associate", "attorney", "author", "bacteriologist", "banker", "bookkeeper", "broker", "builder", 
               "captain", "cashier", "cartoonist", "chairman", "chemist", "clerk", "clergy", "colonel", "constructor", 
               "contractor", "counselor", "custodian", "designer", "dentist", "director", "draftsman", "drawer", 
               "editor", "estimator", "exporter", "farmer", "fellow", "geologist", "geographer", "graduate", "student", 
               "hydrographer", "importer", "inspector", "journalist", "judge", "lawyer", "librarian", "lecturer", "instructor", 
               "lieutenant", "major", "mechanic", "machinist", "manufacturer", "major", "soldier", "master", "metallurgist",
               "miner", "oculist", "teacher", "physicist", "owner", "petrologist", "pathfinder", "pastor", "expert", "pharmacist",
               "photographer", "physician", "planter", "producer", "proprietor", "psychologist", "publisher", "pyrologist", "register",
               "salesman", "specialist", "statistician", "supervisor", "surveyor", "timekeeper", "transitman", "treasurer", "trustee", 
               "musician", "researcher", "deputy", "secretary", "architect", "dealer", "electrician", "optician", "storekeeper", 
               "inventor", "decorator", "computer", "buyer", "controller", "drugist", "foreman", "erector", "investigator", 
               "calculator", "scholar", "died", "deceased", "retired")
profesiones<-tolower(profesiones)
profs1<-paste("(\\w+)(\\s+)", profesiones, sep="")
profs2<-paste(profesiones, "(\\s+)(\\w+)",sep="")
profs3<-rbind(profesiones, profs1, profs2)
profs3<-paste(profs3, collapse ="|")
mit42$role<-str_extract(mit42$profes, profs3)
mit42$location<-str_replace_all(mit42$location, "TH", "Hawaii")
mit42$location<-str_replace_all(mit42$location, "PI", "Philippines")
abbrevm<-read.csv("abrevmit.csv")
abbrevm$abrevia<-paste("(\\b)",abbrevm$abrevia, "(\\b)", sep="")
abbrevm$clave[1]
for(i in 1:nrow(abrevia)){
  mit42$profes<-str_replace_all(mit42$profes, abbrevm$abrevia[i], abbrevm$clave[i])
  mit42$location<-str_replace_all(mit42$location, abbrevm$abrevia[i], abbrevm$clave[i])
}
abbr2<-read.csv("abrevmit2.csv")
for(i in 1:nrow(abbr2)){
  mit42$role<-ifelse(is.na(mit42$role), 
                     str_extract(mit42$profes, abbr2$clave[i]), mit42$role)
}
states<-read.csv("states.csv")
colnames(states)<-"place"
countries<-read.csv("countries.csv")
colnames(countries)<-"place"
lugares<-rbind(states, countries)
for(i in 1:nrow(lugares)){
  mit42$loc<-ifelse(is.na(mit42$lon)&is.na(mit42$loc), 
                    str_extract(mit42$profes, lugares$place[i]),mit42$loc)
}
temp3<-mit42[which(is.na(mit42$lon)&!is.na(mit42$loc)),]
temp3$loc<-str_replace_all(temp3$loc, "[^[:alpha:]]", " ")
temp3$loc<-str_replace_all(temp3$loc, "(\\s+)", " ")
temp3$loc<-trimws(str_replace_all(temp3$loc, "^(\\s+)|(\\s+)$", ""), which="both")
temp3$lon<-NULL
temp3$lat<-NULL
temp3<-buscar2(temp3)
mit42<-rbind(mit42[which(!is.na(mit42$lon)|is.na(mit42$loc)),], temp3)
mit42<-mit43[order(mit43$id),]
temp4<-mit42[which(is.na(mit42$lon)&!is.na(mit42$profes)),]
temp4$lon<-NULL
temp4$lat<-NULL
temp4$loc<-str_extract(temp4$profes, "(\\w+)(\\s+)(\\w+)$")
temp4<-buscar2(temp4)
mit43<-rbind(mit42[which(!is.na(mit42$lon)|is.na(mit42$profes)),], temp4)
mit42<-mit43[order(mit43$id),]
orgis<-c("(\\w+)(\\s+)(\\w+)(\\s+)(\\w+)(\\s+)Company",
         "(\\w+)(\\s+)(\\w+)(\\s+)(\\w+)(\\s+)Corp",
         "(\\w+)(\\s+)(\\w+)(\\s+)(\\w+)(\\s+)Inc",
         "(\\w+)(\\s+)(\\w+)(\\s+)Company",
         "(\\w+)(\\s+)(\\w+)(\\s+)Corp",
         "(\\w+)(\\s+)(\\w+)(\\s+)Inc",
         "USA","USN","of(\\s+)(\\w+)(\\s+)(\\w+)",
         "with(\\s+)(\\w+)(\\s+)(\\w+)")
orgis<-paste(orgis, collapse="|")
mit42$org<-str_extract(mit42$profes, orgis)
mit42$gradyear<-ifelse(mit42$gradyear>40, mit42$gradyear+1800, 1900+mit42$gradyear)
mitd<-read.delim("deadmit.txt", na.strings = c("", " ", "MASSACHUSETTS INSTITUTE OF TECHNOLOGY", "LIVING FORMER STUDENTS"))
colnames(mitd)<-"text"
mitd$text<-ifelse(!is.na(str_extract(mitd$text, "######|^$")), NA, mitd$text)
mitd<-subset(mitd, !is.na(mitd$text))
mitd$id<-as.numeric(rownames(mitd))+nrow(mit40a)
mitd$text<-str_replace_all(mitd$text, "(\\s+)", " ")
mitd$text<-str_replace_all(mitd$text, "^[-]{1,}", "-")
mitd$apellido<-str_extract(mitd$text, "(?<=^)[A-Z]{3,}[-][A-Z]{3,}(?=(\\s)[A-Z][a-z])|(?<=^)Mc[A-Z]{3,}(\\s+)[A-Z]{3,}(?=(\\s)[A-Z][a-z])|(?<=^)Mc[A-Z]{3,}(?=(\\s)[A-Z][a-z])|(?<=^)[A-Z]{3,}(\\s+)[A-Z]{3,}(?=(\\s)[A-Z][a-z])|(?<=^)[A-Z]{3,}(?=(\\s)[A-Z][a-z])")
mitd$apellido<-ifelse(!is.na(str_extract(mitd$text, "^NYC|^USA|^USN|^II")), NA, mitd$apellido)
mitd<-separate_rows(mitd, text,sep="(?<=[A-Z]{3})(\\s)(?=[A-Z][a-z])")
mitd$text<-trimws(str_replace_all(mitd$text, "^(\\s)", ""), which="both")
mitd$nombre<-str_extract(mitd$text, "^(.*?)(?=(\\d){2}(\\b))")
mitd$nombre<-ifelse(!is.na(lag(mitd$apellido))&(lag(mitd$apellido)==lag(mitd$text)), str_extract(mitd$text, "^(.*?)(?=(\\d){2}(\\b))"), mitd$nombre)
#mitd$apellido<-ifelse(!is.na(mitd$apellido)&(!is.na(lead(mitd$apellido)))&(mitd$apellido==lead(mitd$apellido))&!is.na(mitd$nombre),NA, mitd$apellido)
mitd$apellido<-ifelse(!is.na(mitd$apellido)&is.na(mitd$nombre), NA, mitd$apellido)
temp<-mitd[which(!is.na(mitd$apellido)),]
temp$apellidoF<- ifelse(is.na(lag(temp$apellido))|is.na(lag(temp$apellido, 2))|is.na(lead(temp$apellido))|is.na(lead(temp$apellido, 2)), temp$apellido,  
                        ifelse(str_extract(temp$apellido, "^(\\w){1}")==str_extract(lag(temp$apellido), "^(\\w){1}")|
                                 str_extract(temp$apellido, "^(\\w){1}")==str_extract(lead(temp$apellido), "^(\\w){1}")|
                                 str_extract(temp$apellido, "^(\\w){1}")==str_extract(lag(temp$apellido,2), "^(\\w){1}")|
                                 str_extract(temp$apellido, "^(\\w){1}")==str_extract(lead(temp$apellido,2), "^(\\w){1}"),
                               temp$apellido, #this pretty long ifelse tries to situate apellidos that begin with the same letter than other contextual apellidos (lags and lead with to rows above)
                               ifelse((match(str_extract(temp$apellido, "^(\\w){1}"), LETTERS[1:26])-match(str_extract(lag(temp$apellido), "^(\\w){1}"), LETTERS[1:26])>=0)&
                                        (match(str_extract(temp$apellido, "^(\\w){1}"), LETTERS[1:26])-match(str_extract(lag(temp$apellido), "^(\\w){1}"), LETTERS[1:26])<=2), 
                                      temp$apellido, #this pretty hard rule applies for all other apellidos that do not belong to the previous patterns, breaking the alphabetical order
                                      NA)))
temp$apellidoF<-str_replace_all(temp$apellidoF, "(?<=[A-Z]{1})(\\s)?(?=[a-z])", "")
temp$len<-nchar(temp$apellidoF)
mitd<-left_join(mitd, subset(temp, select=c("id", "apellidoF")), by="id")
mitd$apellidoF[1]<-"ABBOT"
mitd<-fill(mitd, apellidoF, .direction = "down")
mitd$gradyear<-ifelse(!is.na(mitd$nombre), str_extract(mitd$text, "(?<=[a-z](\\s))(\\d){2}(\\b)"), NA)
mit4f<-rbind.fill(mit42, mitd)
mit4f$source<-"MIT"
mit4f$nationality<-NA
mit4f$type<-"alumni"
mit4f$lastname<-str_to_title(mit4f$apellidoF)
mit4f$middlename<-str_extract(mit4f$nombre, "(?<=[a-z](\\s))(\\w+)")
mit4f$firstname<-str_extract(mit4f$nombre, "^(\\w+)")
mit4f$simpname<-sname(mit4f)
mit4f$country<-coords2country(mit4f)
mit4f$edu<-ifelse(!is.na(mit4f$degree), mit4f$degree, mit4f$degree2)
mit4f$prof<-mit4f$role
mit4f$gradyear<-ifelse(mit4f$gradyear<100,
                       ifelse(mit4f$gradyear>40, mit4f$gradyear+1800, 1900+mit4f$gradyear), mit4f$gradyear)
write.csv(mit4f, "mit40f.csv", row.names = F)
####II. 4. MIT 1930####
mit30<-read.delim("mit30.txt", na.strings=c("NA", ""))
colnames(mit30)<-"text"
mit30<-separate_rows(mit30, text, sep = "\n")
mit30<-mit30[which(is.na(str_extract(mit30$text, "^#(.*)#$|^CLASS(\\s)REGISTER$|^$"))),]
mit30$class<-as.character(str_extract(mit30$text, "(?<=CLASS(\\s)OF(\\s))(\\d){4}|18(\\d){2}|18(\\d){2}"))
mit30$class<-ifelse((as.numeric(mit30$class)>1867)&(as.numeric(mit30$class)<1930), mit30$class, NA)
mit30$id<-row.names(mit30)
class<-subset(mit30, select=c("class", "id"))
class<-class[which(!is.na(class$class)),]
for(i in 2:nrow(class)){
  class$class[i]<-ifelse(!is.na(class$class[i-1]), 
                         ifelse(as.numeric(class$class[i])>=as.numeric(class$class[i-1])&as.numeric(class$class[i])-as.numeric(class$class[i-1])<2, 
                                class$class[i],NA),
                         ifelse(as.numeric(class$class[i])>=as.numeric(class$class[i-2])&as.numeric(class$class[i])-as.numeric(class$class[i-2])<2, 
                                class$class[i],NA))
}
mit30$class<-NULL
mit30<-left_join(mit30, class, by="id")
mit30<-fill(mit30, class, .direction = "down")
mit30$text<-ifelse(!is.na(str_extract(mit30$text, "CLASS(\\s+)OF|MASSACHUSETTS(\\s)INSTITUTE(\\s)OF(\\s)TECHNOLOGY")), NA, mit30$text)
mit30$text2<-str_replace_all(mit30$text, "(?<=[A-Z])[.](\\s+)(?=[A-Z][.])", "")
mit30$name<-str_extract(mit30$text2, "^(\\w+)(\\s+)(\\w+)[,](.*?)(?=[[:punct:]])|^(\\w+)[-'](\\w+)[,](.*?)(?=[[:punct:]])|^(\\w+)[,](.*?)(?=[[:punct:]])")
lugares<-read.csv("lugares.csv")
lugares$place<-str_replace(lugares$place, "^(\\s+)", "")
lugares$place<-str_replace(lugares$place, "[.]", "[.]")
lugares<-as.vector(lugares$place)
for(i in 1:nrow(lugares)){
  mit30$name<-ifelse(!is.na(str_extract(mit30$name, lugares[i])), NA, mit30$name)
}
mit30$lastname<-str_extract(mit30$name, "^(.*?)(?=[,])")
mit30$name<-ifelse(nchar(mit30$lastname)<3, NA, mit30$name)
mit30$lastname<-str_extract(mit30$name, "^(.*?)(?=[,])")
mit30<-apellido(mit30, mit30$lastname)
mit30$name<-ifelse(is.na(mit30$apellidoF), NA, mit30$name)
mit30$lastname<-str_extract(mit30$name, "^(.*?)(?=[,])")
mit30$text2<-pegar(mit30$text, mit30$lastname)
mit32<-mit30[which(!is.na(mit30$text2)),]
mit32<-separate_rows(mit32, text2, sep="[*]")
mit32$text2<-str_replace(mit32$text2, "^(\\s+)|NA", "")
mit32$text<-mit32$text2
mit32$text2<-str_replace(mit32$text, "(?<=[A-Z])[.]", "")
mit32<-mit32[which(!is.na(mit32$text2)&(mit32$text2!="")),]
nombres<-c("^(\\w+)(\\s+)(\\w+)[,](.*?)(?=[[:punct:]])",
           "^(\\w+)[-'](\\w+)[,](.*?)(?=[[:punct:]])",
           "^(\\w+)[,](.*?)(?=[[:punct:]])",
           "^(\\w+)(\\s+)(\\w+)[,](\\s+)(\\w+)(\\s+) (\\w+)",
           "^(\\w+)[-'](\\w+)[,](\\s+)(\\w+)(\\s+)(\\w+)",
           "^(\\w+)[,](\\s+)(\\w+)",
           "^(\\w+)(\\s+)(\\w+)[,](\\s+)(\\w+)",
           "^(\\w+)[-'](\\w+)[,](\\s+)(\\w+)",
           "^(\\w+)[,](\\s+)(\\w+)")
mit32$name<-NA
for(i in 1:9){
  mit32$name<-ifelse(is.na(mit32$name), 
                     str_extract(mit32$text2, nombres[i]), 
                     mit32$name)
}
mit32$names<-str_extract(mit32$name, "(?<=[,](\\s)).*")
mit32$middlename<-mname(mit32$names)
mit32$firstname<-fname(mit32$names)
mit32$simpname<-sname(mit32)  
mit32$details<-str_replace(mit32$text2, "^(.*?)[,](.*?)[[:punct:]]", "")
mit32$details<-str_replace(mit32$details, "^(\\s+)|^[*]|^[*](\\s+)", "")
key<-read.csv("keymit.csv")
mit32$degree<-NA
for(i in 1:nrow(key)){
  mit32$degree<-ifelse(is.na(mit32$degree), 
                       str_extract(mit32$details, paste("(\\b)", key$key[i],"(\\b)", sep="")), 
                       mit32$degree)
  mit32$degree<-ifelse(!is.na(mit32$degree), 
                       str_replace(mit32$degree, paste("(\\b)", key$key[i],"(\\b)", sep=""), key$degree[i]), 
                       NA)
}
mit32$text2<-pegar(mit32$text2, mit32$name)
mit32<-mit32[which(!is.na(mit32$text2)),]
mit32$name<-NA
for(i in 1:9){
  mit32$name<-ifelse(is.na(mit32$name), 
                     str_extract(mit32$text, nombres[i]), 
                     mit32$name)
}
mit32$names<-str_extract(mit32$name, "(?<=[,](\\s)).*")
mit32$middlename<-mname(mit32$names)
mit32$firstname<-fname(mit32$names)
mit32$simpname<-sname(mit32)  
mit32$details<-mit32$text2
for(i in 1:9){
  mit32$details<-str_replace(mit32$details, nombres[i], "")
  mit32$details<-str_replace(mit32$details, "^[[:punct:]]|^(\\s+)|(\\b)NA(\\b)|(\\b)NA$", "")
  mit32$details<-ifelse(mit32$details==""|mit32$details=="Jr.", NA, mit32$details)
}
mit32$details<-str_replace(mit32$text2, "^(.*?)[,](.*?)[[:punct:]]", "")
mit32$details<-str_replace(mit32$details, "^(\\s+)|^[*]|^[*](\\s+)|(?<=[a-z])[-|-](?=[a-z])", "")
key<-read.csv("keymit.csv")
mit32$degree<-NA
for(i in 1:nrow(key)){
  mit32$degree<-ifelse(is.na(mit32$degree), 
                       str_extract(mit32$details, paste("(\\b)", key$key[i],"(\\b)", sep="")), 
                       mit32$degree)
  mit32$degree<-ifelse(!is.na(mit32$degree), 
                       str_replace(mit32$degree, paste("(\\b)", key$key[i],"(\\b)", sep=""), key$degree[i]), 
                       NA)
}
profesiones<-c("Engr", "Engineer", "Supt", "Superintendent", "Mgr", "Manager", "Surveyor", "Geologist", "Agent",
               "Boss", "Chemist", "Assayer", "Foreman", "Metallurgist", "Dept.", "Technic", "Experimental Work", 
               "manager", "vicepresident", "superintendent", "president","engineer", "accountant", "agent",  "merchant", 
               "professor", "adjuster", "analyst", "assistant", "abstractor", "official", "artist", "assayer", 
               "associate", "attorney", "author", "bacteriologist", "banker", "bookkeeper", "broker", "builder", 
               "captain", "cashier", "cartoonist", "chairman", "chemist", "clerk", "clergy", "colonel", "constructor", 
               "contractor", "counselor", "custodian", "designer", "dentist", "director", "draftsman", "drawer", 
               "editor", "estimator", "exporter", "farmer", "fellow", "geologist", "geographer", "graduate", "student", 
               "hydrographer", "importer", "inspector", "journalist", "judge", "lawyer", "librarian", "lecturer", "instructor", 
               "lieutenant", "major", "mechanic", "machinist", "manufacturer", "major", "soldier", "master", "metallurgist",
               "miner", "oculist", "teacher", "physicist", "owner", "petrologist", "pathfinder", "pastor", "expert", "pharmacist",
               "photographer", "physician", "planter", "producer", "proprietor", "psychologist", "publisher", "pyrologist", "register",
               "salesman", "specialist", "statistician", "supervisor", "surveyor", "timekeeper", "transitman", "treasurer", "trustee", 
               "musician", "researcher", "deputy", "secretary", "architect", "dealer", "electrician", "optician", "storekeeper", 
               "inventor", "decorator", "computer", "buyer", "controller", "drugist", "foreman", "erector", "investigator", 
               "calculator", "scholar", "died", "deceased", "retired", "Prof")
profesiones<-unique(tolower(profesiones))
profs1<-paste("(\\w+)(\\s+)", profesiones, "(\\b)", sep="")
profs2<-paste("(\\b)",profesiones, "(\\s+)(\\w+)",sep="")
profs3<-paste("(\\b)", profesiones, "(\\b)",sep="")
profs4<-as.vector(rbind(profs1, profs2, profs3))
mit32$role<-NA
for(i in 1:length(profs4)){
  mit32$role<-ifelse(is.na(mit32$role), str_extract(tolower(mit32$details), profs4[i]), mit32$role)
}
mit32$location<-str_extract(mit32$details, "(?<=)(\\d){2}(.*)|(\\w+)(\\s+)St[.].*")
for(i in 1:nrow(lugares)){
  mit32$location<-ifelse(is.na(mit32$location), str_extract(mit32$details, lugares$place[i]), mit32$location)
}
mit32$location<-ifelse(!is.na(str_extract(mit32$location, "(?<=(\\d){2})(.*?)(\\d){2}(.*)$")), 
                       str_extract(str_extract(mit32$location, "(?<=^(\\d){2})(.*)$"), "(\\d){2}(.*)"), mit32$location)
mit32$loc<-mit32$location
mit32<-buscar2(mit32)
mit32$loc<-ifelse(is.na(str_extract(mit32$loc, "[[:alpha:]]")), NA, mit32$loc)
mit32$lon<-ifelse(is.na(str_extract(mit32$loc, "[[:alpha:]]")), NA, mit32$lon)
mit32$lat<-ifelse(is.na(str_extract(mit32$loc, "[[:alpha:]]")), NA, mit32$lat)
lugares2<-paste("(\\b)",lugares2, "(\\b)", sep="")
lugares2<-paste(lugares2, collapse = "|")
for(i in 1:length(lugares)){
  mit32$loc<-ifelse(is.na(mit32$lon), 
                    str_extract(mit32$details, lugares[i]),mit32$loc)
}
mit32$loc<-ifelse(is.na(mit32$lon), 
                  str_extract(mit32$details, lugares2),mit32$loc)
temp3<-mit32[which(is.na(mit32$lon)&!is.na(mit32$loc)),]
temp3$loc<-str_replace_all(temp3$loc, "[^[:alpha:]]", " ")
temp3$loc<-str_replace_all(temp3$loc, "(\\s+)", " ")
temp3$loc<-trimws(str_replace_all(temp3$loc, "^(\\s+)|(\\s+)$", ""), which="both")
temp3$lon<-NULL
temp3$lat<-NULL
temp3<-buscar2(temp3)
mit33<-rbind(mit32[which(!is.na(mit32$lon)|is.na(mit32$loc)),], temp3)
mit32<-mit33[order(as.numeric(mit33$id)),]

orgis<-c("(\\w+)(\\s+)(\\w+)(\\s+)(\\w+)(\\s+)Company",
         "(\\w+)(\\s+)(\\w+)(\\s+)(\\w+)(\\s+)Corp",
         "(\\w+)(\\s+)(\\w+)(\\s+)(\\w+)(\\s+)Inc",
         "(\\w+)(\\s+)(\\w+)(\\s+)Company",
         "(\\w+)(\\s+)(\\w+)(\\s+)Corp",
         "(\\w+)(\\s+)(\\w+)(\\s+)Inc",
         "USA","USN","of(\\s+)(\\w+)(\\s+)(\\w+)",
         "with(\\s+)(\\w+)(\\s+)(\\w+)")
orgis<-paste(orgis, collapse="|")
mit32$org<-str_extract(mit32$details, orgis)
write.csv(mit32, "mit30f.csv", row.names = F)
  
####II. 5 juntando 1930 y 1940####
mit42<-read.csv("mit40f.csv")

mit40<-mit42[which(!is.na(mit42$degree)&!is.na(mit42$gradyear)),]
mit32<-read.csv("mit30f.csv")
mit32$gradyear<-mit32$class
mit30<-mit32[which(!is.na(mit32$degree)&!is.na(mit32$gradyear)),]
mit15=read.csv("mitfinal.csv")
mit15<-mit15[which(mit15$year!=1915),]

####II.6. juntando todo####
mit03<-read.csv("mit03.csv")
mit15<-read.csv("mitfinal.csv", na.strings = c("NA", "", " "))
mit30<-read.csv("mit30f.csv")
mit40<-read.csv("mit40f.csv")
mit15$id<-str_replace(mit15$id, "mio", "15mito")
mit15$id<-str_replace(mit15$id, "mig", "15mitg")
mit30<-mit30[which(!is.na(mit30$simpname)),]
mit30$source<-"MIT"
mit40$source<-"MIT"
mit30$id<-paste("30mit", mit30$id, sep="")
mit40$id<-paste("40mit", mit40$id, sep="")
mit30a<-mit30
mit30a$year<-1930
mit30a$prof<-mit30a$role
mit30a$org<-"MIT"
mit30b<-mit30[(which(!is.na(mit30$class))), ]
mit30b$year<-mit30b$class
mit30b$prof<-"student"
mit31<-rbind(mit30a, mit30b)
mit31<-distinct(mit31)
mit40$gradyear<-ifelse(mit40$gradyear<100,
                       ifelse(mit40$gradyear>40, mit40$gradyear+1800, 1900+mit40$gradyear), mit40$gradyear)
mit40a<-mit40
mit40a$year<-1940
mit40a$org<-"MIT"
mit40b<-mit40[(which(!is.na(mit40$gradyear))), ]
mit40b$prof<-"student"
mit40b$year<-mit40b$gradyear
mit41<-rbind(mit40a, mit40b)
mit41<-distinct(mit41)
mit31$nationality<-NA
mit31$type<-"alumni"
mit31$country<-coords2country(mit31)
mit31$edu<-mit31$degree
mit31$gradyear<-mit31$class
mit15$text<-NA
mit15$country<-coords2country(mit15)
mit03$source<-"MIT"
mit03$name<-str_replace(mit03$name, "[,](.*)|(\\s+)$|[A-Z]{1,2}[.][A-Z][.]$|[[:punct:]]$|^(\\d)(.*?)(?=[A-Z])", "")
mit03$name<-trimws(str_replace(mit03$name, "[,](.*)|(\\s+)$|[A-Z]{1,2}[.][A-Z][.]$|[[:punct:]]$", ""), which="both")
mit03$lastname<-str_extract(mit03$name, "(\\w+)$")
mit03$names<-str_replace(mit03$name, "(\\s+)(\\w+)$", "")
mit03$middlename<-mname(mit03$names)
mit03$firstname<-fname(mit03$names)
mit03$simpname<-sname(mit03)
mit03$country<-coords2country(mit03)
mit03$id<-paste("03mit", mit03$X, sep="")
mit03$nationality<-NA
mit03$type<-"alumni"
mit03$edu<-mit03$degree
mit<-rbind(subset(mit03, select=c("source","year","simpname","nationality", "edu", "prof","org", "loc", "lon", "lat", "id", "lastname", "firstname","middlename", "type", "text", "country")),
           subset(mit15[which(mit15$year!=1915),], select=c("source","year","simpname","nationality", "edu", "prof","org", "loc", "lon", "lat", "id", "lastname", "firstname","middlename", "type", "text", "country")),
           subset(mit41, select=c("source","year","simpname","nationality", "edu", "prof","org", "loc", "lon", "lat", "id", "lastname", "firstname","middlename", "type", "text", "country")),
           subset(mit31, select=c("source","year","simpname","nationality", "edu", "prof","org", "loc", "lon", "lat", "id", "lastname", "firstname","middlename", "type", "text", "country")))
mit$edu<-tolower(mit$edu)
mit$firstname<-str_replace(mit$firstname, "[[:punct:]]", "")
mit$lastname<-str_replace(mit$lasstname, "[[:punct:]]", "")
mit$middlename<-str_replace(mit$middlename, "[[:punct:]]", "")
mit<-mit[order(mit$lastname,mit$firstname, mit$simpname,mit$year), ]
for(i in 2:nrow(mit)){
  mit$middlename[i]<-ifelse(is.na(mit$middlename[i])&!is.na(mit$middlename[i-1])&(mit$lastname[i]==mit$lastname[i-1])&(mit$firstname[i]==mit$firstname[i-1]), 
                            mit$middlename[i-1], mit$middlename[i])
  mit$middlename[i]<-ifelse(is.na(mit$middlename[i])&!is.na(mit$middlename[i+1])&(mit$lastname[i]==mit$lastname[i+1])&(mit$firstname[i]==mit$firstname[i+1]), 
                            mit$middlename[i+1], mit$middlename[i])
  mit$middlename[i]<-ifelse((nchar(mit$middlename[i])==1)&(nchar(mit$middlename[i-1])>1)&!is.na(mit$middlename[i-1])&(mit$lastname[i]==mit$lastname[i-1])&(mit$firstname[i]==mit$firstname[i-1]), 
                            mit$middlename[i-1], mit$middlename[i])
  mit$middlename[i]<-ifelse((nchar(mit$middlename[i])==1)&(nchar(mit$middlename[i+1])>1)&!is.na(mit$middlename[i+1])&(mit$lastname[i]==mit$lastname[i+1])&(mit$firstname[i]==mit$firstname[i+1]), 
                            mit$middlename[i+1], mit$middlename[i])
}
mit$simpname<-sname(mit)
mit<-mit[order(mit$simpname,mit$year), ]
mit$id<-ifelse((is.na(str_extract(mit$id, "mito")))|
                 ((mit$simpname==lag(mit$simpname))&(is.na(str_extract(lag(mit$id), "mito")))), 
               mit$id,  NA)
mit<-mit[which(!is.na(mit$id)),]
mit<-mit[order(mit$simpname, mit$edu),]
mit$edu<-llenar2(mit$edu, mit$simpname)
mit<-mit[order(mit$simpname, mit$year, mit$id),]
mit$g<-ifelse((mit$simpname==lag(mit$simpname))&(mit$year>=lag(mit$year)), 1,NA)
mit<-subset(mit, select=c("source","year","simpname","nationality", "edu", "prof","org", "loc", "lon", "lat", "id", "lastname", "firstname","middlename", "type", "text", "country"))
write.csv(mit, "mitjunto")

####III. Rest of the universities####
##the data from the file alumni.csv is result of a previous scrapping of data via a combination of the ocr in adobe, processed in excel, and ocr in tesseract. The cleaning was made by regex.##
alumnirest <- read_csv("alumnirest.csv")
alumnirest$simpname<-trimws(str_replace_all(alumnirest$simpname,  "[^[:alpha:],]", " "))
alumnirest$loc<-trimws(str_replace_all(alumnirest$loc,  "[^[:alpha:].,]", " "))
alumnirest$org<-trimws(str_replace_all(alumnirest$org,  "[^[:alpha:].,]", " "))
alumnirestb<-alumnirest[which(is.na(alumnirest$lon)&!is.na(alumnirest$loc)), ]
alumniresta<-alumnirest[which(!is.na(alumnirest$lon)|is.na(alumnirest$loc)), ]
alumnirll<-buscar(alumnirestb$loc)
alumnirestb$lon<-alumnirll$lon
alumnirestb$lat<-alumnirll$lat
alumnir<-rbind(alumniresta, alumnirestb)
alumnir$nationality<-NA
alumnir$nationality<-NA
alumniold <- read_csv("alumniold.csv")
alumni<-rbind(alumniold, alumnir)
alumni$simpname<-key_collision_merge(as.character(alumni$simpname))
alumni$simpname<-n_gram_merge(as.character(alumni$simpname))
alumni$simpname<-key_collision_merge(as.character(alumni$simpname))
alumni$simpname<-n_gram_merge(as.character(alumni$simpname))
alumni$simpname<-key_collision_merge(as.character(alumni$simpname))
alumni$simpname<-n_gram_merge(as.character(alumni$simpname))
write.csv(alumni, "alumnifinal.csv")
#correct mistakes and this is the real thing
alumni<-read.csv("alumnifinal2.csv")
alumni$source<-trimws(str_replace_all(alumni$source, "alumni", ""), which=c("both"))
alumni<-alumni[which(alumni$source!="MIT"), ]
alumni$source<-ifelse(alumni$source!="list", alumni$source, alumni$org)
alumni<-alumni[order(alumni$source, alumni$simpname), ]
alumni$id<-paste(substr(alumni$source, 1, 2), rownames(alumni), sep="")
alumni$simpname<-trimws(alumni$simpname, which=c("both"))
alumni$lastname<-str_extract(alumni$simpname, "^(\\w+)")
alumni$firstname<-str_extract(alumni$simpname, ",(\\s)(\\w+)")
alumni$middlename<-ifelse(!is.na(str_extract(alumni$simpname, ",(\\s)(\\w)(\\s)(\\w)")), 
                          str_extract(alumni$simpname, "(\\w)$"), NA)
alumni$firstname<-str_replace_all(alumni$firstname, ",(\\s)", "")
alumni$type<-"alumni"
write.csv(alumni, "alumnirest2.csv")
####IV. Germany ####
####IV.i. difficult list####
freb0<-read.csv("Freibergarianna.csv", na.strings = c("", " ", "n/a",NA))
freb0$id<-rownames(freb0)
freb0<-subset(freb0, !is.na(freb0$text))
freb0$text2<-freb0$text
freb0$text2<-str_replace_all(freb0$text2, "emer[.](\\s)", "")
freb0<-separate_rows(freb0, text2,sep="(?=(\\d){4})")
freb0$year<-ifelse(!is.na(str_extract(freb0$text2,"(\\d){4}")), str_extract(freb0$text2,"(\\d){4}"), freb0$year)
freb0$text2[freb0$text2==""]<-NA
freb0<-subset(freb0, !is.na(freb0$text2))
freb0<-separate_rows(freb0, text2, sep="(\\s+)und(\\s+)|(\\s)u[.](\\s)|(\\s)sp?ter(\\s)|(\\s)dann(\\s)")
locations<-c("(?<=(\\s)beim(\\s))(\\w+)",
             "(?<=(\\s)in(\\s)dem(\\s))(\\w+)",
             "(?<=(\\s)auf(\\s)dem(\\s))(\\w+)",
             "(?<=(\\s)in(\\s)der(\\s))(\\w+)",
             "(?<=(\\s)auf(\\s)der(\\s))(\\w+)",
             "(?<=(\\s)in(\\s)die(\\s))(\\w+)",
             "(?<=(\\s)auf(\\s)die(\\s))(\\w+)",
             "(?<=(\\s)in(\\s)d[.](\\s))(\\w+)",
             "(?<=(\\s)auf(\\s)d[.](\\s))(\\w+)",
             "(?<=(\\s)[z|s|Z|S]u(\\s))(\\w+)", 
             "(?<=(\\s)[z|s|Z|S][.](\\s))(\\w+)")
locations<-paste(locations, collapse="|")
freb0$location<-str_extract(freb0$text2, locations)
freb0$location<-ifelse(!is.na(freb0$location), freb0$location, str_extract(freb0$text2, "(?<=(\\s)in(\\s))(\\w+)|(?<=(\\s)auf(\\s))(\\w+)"))
freb0$location<-ifelse(is.na(freb0$location), "Freiberg", freb0$location)
freb0$prof<-str_extract_all(freb0$text2, "(?<=[a|A]ls(\\s))(\\w+)[-](\\w+)|(\\w+)[-](\\w+)(?=(\\s)[a|A]n)|(?<=[w|W]ar(\\s))(\\w+)[-](\\w+)|(\\w+)[-](\\w+)(?=(\\s)[z|Z]u[|r])|(\\w+)[-](\\w+)(?=(\\s)[i|I]n)|(\\w+)[-](\\w+)(?=(\\s)[b|B]eim)|(?<=[a|A]ls(\\s))(\\w+)|(\\w+)(?=(\\s)[a|A]n)|(?<=[w|W]ar(\\s))(\\w+)|(\\w+)(?=(\\s)[z|Z]u[|r])|(\\w+)(?=(\\s)[i|I]n)|(\\w+)(?=(\\s)[b|B]eim)")
frebnoprof<-subset(freb0, is.na(freb0$prof))
freb0<-freb0%>%
  unnest_wider(prof)
colnames(freb0)<-c("year","name","origin","text","bergofficer","id","text2","location","prof1","prof2","prof3")
freb0$prof1<-gsub("(\\d+)", NA, freb0$prof1)
freb0$prof1<-ifelse(is.na(freb0$prof1)&!is.na(freb0$bergofficer), "Bergofficer", freb0$prof1)
freb<-pivot_longer(freb0, c("prof1","prof2","prof3"), names_to = NULL)
freb<-distinct(freb)
colnames(freb)<-c("year","name","origin","text","bergofficer","id","text2","location","prof")
freb$organization<-str_extract(freb$text2, "(?<=(\\s)dem(\\s))(\\w+)[-](\\w+)|(?<=(\\s)der(\\s))(\\w+)[-](\\w+)|(?<=(\\s)die(\\s))(\\w+)[-](\\w+)|(?<=(\\s)d[.](\\s))(\\w+)[-](\\w+)|(?<=(\\s)dem(\\s))(\\w+)|(?<=(\\s)der(\\s))(\\w+)|(?<=(\\s)die(\\s))(\\w+)|(?<=(\\s)d[.](\\s))(\\w+)")
freb$prof<-ifelse(!is.na(freb$prof)&!is.na(freb$organization)&freb$prof==freb$organization, NA, freb$prof)
freb$prof<-gsub("^[a|A]ls$|^[i|I]n$|^[Z|z]u$|^[W|w]ar$|^[i|I]m$|^[D|d]er$", NA, freb$prof)
freb<-distinct(freb)
freb<-subset(freb, !is.na(freb$prof))
okupa<-c("restaurateur", "akademist", "markscheider","professor", "k?mmerer", "insp", "secret", "factor",
         "minister","director", "ingeni", "inspe[k|c]tor", "hauptmann","sekretar", "admiral", "offici",
         "praktikant", "schreiber", "major", "techniker", "assistent", "besitzer", "lehrer", "pastor",
         "kaufmann", "meister", "kontrolleur", "chef", "fahrer", "h?ndler", "advocat", "verwalter", 
         "audit", "brenner", "pr?sident", "pension", "offizier", "pfarrer", "officer", "conducteur", 
         "banquier", "soldat", "bibliothekar", "[s|z]rath", "f?hrer", "candidat", "assessor", "fabrikant",
         "freiherr", "prof", "dichter", "student", "beamter", "copist", "mechanicus", "gouverneur", 
         "governor", "h?lfe", "commmissar", "adjutant", "cassirer", "militair", "chemniker", "phil", 
         "unternehmer", "kaufman", "arbeiter", "chemiker", "actuar", "apotheker", "adminis", "literat", 
         "milit?r", "assistant", "starb", "leher", "maler", "furstl", "magnat", "werk", "g?rtner", 
         "studirt", "mitglied", "arkanist", "cafetier")
okupa<-paste(okupa, collapse="|")
freb$okupa<-str_extract(tolower(freb$prof), okupa)
okupisa<-as.data.frame(unique(freb$prof[which(is.na(freb$okupa))]))
colnames(okupisa)<-"prof"
okupisb<-as.data.frame(unique(freb$okupa[which(!is.na(freb$okupa))]))
colnames(okupisb)<-"prof"
okupisa$len<-nchar(okupisa$prof)
okupisa<-okupisa[order(okupisa$len),]
write.csv(okupisa, "okupisa.csv")
write.csv(okupisa, "okupisb.csv")
okupis2<-read.csv("okupis2.csv")
okupis3<-read.csv("okupis3.csv")
freb<-left_join(freb, okupis2, by="prof")
freb<-left_join(freb, okupis3, by="okupa")
freb$trans<-ifelse(!is.na(freb$trans.x), freb$trans.x, freb$trans.y)
colnames(freb)
freb$id<-paste("fre", freb$id, sep="")
freb$name<-str_replace_all(freb$name, "(\\s)(?=[,])", "")
freb$lastname<-str_extract(freb$name, "(\\w+)(?=[,])")
freb$lastname<-ifelse(is.na(freb$lastname), str_extract(freb$name, "^(\\w+)"), freb$lastname)
freb$names<-str_extract(str_replace_all(freb$name, "[.]", ""), "(?<=[,](\\s))(\\w+)(\\s+)(\\w+)|(?<=[,](\\s))(\\w+)|(?<=(\\w)(\\s))(\\w+)")
freb<-separate(freb, names, into=c("firstname", "middlename"), sep="(\\s)")
freb$type<-"alumni"
freb$source<-"frei"
freb$simpname<-sname(freb)
freb$loc<-freb$origin
origin<-as.data.frame(unique(freb$origin[!is.na(freb$origin)]))
colnames(origin)<-"origin"
origins<-geocode(origin)
origin$lon<-origins$lon
origin$lat<-origins$lat
origin$nationality<-coords2country(origin)
freb<-left_join(freb, subset(origin, select=c("origin", "nationality")), by="origin")
freb$loc<-freb$location
locations<-as.data.frame(unique(freb$loc[!is.na(freb$loc)]))
colnames(locations)<-"loc"
location<-geocode(locations$loc)
locations$lon<-location$lon
locations$lat<-location$lat
freb<-left_join(freb, locations, by="loc")
freb$role<-freb$trans
freb$org<-freb$organization
#adding the original#
frebp<-read.csv("Freibergarianna.csv", na.strings = c("", " ", "n/a",NA))
frebp$type<-"alumni"
frebp$source<-"frei"
frebp$id<-paste("fre",rownames(frebp), sep="")
frebp$name<-str_replace_all(frebp$name, "(\\s)(?=[,])", "")
frebp$lastname<-str_extract(frebp$name, "(\\w+)(?=[,])")
frebp$lastname<-ifelse(is.na(frebp$lastname), str_extract(frebp$name, "^(\\w+)"), frebp$lastname)
frebp$names<-str_extract(str_replace_all(frebp$name, "[.]", ""), "(?<=[,](\\s))(\\w+)(\\s+)(\\w+)|(?<=[,](\\s))(\\w+)|(?<=(\\w)(\\s))(\\w+)")
frebp<-separate(frebp, names, into=c("firstname", "middlename"), sep="(\\s)")
frebp$simpname<-sname(frebp)  
origin<-as.data.frame(unique(frebp$origin[!is.na(frebp$origin)]))
colnames(origin)<-"origin"
origins<-geocode(origin$origin)
origin$lon<-origins$lon
origin$lat<-origins$lat
origin$nationality<-coords2country(origin)
frebp<-left_join(frebp, subset(origin, select=c("origin", "nationality")), by="origin")
frebp$role<-"student"
frebp$loc<-"freiberg"
frebp$lon<-13.337230
frebp$lat<-50.914040
frebp$org<-"Bergakademie Freiberg"
frebf<-rbind(subset(freb, select=c("id", "source", "type","year", "text", "lastname", "firstname", "middlename", 
                                   "simpname","nationality", "loc", "lon", "lat", "role", "org")), 
             subset(frebp, select=c("id", "source", "type","year", "text", "lastname", "firstname", "middlename", 
                                    "simpname","nationality", "loc", "lon", "lat", "role", "org")))
frebf<-frebf[order(as.numeric(str_extract(frebf$id, "(\\d+)")), frebf$year),]
write.csv(frebf, "frebari2.csv", row.names = F)

####IV. ii. Easy list####
freiba<-read.csv("freiba.csv")
freiba$text2<-trimws(str_replace_all(freiba$text,  "^v(\\s)", "V "))
freiba$text2<-trimws(str_replace_all(freiba$text2,  "^des(\\s)", "Des "))
freiba$text2<-trimws(str_replace_all(freiba$text2,  "^de(\\s)", "De "))
freiba$text2<-trimws(str_replace_all(freiba$text2,  "Dr.(\\s)phii.", ""))
freiba$text2<-trimws(str_replace_all(freiba$text2,  "Dr.(\\s)phil.", ""))
freiba$text2<-trimws(str_replace_all(freiba$text2,  "Dr.(\\s)phi!.", ""))
freiba$text2<-trimws(str_replace_all(freiba$text2,  "^v.", "V"))
freiba$text2<-trimws(str_replace_all(freiba$text2,  "^V.", "V "))
freiba$text2<-trimws(str_replace_all(freiba$text2,  "(\\s+)", " "))
freiba$text2<-trimws(str_replace_all(freiba$text2,  "[^[:alnum:].,]", " "))
freiba$text2<-trimws(str_replace_all(freiba$text2,  "(\\s+)", " "))
freiba$text2<-trimws(str_replace_all(freiba$text2,  "(\\s+),", ","))
freiba$lastname<-trimws(ifelse(
  !is.na(str_extract(freiba$text2, "^(\\w+),")),
  str_extract(freiba$text2, "^(\\w+),"), 
  ifelse(
    !is.na(str_extract(freiba$text2, "^(\\w+)(\\s)(\\w+),")),
    str_extract(freiba$text2, "^(\\w+)(\\s)(\\w+),"),
    ifelse(
      !is.na(str_extract(freiba$text2, "^(\\w+)(\\s)(\\w+)(\\s)(\\w+),")),
      str_extract(freiba$text2, "^(\\w+)(\\s)(\\w+)(\\s)(\\w+),"),
      ifelse(
        !is.na(str_extract(freiba$text2, "^(\\w+).(\\s)(\\w+),")),
        str_extract(freiba$text2, "^(\\w+).(\\s)(\\w+),"),
        NA)
    )
  )
))
freiba$lastname<-trimws(str_replace_all(freiba$lastname,  "[:punct:]", ""))
freiba$firstname<-trimws(
  ifelse(
    !is.na(str_extract(freiba$text2, "^(\\w+),(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)."))|
      !is.na(str_extract(freiba$text2, "^(\\w+)(\\s)(\\w+),(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+).")),
    str_extract(freiba$text2, ",(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)."),
    ifelse(
      !is.na(str_extract(freiba$text2, "^(\\w+),(\\s)(\\w+)(\\s)(\\w+)."))|
        !is.na(str_extract(freiba$text2, "^(\\w+)(\\s)(\\w+),(\\s)(\\w+)(\\s)(\\w+).")),
      str_extract(freiba$text2, ",(\\s)(\\w+)(\\s)(\\w+)."),
      ifelse(
        !is.na(str_extract(freiba$text2, "^(\\w+),(\\s)(\\w+)."))|
          !is.na(str_extract(freiba$text2, "^(\\w+)(\\s)(\\w+),(\\s)(\\w+).")),
        str_extract(freiba$text2, ",(\\s)(\\w+)."), 
        ifelse(
          nchar(str_extract(freiba$text2, ",(\\s)(\\w+)."))==4, 
          str_extract(freiba$text2, ",(\\s)(\\w+).(\\s)(\\w+)"),
          NA)
      )
    )
  )
)
freiba$firstname<-trimws(str_replace_all(freiba$firstname,  "[:punct:]", ""))
freiba$initial<-paste(str_extract(freiba$firstname, "^(\\w)"), str_extract(freiba$firstname, "(\\s)(\\w)"), sep=" ")
freiba$initial<-trimws(str_replace_all(freiba$initial,  "NA", ""))
freiba$simpname<-trimws(paste(freiba$lastname, freiba$initial, sep=", "))
#extracting degrees, when present
freiba$ydeg<-
  ifelse(
    !is.na(str_extract(freiba$text2, "Dipl")), 
    str_extract(freiba$text2, "(\\d){3,4}"), 
    NA)
#transforming abbreviations
freiba$text3<-str_replace_all(freiba$text2, "V. St. v. A.", "USA")
freiba$text3<-str_replace_all(freiba$text3, "V. St. v.A.","USA")
freiba$text3<-str_replace_all(freiba$text3, "V. St.v.A.","USA")
freiba$text3<-str_replace_all(freiba$text3, "V.St.v.A.","USA")
#extracting the diplomas
freiba$text4<-trimws(str_replace_all(freiba$text3, "[^[:alnum:]]"," "))
freiba$text4<-trimws(str_replace_all(freiba$text4, "(\\s+)"," "))
freiba$degree<-
  ifelse(
    !is.na(str_extract(freiba$text4, "Dipl(\\w+)")),
    str_extract(freiba$text4, "Dipl(\\w+)"), 
    ifelse(
      !is.na(str_extract(freiba$text4, "Dipl(\\s)(\\w+)")),
      str_extract(freiba$text4, "Dipl(\\s)(\\w+)"),
      NA)
  )
okupa<-c("restaurateur", "professor", "director", "ingeni", "inspektor", "sekretar", "admiral", "major", "techniker", "assistent", "besitzer", "lehrer", "pastor", "kaufmann", "meister", "kontrolleur", "chef")
okupa<-paste(okupa, collapse="|")
freiba$text4<-trimws(tolower(freiba$text4))
freiba$okupa<-str_extract(freiba$text4, okupa)
#separate the base according to the different documents
write.csv(freiba, "freiba2.csv")  
freiba2<-read.csv("freiba2.csv")
freiba2$text3<-tolower(freiba2$text3)
freiA<-freiba2[which(freiba2$year<1916),]
freiB<-freiba2[which(freiba2$year>=1916),]
freiA$text4<-trimws(str_replace_all(freiA$text4, "[^[:alpha:].,]", " "), which=c("both"))
freiA$text3<-str_replace_all(freiA$text3, "[^[:alpha:].,]", " ")
freiA$text3<-str_replace_all(freiA$text3, "dipl.ing.", " ")
freiA$text3<-str_replace_all(freiA$text3, "dipl. ing.", " ")
freiA$text3<-str_replace_all(freiA$text3, "dipl(\\s)", " ")
freiA$text3<-str_replace_all(freiA$text3, "ing(\\s)", " ")
freiA$text3<-str_replace_all(freiA$text3, "(\\s+)", " ")
freiA$text3<-str_replace_all(freiA$text3, "(\\.) ,", ",")
freiA$text3<-str_replace_all(freiA$text3, ", (\\.)", ".")
freiA$text3<-str_replace_all(freiA$text3, "(\\.) (\\.)", ".")
freiA$text3<-trimws(str_replace_all(freiA$text3, ", ,", ","))
freiA$text3<-str_replace_all(freiA$text3, "(\\.)$", "")
freiA$text3<-str_replace_all(freiA$text3, ",", " ")
freiA$text3<-str_replace_all(freiA$text3, "S.A", "SA")
freiA$text3<-str_replace_all(freiA$text3, "S.R", "SA")
freiA$text3<-trimws(str_replace_all(freiA$text3, "(\\s+)", " "),which=c("both"))
freiA$location<-ifelse(!is.na(str_extract(freiA$text3, "(\\w+)(\\s)(\\w+)(\\s)(\\w+)$")),
                       str_extract(freiA$text3, "(\\w+)(\\s)(\\w+)(\\s)(\\w+)$"),
                       ifelse(!is.na(str_extract(freiA$text3, "(\\w+)(\\s)(\\w+)$")),
                              str_extract(freiA$text3, "(\\w+)(\\s)(\\w+)$"),
                              str_extract(freiA$text4, "(\\w+)(\\s)(\\w+)$")))
freiB$text3<-str_replace_all(freiB$text3, "dipl", "")
freiB$text3<-str_replace_all(freiB$text3, "[^[:alpha:].,]", " ")
freiB$text3<-str_replace_all(freiB$text3, "(\\s+)", " ")
freiB$text3<-str_replace_all(freiB$text3, "(\\s+)", " ")
freiB$location<-ifelse(!is.na(str_extract(freiB$text3, "(\\s)(\\w+)(\\.)(\\s)(\\w+)(\\s)(\\w+)"))|
                         !is.na(str_extract(freiB$text3, "(\\s)(\\w+)(\\s)(\\.)(\\s)(\\w+)(\\s)(\\w+)")),
                       str_extract(freiB$text3, "(\\.)(\\s)(\\w+)(\\s)(\\w+)"),
                       ifelse(!is.na(str_extract(freiB$text3, "(\\s)(\\w+)(\\.)(\\s)(\\w+),(\\s)(\\w+)"))|
                                !is.na(str_extract(freiB$text3, "(\\s)(\\w+)(\\s)(\\.)(\\s)(\\w+),(\\s)(\\w+)")),
                              str_extract(freiB$text3, "(\\.)(\\s)(\\w+),(\\s)(\\w+)"), 
                              ifelse(!is.na(str_extract(freiB$text3, "(\\s)(\\w+)(\\.)(\\s)(\\w+)"))|
                                       !is.na(str_extract(freiB$text3, "(\\s)(\\w+)(\\s)(\\.)(\\s)(\\w+)")),
                                     str_extract(freiB$text3, "(\\.)(\\s)(\\w+)"),
                                     NA)))
freiB$location<-str_replace_all(freiB$location, "[^[:alpha:]]", " ")
freiB$location<-str_replace_all(freiB$location, "(\\s+)", " ")
frei3<-rbind(freiA, freiB)
write.csv(frei3, "frei3.csv")
#visual cleaning
frei3<-read.csv("frei4.csv")
freic<-frei3[!is.na(frei3$location), ]
freid<-frei3[is.na(frei3$location), ]
freid$location<-ifelse(!is.na(str_extract(freid$text3, "(\\.)(\\s)(\\w+)(\\s)(\\w+)")),
                       str_extract(freid$text3, "(\\.)(\\s)(\\w+)(\\s)(\\w+)"),
                       ifelse(!is.na(str_extract(freid$text3, "(\\.)(\\s)(\\w+)")),
                              str_extract(freid$text3, "(\\.)(\\s)(\\w+)"),NA))
freid$location<-str_replace_all(freid$location, "[^[:alpha:]]", " ")
freid$location<-str_replace_all(freid$location, "(\\s+)", " ")
frei4<-rbind(freic, freid)
write.csv(frei4, "frei4.csv")
frei<-read.csv("frei4.csv")
frei$location<-str_replace(frei$location, "aue(\\s)sa", "aue schwarzwasser")
frei$location<-str_replace(frei$location, "annen i", "annen westphalia")
frei$location<-str_replace(frei$location, "bock u", "saalfeld")
frei$location<-str_replace(frei$location, "alzey", "alzey rhineland")
frei[is.na(frei$location)]<-"Unknown"
llfrei<-buscar(frei$location)
frei$lon<-llfrei$lon
frei$lat<-llfrei$lat
frei<-subset(frei3, select=c("year", "number", "text", "lastname", "firstname", "simpname", "ydeg", "degree", "location", "lat", "lon"))
freierror<-frei[is.na(frei$lon),]
freicool<-frei[!is.na(frei$lon),]
write.csv(freierror, "freierror.csv")
frei<-read.csv("frei5.csv")
frei<-subset(frei, select=c("year", "number", "text", "lastname", "firstname", "simpname", "ydeg", "degree", "location", "lat", "lon"))
freicool<-frei[!is.na(frei$lon),]
#visual cleaning
freierror<-read.csv("freierror2.csv")
llfreierror<-buscar(freierror$location)
freierror$lon<-llfreierror$lon
freierror$lat<-llfreierror$lat
freierror<-subset(freierror, select=c("year", "number", "text", "lastname", "firstname", "simpname", "ydeg", "degree", "location", "lat", "lon"))
freicool<-rbind(freicool, freierror[!is.na(freierror$lon),])
freierror<-freierror[is.na(freierror$lon),]
write.csv(freierror, "freierror3.csv")
#visual cleaning
freierror<-read.csv("freierror3.csv")
freierror<-subset(freierror, select=c("year", "number", "text", "lastname", "firstname", "simpname", "ydeg", "degree", "location", "lat", "lon"))
freicool<-rbind(freicool, freierror[!is.na(freierror$lon),])
freierror<-freierror[is.na(freierror$lon),]
llfreierror<-buscar(freierror$location)
freierror$lon<-llfreierror$lon
freierror$lat<-llfreierror$lat
freicool<-rbind(freicool, freierror[!is.na(freierror$lon),])
freierror<-freierror[is.na(freierror$lon),]
write.csv(freierror, "freierror4.csv")
write.csv(freicool, "freicool.csv")
#visual cleaning
freierror<-read.csv("freierror4.csv")
frei<-rbind(freierror, freicool)
frei<-unique(frei)
frei<-frei[order(frei$number),]
write.csv(frei, "frei.csv")
okupa<-c("restaurateur", "professor", "director", "ingeni", "inspektor", "sekretar", "admiral", "major", "techniker", "assistent", "besitzer", "lehrer", "pastor", "kaufmann", "meister", "kontrolleur", "chef")
okupa<-paste(okupa, collapse="|")
freiok<-read.csv("frei00.csv")
freiok$text2<-trimws(tolower(freiok$text))
freiok$okupa<-str_extract(freiok$text2, okupa)
write.csv(freiok, "freiok.csv") 
freiberg<-read.csv("freiberg.csv")
freiberg$id<-paste("fr", freiberg$number, sep="")
temp<-freiberg[which(!is.na(freiberg$ydeg)), ]
temp$year<-temp$ydeg
freiberg<-rbind(freiberg, temp)
freiberg$lastname<-tolower(freiberg$lastname)
freiberg$firstname<-tolower(freiberg$firstname)
freiberg$middlename<-trimws(ifelse(!is.na(str_extract(freiberg$firstname, "(\\s)(\\w+)")),
                                   str_extract(freiberg$firstname, "(\\s)(\\w+)"), NA), which=c("both"))
freiberg$simpname<-tolower(freiberg$simpname)
freiberg$edu<-tolower(freiberg$edu)
freiberg$loc<-tolower(freiberg$loc)  
fre<-subset(freiberg, select=c("id", "type", "source", "year", "simpname", "lastname", "firstname", "middlename", "nationality", "edu", "prof", "org", "loc", "lon", "lat"))
fre$type<-"alumni"
write.csv(fre, "fre.csv")
####IV. 3. Together####
fre<-read.csv("fre.csv")
fre$X<-NULL
freiba<-read.csv("freiba.csv")
freiba$id<-paste("fr", freiba$number, sep="")
freiba<-subset(freiba, select=c("id", "text"))
fre<-left_join(fre, freiba, by="id")
freiar<-read.csv("frebari2.csv")
freiar$source<-"frei"
freiar$id<-paste("fr", str_extract(freiar$id, "(\\d+)"), sep="")
freiar$edu<-"mining engineer"
freiar$prof<-freiar$role
freiar$role<-NULL
frei<-rbind(fre, freiar)
write.csv(frei, "freifinal.csv")

####VI. 

####V. Great Britain####
rsm<-read.csv("rsm.csv")
rsm<-rsm[which(!is.na(rsm$text)), ]
rsm$id<-rownames(rsm)
rsm$text1<-str_replace_all(rsm$text, "REGISTER", "")
rsm$text1<-str_replace_all(rsm$text1, "[*]", "")
rsm$text1<-str_replace_all(rsm$text1, "[.]", "")
rsm$text1<-str_replace_all(rsm$text1, "l8", "")
rsm$text1<-str_replace_all(rsm$text1, "I8", "")
rsm$text1<-ifelse(!is.na(str_extract(rsm$text1, "(?<=^f)(\\w+)[,]"))&
                    str_extract(rsm$text1, "(?<=^f)(\\w+)[,]")==toupper(str_extract(rsm$text1, "(?<=^f)(\\w+)[,]")), 
                  str_replace(rsm$text1, "^f", ""), rsm$text1)
rsm$name<-ifelse(is.na(str_extract(rsm$text1, "^(\\d+)[,]"))
                 &str_extract(rsm$text1, "^(\\w+)[,](\\s+)")==toupper(str_extract(rsm$text1, "^(\\w+)[,](\\s+)")), 
                 ifelse(!is.na(str_extract(rsm$text1, "^(\\w+)[,](\\s+)(\\w+)(\\s+)(\\w+)")), 
                        str_extract(rsm$text1, "^(\\w+)[,](\\s+)(\\w+)(\\s+)(\\w+)"),
                        str_extract(rsm$text1, "^(\\w+)[,](\\s+)(\\w+)")), 
                 NA)
rsm$y<-ifelse(is.na(str_extract(rsm$text1, "^(\\d+)[,]"))
              &str_extract(rsm$text1, "^(\\w+)[,](\\s+)")==toupper(str_extract(rsm$text1, "^(\\w+)[,](\\s+)")), 
              str_extract(rsm$text1, "(\\d){4}"), NA)
rsm$text1<-trimws(tolower(str_replace_all(rsm$text1, "[,]", "")), which=c("both"))
rsm$text1<-trimws(tolower(str_replace_all(rsm$text1, "(\\s)[:]", ":")), which=c("both"))
rsm$address<-ifelse(!is.na(str_extract(rsm$text1, "(?<=address:)(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)")),
                    str_extract(rsm$text1, "(?<=address:)(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)"), 
                    ifelse(!is.na(str_extract(rsm$text1, "(?<=address:)(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)")),
                           str_extract(rsm$text1, "(?<=address:)(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)"),
                           ifelse(!is.na(str_extract(rsm$text1, "(?<=address:)(\\s)(\\w+)(\\s)(\\w+)")),
                                  str_extract(rsm$text1, "(?<=address:)(\\s)(\\w+)(\\s)(\\w+)"), NA)))
edu<-c("mining", "metallurgy", "chemistry", "geology", "mechanics", "physics")
edu<-paste(edu, collapse="|")
rsm$edu<-str_extract(rsm$text1, edu)
rsm$name<-repetir(rsm$name, llenar, 100)
##rsm1 processing
rsm1<-rsm[which(rsm$year==1896), ]
rsm1$record<-rsm1$text1
loclon<-read.csv("loclon.csv")
loc<-as.vector(loclon$loc)
loc<-paste(loc, collapse="|")
rsm1$address2<-ifelse(rsm1$name!=lead(rsm1$name), rsm1$record, NA)
rsm1$loc<-ifelse(!is.na(rsm1$address), 
                 rsm1$address,
                 ifelse(!is.na(rsm1$address2), 
                        rsm1$address2, 
                        str_extract(rsm1$record, loc)))
professions<-c("manager","vicar", "vicepresident", "superintendent", "president","engineer", "accountant", "agent",  "merchant", "professor", "adjuster", "analyst", "assistant", "abstractor", "official", "artist", "assayer", "associate", "attorney", "author", "bacteriologist", "banker", "bookkeeper", "broker", "builder", "captain", "cashier", "cartoonist", "chairman", "chemist", "clerk", "clergy", "colonel", "constructor", "contractor", "counselor", "custodian", "designer", "dentist", "director", "draftsman", "drawer", "editor", "estimator", "exporter", "farmer", "fellow", "geologist", "geographer", "graduate", "student", "hydrographer", "importer", "inspector", "journalist", "judge", "lawyer", "librarian", "lecturer", "instructor", "lieutenant", "major", "mechanic", "machinist", "manufacturer", "major", "soldier", "master", "metallurgist", "miner", "oculist", "teacher", "physicist", "owner", "petrologist", "pathfinder", "pastor", "expert", "pharmacist", "photographer", "physician", "planter", "producer", "proprietor", "psychologist", "publisher", "pyrologist", "register", "salesman", "specialist", "statistician", "supervisor", "surveyor", "timekeeper", "transitman", "treasurer", "trustee", "musician", "researcher", "deputy", "secretary", "architect", "dealer", "electrician", "optician", "storekeeper", "inventor", "decorator", "computer", "buyer", "controller", "drugist", "foreman", "erector", "investigator", "calculator", "scholar", "died", "deceased", "retired")
profes1<-paste(professions, collapse="|")
rsm1$prof<-str_extract(rsm1$record, profes1)
conn0<-c("to(\\s)", "at(\\s)", "of(\\s)", "for(\\s)", "with(\\s)")
conn1<-paste("(?<=" , conn0, ")(\\w+)(\\s)(\\w+)(\\s)(\\w+)", sep="")
conn2<-paste("(?<=" , conn0, ")(\\w+)(\\s)(\\w+)", sep="")
conn3<-paste("(?<=" , conn0, ")(\\w+)", sep="")
conn<-c(conn1, conn2, conn3)
conn<-paste(conn, collapse="|")
conn0<-paste(conn0, collapse="|")
orgs<-as.vector(c("(\\s)works", "(\\s)co(\\s)", "(\\s)ltd", "(\\s)limited", "(\\s)company", "(\\s)station", "(\\s)smelter", "(\\s)mine", "(\\s)manufacturer","(\\s)society(\\s)(\\w+)(\\s)","(\\s)society", "mines(\\s)(\\w+)"))
orgs0<-paste("(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)", orgs, sep="")
orgs1<-paste("(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)", orgs, sep="")
orgs2<-paste("(\\s)(\\w+)(\\s)(\\w+)", orgs, sep="")
orgs3<-paste("(\\s)(\\w+)", orgs, sep="")
orgs<-c(orgs0, orgs1, orgs2, orgs3)
orgs<-paste(orgs, collapse="|")
rsm1$org<-ifelse(!is.na(str_extract(rsm1$record, orgs)), 
                 str_extract(rsm1$record, orgs), 
                 str_extract(rsm1$record, conn))
rsm1$org<-trimws(str_replace(rsm1$org, conn0, ""), which=c("both"))
##rsm2 processing
rsm2<-rsm[which(rsm$year!=1896), ]
rsm2$text1<-str_replace_all(rsm2$text1, "[.][-]", "- ")
rsm2$text1<-str_replace_all(rsm2$text1, "[-]{2,}", "-")
rsm2$text1<-str_replace_all(rsm2$text1, "[-]{2,}", "-")
rsm2$text1<-str_replace_all(rsm2$text1, "[-]", "- ")
rsm2$text1<-str_replace_all(rsm2$text1, "[-]", "-")
rsm2$text1<-str_replace_all(rsm2$text1, "(\\s)[-]", "-")
rsm2$text1<-str_replace_all(rsm2$text1, "(\\s+)", " ")
rsm2$address2<-ifelse(!is.na(str_extract(rsm2$text1, "(?<=address-)(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)")),
                      str_extract(rsm2$text1, "(?<=address-)(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)"), 
                      ifelse(!is.na(str_extract(rsm2$text1, "(?<=address-)(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)")),
                             str_extract(rsm2$text1, "(?<=address-)(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)"),
                             ifelse(!is.na(str_extract(rsm2$text1, "(?<=address-)(\\s)(\\w+)(\\s)(\\w+)")),
                                    str_extract(rsm2$text1, "(?<=address-)(\\s)(\\w+)(\\s)(\\w+)"), NA)))
rsm2$loc<-ifelse(!is.na(rsm2$address), rsm2$address, rsm2$address2)
rsm2$record1<-ifelse(!is.na(str_extract(rsm2$text1, "^record-")), rsm2$text1, NA)
record<-c("address-", "publications-", "military service", "addres", "publication", "military", "war service")
record<-paste(record, collapse="|")
rsm2$record2<-ifelse((rsm2$name==lead(rsm2$name, 1))&is.na(str_extract(lead(rsm2$text1, 1), record))&!is.na(rsm2$record1),  
                     lead(rsm2$text1, 1), NA)
rsm2$record3<-ifelse((rsm2$name==lead(rsm2$name, 2))&is.na(str_extract(lead(rsm2$text1, 2), record))&!is.na(rsm2$record2),  
                     lead(rsm2$text1, 2), NA)
rsm2$record4<-ifelse((rsm2$name==lead(rsm2$name, 3))&is.na(str_extract(lead(rsm2$text1, 3), record))&!is.na(rsm2$record3),  
                     lead(rsm2$text1, 3), NA)
rsm2$record5<-ifelse((rsm2$name==lead(rsm2$name, 4))&is.na(str_extract(lead(rsm2$text1, 4), record))&!is.na(rsm2$record4),  
                     lead(rsm2$text1, 4), NA)
rsm2$record6<-ifelse((rsm2$name==lead(rsm2$name, 5))&is.na(str_extract(lead(rsm2$text1, 5), record))&!is.na(rsm2$record5),  
                     lead(rsm2$text1, 5), NA)
rsm2$record7<-ifelse((rsm2$name==lead(rsm2$name, 6))&is.na(str_extract(lead(rsm2$text1, 6), record))&!is.na(rsm2$record6),  
                     lead(rsm2$text1, 6), NA)
rsm2$record8<-ifelse((rsm2$name==lead(rsm2$name, 7))&is.na(str_extract(lead(rsm2$text1, 7), record))&!is.na(rsm2$record7),  
                     lead(rsm2$text1, 7), NA)
rsm2$record9<-ifelse((rsm2$name==lead(rsm2$name, 8))&is.na(str_extract(lead(rsm2$text1, 8), record))&!is.na(rsm2$record8),  
                     lead(rsm2$text1, 8), NA)
rsm2$record10<-ifelse((rsm2$name==lead(rsm2$name, 9))&is.na(str_extract(lead(rsm2$text1, 9), record))&!is.na(rsm2$record9),  
                      lead(rsm2$text1, 9), NA)
rsm2$record11<-ifelse((rsm2$name==lead(rsm2$name, 10))&is.na(str_extract(lead(rsm2$text1, 10), record))&!is.na(rsm2$record6),  
                      lead(rsm2$text1, 10), NA)
rsm2$record12<-ifelse((rsm2$name==lead(rsm2$name, 11))&is.na(str_extract(lead(rsm2$text1, 11), record))&!is.na(rsm2$record7),  
                      lead(rsm2$text1, 11), NA)
rsm2$record13<-ifelse((rsm2$name==lead(rsm2$name, 12))&is.na(str_extract(lead(rsm2$text1, 12), record))&!is.na(rsm2$record8),  
                      lead(rsm2$text1, 12), NA)
rsm2$record14<-ifelse((rsm2$name==lead(rsm2$name, 13))&is.na(str_extract(lead(rsm2$text1, 13), record))&!is.na(rsm2$record9),  
                      lead(rsm2$text1, 13), NA)
rsm2$record<-paste(rsm2$record1, rsm2$record2, rsm2$record3, rsm2$record4, rsm2$record5, rsm2$record6, rsm2$record7, rsm2$record8, rsm2$record9, rsm2$record10, rsm2$record11, rsm2$record12, rsm2$record13, rsm2$record14, sep=" ")
rsm2$record<-trimws(str_replace_all(rsm2$record, "NA(\\s+)", " "))
rsm2$record<-trimws(str_replace_all(rsm2$record, "(\\s+)NA", " "))
rsm2$record<-trimws(str_replace_all(rsm2$record, "^NA", " "))
rsm2$record<-trimws(str_replace_all(rsm2$record, "(\\s+)", " "))
rsm2$record<-gsub("^(\\s+)$", NA, rsm2$record)
rsm2$record<-gsub("^$", NA, rsm2$record)
rsm2<-subset(rsm2, select=c("id", "source", "year", "name", "y", "loc","edu", "text", "record" ))
rsm2rec<-as.data.frame(str_split_fixed(rsm2$record, "[;]", n=17))
rsm2rec$id<-rsm2$id
rsm2rec$name<-rsm2$name
rsm2rec$text<-rsm2$text
rsm2rec$V1<-gsub("^$", NA, rsm2rec$V1)
rsm2rec<-rsm2rec[which(!is.na(rsm2rec$V1)), ]
rsm2rec<-melt(rsm2rec, id.vars =c("id", "name", "text"),na.rm=TRUE)
rsm2rec$value<-trimws(str_replace_all(rsm2rec$value,"(\\s+)", " " ), which = c("both"))
rsm2rec$value<-gsub("^$", NA, rsm2rec$value)
rsm2rec<-rsm2rec[which(!is.na(rsm2rec$value)),]
rsm2rec$y<-str_extract(rsm2rec$value, "(\\d){4}")
rsm2rec<-rsm2rec[order(rsm2rec$id, rsm2rec$variable), ]
rsm2rec$prof<-str_extract(rsm2rec$value, profes1)
rsm2rec$org<-ifelse(!is.na(str_extract(rsm2rec$value, orgs)), 
                    str_extract(rsm2rec$value, orgs), 
                    str_extract(rsm2rec$value, conn))
rsm2rec$org<-trimws(str_replace(rsm2rec$org, conn0, ""), which=c("both"))
rsm2rec$loc<-ifelse(!is.na(str_extract(rsm2rec$value, loc)), 
                    str_extract(rsm2rec$value, loc), 
                    ifelse(!is.na(str_extract(rsm2rec$value, "(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)$")), 
                           str_extract(rsm2rec$value, "(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)$"),
                           str_extract(rsm2rec$value, "(\\w+)(\\s)(\\w+)$")))
rsm2rec$source<-"rsm"
rsm2rec$year<-1920
rsm2rec$edu<-NA
rsm2rec$record<-rsm2rec$value
rsm2$prof<-NA
rsm2$org<-NA
#reintegrating
rsm1<-subset(rsm1, select=c("id", "source", "year", "name", "y", "loc","edu","prof","org", "text", "record" ))
rsm2rec<-subset(rsm2rec, select=c("id", "source", "year", "name", "y", "loc","edu","prof","org", "text", "record" ))
rsm2<-subset(rsm2, select=c("id", "source", "year", "name", "y", "loc","edu","prof","org", "text", "record" ))
rsm2<-rbind(rsm2, rsm2rec)
rsm2<-rsm2[order(rsm2$name, rsm2$id, decreasing=F), ]
rsmf<-rbind(rsm1, rsm2)
rsmf$loc<-trimws(rsmf$loc, which=c("both"))
rsmf$loc<-ifelse(nchar(rsmf$loc)>4, rsmf$loc, NA)
rsmf$org<-ifelse(nchar(rsmf$org)>4, rsmf$org, NA)
rsmf$loc<-ifelse((!is.na(rsmf$y)&!is.na(rsmf$edu))|(!is.na(rsmf$y)&(rsmf$prof=="student")), 
                 "london",
                 rsmf$loc)
rsmf$org<-ifelse((!is.na(rsmf$y)&!is.na(rsmf$edu))|(!is.na(rsmf$y)&(rsmf$prof=="student")), 
                 "royal school of mines",
                 rsmf$org)
rsmf$y<-str_replace(rsmf$y, "^13", "18")
rsmf<-rsmf[which((!is.na(rsmf$loc)|!is.na(rsmf$org))),]
rsmf$year<-ifelse(!is.na(rsmf$y), rsmf$y, rsmf$year)
rsmf$name<-trimws(rsmf$name, which=c("both"))
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$edu<-llenar2(rsmf$edu, rsmf$name)
rsmf$loc<-str_replace_all(rsmf$loc, "[^[:alpha:]]", " ")
rsmf$loc<-trimws(str_replace_all(rsmf$loc, "(\\s+)", " "), which=c("both"))
rsmf$loc<-ifelse(!is.na(rsmf$loc)&nchar(rsmf$loc)>3, rsmf$loc, NA)
llrsm<-as.data.frame(unique(rsmf$loc))
colnames(llrsm)<-"loc"
llrsm<-left_join(llrsm, loclon, by="loc", all=T)
llrsm1<-llrsm[which(!is.na(llrsm$lon)), ]
llrsm2<-llrsm[which(is.na(llrsm$lon)&!is.na(llrsm$loc)), ]
lonrsm2<-buscar(llrsm2$loc)
llrsm2$lon<-lonrsm2$lon
llrsm2$lat<-lonrsm2$lat
llrsm<-rbind(llrsm1, llrsm2)
write.csv(llrsm, "llrsm.csv")
llrsm<-subset(read.csv("llrsm.csv"), select=c("loc", "lon", "lat"))
rsmf<-left_join(rsmf, llrsm, by="loc", match="all")
rsmf$lastname<-tolower(str_extract(rsmf$name, "^(\\w+)"))
rsmf$firstname<-tolower(str_extract(rsmf$name, "(?<=,(\\s))(\\w+)"))
rsmf$middlename<-ifelse(!is.na(str_extract(rsmf$name, ",(\\s)(\\w+)(\\s)(\\w+)")), 
                        tolower(str_extract(rsmf$name, "(\\w+)$")), NA)
rsmf$simpname<-tolower(trimws(paste(rsmf$lastname, ", ", substr(rsmf$firstname, 1, 1), " " ,substr(rsmf$middlename, 1, 1), sep="")))
rsmf$year<-as.numeric(as.character(rsmf$year))
rsmf<-distinct(rsmf)
rsmfll<-as.data.frame(rsmf$lon)
colnames(rsmfll)[1]<-"lon"
rsmfll$lat<-rsmf$lat
rsmfll[is.na(rsmfll)]<-FALSE
rsmf$country<-coords2country(rsmfll)
rsmf$type<-"alumni"
rsmf$nationality<-NA
rsmf<-subset(rsmf, select=c("id", "type", "source", "year", "simpname", "lastname", "firstname", "middlename", "nationality", "edu", "prof", "org", "loc", "lon", "lat", "country"))
write.csv(rsmf, "rsmf.csv")
rsmf<-subset(read.csv("rsmf2.csv"), select=c("id", "type", "source", "year", "simpname", "lastname", "firstname", "middlename", "nationality", "edu", "prof", "org", "loc", "lon", "lat"))
rsmf$id<-paste("rs", rsmf$id, sep="")
rsmf$lastname<-str_replace_all(rsmf$lastname, "0", "o")
rsmf$lastname<-str_replace_all(rsmf$lastname, "[^[:alpha:],]", "")
rsmf$firstname<-str_replace_all(rsmf$firstname, "0", "o")
rsmf$firstname<-str_replace_all(rsmf$firstname, "[^[:alpha:],]", "")
rsmf$middlename<-str_replace_all(rsmf$middlename, "0", "o")
rsmf$middlename<-str_replace_all(rsmf$middlename, "[^[:alpha:],]", "")
rsmf$simpname<-tolower(trimws(paste(rsmf$lastname, ", ", substr(rsmf$firstname, 1, 1), " " ,substr(rsmf$middlename, 1, 1), sep="")))
write.csv(rsmf, "rsmf3.csv")
####VI. France ####
annales<-read.csv("Annales.csv")
annales$firstname<-str_extract(annales$name, "(?<=[(])(.*)(?=[)])")
annales$lastname<-str_replace(annales$name, "[(](.*)[)]", "")
annales$simpname<-sname(annales)
write.csv(annales, "annales2.csv")

####VII. Columbia ####
####VII. 1. Several####
csm1 = read.csv("CSM Compiled 1.csv")
view(csm1)
# clean and organize by number
csm1$text2 = str_replace_all(csm1$text, "(?<=(\\d))[I|l|[|]](?=[.])", "1")
csm1$text2 = str_replace_all(csm1$text2, "(?<=(\\d))[I|l|[|]](?=(\\d))", "1")
csm1$text2 = str_replace_all(csm1$text2, "(?<=(\\d))[I|l|[|]](?=[.])", "1")
csm1$text2 = str_replace_all(csm1$text2, "[I|l|[|]](?=(\\d))", "1")
csm1$text2 = str_replace_all(csm1$text2, "^[I|l|[|]]", "1")
csm1$text2 = str_replace_all(csm1$text2, "(?<=(\\d))[O|0](?=[.])", "0")
csm1$text2 = str_replace_all(csm1$text2, "(?<=(\\d))[O|0](?=(\\d))", "0")
csm1$text2 = str_replace_all(csm1$text2, "(?<=(\\d))[O|0](?=[.])", "0")
csm1$text2 = str_replace_all(csm1$text2, "[O|0](?=(\\d))", "0")
csm1$text2 = str_replace_all(csm1$text2, "^[O|0]", "0")
csm1$text2 = str_replace_all(csm1$text2, "(?<=(\\s))IO(?=[.])", "10")
csm1 = separate_rows(csm1, text2, sep="(\\s+)(?=(\\d))")
csm1 = separate_rows(csm1, text2, sep="[,]")
# get degree
csm1$degree = ifelse(!is.na(str_extract(csm1$text, "[E|e][N|n][G|g][I|i][N|n][E|e][E|e][R|r]")), csm1$text, NA)
# get year
csm1$year = ifelse(!is.na(str_extract(csm1$text, "1[8|9]\\d\\d")), csm1$text, NA)
csm1 = fill(csm1, year, .direction=c("down"))
# Clean to just full name
csm1$text2 = str_replace_all(csm1$text2, "(\\d+)", "")
csm1$text2 = str_replace_all(csm1$text2, "Ph[.]", "")
csm1$text2 = str_replace_all(csm1$text2, "Jr[.]", "")
csm1$text2 = str_replace_all(csm1$text2, "Rev[.]", "")
csm1$text2 = str_replace_all(csm1$text2, "(\\W+)", " ")
csm1$text2 = str_replace_all(csm1$text2, "([[:upper:]])(?=(\\s+))", "")
csm1$text2 = str_replace_all(csm1$text2, "([[:upper:]])$", "")
csm1$text2 = str_replace_all(csm1$text2, "(\\s+)(?=[[:lower:]])", "")
csm1$text2 = trimws(csm1$text2)
# break full name into first, middle, and last
csm1$lastname = ifelse(is.na(csm1$degree), str_extract(csm1$text2, "([[:alpha:]]+)$"), NA)
csm1$text2 = str_replace_all(csm1$text2, "([[:alpha:]]+)$", "")
csm1$firstname = ifelse(is.na(csm1$degree), str_extract(csm1$text2, "^(\\w+)"), NA)
csm1$text2 = str_replace_all(csm1$text2, "^(\\w+)(\\s+)", "")
csm1$middlename = ifelse(is.na(csm1$degree), csm1$text2, NA)
csm1 = fill(csm1, degree, .direction=c("down"))
csm2 = read.csv("CSM Compiled 2.csv")
view(csm2)
# organize into separate rows
csm2 = paste(csm2$text, collapse=" : ")
csm2 = as.data.frame(csm2)
csm2 = separate_rows(csm2, csm2, sep="[:|-|;|,|-]") 
view(csm2)
names(csm2) = c("text")
# initial cleaning
csm2$text2 = trimws(csm2$text)
# get degree
csm2$degree = ifelse(!is.na(str_extract(csm2$text2, "[E|e][N|n][G|g][I|i][N|n][E|e][E|e][R|r]")), csm2$text2, NA)
# get year
csm2$year = ifelse(!is.na(str_extract(csm2$text2, "1[8|9]\\d\\d$")), csm2$text2, NA)
csm2 = fill(csm2, year, .direction=c("down"))
view(csm2)
# Clean to just full name
csm2$text2 = str_replace_all(csm2$text2, "(\\d+)", "")
csm2$text2 = str_replace_all(csm2$text2, "(\\W+)", " ")
csm2$text2 = str_replace_all(csm2$text2, "P[H|h]", "")
csm2$text2 = str_replace_all(csm2$text2, "J[R|r]", "")
csm2$text2 = str_replace_all(csm2$text2, "R[E|e][V|v]", "")
csm2$text2 = str_replace_all(csm2$text2, "(?<=[[:upper:]])((\\s+)([[:lower:]]))", "")
csm2$text2 = str_replace_all(csm2$text2, "([[:upper:]])(?=(\\s+)$)", "") # this is the line that gets rid of all A B or B S / degree specifications
csm2$text2 = str_replace_all(csm2$text2, "(\\s+)(?=[[:lower:]])", "")
csm2$text2 = str_replace_all(csm2$text2, "^([[:lower:]])(\\s+)", "")
csm2$text2 = trimws(csm2$text2)
# break full name into first, middle, and last
csm2$lastname = ifelse(is.na(csm2$degree), str_extract(csm2$text2, "([[:alpha:]]+)$"), NA)
csm2$text2 = str_replace_all(csm2$text2, "([[:alpha:]]+)$", "")
csm2$firstname = ifelse(is.na(csm2$degree), str_extract(csm2$text2, "^(\\w+)"), NA)
csm2$text2 = str_replace_all(csm2$text2, "^(\\w+)(\\s+)", "")
csm2$middlename = csm2$text2
csm2 = fill(csm2, degree, .direction=c("down"))
csm3 = read.csv("CSM Compiled 3.csv")
view(csm3)
# organize into separate rows
csm3 = paste(csm3$text, collapse=";")
csm3 = as.data.frame(csm3)
csm3 = separate_rows(csm3, csm3, sep="[,|-|;|*|.]")
view(csm3)
names(csm3) = c("text")
# get degree
csm3$degree = ifelse(!is.na(str_extract(csm3$text, "[E|e][N|n][G|g][I|i][N|n][E|e][E|e][R|r]")), csm3$text, NA)
# get year
csm3$year = ifelse(!is.na(str_extract(csm3$text, "^1[8|9]\\d\\d$")), csm3$text, NA)
csm3 = fill(csm3, year, .direction=c("down"))
# Clean to just full name
csm3$text2 = str_replace_all(csm3$text, "(\\d+)", "")
csm3$text2 = str_replace_all(csm3$text2, "(\\W+)", " ")
csm3$text2 = str_replace_all(csm3$text2, "P[H|h]", "")
csm3$text2 = str_replace_all(csm3$text2, "J[R|r]", "")
csm3$text2 = str_replace_all(csm3$text2, "R[E|e][V|v]", "")
csm3$text2 = str_replace_all(csm3$text2, "(?<=[[:upper:]])((\\s+)([[:lower:]]))", "")
csm3$text2 = str_replace_all(csm3$text2, "([[:upper:]])(?=(\\s+)$)", "") # this is the line that gets rid of all A B or B S / degree specifications
csm3$text2 = str_replace_all(csm3$text2, "(\\s+)(?=[[:lower:]])", "")
csm3$text2 = str_replace_all(csm3$text2, "^([[:lower:]])(\\s+)", "")
csm3$text2 = trimws(csm3$text2)
# break full name into first, middle, and last
csm3$lastname = ifelse(is.na(csm3$degree), str_extract(csm3$text2, "([[:alpha:]]+)$"), NA)
csm3$text2 = str_replace_all(csm3$text2, "([[:alpha:]]+)$", "")
csm3$firstname = ifelse(is.na(csm3$degree), str_extract(csm3$text2, "^(\\w+)"), NA)
csm3$text2 = str_replace_all(csm3$text2, "^(\\w+)(\\s+)", "")
csm3$middlename = csm3$text2
csm3 = fill(csm3, degree, .direction=c("down"))
csm4 = read.csv("CSM Compiled 4.csv")
view(csm4)
# clean and organize rows
csm4 = separate_rows(csm4, text, sep="[,]")
# get degree
csm4$degree = ifelse(!is.na(str_extract(csm4$text, "[E|e][N|n][G|g][I|i][N|n][E|e][E|e][R|r]")), csm4$text, NA)
# get year
csm4$year = ifelse(!is.na(str_extract(csm4$text, "^1[8|9]\\d\\d")), csm4$text, NA)
csm4 = fill(csm4, year, .direction=c("down"))
# Clean to just full name
csm4$text2 = str_replace_all(csm4$text, "(\\d+)", "")
csm4$text2 = str_replace_all(csm4$text2, "Ph[.]", "")
csm4$text2 = str_replace_all(csm4$text2, "Jr[.]", "")
csm4$text2 = str_replace_all(csm4$text2, "Rev[.]", "")
csm4$text2 = str_replace_all(csm4$text2, "(\\W+)", " ")
csm4$text2 = str_replace_all(csm4$text2, "([[:upper:]])(?=(\\s+))", "")
csm4$text2 = str_replace_all(csm4$text2, "([[:upper:]])$", "")
csm4$text2 = str_replace_all(csm4$text2, "(\\s+)(?=[[:lower:]])", "")
csm4$text2 = trimws(csm4$text2)
# break full name into first, middle, and last
csm4$lastname = ifelse(is.na(csm4$degree), str_extract(csm4$text2, "([[:alpha:]]+)$"), NA)
csm4$text2 = str_replace_all(csm4$text2, "([[:alpha:]]+)$", "")
csm4$firstname = ifelse(is.na(csm4$degree), str_extract(csm4$text2, "^(\\w+)"), NA)
csm4$text2 = str_replace_all(csm4$text2, "^(\\w+)(\\s+)", "")
csm4$middlename = ifelse(is.na(csm4$degree), csm4$text2, NA)
csm4 = fill(csm4, degree, .direction=c("down"))
csm<-rbind(csm1, csm2, csm3, csm4)
csm<-csm[which(!is.na(csm$lastname)),]
csm$id<-rownames(csm)
csm$simpname<-sname(csm)
csm$degree<-str_replace(csm$degree, "Degree(\\s)of(\\s)", "")
csm$degree<-tolower(csm$degree)
csm$degree<-str_replace(csm$degree, "[.](\\s)[-](.*)$", "")
csm$type<-"alumni"
csm$source<-"columbia"
csm$loc<-NA
csm$lon<-NA
csm$lat<-NA
csm$country<-NA
csm$org<-NA
csm$edu<-csm$degree
csm$position<-NA
write.csv(csm, "csmfinal.csv")
####VII. 2. Segunda lista ####
csmf<-read.csv("CSM 1867-94.csv")
csmf$year<-str_extract(csmf$text, "^(\\d){4}$")
csmf<-fill(csmf, year, .direction="down")
csmf$text2<-str_replace_all(csmf$text, "(?<=[,])(?=[[:alpha:]])|(?<=[,])(?=[[:punct:]])", " ")
csmf$text2<-str_replace_all(csmf$text2, "i(?=(\\d){3})", "1")
patrones<-c(".*(?=[,|.](\\s)[(])",
            ".*?(?=[,|.](\\s)\\[)",
            ".*?(?=[,|.][*](\\s)[(])",
            ".*?(?=[,|.](\\s)[E|C|M|B][.][A-Z][.])",
            ".*?(?=[,|.](\\s)Ph[.])",
            ".*?(?=[,|.][*](\\s)Ph[.][A-Z][.])",
            ".*?(?=[,|.][*](\\s)[A-Z][.][A-Z][.])",
            ".*?(?=[,|.](\\s)[*](\\s))",
            ".*?(?=[,|.](\\s))Jr", 
            ".*?(?=[,|.](\\s)Met[.](\\s)Eng[.])")
patrones<-paste(patrones, collapse="|")
csmf$name<-str_extract(csmf$text2, patrones)
csmf$name<-ifelse(is.na(csmf$name)&!is.na(str_extract(lead(csmf$text2), "^[(]|^[E|C|M|B][.]|^[(]|^[E|C|M|B](\\s)|^Ph[.]|Jr[.]")), 
                  str_extract(csmf$text2, ".*[,|.]"), csmf$name)
eliminados<-c("^[(]|^[E|C|M|B][.]|^Ph[.]|Box(\\s)|Station|Jr[.]|Govt(\\b)|(\\b)Cal(\\b)|(\\b)Co(\\b)|[N|M][.][Y||J][.]|[N|M][.](\\s)[Y|J][.]|GRADUATES|(\\b)Mo[.]|(\\b)Pa[.]|^[A-Z][.]|(\\s)Coll[.]")
csmf$name<-ifelse(!is.na(str_extract(csmf$name, eliminados)), NA, csmf$name)
csmf$name<-trimws(str_replace_all(csmf$name, "[[:punct:]](\\d+)|[(].*[)]|Ph[.]B|[A-Z][.][A-Z][.]|(\\b)Asst(\\b)|(\\b)Mgr(\\b)", ""), which="both")
csmf$name<-trimws(str_replace_all(csmf$name, "[^[:alpha:]]", " "), which="both")
csmf$name<-ifelse((toupper(csmf$name)==csmf$name)|
                    !is.na(str_extract(csmf$name, "^[a-z]"))|
                    !is.na(str_extract(csmf$name, "^(\\w+)$")), NA, csmf$name)
csmname<-csmf[which(!is.na(csmf$name)),]
csmf$lastname<-lname(csmf$name)
csmf$firstname<-fname(csmf$name)
csmf$middlename<-mname(csmf$name)
csmf$simpname<-sname(csmf)
csmf<-separate_rows(csmf, text2, sep="(?<=\\))")
csmf$loc<-ifelse(is.na(str_extract(csmf$text2, "^18"))&!is.na(str_extract(csmf$text2, "^(\\d+){2,}")), 
                 str_extract(csmf$text2, "^(\\d+){2,}.*"), str_extract(csmf$text2, "(\\d+)(.*?)St[.].*|(\\d+)(.*?)Pl[.].*|(\\d+)(.*?)Bldg[.].*|(\\d+)(\\s+)St[.].*|(\\d+)(\\s+)Pl[.].*|(\\d+)(\\s+)Bldg[.].*"))
csmf<-buscar2(csmf)
csmf$details<-pegar(csmf$text2, csmf$name)
csmf<-fill(csmf, simpname, .direction="down")
csmf$loc<-llenar2(csmf$loc, csmf$simpname)\
for(i in 1:10){
  csmf$lon<-llenar2(csmf$lon, csmf$loc)
  csmf$lat<-llenar2(csmf$lat, csmf$loc)
}
csmf<-subset(csmf, !is.na(csmf$name))
csmf$text<-csmf$details
write.csv(csmf, "csmf.csv")
####VII. 3. todas las listas####
csmf<-read.csv("csmf.csv")
csmfinal<-read.csv("csmfinal.csv")
csmf$X<-NULL
csmfinal$X<-NULL
csmf$text<-str_replace_all(csmf$text, "[&]", "and")
csmf$org<-str_extract(csmf$text, "(?<=[[:punct:]])(.*)Co[.]|(?<=[[:punct:]])(.*?)Inc[.]|(?<=[[:punct:]])(.*?)Company|(?<=[[:punct:]])(.*?)Dept[.]")
for(i in 1:50){
  csmf$org<-ifelse(!is.na(str_extract(csmf$org, "(?<=[[:punct:]])(.*)Co[.]|(?<=[[:punct:]])(.*?)Inc[.]|(?<=[[:punct:]])(.*?)Company|(?<=[[:punct:]])(.*?)Dept[.]")), 
                   str_extract(csmf$org, "(?<=[[:punct:]])(.*)Co[.]|(?<=[[:punct:]])(.*?)Inc[.]|(?<=[[:punct:]])(.*?)Company|(?<=[[:punct:]])(.*?)Dept[.]"), 
                   csmf$org)
}
csmf$org<-str_replace_all(csmf$org, "^(\\s+)", "")
profesiones<-c("Engr", "Engineer", "Supt", "Superintendent", "Mgr", "Manager", "Surveyor", "Geologist", "Agent", "asst", "mgr",
               "Boss", "Chemist", "Assayer", "Foreman", "Metallurgist", "Dept.", "Technic", "Experimental Work", "prest",
               "manager", "vicepresident", "superintendent", "president","engineer", "accountant", "agent",  "merchant", 
               "professor", "adjuster", "analyst", "assistant", "abstractor", "official", "artist", "assayer", "eng'r",
               "associate", "attorney", "author", "bacteriologist", "banker", "bookkeeper", "broker", "builder", 
               "captain", "cashier", "cartoonist", "chairman", "chemist", "clerk", "clergy", "colonel", "constructor", 
               "contractor", "counselor", "custodian", "designer", "dentist", "director", "draftsman", "drawer", 
               "editor", "estimator", "exporter", "farmer", "fellow", "geologist", "geographer", "graduate", "student", 
               "hydrographer", "importer", "inspector", "journalist", "judge", "lawyer", "librarian", "lecturer", "instructor", 
               "lieutenant", "major", "mechanic", "machinist", "manufacturer", "major", "soldier", "master", "metallurgist",
               "miner", "oculist", "teacher", "physicist", "owner", "petrologist", "pathfinder", "pastor", "expert", "pharmacist",
               "photographer", "physician", "planter", "producer", "proprietor", "psychologist", "publisher", "pyrologist", "register",
               "salesman", "specialist", "statistician", "supervisor", "surveyor", "timekeeper", "transitman", "treasurer", "trustee", 
               "musician", "researcher", "deputy", "secretary", "architect", "dealer", "electrician", "optician", "storekeeper", 
               "inventor", "decorator", "computer", "buyer", "controller", "drugist", "foreman", "erector", "investigator", 
               "calculator", "scholar", "died", "deceased", "retired")
profesiones<-tolower(profesiones)
profesiones<-paste(profesiones, collapse = "|")
csmf$position<-str_extract(tolower(csmf$text), profesiones)
csmf$edu<-str_extract(csmf$text, "E[.]M[.]|C[.]E[,]")
csmf$edu<-str_replace(csmf$edu, "E[.]M[.]", "mining eng")
csmf$edu<-str_replace_all(csmf$edu, "C[.]E[.]", "civil eng")
for(i in 1:10){
  csmf$edu<-llenar2(csmf$edu, csmf$simpname)
}
csmtotal<-rbind(
  subset(csmf, select=c("text", "year", "lastname", "firstname", "middlename", "simpname", "loc", "lon", "lat", "org", "edu")), 
  subset(csmfinal, select=c("text", "year", "lastname", "firstname", "middlename", "simpname", "loc", "lon", "lat", "org", "edu"))
)
csmtotal$id<-rownames(csmtotal)
csmtotal$type<-"alumni"
csmtotal$source<-"columbia"
write.csv(csmtotal, "csmtotal.csv", row.names = F)

####VIII. Berkeley ####
berk<-read.csv("Berkeleyv2.csv")
berk$id<-rownames(berk)
berk$year<-as.numeric(str_extract(berk$text, "^18(\\d){2}|^19(\\d){2}"))
berk$year<-ifelse((berk$year>1916)|(berk$year<1864), NA, berk$year)
berk<-fill(berk, year, .direction = "down")
# get names
berk$text2<-str_replace_all(berk$text, "\\\\[N|V]|VV", "W")
berk$text2<-trimws(str_replace_all(berk$text2, "[^[:alpha:].,]", " "), which=c("both"))
berk$name<-ifelse(!is.na(str_extract(berk$text2, "[*][A-Z]")), str_extract(berk$text2, "(?<=[*])(.*)"), 
                  ifelse(!is.na(str_extract(lead(berk$text2), "^A[.,][B|3|8]")), berk$text2, 
                         ifelse(!is.na(str_extract(berk$text2, "^(\\w+)[.,](\\s+)(\\w+)(\\s+)(\\w+)"))&is.na(str_extract(berk$text2, "^(\\d+)"))&is.na(str_extract(berk$text2, "(?<=[.,](\\s))(\\d+)")), berk$text2,  
                                ifelse(!is.na(str_extract(berk$text2, "^(\\w+)[.,](\\s+)(\\w+)"))&is.na(str_extract(berk$text2, "^(\\d+)"))&is.na(str_extract(berk$text2, "(?<=[.,](\\s))(\\d+)")), berk$text2, 
                                       ifelse(!is.na(str_extract(berk$text2, "^(\\w+)(\\s+)(\\w+)[.,](\\s+)(\\w+)(\\s+)(\\w+)"))&is.na(str_extract(berk$text2, "^(\\d+)"))&is.na(str_extract(berk$text2, "(?<=[.,](\\s))(\\d+)")), berk$text2, 
                                              NA)))))
berk$name<-ifelse(!is.na(str_extract(berk$text, "^(\\d+)$|(\\w+)(\\s+)[&](\\s+)(\\w+)|[(].*[)]|st[.]")), NA, berk$name)
eliminate<-"(\\w){1}[.](\\s+)(\\w){1}[.]|Cal[.]|Residence|(\\b)Club(\\b)|Commission|(//b)st(//b)|[[:alpha:]]{2,}[.]$"
berk$name<-ifelse(!is.na(str_extract(berk$name, eliminate)), NA, berk$name)
berk$name<-ifelse(is.na(str_extract(berk$name, "^[a-z]{1}")), berk$name, NA)
berk$name<-trimws(str_replace(berk$name, "^[^[:alpha:]]|^[I|l](\\s)", ""), which="both")
temp<-berk[which(!is.na(berk$name)),]
temp$nameF<- ifelse(is.na(lag(temp$name))|is.na(lag(temp$name, 2))|is.na(lead(temp$name))|is.na(lead(temp$name, 2)), temp$name,  
                    ifelse(str_extract(temp$name, "^(\\w){1}")==str_extract(lag(temp$name), "^(\\w){1}")|
                             str_extract(temp$name, "^(\\w){1}")==str_extract(lead(temp$name), "^(\\w){1}")|
                             str_extract(temp$name, "^(\\w){1}")==str_extract(lag(temp$name,2), "^(\\w){1}")|
                             str_extract(temp$name, "^(\\w){1}")==str_extract(lead(temp$name,2), "^(\\w){1}"),
                           temp$name, #this pretty long ifelse tries to situate names that begin with the same letter than other contextual names (lags and lead with to rows above)
                           ifelse((match(str_extract(temp$name, "^(\\w){1}"), LETTERS[1:26])-match(str_extract(lag(temp$name), "^(\\w){1}"), LETTERS[1:26])>=0)|
                                    (temp$year>lag(temp$year)), 
                                  temp$name, #this pretty hard rule applies for all other names that do not belong to the previous patterns, breaking the alphabetical order
                                  NA)))
temp$nameF<-str_replace_all(temp$nameF, "(?<=[A-Z]{1})(\\s)?(?=[a-z])", "")
berk<-left_join(berk, subset(temp, select=c("id", "nameF")), by="id")
temp2<-temp[which(is.na(temp$nameF)),]


# concatenate text
berk$newtext<-ifelse(is.na(berk$name), NA,
                     ifelse(is.na(lead(berk$name))&is.na(lead(berk$name, 2))&is.na(lead(berk$name, 3)), paste(berk$text, lead(berk$text, 1), lead(berk$text, 2), lead(berk$text, 3)),
                            ifelse(is.na(lead(berk$name))&is.na(lead(berk$name, 2)), paste(berk$text, lead(berk$text, 1), lead(berk$text, 2)),
                                   paste(berk$text, lead(berk$text, 1)))))
berk$newtext<-str_replace_all(berk$newtext, "[_][.]", ".")
berk2<-subset(berk, !is.na(berk$newtext))
berk2<-separate_rows(berk2, newtext, sep="(?=[A-Z]{1}[.][A-Z]{1}[.])")
# check for CE degree or eng employment
berk2$degree<-str_extract(berk2$newtext, "[A-Z]{1}[.][A-Z]{1}[.]|Ph[.][A-Z]{1}[.]")
berk2$year2<-str_extract(berk2$newtext, "(\\b)(\\d){2}(\\b)")
berk2$prof<-str_extract(berk2$newtext, "(?<=;(\\s))[a-z]{3,}(\\s)[a-z]{3,}(\\s)[a-z]{3,}|(?<=;(\\s))[a-z]{3,}(\\s)[a-z]{3,}|(?<=;(\\s))[a-z]{3,}[-][a-z]{3,}|(?<=;(\\s))[a-z]{3,}")
berk2<-separate_rows(berk2, prof, sep="(\\s)and(\\s)")
# first, middle, last name
berk2$firstname<-str_extract(berk2$nameF, "(?<=[.,](\\s))(\\w+)")
berk2$middlename<-ifelse(!is.na(str_extract(berk2$nameF, "(?<=[.,](\\s))(\\w+)(\\s)(\\w+)(\\s)(\\w+)")), 
                         str_extract(berk2$nameF, "(\\w+)(\\s)(\\w+)$"), 
                         ifelse(!is.na(str_extract(berk2$nameF, "(?<=[.,](\\s))(\\w+)(\\s)(\\w+)")), 
                                str_extract(berk2$nameF, "(\\w+)$|(\\w+)[.]$"), NA))
berk2$middlename<-str_extract(berk2$middlename, "^(\\w+)")
berk2$lastname<-trimws(str_extract(berk2$nameF, "(.*?)(?=[[:punct:]])"), which = "both")
berk2$simpname<-sname(berk2)
berk2$loc<-str_extract(berk2$newtext, "(?<=Residence[,](\\s))(.*)|(?<=Resi(\\s)dence[,](\\s))(.*)")
berk2$loc<-trimws(str_replace_all(berk2$loc, "(\\d+)", ""), which="both")
locations<-buscar2(berk2)
berk2<-left_join(berk2, locations, by="loc")
berk<-read.csv("berk2.csv")
berk$country<-coords2country(berk)
berk$edu<-berk$degree
berk<-subset(berk,select=c("id","source", "year", "simpname", "nationality", "edu", "prof", "org", "loc", "lon", "lat", "lastname", "firstname","middlename", "text", "country", "type")) 
write.csv(berk2, "berk2.csv")

####IX. Harvard####
harvard<-read.delim("harvard1914.txt", na.strings=c("", " ", "Digitized by Google", "Digitized by Google ", "^[A-Z]$"))
colnames(harvard)<-"text"
harvard$id<-rownames(harvard)
harvard$text<-ifelse(!is.na(str_extract(harvard$text, "^[A-Z]{3,}[-][A-Z]{3,}")), 
                     NA, harvard$text)
harvard$text<-str_replace_all(harvard$text, "^[?](\\s)", "")
harvard<-subset(harvard, !is.na(harvard$text))
harvard$text<-ifelse(toupper(harvard$text)==harvard$text, NA, harvard$text)
harvard$text<-trimws(str_replace_all(harvard$text, "[,](\\s)Jr.", ""), which="both")
harvard$text<-ifelse(!is.na(str_extract(harvard$text, "^$")), NA, harvard$text)
harvard<-subset(harvard, !is.na(harvard$text))
harvard$text<-str_replace_all(harvard$text, "\\(", "[")
harvard$text<-str_replace_all(harvard$text, "\\)", "]")
harvard$text<-gsub("(\\s+\\w+\\,\\s\\w+\\s+\\w+\\s+\\[)", " ?\\1", harvard$text)
harvard$text<-gsub("(\\s+\\w+\\,\\s\\w+\\s+\\[)", " ?\\1", harvard$text)
harvard<-separate_rows(harvard, text, sep="(\\b)[?](\\s)|(\\s)[?](\\s)")
harvard$text<-str_replace_all(harvard$text, "^?(\\s+)", "")
harvard$name<-str_extract(harvard$text, 
                          "^(\\w+)[,](\\s+)(\\w+)(\\s+)(\\w+)(\\s+)(\\w+)(\\s+)(?=\\[)|^(\\w+)[,](\\s+)(\\w+)(\\s+)(\\w+)(\\s+)(?=\\[)|^(\\w+)[,](\\s+)(\\w+)(\\s+)(?=\\[)")
harvard$name<-ifelse(!is.na(harvard$name), harvard$name, 
                     str_extract(harvard$text, 
                                 "^(\\w+)[,](\\s+)(\\w+)(\\s+)(\\w+)(\\s+)(\\w+)(\\s+)(?=[:punct:])|^(\\w+)[,](\\s+)(\\w+)(\\s+)(\\w+)(\\s+)(?=[:punct:])|^(\\w+)[,](\\s+)(\\w+)(\\s+)(?=[:punct:])"))  
harvard$name<-ifelse(!is.na(harvard$name), harvard$name, 
                     str_extract(harvard$text, 
                                 "^(\\w+)[,](\\s+)(\\w+)(\\s+)(\\w+)(\\s+)(\\w+)(?=[:punct:])|^(\\w+)[,](\\s+)(\\w+)(\\s+)(\\w+)(?=[:punct:])|^(\\w+)[,](\\s+)(\\w+)(?=[:punct:])"))  
harvard$name<-ifelse(!is.na(harvard$name), harvard$name, 
                     str_extract(harvard$text, 
                                 "^(\\w+)[,](\\s+)(\\w+)(\\s+)(\\w+)(\\s+)(\\w+)|^(\\w+)[,](\\s+)(\\w+)(\\s+)(\\w+)|^(\\w+)[,](\\s+)(\\w+)"))
harvard$name<-ifelse(!is.na(harvard$name), harvard$name, 
                     str_extract(harvard$text, 
                                 "(?<=[.])(\\w+)[,](\\s+)(\\w+)\\["))
harvard$text2<-pegar(harvard$text, harvard$name)
harvard2<-subset(harvard, !is.na(harvard$text2))                    
harvard2<-separate_rows(harvard2, text2, sep="[|]|[;]")
harvard2$text2<-trimws(harvard2$text2, which="both")
harvard2$text2[harvard2$text2==" "]<-NA
harvard2$text2[harvard2$text2==""]<-NA
harvard2<-harvard2[which(!is.na(harvard2$text2)),]
harvard2$year<-str_extract(harvard2$text2, "16(\\d){2}|17(\\d){2}|18(\\d){2}|19(\\d){2}")
harvard2$text2<-str_replace_all(harvard2$text2, "(?<=[a-z]{1})(?=[A-Z]{1})|(?<=[[:punct:]]{1})(?=[[:alpha:]]{1})|(?<=[[:digit:]]{1})(?=[[:alpha:]]{1})", " ")
harvard2$text2<-str_replace_all(harvard2$text2, "[I|l][I|l][.]", "M.")
for(i in 1:20){
  harvard2$year<-ifelse(!is.na(lag(harvard2$year))&(lag(harvard2$name)==harvard2$name)&(is.na(harvard2$year)),
                        lag(harvard2$year), harvard2$year)
}
length(harvard2$year[which(is.na(harvard2$year))])
edu<-read.csv("haredu.csv")
edu$edu<-str_replace_all(edu$edu, "[.]$","")
edu$edu<-str_replace_all(edu$edu, "[.]","[.]")
edu<-paste(edu$edu, collapse="|")
harvard2$text<-harvard2$text2
harvard2$text2<-str_replace_all(harvard2$text2, "(?<=[A-Z][.])(\\s)(?=[A-Z][.])", "")
harvard2$edu<-toupper(str_extract(tolower(harvard2$text2), tolower(edu)))
abbrev<-read.csv("abbrev.csv")
abbrev$significado<-str_replace_all(abbrev$significado, "^[.]", "")
abbrev$abbrev<-str_replace_all(abbrev$abbrev, "(?<=[[:alpha:]])$|(?<=[[:alpha:]])(\\s)$", ".")
abbrev$prof<-abbrev$abbrev
abbrev$abbrev<-str_replace_all(abbrev$abbrev, "[.]", "[.]")
abbrev1<-paste(abbrev$abbrev, collapse="|")
harvard2$prof<-str_extract(harvard2$text2, abbrev1)
abbrev$prof<-str_replace_all(abbrev$abbrev, "\\[", "")
abbrev$prof<-str_replace_all(abbrev$prof, "\\]", "")
harvard2<-left_join(harvard2, subset(abbrev, select=c("prof", "significado")), by="prof")
harvard2$prof<-harvard2$significado
harvard2$lastname<-str_extract(harvard2$name, "^(.*?)(?=[,])")
harvard2$names<-trimws(str_extract(harvard2$name, "(?<=[,](\\s))(.*?)$"), which = "both")
harvard2$middlename<-mname(harvard2$names)
harvard2$firstname<-str_extract(harvard2$names, "^(\\w+)")
harvard2$simpname<-sname(harvard2)  
harvard2$source<-"harvard"
harvard2$nationality<-NA
harvard2$loc<-ifelse(is.na(str_extract(harvard2$text2, "(?<=])$"))&is.na(str_extract(harvard2$text2, "(?<=])(\\d+)$")),
                     str_extract(harvard2$text2, "(?<=])(.*?)$"), NA)
for(i in 1:6){
  harvard2$loc<-trimws(
    ifelse(!is.na(str_extract(harvard2$loc, "(?<=])(.*?)$"))&is.na(str_extract(harvard2$loc, "(?<=])$")), 
           str_extract(harvard2$loc, "(?<=])(.*?)$"), harvard2$loc), 
    which="both")
}
harvard3<-buscar2(harvard2)
harvard3$org<-str_extract(harvard3$text2, "(?<=])(.*?)Co[.]")
for(i in 1:6){
  harvard3$org<-trimws(
    ifelse(!is.na(str_extract(harvard3$org, "(?<=])(.*?)Co[.]")), 
           str_extract(harvard3$org, "(?<=])(.*?)Co[.]"), harvard3$org), 
    which="both")
  harvard3$org<-trimws(
    ifelse(!is.na(str_extract(harvard3$org, "(?<=Care(\\s)of)(.*?)Co[.]")), 
           str_extract(harvard3$org, "(?<=Care(\\s)of)(.*?)Co[.]"), harvard3$org), 
    which="both")
  harvard3$org<-trimws(
    ifelse(!is.na(str_extract(harvard3$org, "(?<=[.][,])(.*?)Co[.]")), 
           str_extract(harvard3$org, "(?<=[.])(.*?)Co[.]"), harvard3$org), 
    which="both")
}
harvard3$org<-str_replace_all(harvard3$org, "[^[:alpha:]]", " ")
for(i in 1:10){
  harvard3$org<-str_replace_all(harvard3$org, "(\\s+)", " ")
  harvard3$org<-str_replace_all(harvard3$org, "^(\\s+)", "")
}
harvard3$country<-coords2country(harvard3)
harvard3$type<-"alumni"
harvard4<-subset(harvard3, select=c("id","source", "year", "simpname", "nationality", "edu", "prof", "org", "loc", "lon", "lat", "lastname", "firstname","middlename", "text", "country", "type"))
harvengi<-harvard3[which(!is.na(str_extract(tolower(harvard3$text2), "m[.]c[.]e(\\b)|c[.]e(\\b)|m[.]m[.]e(\\b)|m[.]e(\\b)|met[.]e(\\b)|s[.]m[.]chem(\\b)|s[.]m[.]geol(\\b)"))|
                           !is.na(str_extract(harvard3$prof, "Enginee|Geol|Minin|Chem"))),]
harvengi<-subset(harvengi, select=c("id","source", "year", "simpname", "nationality", "edu", "prof", "org", "loc", "lon", "lat", "lastname", "firstname","middlename", "text", "country", "type"))
write.csv(harvard4, "harvardfinal.csv")
write.csv(harvengi, "harvengi.csv")
####X. Lehigh####
lugares<-read.csv("lugares2.csv")
lugares$loc<-trimws(str_replace_all(lugares$loc, "^(\\s+)", ""), which="both")
lugares1<-lugares
lugares1$loc<-paste("(\\w+)[,](\\s+)",lugares1$loc, sep="")
lugares2<-lugares
lugares2$loc<-paste("(\\w+)[.](\\s+)",lugares2$loc, sep="")
lugaresb<-rbind(lugares1, lugares2)
lugares1<-paste(lugares$loc, collapse="|")
lugares2<-paste(lugaresb$loc, collapse="|")  
lehigh<-read.csv("lehigh.csv")
lehigh<-separate_rows(lehigh, text, sep="^f(?=[A-Z][a-z])|^-(?=[A-Z][a-z])|^t(?=[A-Z][a-z])|^f(\\s)(?=[A-Z][a-z])|^[\\*](?=[A-Z][a-z])|^[\\*](\\s)(?=[A-Z][a-z])|^-(\\s)(?=[A-Z][a-z])|^t(\\s)(?=[A-Z][a-z])")
lehigh$text<-trimws(lehigh$text, which="both")
lehigh<-lehigh[which(lehigh$text!=""&is.na(str_extract(lehigh$text, "^ALUMNI(\\s)AND(\\s)STUDENTS"))),]
lehigh$id<-rownames(lehigh)
lehigh$name<-str_extract(lehigh$text, "^[A-z]{3,}[,](\\s+)[A-Z][a-z]{2,}|^[A-z]{3,}\\^(\\s+)[A-Z][a-z]{2,}")
temp<-subset(lehigh, select=c("id", "name"), !is.na(name))
temp$name<- ifelse(str_extract(temp$name, "^(\\w){1}")==str_extract(lag(temp$name), "^(\\w){1}")|
                     str_extract(temp$name, "^(\\w){1}")==str_extract(lead(temp$name), "^(\\w){1}")|
                     str_extract(temp$name, "^(\\w){1}")==str_extract(lag(temp$name,2), "^(\\w){1}")|
                     str_extract(temp$name, "^(\\w){1}")==str_extract(lead(temp$name,2), "^(\\w){1}"),
                   temp$name, 
                   ifelse((match(str_extract(temp$name, "^(\\w){1}"), LETTERS[1:26])-match(str_extract(lag(temp$name), "^(\\w){1}"), LETTERS[1:26])>=0)&
                            (match(str_extract(temp$name, "^(\\w){1}"), LETTERS[1:26])-match(str_extract(lag(temp$name), "^(\\w){1}"), LETTERS[1:26])<=2), 
                          temp$name, NA))
lehigh$name1<-lehigh$name
lehigh$name<-NULL
lehigh<-left_join(lehigh, temp, by="id")
lehigh$text2<-ifelse(!is.na(lehigh$name)&is.na(lead(lehigh$name))&is.na(lead(lehigh$name, 2)), 
                     paste(lehigh$text, 
                           ifelse((toupper(str_extract(lead(lehigh$text), "^(\\w){1}"))!=str_extract(lead(lehigh$text), "^(\\w){1}"))|
                                    (is.na(str_extract(lead(lehigh$text), "^(\\w){1}"))), "", " "),
                           lead(lehigh$text), 
                           ifelse(toupper(str_extract(lead(lehigh$text,2), "^(\\w){1}"))!=str_extract(lead(lehigh$text,2), "^(\\w){1}")|
                                    (is.na(str_extract(lead(lehigh$text,2), "^(\\w){1}"))) , "", " "),
                           lead(lehigh$text,2), sep=""), 
                     ifelse(!is.na(lehigh$name)&is.na(lead(lehigh$name)), 
                            paste(lehigh$text, 
                                  ifelse(toupper(str_extract(lead(lehigh$text), "^(\\w){1}"))!=str_extract(lead(lehigh$text), "^(\\w){1}")|
                                           (is.na(str_extract(lead(lehigh$text), "^(\\w){1}"))), "", " "),
                                  lead(lehigh$text), sep=""), 
                            ifelse(!is.na(lehigh$name), lehigh$text, NA)))
lehigh<-lehigh[which(!is.na(lehigh$text2)),]
lehigh$name<-str_extract(lehigh$text2, "^(.*?)[,|\\^](.*?)(?=[,|.|\\^])")
lehigh$name<-str_replace_all(lehigh$name, "[,|.|\\^]", ",")
lehigh$name<-str_to_title(lehigh$name)
lehigh$lastname<-str_extract(lehigh$name, "(.*?)(?=[,])")
lehigh$names<-str_extract(lehigh$name, "(?<=[,](\\s)).*$")
lehigh$firstname<-str_extract(str_extract(lehigh$names, "^(\\w+)"), "(\\w+)$")
lehigh$middlename<-str_extract(str_extract(lehigh$names, "^(\\w+)(\\s+)(\\w+)"), "(\\w+)$")
lehigh$simpname<-sname(lehigh)
lehigh$text2<-str_replace_all(lehigh$text2, "[']r", "r")
lehigh$text2<-trimws(str_replace_all(lehigh$text2, "&", "and"), which = "both")
lehigh$text2<-str_replace_all(lehigh$text2, "(\\s+)", " ")
lehigh<-separate_rows(lehigh, text2, sep ="(?=['](\\d))")
lehigh$year<-str_extract(lehigh$text2, "(?<=['])(\\d+)")
lehigh$degree<-str_extract(lehigh$text2, "[A-Z][.]E[.]")
lehigh$role<-trimws(str_extract(lehigh$text2, "(?<=(\\d)[,])(.*?)(?=[,])"), which="both")
lehigh$role<-ifelse(!is.na(str_extract(lehigh$role, "^with|^of|^(\\d)|^Box|^[A-Z][.][A-Z][.]|Address")), NA, lehigh$role)
lehigh$with<-str_extract(lehigh$text2, "(?<=with(\\s))(.*?)[[:punct:]]")
lehigh$company<-str_extract(lehigh$text2, "(?<=[,])(.*?)Co[[:punct:]]")
for(i in 1:10){
  lehigh$company<-trimws(
    ifelse(!is.na(str_extract(lehigh$company, "(?<=[,])(.*?)Co[[:punct:]]$")), 
           str_extract(lehigh$company, "(?<=[,])(.*?)Co[[:punct:]]$"), 
           lehigh$company), 
    which="both")
}
lehigh$organization<-trimws(str_extract(lehigh$text2, "(?<=(\\d)[,])(.*?)[,](.*?)(?=[[:punct:]])"), which="both")
lehigh$organization<-trimws(str_extract(lehigh$organization, "(?<=[,])(.*?)$"), which="both")
lehigh<-fill(lehigh, c("names", "firstname", "middlename", "simpname", "lastname", "name"), .direction="down")
lehigh$location<-str_extract(lehigh$text2, lugares2)
lehigh$location<-ifelse(!is.na(lehigh$location), lehigh$location, 
                        str_extract(lehigh$text2, lugares1))
for(i in 1:10){
  lehigh$location<-ifelse(!is.na(lehigh$location), lehigh$location,
                          str_extract(lehigh$text2, "(?<=Res(\\b))(.*?)$|(?<=Address(\\b))(.*?)$"))
  lehigh$location<-ifelse(!is.na(lehigh$location), lehigh$location,
                          str_extract(lehigh$text2, "(\\w+)[,](\\s+)(\\w+)(?=[.](\\s)Res(\\b))|(\\w+)[,](\\s+)(\\w+)(?=[.](\\s)Address(\\b))"))
  lehigh$location<-ifelse(!is.na(lehigh$location), lehigh$location, 
                          str_extract(lehigh$text2, "(\\w+)[,](\\s+)(\\w+)(\\s+)(\\w+)(\\s+)(\\w+)[.]$|(\\w+)[,](\\s+)(\\w+)(\\s+)(\\w+)[.]$|(\\w+)[,](\\s+)(\\w+)[.]$"))
}
#lehigh$location<-str_extract(lehigh$text2, "(.*)(\\w+)[,](.*?)$")
lehigh$organization2<-ifelse(!is.na(lehigh$with), lehigh$with, 
                             ifelse(!is.na(lehigh$with), lehigh$with, lehigh$organization))
temp<-lehigh[which(!is.na(lehigh$name)&is.na(lehigh$location)),]
lehigh$source<-"lehigh"
professions<-c("manager", "vicepresident", "superintendent", "president","engineer", "accountant", "agent",  "merchant", "professor", "adjuster", "analyst", "assistant", "abstractor", "official", "artist", "assayer", "associate", "attorney", "author", "bacteriologist", "banker", "bookkeeper", "broker", "builder", "captain", "cashier", "cartoonist", "chairman", "chemist", "clerk", "clergy", "colonel", "constructor", "contractor", "counselor", "custodian", "designer", "dentist", "director", "draftsman", "drawer", "editor", "estimator", "exporter", "farmer", "fellow", "geologist", "geographer", "graduate", "student", "hydrographer", "importer", "inspector", "journalist", "judge", "lawyer", "librarian", "lecturer", "instructor", "lieutenant", "major", "mechanic", "machinist", "manufacturer", "major", "soldier", "master", "metallurgist", "miner", "oculist", "teacher", "physicist", "owner", "petrologist", "pathfinder", "pastor", "expert", "pharmacist", "photographer", "physician", "planter", "producer", "proprietor", "psychologist", "publisher", "pyrologist", "register", "salesman", "specialist", "statistician", "supervisor", "surveyor", "timekeeper", "transitman", "treasurer", "trustee", "musician", "researcher", "deputy", "secretary", "architect", "dealer", "electrician", "optician", "storekeeper", "inventor", "decorator", "computer", "buyer", "controller", "drugist", "foreman", "erector", "investigator", "calculator", "scholar", "died", "deceased", "retired")
professions<-paste(professions, collapse = "|")
lehigh$role<-ifelse(!is.na(lehigh$role), lehigh$role, str_extract(lehigh$text2, professions))
lehigh<-subset(lehigh, select=c("source","year","id", "text2", "name", "lastname", "middlename", "firstname", "location", "degree", "role", "organization2"))
colnames(lehigh)<-c("source","year","id", "text", "name", "lastname", "middlename", "firstname", "loc", "edu", "prof", "org")
lehigh$type<-"alumni"
lehigh$nationality<-NA
lehigh<-buscar2(lehigh)
lehigh$country<-coords2country(lehigh)
write.csv(lehigh, "lehigh2.csv")

####XI. South Dakota####
sdakota<-read.csv("sdakota.csv", na.strings = c("", " "))
states<-read.csv("states.csv")
countries<-read.csv("countries.csv")
countries$country<-trimws(str_replace_all(countries$country, "^(\\s)", ""), which="both")
state<-paste(states$State, collapse = "|")
countrie<-paste(countries$country, collapse = "|")
sdakota$id<-rownames(sdakota)
sdakota$text2<-paste(sdakota$text, 
                     ifelse(toupper(str_extract(sdakota$X, "^[[:alpha:]]{1}"))!=str_extract(sdakota$X, "^[[:alpha:]]{1}"),"", " "), sdakota$X,
                     ifelse(toupper(str_extract(sdakota$X1, "^[[:alpha:]]{1}"))!=str_extract(sdakota$X1, "^[[:alpha:]]{1}"),"", " "), sdakota$X1,      
                     ifelse(toupper(str_extract(sdakota$X2, "^[[:alpha:]]{1}"))!=str_extract(sdakota$X2, "^[[:alpha:]]{1}"),"", " "), sdakota$X2,
                     ifelse(toupper(str_extract(sdakota$X3, "^[[:alpha:]]{1}"))!=str_extract(sdakota$X3, "^[[:alpha:]]{1}"),"", " "), sdakota$X3,
                     ifelse(toupper(str_extract(sdakota$X4, "^[[:alpha:]]{1}"))!=str_extract(sdakota$X4, "^[[:alpha:]]{1}"),"", " "), sdakota$X4)
sdakota$text2<-trimws(str_replace_all(str_replace_all(sdakota$text2, "NA", ""),"(\\s+)", " "), which = "both")
sdakota<-subset(sdakota, select="text2")
sdakota$id<-rownames(sdakota)
sdakota$year<-str_extract(sdakota$text2, "^(\\d+)$")
sdakota<-fill(sdakota, year, .direction="down")
sdakota$name<-str_extract(sdakota$text2, "(.*?)[,]")
sdakota<-subset(sdakota, !is.na(sdakota$name))
sdakota$text2<-str_replace_all(sdakota$text2, "(?<=[A-Z][.])(?=[A-Z][.])", " ")
sdakota$text2<-str_replace_all(sdakota$text2, "-(?=[.])", "")
sdakota$text2<-str_replace_all(sdakota$text2, "[[:punct:]]$", ".")
sdakota$degree<-str_extract(sdakota$text2, "M.(\\s)S.|M.(\\s)A.|E.(\\s)M.|M.(\\s)E.|A.(\\s)B.|M.S.|M.A.|E.M.|M.E.|A.B.")
sdakota$text2<-str_replace_all(sdakota$text2, "(?<=[A-Z][.])(\\s+)(?=[A-Z][.])", "")
sdakota$text2<-str_replace_all(sdakota$text2, "(\\s+)", " ")
sdakota$location<-str_extract(sdakota$text2, "(\\w+)(\\s+)(\\w+)[,](\\s+)(\\w+)[.]$|(\\w+)[,](\\s+)(\\w+)[.]$")
sdakota$location<-ifelse(!is.na(sdakota$location), sdakota$location, str_extract(sdakota$text2, state))
sdakota$location<-ifelse(!is.na(sdakota$location), sdakota$location, str_extract(sdakota$text2, countrie))
sdakota$edu<-ifelse(!is.na(sdakota$degree), sdakota$degree, "B.S.")
edu<-paste(unique(sdakota$edu), collapse="(.*?)(?=[,])|")
sdakota$prof<-str_extract(sdakota$text2, edu)
sdakota$prof<-str_replace_all(sdakota$prof, "^[A-Z][.](\\s+)[A-Z][.](\\s+)|^[A-Z][.][A-Z][.](\\s+)|^[A-Z][.](\\s+)[A-Z][.]|^[A-Z][.][A-Z][.]", "")
sdakota$prof<-str_replace_all(sdakota$prof, "(?<=[a-z])(\\s+)(?=[a-z])", "")
sdakota<-separate(sdakota, prof, into=c("prof", "org"), sep="With|(?<=Laboratory)|(?<=ident)|(?<=ndent)|(?<=mist)|(?<=tive)")
sdakota$prof<-ifelse(sdakota$prof==""|sdakota$prof==" ", NA, sdakota$prof)
sdakota$prof<-ifelse(sdakota$org==""|sdakota$org==" ", NA, sdakota$prof)
sdakota$type<-"alumni"
sdakota$source<-"sdakota"
sdakota$loc<-sdakota$location
sdakota<-buscar2(sdakota)
sdakota$country<-coords2country(sdakota)
sdakota$lastname<-str_extract(sdakota$name, "[A-Z]'(\\w+)(?=[,])|(\\w+)(?=[,])")
sdakota$firstname<-fname(sdakota$name)
sdakota$names<-str_replace(sdakota$name, "(\\s+)[A-Z]'(\\w+)[,]|(\\s+)(\\w+)[,]", "")
sdakota$middlename<-mname(sdakota$names)
sdakota$simpname<-sname(sdakota)
sdakota$nationality<-NA
sdakota$text<-sdakota$text2
sdakota2<-subset(sdakota, select=c("id","source", "year", "simpname", "nationality", "edu", "prof", "org", "loc", "lon", "lat", "lastname", "firstname","middlename", "text", "country", "type"))
write.csv(sdakota2, "sdakotafinal.csv")
####XII. Nevada####
states<-read.csv("states.csv")
countries<-read.csv("countries.csv")
countries$country<-trimws(str_replace_all(countries$country, "^(\\s)", ""), which="both")
state<-paste(states$State, collapse = "|")
countrie<-paste(countries$country, collapse = "|")
location<-paste(countrie, state, sep="|")
nevada<-read.csv("nevada.csv")
nevada$id<-rownames(nevada)
nevada$text2<-ifelse(!is.na(str_extract(nevada$text, "UNIVERSITY GRADUATES")), NA, nevada$text)
nevada$text2<-str_replace_all(nevada$text2, "^[[:punct:]](?=[A-Z])|^f(?=[A-Z])", "")
for(i in 1:10){
  nevada$text2<-str_replace_all(nevada$text2, "(?<=[A-Z])[a-z](?=[A-Z])", toupper(str_extract(nevada$text2, "(?<=[A-Z])[a-z](?=[A-Z])")))
  nevada$text2<-str_replace_all(nevada$text2, "(?<=[A-Z]{2})[a-z]", toupper(str_extract(nevada$text2,"(?<=[A-Z]{2})[a-z]")))
  nevada$text2<-str_replace_all(nevada$text2, "(\\s+)", " ")
  nevada$text2<-str_replace_all(nevada$text2, "(?<=[A-Z]{3})[.](?=(\\s)[A-Z]{2})", ",")
}
nevada$text2<-trimws(str_replace_all(nevada$text2, "(\\s+)REV[.]", ""), which="both")
nevada$name<-ifelse(!is.na(str_extract(nevada$text2, "^[A-Z]{3}")), 
                    str_extract(nevada$text2, "^(\\w+)[,](\\s+)(\\w){2,}"), NA)
nevada<-nevada[which(!is.na(nevada$text2)),]
nevada$text2<-pegar(nevada$text2, nevada$name)
nevada$text2<-str_replace_all(nevada$text2, "Mrs[.]", "Mrs")
nevada$text2<-str_replace_all(nevada$text2, "Mr[.]", "Mr")
nevada$text2<-trimws(str_replace_all(nevada$text2, "(Last(//s)address[.])|(Lastaddress[.])", ""), which="both")
nevada$text2<-trimws(str_replace_all(nevada$text2, "\\(|\\)", ""), which="both")
nevada$text2<-trimws(str_replace_all(nevada$text2, "(\\s+)", " "), which="both")
nevada$text2<-trimws(str_replace_all(nevada$text2, "(\\s+)", " "), which="both")
nevada2<-nevada[which(!is.na(nevada$text2)),]
nevada2<-separate_rows(nevada2, text2, sep="(?<=[.])(\\s)(?=[A-Z]{2})|(?<=(\\b))f(\\s)(?=[A-Z]{2})|(?<=(\\b))f[.](?=[A-Z]{2})|(?<=(\\b))f(?=[A-Z])")
nevada2<-nevada2[which(nevada2$text2!=""),]
nevada2$text2<-str_replace_all(nevada2$text2, "[,]", ", ")
nevada2$text2<-str_replace_all(nevada2$text2, "(\\s+)", " ")
nevada2<-separate_rows(nevada2, text2, sep=":")
nevada2$text2<-ifelse(!is.na(str_extract(nevada2$text2, "^$|ATALOGUE")), NA, nevada2$text2)
nevada2<-nevada2[which(!is.na(nevada2$text2)),]
nevada2$name<-ifelse(!is.na(str_extract(nevada2$text2, "^[A-Z]{3}")), 
                     str_extract(nevada2$text2, "^(\\w+)[,](\\s+)(\\w){2,}"), NA)
nevada2$text2<-pegar(nevada2$text2, nevada2$name)
nevada2<-nevada2[which(!is.na(nevada2$text2)),]
nevada3<-separate_rows(nevada2, text2, sep=";")
nevada3$text2<-str_replace(nevada3$text2, "^(\\s+)", "")
nevada3$text2<-str_replace(nevada3$text2, "(\\s+)Last(\\s)address[.]", "")
nevada3$degree<-str_extract(nevada3$text2, "[A-Z][.][A-Z][.]")
nevada3$degree2<-ifelse(!is.na(str_extract(nevada3$text,"(?<=\\()(.*?)(?=\\))")),
                        str_extract(nevada3$text,"(?<=\\()(.*?)(?=\\))"), 
                        ifelse(str_extract(nevada3$text, "(?<=\\()(\\w+)")==str_extract(nevada3$text, "(\\w+)(?=[.]\\))"), 
                               str_extract(nevada3$text, "(?<=\\()(\\w+)"), 
                               paste(str_extract(nevada3$text, "(?<=\\()(\\w+)"), str_extract(nevada3$text, "(\\w+)(?=[.]\\))"), sep=" ")))
nevada3$degree2<-ifelse(!is.na(str_extract(nevada3$degree2,"Mrs|ddress")), NA, nevada3$degree2)
nevada3$year<-str_extract(nevada3$text2, "18[[:digit:]]{2}|19[[:digit:]]{2}")
nevada3$lastname<-str_extract(nevada3$name, "^(.*?)(?=[,])")
nevada3$middlename<-str_extract(nevada3$name, "(?<=(\\w)(\\s))(\\w+)$")
nevada3$firstname<-str_extract(nevada3$name, "(?<=[,](\\s))(\\w+)")
nevada3$simpname<-sname(nevada3)
nevada3$role<-trimws(str_extract(nevada3$text2, "(?<=19(\\d){2}[,])(.*?)(?=[,])|(?<=18(\\d){2}[,])(.*?)(?=[,])"), which="both")
nevada3<-separate(nevada3, role, sep="care(\\s)of(\\b)|(\\b)with(\\b)", into=c("role", "organization"))
nevada3$role<-ifelse(!is.na(str_extract(nevada3$role, "^(\\d+)|Block|Mrs|Building|(\\b)at(\\b)|^$|Ave[.]|St[.]|Box")), NA, nevada3$role)
nevada3$location<-str_extract(nevada3$text2, location)
nevada3$location<-ifelse(!is.na(nevada3$location), nevada3$location, 
                         str_extract(nevada3$text2, "(\\w+)(\\s+)(\\w+)[.]$|(\\w+)[.]$"))
nevada3$location<-ifelse(!is.na(nevada3$location), nevada3$location, 
                         ifelse(!is.na(str_extract(nevada3$location, "^[A-Z][.]$")), 
                                NA, nevada3$location))
nevada3$loc<-nevada3$location
temp<-nevada3[which(is.na(nevada3$location)),]
nevada3<-buscar2(nevada3)
nevada3$country<-coords2country(nevada3)
nevada3$nationality<-NA  
nevada3$source<-"nevada"
nevada3$prof<-nevada3$role
nevada3$edu<-ifelse(!is.na(nevada3$degree2), nevada3$degree2, nevada3$degree)
nevada3$text<-nevada3$text2
nevada3$type<-"alumni"
nevada3$org<-nevada3$organization
nevada4<-subset(nevada3, select=c("id","source", "year", "simpname", "nationality", "edu", "prof", "org", "loc", "lon", "lat", "lastname", "firstname","middlename", "text", "country", "type"))
write.csv(nevada4, "nevadafinal.csv")

####XIII. Missouri####
states<-read.csv("states.csv")
countries<-read.csv("countries.csv")
countries$country<-trimws(str_replace_all(countries$country, "^(\\s)", ""), which="both")
state<-paste(states$State, collapse = "|")
countrie<-paste(countries$country, collapse = "|")
location<-paste(countrie, state, sep="|")
missouri<-read.csv("missouri.csv")
missouri$text<-str_replace_all(missouri$text, "(\\s+)", " ")
missouri$text<-str_replace_all(missouri$text, "(\\s)I2(\\s)|For(\\s)list(\\s)of(\\s)deceased(\\s)Alumni(\\s)see(\\s)page(\\s)[.]31|MISSOURI(\\s)SCHOOL(\\s)OF(\\s)M(\\s)INES(\\s)(\\d){2}|MISSOURI(\\s)SCHOOL(\\s)OF(\\s)MINES(\\s)(\\d){2}|MISSOURI SCHOOL OF MINES", "")
miss<-as.data.frame(paste(missouri$text, collapse=" "))
colnames(miss)<-"text"
miss<-separate_rows(miss, text, sep="(?<=[.](\\s))(?=[A-Z])|\\*")
miss$id<-rownames(miss)
miss$text<-str_replace_all(miss$text, "(?<=(\\w))(\\s+)(?=[.])", "")
miss$name<-str_extract(miss$text, "^(\\w+)(\\s+)[,](\\s+)(\\w){2,}(\\s+)")
temp<-subset(miss, select=c("id", "name"), !is.na(name))
temp$name<- ifelse(str_extract(temp$name, "^(\\w){1}")==str_extract(lag(temp$name), "^(\\w){1}")|
                     str_extract(temp$name, "^(\\w){1}")==str_extract(lead(temp$name), "^(\\w){1}")|
                     str_extract(temp$name, "^(\\w){1}")==str_extract(lag(temp$name,2), "^(\\w){1}")|
                     str_extract(temp$name, "^(\\w){1}")==str_extract(lead(temp$name,2), "^(\\w){1}"),
                   temp$name, 
                   ifelse((match(str_extract(temp$name, "^(\\w){1}"), LETTERS[1:26])-match(str_extract(lag(temp$name), "^(\\w){1}"), LETTERS[1:26])>=0)&
                            (match(str_extract(temp$name, "^(\\w){1}"), LETTERS[1:26])-match(str_extract(lag(temp$name), "^(\\w){1}"), LETTERS[1:26])<2), 
                          temp$name, NA))
#miss$name1<-miss$name
miss$name<-NULL
miss<-left_join(miss, temp, by="id")
miss$text2<-pegar(miss$text, miss$name)
miss2<-miss[which(!is.na(miss$text2)),]
for(i in 1:10){
  miss2$text2<-str_replace_all(miss2$text2, "(?<=(\\w))(\\s+)(?=[[:punct:]])|(?<=[[:punct:]])(\\s+)(?=[[:punct:]])", "")
  miss2$text2<-str_replace_all(miss2$text2, "[.][.]", ".")
  miss2$text2<-str_replace_all(miss2$text2, "(?<=[[:punct:]])(?=(\\w))", " ")
}
miss3<-separate_rows(miss2, text2, sep="(?=B[.](\\s)S[.])|(?=E[.](\\s)M[.])|(?=C[.](\\s)E[.])|(?=M[.](\\s)E[.])|M[.](\\s)S[.]")
miss3$edu<-str_extract(miss3$text2, "[A-Z][.](\\s)[A-Z][.]")
miss3$year<-str_extract(miss3$text2, "(?<='(\\s))(\\d){2}|(?<=')(\\d){2}")
miss3<-separate(miss3, text2, into = c("rest0","rest"),sep="'(\\s)(\\d){2}|'(\\d){2}", remove=F)
miss3$rest<-trimws(str_replace_all(miss3$rest, "^[[:punct:]](\\s+)|^[[:punct:]]", ""), which="both")
miss3$rest<-str_replace_all(miss3$rest, "^(\\s+)", "")
miss3$rest<-str_replace_all(miss3$rest, "(\\s+)", " ")
miss3$rest<-str_replace_all(miss3$rest, "[,][.]", ",")
miss3$rest<-str_replace_all(miss3$rest, "(?<=[A-Z])[.](\\s+)(?=[A-Z][.])|(?<=[A-Z]t)[.]|(?<=E)[.]", "")
miss3$rest<-str_replace_all(miss3$rest, "^(\\d+)(\\s+)[A-Z][a-z][.]|^(\\d+)(\\s+)[A-Z][.]", "")
miss3$loc<-str_extract(miss3$rest, "(.*?)[.]")
miss3$rest2<-str_extract(miss3$rest, "(?<=[.](\\s)).*")
miss3<-separate(miss3, rest2, into=c("prof", "org"), sep="with|[,]")
profesiones<-c("Engr.", "Engineer", "Supt.", "Superintendent", "Mgr.", "Manager", "Surveyor", "Geologist", "Agent",
               "Boss", "Chemist", "Assayer", "Foreman", "Metallurgist", "Dept.", "Technic", "Experimental Work")
profs1<-paste("(\\w+)(\\s+)", profesiones, sep="")
profs2<-rbind(profesiones, profs1)
profs2<-paste(profs2, collapse ="|")
miss3$prof<-ifelse(!is.na(str_extract(miss3$rest, profs2)), str_extract(miss3$rest, profs2), miss3$prof)
miss3$loc<-ifelse(!is.na(str_extract(miss3$rest, location)), str_extract(miss3$rest, location), miss3$loc)
miss3$org<-ifelse(!is.na(str_extract(miss3$rest, "(.*?)Co(\\b)")), str_extract(miss3$rest, "(.*?)Co(\\b)"), miss3$org)
miss3$name<-str_extract(miss3$text, "(.*?)[,](.*?)(?=[,])")
miss3<-separate(miss3, name, sep="[,](\\s)", into=c("lastname", "names"), remove=F)
miss3$lastname<-trimws(miss3$lastname, which="both")
miss3$firstname<-str_extract(miss3$names, "(\\w+)")
miss3$middlename<-mname(miss3$names)
miss3$names<-trimws(miss3$names, which="both")
miss3<-buscar2(miss3)
miss3$country<-coords2country(miss3)
miss3$nationality<-NA
miss3$type<-"alumni"
miss3$source<-"missouri"
miss3$simpname<-sname(miss3)
miss4<-subset(miss3, select=c("id","source", "year", "simpname", "nationality", "edu", "prof", "org", "loc", "lon", "lat", "lastname", "firstname","middlename", "text", "country", "type"))
write.csv(miss4, "missoufinal.csv")
#### XIV. U Minnesota####
umn = read.csv("UMN Alumni.csv")
# Clean numbers
umn$text2 = str_replace_all(umn$text, "(?<=(\\d|[I]))[Q](?=(\\d))", "9")
umn$text2 = str_replace_all(umn$text2, "IOI I", "1911")
umn$text2 = str_replace_all(umn$text2, "(?<=(\\d))[I](?=[.])", "1")
umn$text2 = str_replace_all(umn$text2, "(?<=(\\d))[I](?=(\\d))", "1")
umn$text2 = str_replace_all(umn$text2, "[I](?=(\\d))", "1")
umn$text2 = str_replace_all(umn$text2, "[I](\\s)[1]", "11")
umn$text2 = str_replace_all(umn$text2, "[1](\\s)[I]", "11")
umn$text2 = str_replace_all(umn$text2, "[I](\\s)[I]", "11")
umn$text2 = str_replace_all(umn$text2, "[1](\\s)[1]", "11")
umn$text2 = str_replace_all(umn$text2, "(?<=(\\w))[1](?=(\\w))", "I")
umn$text2 = str_replace_all(umn$text2, "(?<=(\\d))[O|0](?=[.])", "0")
umn$text2 = str_replace_all(umn$text2, "(?<=(\\d))[O|0](?=(\\d))", "0")
umn$text2 = str_replace_all(umn$text2, "(?<=(\\d))[O|0](?=[.])", "0")
umn$text2 = str_replace_all(umn$text2, "[O|0](?=(\\d))", "0")
umn$text2 = str_replace_all(umn$text2, "^[O|0]", "0")
umn$text2 = str_replace_all(umn$text2, "(?<=(\\s))IO(?=[.])", "10")
umn$text2 = str_replace_all(umn$text2, "(?<=(\\s))II", "11")
umn$text2 = str_replace_all(umn$text2, "(?<=(\\d)(\\d))(\\s)(?=(\\d)(\\d))", "")
umn$text2 = str_replace_all(umn$text2, "(?<=(\\d))[I](?=(\\d))", "1")
# Clean text - *do a bit more here to clean names*
umn$text2 = str_replace_all(umn$text2, "(?<=([[:upper:]][[:upper:]]))(\\s)(?=([[:upper:]][[:upper:]]))", "")
umn$text2 = str_replace_all(umn$text2, "[*]", "")
umn$text2 = str_replace_all(umn$text2, "MAURICE.", "MAURICE,")
# Split data by name entries
umn = separate_rows(umn, text2, sep="([.](\\s)){3,}")
umn = separate_rows(umn, text2, sep="([-](\\s)){3,}")
umn = separate_rows(umn, text2, sep="[.]{5,}")
# Extract name and degree
umn$name = ifelse(!is.na(str_extract(umn$text2, "^[[:upper:]][[:upper:]]")), str_extract(umn$text2, "^([[:print:]]+)[,]([[:print:]]+)[,]"), NA)
umn$lastname = ifelse(!is.na(umn$name), str_extract(umn$name, "^(\\w+)"), NA)
umn$firstname = ifelse(!is.na(umn$name), str_extract(umn$name, "(?<=[,])(\\s)(\\w+)"), NA)
umn$middlename = ifelse(!is.na(umn$name), str_extract(umn$name, "(\\w+)([.][,])$"), NA)
umn$degree = ifelse(!is.na(umn$name), 
                    ifelse(!is.na(str_extract(umn$text2, "[[:upper:]][.][[:upper:]][.]+$")), str_extract(umn$text2, "[[:upper:]][.][[:upper:]][.]+$"),
                           ifelse(!is.na(str_extract(umn$text2, "(\\w+)[.][[:upper:]][.]+$")), str_extract(umn$text2, "(\\w+)[.][[:upper:]][.]+$"), NA)), NA)
#one way#
umn$details<-ifelse(!is.na(umn$name), NA, umn$text2)
for(i in nrow(umn):1){
  if(is.na(umn$name[i+1])){
    umn$details[i]<-paste(umn$details[i], umn$details[i+1], sep=" ")}
  else{umn$details[i]}
}
umn$details<-str_replace_all(umn$details, "^NA(\\s)", "")
umn$details=trimws(umn$details,  which="both")
# Make a "details" column and include everything between names into the previous person's details 
## Extract info from "details"
## e.g. grad_year, city/state, occupation/job, company
## If name is NA, and name for following row is also NA, paste the rows together
umn$details=trimws(umn$details,  which="both")
#separating the rows with individuals information
umn2<-umn[which(!is.na(umn$name)),]
umn2$grad_year = str_extract(umn2$details, "^(\\d){4}")
#I think one way is to break the lines based on the years
umn2<-separate_rows(umn2, details, sep="(?<=19(\\d){2}(\\b))")
umn2$details<-trimws(str_replace_all(umn2$details, "^(\\s+)|^[[:punct:]](\\s+)", ""), which="both")
#take the ones that start with to and put them on their place
umn2$extra<-str_extract(umn2$details, "^to(.*?)19(\\d){2}(\\b)|^to(\\s+)date")
umn2$details<-str_replace_all(umn2$details, "^to(.*?)19(\\d){2}(\\b)|^to(\\s+)date", "")
umn2$details<-ifelse(!is.na(lead(umn2$extra)), paste(umn2$details, lead(umn2$extra), sep=" "), umn2$details)
umn2<-umn2[which(umn2$details!=""&umn2$details!="."), ]
umn2$year<-str_extract(umn2$details, "19(\\d){2}")
#after this, I see two solutions. One is to extract the employment though a small set of suffixes
suffix<-c("(\\b)co(\\b)",
          "(\\b)corp(\\b)", 
          "(\\b)company(\\b)",
          "(\\b)corporation(\\b)", 
          "(\\b)corp(\\b)", 
          "(\\b)department(\\b)", 
          "(\\b)dept(\\b)", 
          "college", 
          "service")
suffix1<-paste("(?<=[[:punct:]])([[:print:]]+)", suffix, sep="")
suffix2<-paste("(?<=for)([[:print:]]+)", suffix, sep="")
suffix3<-paste("(?<=with)([[:print:]]+)", suffix, sep="")
suffixes<-as.vector(rbind(suffix1, suffix2,suffix3))
suffixes<-paste(suffixes, collapse="|")
umn2$details<-str_replace(umn2$details, "U[.]S[.]", "US")
umn2$employment<-str_to_title(str_extract(tolower(umn2$details), suffixes))
length(umn2$employment[which(!is.na(umn2$employment))])
#the problem with this approach, is that you have to clean it several times
for(i in 1:10){
  umn2$employment<-trimws(
    ifelse(!is.na(str_extract(tolower(umn2$employment), suffixes)), 
           str_to_title(str_extract(tolower(umn2$employment), suffixes)), 
           str_to_title(umn2$employment)), 
    which="both")
}
length(umn2$employment[which(!is.na(umn2$employment))])
#I am not sure if this is comprehensive, but should catch a fair amount of employment locations. You can take a similar approach with the abbreviations of states and countries
states<-read.csv("states.csv")
countries<-read.csv("countries.csv")
countries$loc<-trimws(str_replace_all(countries$loc, "^(\\s)", ""), which="both")
location<-rbind(countries, states)
location<-as.vector(location$loc)
umn2$loc<-NA
for(j in 1:170){
  umn2$loc<-ifelse(is.na(umn2$loc),str_extract(umn2$details, paste("(\\w+)(\\s+)(\\w+),(\\s+)(\\w+)(\\s+)", as.character(location[j]), sep="")), umn2$loc)
  umn2$loc<-ifelse(is.na(umn2$loc), str_extract(umn2$details, paste("(\\w+),(\\s+)(\\w+)(\\s+)", as.character(location[j]), sep="")), umn2$loc)
  umn2$loc<-ifelse(is.na(umn2$loc), str_extract(umn2$details, paste("(\\w+)(\\s+)(\\w+),(\\s+)", as.character(location[j]), sep="")), umn2$loc)
  umn2$loc<-ifelse(is.na(umn2$loc), str_extract(umn2$details, paste("(\\w+),(\\s+)", as.character(location[j]), sep="")), umn2$loc)
}
umn2<-buscar2(umn2)
companies<-as.vector(unique(umn2$employment[which(!is.na(umn2$employment))]))
umn2$role<-NA
for(i in 1:503){
  for(j in 1:nrow(umn2))
  if(is.na(umn2$role[j])){
    umn2$role[j]<-str_extract(umn2$details[j], paste("(.*?)(?=[[:punct:]](\\s)",companies[i], ")", sep=""))
  }else{
    umn2$role[j]<-umn2$role[j]
  }
}
umn2$role<-str_replace_all(umn2$role, "[.](?=[.])|[.]$", "")
prof<-as.vector(read.csv("prof2021.csv", header=F))
prof<-as.vector(prof$V1)
umn2$prof<-NA
for(i in 1:136){
  umn2$prof<-ifelse(is.na(umn2$prof), str_extract(umn2$role, prof[i]), umn2$prof)
}
umn2$prof<-trimws(ifelse(!is.na(str_extract(umn2$role2, "(\\w+)(?=(\\s)department)"))&is.na(umn2$prof), 
                         str_extract(umn2$role2, "(\\w+)(?=(\\s)department)"), 
                         umn2$prof), which="both")
length(umn2$role2[which(!is.na(umn2$role)&is.na(umn2$prof))])
umn2$country<-coords2country(umn2)
umn2$lastname<-umn2$lastname%>%str_replace_all("[^[:alpha:]]", "")%>%str_to_lower()%>%trimws()
umn2$firstname<-umn2$firstname%>%str_replace_all("[^[:alpha:]]", "")%>%str_to_lower()%>%trimws()
umn2$middlename<-umn2$middlename%>%str_replace_all("[^[:alpha:]]", "")%>%str_to_lower()%>%trimws()
umn2$simpname<-sname(umn2)
umn2$nationality<-NA
umn2$source<-"umn"
umn2$edu<-ifelse(is.na(umn2$degree), str_extract(umn2$text2, "(?<=\\().*?(?=\\))"), umn2$degree)
umn2$text<-umn2$details
umn2$id<-rownames(umn2)
umn2$org<-umn2$employment
umn2$type<-"alumni"
colnames(umn2)
umn2<-subset(umn2, select=c("id","source", "year", "simpname", "nationality", "edu", "prof", "org", "loc", "lon", "lat", "lastname", "firstname","middlename", "text", "country", "type"))
write.csv(umn2, "umnfin.csv")

####XV. Worcester####
worcester<-read.csv("alumniworcester.csv", na.strings = c("NA", "", " ", "0"))
worcester$class<-NULL
umn<-read.csv("umnfin.csv")
umn$X<-NULL
umn$id<-paste("umn", umn$id, sep="")
key<-data.frame(key=c("E.M", "Met.", "Geol", "M.S"), edu=c("mining eng", "metallurgical eng", "geology", "master science"))
key2<-paste(c("E[.]M", "Met[.]", "Geol", "M[.]S"), collapse="|")
umn$key<-str_extract(umn$edu, key2)
umn$edu<-NULL
umn<-left_join(umn, key)
worcester$source<-"worcester"
worcester$simpname<-sname(worcester)
worcester$id<-paste("wo", worcester$id, sep="")
worcester$loc<-worcester$location
worcester$type<-"alumni"
worcester$year<-ifelse()
worcester<-buscar2(worcester)
worcester$location<-NULL
worcester$country<-coords2country(worcester)
umn$year<-ifelse(is.na(umn$year), 1924, umn$year)
correction<-rbind.fill(umn, worcester)
correction$year<-str_replace_all(correction$year, "[^[0-9]]", "")
correction$year<-str_replace(correction$year, "^(\\d){1,2}$", "")
correction$year[which(correction$year=="")]<-NA
correction$year<-ifelse(is.na(correction$year), 1924, correction$year)
correction$year<-ifelse(correction$year>1950, 
                        as.numeric(as.character(str_replace(correction$year, "(?<=^(\\d))(\\d)", "8"))), 
                        ifelse(correction$year>1924, 
                        as.numeric(as.character(str_replace(correction$year, "(\\d)(?=(\\d)$)", "1"))), 
                        correction$year))
write.csv(correction, "correcion.csv", row.names=F)

####XV. New Mexico####
nmex<-read.csv("newmexico.csv")
nmex$text<-ifelse(is.na(str_extract(nmex$text, "MEXICO(\\s)STATE(\\s)SCHOOL")),nmex$text, NA)
nmex<-subset(nmex, !is.na(nmex$text))
for(i in 1:10){
nmex$text<-str_replace_all(nmex$text, "[.](\\s)[.](\\s)[.]", ". .")
}
nmex<-separate_rows(nmex, text, sep="[.](\\s)[.]|(?<=[A-Z]{3})(\\s+)(?=[A-Z][a-z])|(?<=[A-Z]{3}(\\s))(\\d+)(\\s+)(?=[A-Z][a-z])|(?<=[A-Z]{3}(\\s))[*|-](\\s+)(?=[A-Z][a-z])|(?<=JR[.])(\\s)|(?<=J[.]R[.])(\\s)")
nmex$id<-rownames(nmex)
nmex$name<-ifelse(!is.na(str_extract(nmex$text, "^[A-Z]{3}|^[A-Z][.](\\s)[A-Z]{3}|^[A-Z][.](\\s)[A-Z][.](\\s)[A-Z]{3}")), T, NA)
nmex$loc<-ifelse(is.na(nmex$name)&!is.na(lag(nmex$name)), nmex$text, NA)
nmex$details<-ifelse(!is.na(nmex$name)|!is.na(nmex$loc), NA, nmex$text)
for(i in nrow(nmex):1){
  if(is.na(nmex$name[i+1])&is.na(nmex$loc[i+1])){
    nmex$details[i]<-paste(nmex$details[i], nmex$details[i+1], sep=" ")}
  else{nmex$details[i]}
}
nmex$details<-str_replace_all(nmex$details, "^NA(\\s)", "")
nmex$details=trimws(nmex$details,  which="both")
nmex$name<-ifelse(!is.na(nmex$name), nmex$text, NA)
nmex<-fill(nmex, name, .direction="down")
nmex<-subset(nmex, !is.na(nmex$loc))
nmex<-separate_rows(nmex, details, sep="(?<=\\))(\\s+)")
nmex$edu<-str_extract(nmex$details, "(?<=\\().*?(?=\\))")
nmex<-separate_rows(nmex, edu, sep="[;](\\s)")
nmex$details<-ifelse(!is.na(nmex$edu), NA, nmex$details)
nmex<-separate_rows(nmex, details, sep="[;](\\s)|(\\s)(?=From)")
nmex<-distinct(nmex)
nmex$edu2<-str_extract(nmex$edu, "(?<=in(\\s))(.*?)(?=[[:punct:]])|E[.](\\s)M[.]")
nmex<-separate_rows(nmex, edu2, sep="(\\s)and(\\s)")
nmex$year<-str_extract(nmex$details, "18(\\d){2}|19(\\d){2}")
nmex$origin<-str_extract(nmex$details, "From(.*?)[.]")
nmex$details<-ifelse(!is.na(nmex$origin), str_replace(nmex$details, "[.]", "#"), nmex$details)
nmex<-separate_rows(nmex, details, sep="#(\\s)")
nmex$origin<-str_extract(nmex$details, "From(.*?)$")
nmex$role<-ifelse(is.na(nmex$edu)&is.na(nmex$origin), 
                  str_extract(nmex$details, "(.*?)(?=[,])"), NA)
nmex$details<-str_replace_all(nmex$details, "[&]", "and")
cis<-c("(?<=at(\\s))(.*?)(?=[:punct:])", "(?<=with(\\s))(.*?)(?=[:punct:])", 
        "(?<=)Cia(\\s)(.*?)(?=[:punct:])", "same(\\s)company", 
       "(?<=[:punct:])(.*?)[C|c]ompany", "(?<=[:punct:])(.*?)[C|c]o[.]", 
       "(\\w+)(\\s+)(\\w+)(\\s+)(\\w+)(\\s+)(\\w+)(\\s+)[C|c]ompany",
       "(\\w+)(\\s+)(\\w+)(\\s+)(\\w+)(\\s+)[C|c]ompany",
       "(\\w+)(\\s+)(\\w+)(\\s+)[C|c]ompany",
       "(\\w+)(\\s+)(\\w+)(\\s+)(\\w+)(\\s+)(\\w+)(\\s+)[C|c]o[.]",
       "(\\w+)(\\s+)(\\w+)(\\s+)(\\w+)(\\s+)[C|c]o[.]",
       "(\\w+)(\\s+)(\\w+)(\\s+)[C|c]o[.]")
cis<-paste(cis, collapse="|")
nmex$org<-NA
for(i in 1:length(cis)){
nmex$org<-ifelse(is.na(nmex$org), str_extract(nmex$details, cis[i]), nmex$org)
}
prof<-as.vector(read.csv("prof2021.csv", header=F))
prof<-as.vector(prof$V1)
nmex$prof<-NA
for(i in 1:136){
  nmex$prof<-ifelse(is.na(nmex$prof), str_extract(nmex$details, prof[i]), nmex$prof)
}
nmex$origin<-str_replace_all(nmex$origin, "From(\\s)", "")
origin<-subset(nmex, !is.na(nmex$origin), select=c("origin"))
origin$loc<-origin$origin
origin<-buscar2(origin)
origin$nationality<-coords2country(origin)
origin$loc<-NULL
nmex<-left_join(nmex, origin, by="origin")
states<-read.csv("states.csv")
countries<-read.csv("countries.csv")
countries$loc<-trimws(str_replace_all(countries$loc, "^(\\s)", ""), which="both")
location<-rbind(countries, states)
location<-as.vector(location$loc)
nmex$loc<-NA
for(j in 1:170){
  nmex$loc<-ifelse(is.na(nmex$loc),str_extract(nmex$details, paste("(\\w+)(\\s+)(\\w+),(\\s+)(\\w+)(\\s+)", as.character(location[j]), sep="")), nmex$loc)
  nmex$loc<-ifelse(is.na(nmex$loc), str_extract(nmex$details, paste("(\\w+),(\\s+)(\\w+)(\\s+)", as.character(location[j]), sep="")), nmex$loc)
  nmex$loc<-ifelse(is.na(nmex$loc), str_extract(nmex$details, paste("(\\w+)(\\s+)(\\w+),(\\s+)", as.character(location[j]), sep="")), nmex$loc)
  nmex$loc<-ifelse(is.na(nmex$loc), str_extract(nmex$details, paste("(\\w+),(\\s+)", as.character(location[j]), sep="")), nmex$loc)
}
nmex$loc<-ifelse(is.na(nmex$loc), nmex$org, nmex$loc)
nmex$lon<-NULL
nmex$lat<-NULL
nmex<-buscar2(nmex)
for(i in 1:136){
  nmex$org<-str_replace_all(nmex$org, prof[i], "")
  nmex$org<-str_replace_all(nmex$org, "(\\s+)", " ")
  nmex$org<-str_replace_all(nmex$org, "^(\\s+)", "")
  nmex$org<-ifelse(!is.na(str_extract(nmex$org, "(?<=[[:punct:]]).*")),
                   str_extract(nmex$org, "(?<=[[:punct:]]).*"),  nmex$org)
  nmex$org<-ifelse(nmex$org=="", NA, nmex$org)
}
nmex$name<-str_replace(nmex$name, "[,].*", "")
nmex$name<-trimws(str_replace(nmex$name, "[']|(\\s+)$|[.]", ""), which = "both")
nmex$name<-str_to_title(nmex$name)
nmex$lastname<-str_extract(nmex$name, "(\\w+)$")
nmex$middlename<-ifelse(!is.na(str_extract(nmex$name, "(\\w+)(\\s+)(\\w+)(\\s+)(\\w+)")), 
                        str_extract(nmex$name, "(\\w+)(\\s+)(\\w+)$"),NA)
nmex$middlename<-str_extract(nmex$middlename, "(\\w+)")
nmex$firstname<-str_extract(nmex$name, "(\\w+)")
nmex$simpname<-sname(nmex)
nmex$type<-"alumni"
nmex$source<-"nmex"
nmex$country<-coords2country(nmex)
colnames(nmex)
nmex<-subset(nmex, select=c("id","source", "year", "simpname", "nationality", "edu", "prof", "org", "loc", "lon", "lat", "lastname", "firstname","middlename", "text", "country", "type"))
write.csv(nmex, "nmexf.csv")

####XIV. Colorado School of Mines####
####XIV. 1. 1917####
states<-read.csv("states.csv")
countries<-read.csv("countries.csv")
countries$country<-trimws(str_replace_all(countries$country, "^(\\s)", ""), which="both")
state<-paste(states$State, collapse = "|")
countrie<-paste(countries$country, collapse = "|")
location<-paste(countrie, state, sep="|")
colorado<-read.csv("colorado1917.csv")
colorado$id<-rownames(colorado)
colorado<-separate_rows(colorado, text, sep = "(?<=\\)[.])")
colorado$text<-ifelse(!is.na(str_extract(colorado$text, "^$")), NA, colorado$text)
colorado<-colorado[which(!is.na(colorado$text)),]
colorado$name<-ifelse(!is.na(str_extract(colorado$text, "\\)|Deceased")), colorado$text, NA)
colorado$name<-str_extract(colorado$name, "(\\w+)[,](.*?)(?=[[:punct:]])")
colorado$text<-trimws(str_replace_all(colorado$text, "^(\\s)", ""), which="both")
colorado$text2<-pegar(colorado$text, colorado$name)
colorado$text2<-ifelse(!is.na(str_extract(colorado$text2, "Where")), NA, colorado$text2)
colorado<-colorado[which(!is.na(colorado$text2)),]
colorado$year<-str_extract(colorado$text, "(?<=['])(\\d+)|(\\b)(\\d){2}(\\b)")
colorado$text2<-str_replace_all(colorado$text2, "(?<=[.|,])(?=[[:alpha:]])", " ")
colorado$text2<-str_replace_all(colorado$text2, "(?<=[a-z])(?=[A-Z])|(?<=[a-z])(?=(\\d))", " ")
colorado$text2<-str_replace_all(colorado$text2, "(?<=\\))(\\d){1}(?=[.])", "")
colorado<-separate_rows(colorado, text2, sep = "(?<=\\)[.])")
colorado$text2<-trimws(str_replace(colorado$text2,"^(\\s)", ""), which="both")
colorado$name<-NULL
colorado$name<-ifelse(!is.na(str_extract(colorado$text2, "\\)|Deceased")), colorado$text2, NA)
colorado$name<-str_extract(colorado$name, "(\\w+)[,](.*?)(?=[[:punct:]])")
colorado$text2<-str_replace_all(colorado$text2, "(?<=St)[.]", "")
colorado$text2<-ifelse(!is.na(colorado$name), NA, colorado$text2)
colorado$text<-ifelse(!is.na(colorado$text2), colorado$text2, colorado$text)
colorado$text2<-str_replace_all(colorado$text, "[[:punct:]]$", "")
colorado$text2<-trimws(colorado$text2, which="both")
colorado$location<-str_extract(colorado$text2, "(\\w+)[,](\\s+)(\\w+)$|(\\w+)[,](\\s+)(\\w+)(\\s+)(\\w+)$|(\\w+)[.](\\s+)(\\w+)[.][,](\\s+)(\\w+)$")
colorado$location<-ifelse(!is.na(colorado$location), colorado$location, str_extract(colorado$text2, location))
colorado<-fill(colorado, name, .direction="down")
colorado$org<-str_extract(colorado$text2, "Care(.*?)(?=[,])|(?<=[,]).*?Co[.]|(?<=[,]).*?Corp[.]")
for(i in 1:15){
  colorado$org<-ifelse(!is.na(str_extract(colorado$org, "(?<=[,])(.*?)Co[.]|(?<=[,])(.*?)Corp[.]")), 
                       str_extract(colorado$org, "(?<=[,])(.*?)Co[.]|(?<=[,])(.*?)Corp[.]"), 
                       colorado$org) 
  colorado$org<-ifelse(!is.na(colorado$org), colorado$org, 
                       str_extract(colorado$text2, "(?<=^).*?Co[.]|(?<=^)(.*?)Corp[.]"))
}
profesiones<-c("Engr.", "Engineer", "Supt.", "Superintendent", "Mgr.", "Manager", "Surveyor", "Geologist", "Agent",
               "Boss", "Chemist", "Assayer", "Foreman", "Metallurgist", "Dept.", "Technic", "Experimental Work")
profs1<-paste("(\\w+)(\\s+)", profesiones, sep="")
profs2<-rbind(profesiones, profs1)
profs2<-paste(profs2, collapse ="|")
colorado$prof<-str_extract(colorado$text2, profs2)
colorado$org<-str_replace_all(colorado$org, profs2, "")
colorado$org<-trimws(str_replace(colorado$org, "^(\\s+)|(\\s+)$", ""), which="both")


colorado<-separate_rows(colorado, text2, sep="(?<=[.])[,]")
colorado<-separate_rows(colorado, text2, sep="(?<=[.])")
colorado$text2<-trimws(str_replace(colorado$text2,"^(\\s)", ""), which="both")
colorado$text2<-ifelse(!is.na(str_extract(colorado$text2, "^$")), NA, colorado$text2)
colorado<-colorado[which(!is.na(colorado$text2)|!is.na(colorado$name)),]
colorado$location<-ifelse(!is.na(lead(colorado$name)), colorado$text2, NA)
colorado$edu<-NA
colorado$nationality<-NA
colorado$loc<-colorado$location
colorado<-buscar2(colorado)
colorado$country<-coords2country(colorado)
colorado$source<-"colorado"
colorado$names<-str_extract(colorado$name, "(?<=[,])(.*?)")
colorado$lastname<-str_extract(colorado$name, "(.*?)(?=[,])")
colorado$middlename<-mname(colorado$names)
colorado$firstname<-str_extract(colorado$names, "^(\\w+)")
colorado$type<-"alumni"
colorado$simpname<-sname(colorado)
colorado2<-subset(colorado, select=c("id","source", "year", "simpname", "nationality", "edu", "prof", "org", "loc", "lon", "lat", "lastname", "firstname","middlename", "text", "country", "type"))
write.csv(colorado2, "colfinal.csv", row.names = F)

#corregir
colorado2<-read.csv("colfinal.csv")
colorado2$name<-ifelse(!is.na(str_extract(colorado2$text, "\\)|Deceased")), colorado2$text, NA)
colorado2$name<-str_extract(colorado2$name, "(\\w+)[,](.*?)(?=[[:punct:]])")
colorado2<-fill(colorado2, name, .direction = "down")
colorado2$names<-str_extract(colorado2$name, "(?<=[,](\\s)).*|(?<=[,]).*")
colorado2$names<-str_replace(colorado2$names, "^(\\s)", "")
colorado2$firstname<-str_extract(colorado2$names, "^(\\w+)")
colorado2$middlename<-mname(colorado2$names)
colorado2$simpname<-sname(colorado2)
colorado2<-subset(colorado2, select=c("id","source", "year", "simpname", "nationality", "edu", "prof", "org", "loc", "lon", "lat", "lastname", "firstname","middlename", "text", "country", "type"))
write.csv(colorado2, "col1917.csv", row.names = F)
####XIV. 2. las demas####
colmines<-read.csv("colminest.csv", na.strings = c(NA, "", " "))
colnames(colmines)<-c("name", "status", "catalog", "origin")
colmines<-separate_rows(colmines, status, sep="[;](\\s)")
colmines$year<-ifelse(!is.na(str_extract(colmines$status, "(\\d+){4}")),str_extract(colmines$status, "(\\d+){4}"), 
                      str_extract(colmines$catalog, "(\\d+){4}"))
colmines$degree<-str_extract(colmines$status, "(?<=dent[,](\\s)).*?(?=(\\s+))")
colmines$loc<-colmines$origin
colmines<-buscar2(colmines)
colmines$nationality<-coords2country(colmines)
colmines$source<-"colmines"
colmines<-separate(colmines, name, into=c("lastname", "names"), sep="[,](\\s)")
colmines<-separate(colmines, names, into=c("firstname", "middlename"), sep="(\\s)")
colmines$simpname<-sname(colmines)
colmines<-subset(colmines, select=-c(loc, lon, lat, catalog, origin))
write.csv(colmines, "colminest2.csv", row.names=F)

####XIV. 3. juntar####
col17<-read.csv("col1917.csv")
colmines<-read.csv("colminest2.csv")
colo2<-read.csv("alumniviejo.csv")
colo2<-subset(colo2, colo2$source=="colmines")
col17$year<-ifelse(col17$year<18, col17$year+1900, col17$year+1800)
colmines$year<-str_replace(colmines$year, "^12", "18")
colmines$year<-str_replace(colmines$year, "^13", "19")
colmines$year<-str_replace(colmines$year, "^7", "1")
colmines$year<-str_extract(colmines$year, "^(\\d){4}")
colorado<-rbind.fill(col17, colmines, colo2)
colorado$id<-rownames(colorado)
colorado$source<-"colorado"
colo<-subset(colorado, select=c("id","source", "year", "simpname", "nationality", "edu", "prof", "org", "loc", "lon", "lat", "lastname", "firstname","middlename", "text", "country", "type"))
colo<-colo[order(colo$simpname, colo$source),]
colo<-colo[which(is.na(str_extract(colo$simpname, "^(\\d)"))&!is.na(colo$firstname)),] 
colo$simpname<-ifelse((lead(colo$simpname)==colo$simpname)&(colo$source=="colmines"), NA, colo$simpname )
colo<-colo[which(!is.na(colo$simpname)),] 
colo<-distinct(colo)
colo$year<-as.numeric(colo$year)
colo$year<-ifelse(colo$year<1874, 
                  as.numeric(str_replace(as.character(colo$year), "^18", "19")), 
                  colo$year)
write.csv(colo, "colfinal.csv", row.names=F)

####XIV. Combine####
####XV. extras####
harv<-subset(read.csv("harvengi.csv"), select=c("id","source", "year", "simpname", "nationality", "edu", "prof", "org", "loc", "lon", "lat", "lastname", "firstname","middlename", "text", "country", "type"))
lehigh<-read.csv("lehigh2.csv")
lehigh$edu<-str_extract(lehigh$text, "E[.]M[.]|Met[.]|Metallurgy")
lehigh$year<-str_extract(lehigh$text, "(?<=['])(\\d){2}|(?<=['])[l|I][l|I]|(?<=['])(\\d){2}|(?<=['])[l|I][l|I]|(?<=['])(\\d)[l|I]{1}|(?<=['])[l|I](\\d){1}|(?<=['])(\\d)[l|I]{1}|(?<=['])[l|I](\\d){1}|(?<=['|'])(\\d)[O|B]{1}|(?<=['|'])[O|B](\\d){1}")
lehigh$year<-as.character(str_replace(lehigh$year, "O", "0"))
lehigh$year<-as.character(str_replace(lehigh$year, "B", "8"))
lehigh$year<-as.character(str_replace_all(lehigh$year, "[I|l]", "1"))
lehigh$year<-ifelse(as.numeric(lehigh$year)>17, paste("18", lehigh$year, sep=""), paste("19", lehigh$year, sep=""))
lehigh$simpname<-sname(lehigh)
for(i in 1:15){
  lehigh$edu<-ifelse(!is.na(lag(lehigh$edu))&(lag(lehigh$simpname)==lehigh$simpname)&is.na(lehigh$edu), 
                     lag(lehigh$edu), lehigh$edu)
  lehigh$loc<-ifelse(!is.na(lag(lehigh$loc))&(lag(lehigh$simpname)==lehigh$simpname)&is.na(lehigh$loc), 
                     lag(lehigh$loc), lehigh$loc)
  lehigh$lon<-ifelse(!is.na(lag(lehigh$lon))&(lag(lehigh$simpname)==lehigh$simpname)&is.na(lehigh$lon), 
                     lag(lehigh$lon), lehigh$lon)
  lehigh$lat<-ifelse(!is.na(lag(lehigh$lat))&(lag(lehigh$simpname)==lehigh$simpname)&is.na(lehigh$lat), 
                     lag(lehigh$lat), lehigh$lat)
  lehigh$prof<-ifelse(!is.na(lag(lehigh$prof))&(lag(lehigh$simpname)==lehigh$simpname)&is.na(lehigh$prof), 
                      lag(lehigh$prof), lehigh$prof)
  lehigh$org<-ifelse(!is.na(lag(lehigh$org))&(lag(lehigh$simpname)==lehigh$simpname)&is.na(lehigh$org), 
                     lag(lehigh$org), lehigh$org)
}
lehigh<-lehigh[which(!is.na(lehigh$edu)),]
lehigh<-subset(lehigh, select=c("id","source", "year", "simpname", "nationality", "edu", "prof", "org", "loc", "lon", "lat", "lastname", "firstname","middlename", "text", "country", "type"))
sdakota<-subset(read.csv("sdakotafinal.csv"), select=c("id","source", "year", "simpname", "nationality", "edu", "prof", "org", "loc", "lon", "lat", "lastname", "firstname","middlename", "text", "country", "type"))
colo<-subset(read.csv("colfinal.csv"), select=c("id","source", "year", "simpname", "nationality", "edu", "prof", "org", "loc", "lon", "lat", "lastname", "firstname","middlename", "text", "country", "type"))
miss<-subset(read.csv("missoufinal.csv"), select=c("id","source", "year", "simpname", "nationality", "edu", "prof", "org", "loc", "lon", "lat", "lastname", "firstname","middlename", "text", "country", "type"))
nevada<-subset(read.csv("nevadafinal.csv"), select=c("id","source", "year", "simpname", "nationality", "edu", "prof", "org", "loc", "lon", "lat", "lastname", "firstname","middlename", "text", "country", "type"))
minn<-read.csv("umnfin.csv")
nmex<-read.csv("nmexf.csv")
csm<-read.csv("csmtotal.csv")
minn$X<-NULL
nmex$X<-NULL
ecole<-read.csv("Annales.csv")
ecole$names<-str_extract(ecole$name, "(?<=\\()(.*)(?=\\))")
ecole$firstname<-str_extract(ecole$names, "^(\\w+)")
ecole$middlename<-mname(ecole$firstname)
ecole$lastname<-ifelse(!is.na(str_extract(ecole$name, "(.*?)(?=\\()")), str_extract(ecole$name, "(.*?)(?=\\()"), ecole$name)
ecole$simpname<-sname(ecole)
ecole$org<-ecole$school
ecole$source<-"enm"
ecole$prof<-NA
ecole$nationality<-NA
ecole$prof<-NA
ecole$loc<-ecole$school
ecole<-buscar2(ecole)
ecole$text<-NA
ecole$country<-coords2country(ecole)
ecole$type<-"alumni"
ecole$edu<-"mining eng"
ecole<-subset(ecole, select=c("id","source", "year", "simpname", "nationality", "edu", "prof", "org", "loc", "lon", "lat", "lastname", "firstname","middlename", "text", "country", "type"))
harv<-distinct(harv, simpname, lon, edu, prof, org,.keep_all = T)
lehigh<-distinct(lehigh, simpname, lon, edu, prof, org,.keep_all = T)
sdakota<-distinct(sdakota, simpname, lon, edu, prof, org,.keep_all = T)
colo<-distinct(colo, simpname, lon, edu, prof, org,.keep_all = T)
miss<-distinct(miss, simpname, lon, edu, prof, org,.keep_all = T)
nevada<-distinct(nevada, simpname, lon, edu, prof, org,.keep_all = T)
csm$prof<-NA
csm$nationality<-NA
csm<-distinct(csm, simpname, lon, edu, prof, org,.keep_all = T)
harv$id<-paste("hvd",harv$id, sep="")
lehigh$id<-paste("lhg",lehigh$id, sep="")
sdakota$id<-paste("sdk",sdakota$id, sep="")
colo$id<-paste("colo",colo$id, sep="")
miss$id<-paste("miss",miss$id, sep="")
nevada$id<-paste("nvd",nevada$id, sep="")
ecole$id<-paste("enm",ecole$id, sep="")
nmex$id<-paste("nmx",nmex$id, sep="")
minn$id<-paste("minn",minn$id, sep="")
csm$id<-paste("csm",csm$id, sep="")
csm$country<-coords2country(csm)
extra<-rbind(harv, lehigh, sdakota, colo, miss, nevada, ecole, minn, nmex, csm)
extra$edu<-ifelse((extra$source!="harvard")&(is.na(extra$edu)), "Mining Engineer", extra$edu)
for(i in 1:15){
  extra$edu<-ifelse(!is.na(lag(extra$edu))&(lag(extra$simpname)==extra$simpname)&is.na(extra$edu), 
                    lag(extra$edu), extra$edu)
  extra$loc<-ifelse(!is.na(lag(extra$loc))&(lag(extra$simpname)==extra$simpname)&is.na(extra$loc), 
                    lag(extra$loc), extra$loc)
  extra$lon<-ifelse(!is.na(lag(extra$lon))&(lag(extra$simpname)==extra$simpname)&is.na(extra$lon), 
                    lag(extra$lon), extra$lon)
  extra$lat<-ifelse(!is.na(lag(extra$lat))&(lag(extra$simpname)==extra$simpname)&is.na(extra$lat), 
                    lag(extra$lat), extra$lat)
  extra$prof<-ifelse(!is.na(lag(extra$prof))&(lag(extra$simpname)==extra$simpname)&is.na(extra$prof), 
                     lag(extra$prof), extra$prof)
  extra$org<-ifelse(!is.na(lag(extra$org))&(lag(extra$simpname)==extra$simpname)&is.na(extra$org), 
                    lag(extra$org), extra$org)
  extra$edu<-ifelse(!is.na(lead(extra$edu))&(lead(extra$simpname)==extra$simpname)&is.na(extra$edu), 
                    lead(extra$edu), extra$edu)
  extra$loc<-ifelse(!is.na(lead(extra$loc))&(lead(extra$simpname)==extra$simpname)&is.na(extra$loc), 
                    lead(extra$loc), extra$loc)
  extra$lon<-ifelse(!is.na(lead(extra$lon))&(lead(extra$simpname)==extra$simpname)&is.na(extra$lon), 
                    lead(extra$lon), extra$lon)
  extra$lat<-ifelse(!is.na(lead(extra$lat))&(lead(extra$simpname)==extra$simpname)&is.na(extra$lat), 
                    lead(extra$lat), extra$lat)
  extra$prof<-ifelse(!is.na(lead(extra$prof))&(lead(extra$simpname)==extra$simpname)&is.na(extra$prof), 
                     lead(extra$prof), extra$prof)
  extra$org<-ifelse(!is.na(lead(extra$org))&(lead(extra$simpname)==extra$simpname)&is.na(extra$org), 
                    lead(extra$org), extra$org)
}

write.csv(extra, "extralumni.csv", row.names = F)

####XIV. 1. combine ####
alumnirest<-read.csv("alumnirest2.csv", na.strings = c("NA", "", " "))
alumnirest<-alumnirest[which(alumnirest$source!="colmines"),]
mit<-read.csv("mitjunto.csv", na.strings = c("NA", "", " "))
mit$X<-NULL
rsmf<-read.csv("rsmf3.csv", na.strings = c("NA", "", " "))
fre<-read.csv("freifinal.csv", na.strings = c("NA", "", " "))
extra<-read.csv("extralumni.csv", na.strings = c("NA", "", " "))
alumni<-rbind(alumnirest, rsmf)
colnames(alumni)
alumni$text<-NA
alumni<-rbind(alumni, fre)
alumni$X<-NULL
alumnill<-as.data.frame(alumni$lon)
colnames(alumnill)[1]<-"lon"
alumnill$lat<-alumni$lat
alumnill[is.na(alumnill)]<-FALSE
alumni$country<-coords2country(alumnill)
alumni<-distinct(alumni)
alumni<-rbind(alumni, extra, mit)
alumni<-alumni[order(alumni$simpname, alumni$year), ]
alumni$simpname<-repetir(alumni$simpname, rfin, 10)
alumni$firstname<-ifelse(
  (nchar(alumni$firstname))>1, 
  alumni$firstname, 
  ifelse((nchar(lead(alumni$firstname, 1))>1)&(alumni$lastname==lead(alumni$lastname, 1))&(substr(alumni$firstname, 1, 1)==lead(substr(alumni$firstname, 1, 1), 1))&(nchar(lead(alumni$firstname, 1))>1), 
         lead(alumni$firstname, 1), alumni$firstname))
alumni$firstname<-ifelse(
  (nchar(alumni$firstname))>1, 
  alumni$firstname, 
  ifelse((!is.na(lag(alumni$firstname, 1)))&(alumni$lastname==lag(alumni$lastname, 1))&(substr(alumni$firstname, 1, 1)==lag(substr(alumni$firstname, 1, 1), 1)), 
         lag(alumni$firstname, 1), alumni$firstname))
alumni$middlename<-ifelse(
  !is.na(alumni$middlename), alumni$middlename, 
  ifelse((!is.na(lag(alumni$middlename, 1)))&(alumni$lastname==lag(alumni$lastname, 1))&(alumni$firstname==lag(alumni$firstname, 1)), 
         lag(alumni$middlename, 1),  NA))
alumni$middlename<-ifelse(
  !is.na(alumni$middlename), alumni$middlename, 
  ifelse((!is.na(lead(alumni$middlename, 1)))&(alumni$lastname==lead(alumni$lastname, 1))&(alumni$firstname==lead(alumni$firstname, 1)), 
         lead(alumni$middlename, 1),  NA))
alumni$simpname<-tolower(trimws(paste(alumni$lastname, ", ", substr(alumni$firstname, 1, 1), " " ,substr(alumni$middlename, 1, 1), sep="")))
alumni<-alumni[order(alumni$simpname, alumni$year), ]
alumni$edu<-llenar2(alumni$edu, alumni$simpname)
alumni$edu<-llenar2(alumni$edu, alumni$simpname)
alumni$edu<-llenar2(alumni$edu, alumni$simpname)
alumni$edu<-llenar2(alumni$edu, alumni$simpname)
alumni$edu<-llenar2(alumni$edu, alumni$simpname)
alumni$edu<-trimws(str_replace_all(alumni$edu, "engineering", ""), which=c("both"))
alumni$edu<-trimws(str_replace_all(alumni$edu, "engineer", ""), which=c("both"))
alumni$type<-"alumni"
alumni<-distinct(alumni)
alumni$simpname<-str_replace(alumni$simpname, "(\\s)na$", "")
alumni$lastname<-tolower(alumni$lastname)
alumni$firstname<-tolower(alumni$firstname)
alumni$middlename<-tolower(alumni$middlename)
alumni<-alumni[which(!is.na(alumni$firstname)),]
alumnifin<-distinct(alumni)
write.csv(alumnifin, "alumnit.csv", row.names = F)
#aqui me quede
####XV. cleaning again ####
alumni<-read.csv("alumnit.csv")
alumni$X<-NULL
alumni$edu<-tolower(str_replace_all(alumni$edu, "(\\s+)", " "))
alumni$edu<-ifelse(!is.na(str_extract(alumni$edu, "minas|mine|mining|e[.]m[.]|e[.](\\s)m[.]")), "mining eng", alumni$edu)
alumni$edu<-ifelse(!is.na(str_extract(alumni$edu, "met")), "metallurgical eng", alumni$edu)
alumni$edu<-ifelse(!is.na(str_extract(alumni$edu, "civil|c[.]e|c[.](\\s)e")), "civil eng", alumni$edu)
alumni$edu<-ifelse(!is.na(str_extract(alumni$edu, "electric")), "electrical eng", alumni$edu)
alumni$edu<-ifelse(!is.na(str_extract(alumni$edu, "industrial")), "industrial eng", alumni$edu)
alumni$edu<-ifelse(!is.na(str_extract(alumni$edu, "geol")), "geology", alumni$edu)
alumni$edu<-ifelse(!is.na(str_extract(alumni$edu, "naval")), "naval eng", alumni$edu)
alumni$edu<-ifelse(!is.na(str_extract(alumni$edu, "mech|m[.]e|m[.](\\s)e")), "mechanical eng", alumni$edu)
alumni$edu<-ifelse(!is.na(str_extract(alumni$edu, "m[.]s|m[.](\\s)s|s[.]m|s[.](\\s)m")), "master science", alumni$edu)
alumni$edu<-ifelse(!is.na(str_extract(alumni$edu, "m[.]a|m[.](\\s)a|a[.]m|a[.](\\s)m")), "master arts", alumni$edu)
alumni$edu<-ifelse(!is.na(str_extract(alumni$edu, "b[.]s|b[.](\\s)s|s[.]b|s[.](\\s)b")), "bachelor science", alumni$edu)
alumni$edu<-ifelse(!is.na(str_extract(alumni$edu, "b[.]a|b[.](\\s)a|a[.]b|a[.](\\s)b")), "bachelor arts", alumni$edu)
alumni$edu<-ifelse(!is.na(str_extract(alumni$edu, "ll[.]d|ll[.](\\s)d")), "law", alumni$edu)
alumni$edu<-ifelse(!is.na(str_extract(alumni$edu, "^$")), NA, alumni$edu)
temp<-as.data.frame(unique(alumni$edu))
colnames(temp)<-"edu"
write.csv(temp, "temp.csv", row.names = F)
temp2<-subset(read.csv("edual.csv", na.strings = c("NA", "", " ")), select=c("edu", "edu2"))
temp2$edu2<-tolower(temp2$edu2)
alumni<-left_join(alumni, temp2, by="edu")
alumni$edu<-ifelse(!is.na(alumni$edu2), alumni$edu2, alumni$edu)
alumni$edu2<-NULL
alumni$edu<-ifelse(!is.na(str_extract(alumni$edu, "unknown")), NA, alumni$edu)
alumni$firstname<-str_extract(alumni$firstname, "^(\\w+)")
alumni<-alumni[order(alumni$simpname, alumni$lastname, alumni$firstname, alumni$middlename, alumni$year), ]
alumni$firstname<-llenar2(alumni$firstname, alumni$simpname)
alumni$edu<-llenar2(alumni$edu, alumni$simpname)
alumni$edu<-llenar2(alumni$edu, alumni$simpname)
alumni$edu<-llenar2(alumni$edu, alumni$simpname)
alumni$edu<-llenar2(alumni$edu, alumni$simpname)
alumni$edu<-ifelse(is.na(alumni$edu)&alumni$org=="college of mine", "mining eng", alumni$edu )
alumni$edu<-ifelse(is.na(alumni$edu)&alumni$org=="college of mechanics university", "mechanical eng", alumni$edu )
alumni$org<-ifelse(alumni$org=="college of mine", "MIT", alumni$org)
alumni$org<-ifelse(alumni$org=="college of mechanics university", "MIT", alumni$org )
alumni$org<-ifelse(alumni$org=="the royal school mine", "royal school of mines", alumni$org )
alumni$edu<-ifelse((alumni$org=="uariz")&(alumni$edu=="student"), "mining eng", alumni$edu )
alumni$year<-gsub("^0$", NA, alumni$year)
alumni$year<-ifelse((alumni$year>1940)&(alumni$source!="colorado"), str_replace_all(alumni$year, "^19", "18"), alumni$year)
alumni$year<-as.numeric(as.character(alumni$year))
alumni$year<-llenar2(alumni$year, alumni$simpname)
alumni$prof<-ifelse(is.na(alumni$prof)&alumni$org=="freiberg school", "student", alumni$prof )
alumni$org<-ifelse(alumni$org=="freiberg mining academy", "freiberg school", alumni$org)
alumni$edu<-ifelse(is.na(alumni$edu)&alumni$org=="montana school of mines", "mining eng", alumni$edu)
alumni$edu<-ifelse(is.na(alumni$edu)&alumni$org=="royal school of mines", "mining eng", alumni$edu)
alumni<-distinct(alumni)
alumni$X<-NULL
alumni<-alumni[which(is.na(str_extract(alumni$simpname, "^(\\d)"))),]
alumni<-distinct(alumni)
write.csv(alumni, "alumnix.csv", row.names = F)
####SVI. Correction####
alumni<-read.csv("alumnix.csv")
correction<-read.csv("correcion.csv")
correction$X<-NULL
correction$key<-NULL
alumni<-rbind( subset(alumni, (alumni$source!="worcester")&(alumni$source!="umn")), 
               correction)
write.csv(alumni, "alumniy.csv", row.names = F)

####XVI. Test ####
alumni<-read_csv("alumnix.csv")
combo<-read.csv("combo4.csv")
altest<-read_csv("altest.csv")
altest<-left_join(altest, alumni, multiple="first")
alumni<-subset(alumni, select=c("id", "source", "year",  "simpname","lastname", "firstname", "middlename", "prof", "org", "loc"))
#rearmar combo4#
#sample variable
alumnitest<-subset(altest, select=c("id", "source", "year",  "simpname","lastname", "firstname", "middlename", "prof", "org", "loc"))
write.csv(alumnitest, "alumnitest.csv", row.names = F)


#### Blueprint for modernity####
## Alumni lists##
#Author: Israel G Solares#
#contact:isgarcia@colmex.mx#
#Licence: CC by 4#
#language: English#

#### I. defining, extracting and cleaning####
####I.0.1 installing packages####
install.packages("countrycode")
install.packages("dplyr")
install.packages("gganimate")
install.packages("ggmap")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("grid")
install.packages("gridExtra")
install.packages("igraph")
install.packages("leaflet")
install.packages("leaflet.extras")
install.packages("magick")
install.packages("magrittr")
install.packages("maps")
install.packages("network")
install.packages("networkD3")
install.packages("OpenStreetMap")
install.packages("plyr")
install.packages("RColorBrewer")
install.packages("readr")
install.packages("refinr")
install.packages("remotes")
install.packages("rnaturalearth")
install.packages("rworldmap")
install.packages("sf")
install.packages("sp")
install.packages("stringi")
install.packages("stringr")
install.packages("tesseract")
install.packages("tibble")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("tmap")
install.packages("tmaptools")
install.packages("vctrs")
install.packages("viafr")
install.packages("viridis")
install.packages("listviewer")
install.packages("cartogram")
install.packages("albersusa")
install.packages("transformr")
devtools::install_github("ropensci/rnaturalearthhires") 
install.packages("GEOmap")
install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")

####I.0. 2 defining working directory####
#setwd("C:/Users/lito_/Dropbox/Blueprint for modernity/2021/blueprint")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#### I.0.3 loading packages####
library(countrycode)
library(dplyr)
library(ggmap)
library(gganimate)
library(ggplot2)
library(ggthemes)
library(grid)
library(gridExtra)
library(igraph)
library(leaflet)
library(leaflet.extras)
library(listviewer)
library(magick)
library(magrittr)
library(maps)
library(network)
library(networkD3)
library(OpenStreetMap)
library(plyr)
library(plotly)
library(RColorBrewer)
library(readr)
library(refinr)
library(remotes)
library(rworldmap)
library(rnaturalearth)
library(sf)
library(sp)
library(stringi)
library(stringr)
library(tesseract)
library(tibble)
library(tidyr)
library(tidyverse)
library(tmap)
library(tmaptools)
library(vctrs)
library(viafr)
library(viridis)
#install_github("dgrtwo/gganimate", ref = "26ec501")
library(gganimate)
library(cartogram)
library(albersusa)
library(transformr)
library(rnaturalearthhires)
library(GEOmap)
library(spDataLarge)
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
      panel.spacing = unit(0.5, "lines"),   
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
base<- getMap(resolution = "high")
base<- ggplot() + geom_polygon(data = base, aes(x=long, y = lat, group = group),  fill = "white", color="black")
base2<- getMap(resolution = "high")
base2<- ggplot(data = base2, aes(x=long, y = lat)) + geom_polygon(data = base2, aes(x=long, y = lat, group = group),  fill = "black", color="black")



####I.0.4 defining functions####
cargar<-function(x){
  y<-read.csv(x,na.strings=c(""," ","NA"))
  y$X<-NULL
  return(y)
}
buscar<-function(x){
  register_google(key = "")
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
cias<-function(x){
  y<-ifelse(str_extract(x, "^(\\w+)")==str_extract(lag(x,1), "^(\\w+)"), 
            lag(x, 1), x)
  y[1]<-x[1]
  return(y)
}
consulta<-function(df, columna, vector){
  y<-as.data.frame(df)
  x<-as.vector(vector)
  #x<-str_replace_all(x, "[.]", "[.]")
  x<-str_replace_all(x, "S", "[S|5]")
  x<-str_replace_all(x, "O", "[O|0]")
  x<-str_replace_all(x, "G", "[G|6]")
  x<-str_replace_all(x, "F", "[F|7]")
  x<-str_replace_all(x, "I", "[I|l|1]")
  x<-str_replace_all(x, "B", "[B|8|3]")
  x<-str_replace_all(x, "Z", "[Z|2]")
  x<-str_replace_all(x, "A", "[A|4]")
  z<-tolower(x)
  x<-c(x, z)
  x<-paste(x, collapse="|")
  y$srch<-str_extract(y$text, x)
  yb<-y[which(!is.na(y$srch)), ]
  yb<-subset(yb, select=c(columna, "srch"))
  y$srch<-NULL
  y<-left_join(y, yb, by=columna, match="all")
  yc<-distinct(y[which(!is.na(y$srch)), ])
  yc<-ordenar(yc)
  yc$srch<-str_extract(yc$text, x)
  return(yc)
}
conv<-function(x, y) {
  #limpiando el texto
  #el argumento debe de tener la estructura abc$text
  x<- tesseract::ocr(x, engine = "eng")
  trimws(x, which=c("both"))
  write.csv(x, y)
}
coords2country <- function(df){  
  ll<-subset(df, select=c("lon", "lat"))
  ll[is.na(ll)]<-FALSE
  countriesSP <- getMap(resolution='high')
  pointsSP <- SpatialPoints(ll, proj4string=CRS(proj4string(countriesSP)))  
  indices <- over(pointsSP, countriesSP, use="complete.obs")
  indices$ADMIN<-tolower(indices$ADMIN)
  indices$ADMIN
}
coords2countiso <- function(df){  
  ll<-subset(df, select=c("lon", "lat"))
  ll[is.na(ll)]<-FALSE
  countriesSP <- getMap(resolution='high')
  pointsSP <- SpatialPoints(ll, proj4string=CRS(proj4string(countriesSP)))  
  indices <- over(pointsSP, countriesSP, use="complete.obs")
  return(indices$ISO_A2)
}

fname<-function(x){ 
  y<-tolower(str_extract(x, "^(\\w+)"))
  return(y)
}
initials<-function(x){
  y<-str_replace_all(x, "(?<=(\\s)(\\w){1})[.](?=(\\w){1}[.])", "")
  y<-str_replace_all(y, "(?<=(\\s)(\\w){1})[.](?=(\\s))", "")
  y<-str_replace_all(y, "(?<=(\\s)(\\w){2})[.](?=(\\s))", "")
  y<-str_replace_all(y, "(?<=^(\\w){1})[.](?=(\\w){1}[.])", "")
  y<-str_replace_all(y, "(?<=^(\\w){1})[.](?=(\\s))", "")
  y<-str_replace_all(y, "(?<=^(\\w){2})[.](?=(\\s))", "")
  y<-str_replace_all(y, "(\\s)La(\\s)", " La")
  y<-str_replace_all(y, "(\\s)De(\\s)", " De")
  y<-str_replace_all(y, "(\\s)L['](\\s)", " L")
  y<-str_replace_all(y, "(\\s)D['](\\s)", " D")
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
llenar<-function(x){
  y<-ifelse(!is.na(x), x, lag(x, 1))
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
mapear<-function(df, titulo){
  df<-base+ geom_point(data=df, aes(x=lon, y=lat, fill=year), color = "white", shape=21, size=3.0, alpha=0.5)+
    coord_fixed(ratio = 1.1)+
    scale_colour_distiller(palette="Purples", name="year", guide = "colorbar")+
    ggtitle(titulo)+
    theme(plot.title = element_text(hjust = 0.5))
  df}
mname<-function(x){
  y<-ifelse(!is.na(str_extract(x, "^(\\w+)(\\s)(\\w+)(\\s)(\\w+)")), 
            tolower(str_extract(x, "^(\\w+)(\\s)(\\w+)")), NA)
  y<-str_replace(y, "^(\\w+)(\\s)", "")
  return(y)
}
ordenar<-function(df){
  y<-as.data.frame(df)
  y$temp<-str_replace_all(y$id, "[[:digit:]]", "")
  y$id<-as.numeric(as.character((str_replace_all(y$id, "[^[:digit:]]", ""))))
  y<-y[order(y$source, y$id),]
  y$id<-paste(y$temp, y$id, sep="")
  return(y)
}
rellenar<-function(x, cond){
  y<-ifelse(!is.na(x), x, 
            ifelse((cond==lag(cond, 1))&!is.na(lag(x,1)), lag(x, 1), 
                   ifelse((cond==lead(cond, 1))&!is.na(lead(x,1)), lead(x, 1),   
                          x)))
  return(y)
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
rep2<-function(x, cond, funcion){
  y<-funcion(x, cond)
  x<-funcion(y, cond)
  return(x)
}
rep10<-function(x, cond, funcion){
  y<-rep2(x, cond, funcion)
  x<-rep2(y, cond, funcion)
  y<-rep2(x, cond, funcion)
  x<-rep2(y, cond, funcion)
  y<-rep2(x, cond, funcion)
  return(y)
}
rep100<-function(x, cond, funcion){
  y<-rep10(x, cond, funcion)
  x<-rep10(y, cond, funcion)
  y<-rep10(x, cond, funcion)
  x<-rep10(y, cond, funcion)
  y<-rep10(x, cond, funcion)
  x<-rep10(y, cond, funcion)
  y<-rep10(x, cond, funcion)
  x<-rep10(y, cond, funcion)
  y<-rep10(x, cond, funcion)
  x<-rep10(y, cond, funcion)
  return(x)
}
rep1000<-function(x, cond, funcion){
  y<-rep100(x, cond, funcion)
  x<-rep100(y, cond, funcion)
  y<-rep100(x, cond, funcion)
  x<-rep100(y, cond, funcion)
  y<-rep100(x, cond, funcion)
  x<-rep100(y, cond, funcion)
  y<-rep100(x, cond, funcion)
  x<-rep100(y, cond, funcion)
  y<-rep100(x, cond, funcion)
  x<-rep100(y, cond, funcion)
  return(x)
}
rep<-function(x, cond, funcion, n){
  if(n==2){
    y<-rep2(x, cond,funcion)
    return(y)
  }
  else if(n==10){
    y<-rep10(x, cond,funcion)
    return(y)
  }
  else if(n==100){
    y<-rep100(x, cond,funcion)
    return(y)
  }
  else if(n==1000){
    y<-rep1000(x, cond,funcion)
    return(y)
  }
  else{
    warning('n has to be 2, 10, 100 or 1000')
    return(x)
  }
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
sname<-function(x){
  y<-ifelse(!is.na(x$lastname), 
            paste(x$lastname, ", ", substr(x$firstname, 1, 1), " " ,
                  ifelse(!is.na(substr(x$middlename, 1, 1)), substr(x$middlename, 1, 1),""), sep=""), 
            NA)
  y<-tolower(trimws(y))
  return(y)
}
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
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


#Z is 1.96 at 95% confidence level, we suppose a p=0.5, for a sample with replacement
muestra<-function(df, source){
  n<-ceiling((1.96^2)*((0.5*(1-0.5)/(0.05^2)))/(1+(0.5*(1-0.5)/((0.05^2)*nrow(df)))))
  y<-df[sample(nrow(df), n, replace=T), ]
  #y$id<-as.numeric(as.character((str_replace_all(y$id, ".*[^[:digit:]]", ""))))
  y<-y[order(y$id),]
  #y$id<-paste(source, y$id, sep="")
  return(y)
}


####II. 1 Integrate####
alumni<-read.csv("alumnix.csv")
alumni<-subset(alumni, select=c("id", "type","source", "year", "lastname","firstname","middlename","simpname", "prof","edu","org", "loc", "lon", "lat", "country"))
alumni$text<-NA
off<-read.csv("off.csv")
off$org<-off$company
off$prof<-off$role
off$type<-"corpis"
off<-subset(off, select=c("id", "text","type","source", "year","lastname","firstname","middlename", "simpname", "prof","edu","org", "loc", "lon", "lat"))
off$country<-coords2country(off)
aime<-read.csv("combo14.csv")
aime<-aime[which(aime$source=="taime"),]
aime$id<-paste("aime", rownames(aime), sep="")
aime$lastname<-str_extract(aime$simpname, "^(\\w+)")
aime$firstname<-str_extract(aime$simpname, "(?<=[,](\\s))(\\w+)")
aime$middlename<-ifelse(str_extract(aime$simpname, "(\\w+)$")!=str_extract(aime$simpname, "(?<=[,](\\s))(\\w+)"), 
                        str_extract(aime$simpname, "(\\w+)$"), NA)
aime<-subset(aime, select=c("id", "type","source", "year","lastname","firstname","middlename", "simpname", "prof","edu","org", "loc", "lon", "lat"))
aime$country<-coords2country(aime)
aime$text<-NA
emj<-read.csv("emj3.csv")
emj$id<-paste("emj", emj$id, sep="")
emj$count<-NULL
colnames(emj)<-c("id", "year", "text", "loc", "simpname", "topic", "lon", "lat", "country")
emj$type<-"journal"
emj$source<-"emj"
emj$lastname<-str_extract(emj$simpname, "^(\\w+)")
emj$firstname<-str_extract(emj$simpname, "(?<=[,](\\s))(\\w+)")
emj$middlename<-ifelse(str_extract(emj$simpname, "(\\w+)$")!=str_extract(emj$simpname, "(?<=[,](\\s))(\\w+)"), 
                       str_extract(emj$simpname, "(\\w+)$"), NA)
emj$prof<-NA
emj$edu<-NA
emj$org<-NA
combo<-rbind(alumni, aime, off)
combo$topic<-NA
combo<-rbind(combo, emj)
combo<-combo[order(combo$simpname, combo$year, combo$type), ]
#combo$lastname<-repetir(combo$lastname, rfin, 10)
combo$middlename<-ifelse(!is.na(combo$middlename), 
                         combo$middlename, 
                         ifelse(combo$lastname==lag(combo$lastname)&combo$firstname==lag(combo$firstname), 
                                lag(combo$middlename),
                                ifelse(combo$lastname==lead(combo$lastname)&combo$firstname==lead(combo$firstname), 
                                       lead(combo$middlename),
                                       NA)))
combo$notes<-NA
combo$position<-combo$prof
combo$prof<-NULL
crp<-read.csv("crp.csv")
crp$org<-crp$company
crp$position<-crp$role
crp$role<-NULL
crp$company<-NULL
crp$X<-NULL
combo2<-rbind.fill(combo, crp)
write.csv(combo2, "comboprueba.csv", row.names = F)
write.csv(combo, "combob.csv", row.names = F)
apellidos<-subset(combo, select="lastname")
apellidos$lastname<-tolower(apellidos$lastname)
apellidos<-distinct(apellidos)
write.csv(apellidos, "apellidosb.csv", row.names = F)
#### II.2 After clean on openrefine####
combo<-read.csv("combob.csv")
apellidos1<-read.csv("apellidosd.csv")
apellidos2<-read.csv("apellidosc.csv")
apellidos1$lname<-apellidos2$lastname
apellidos1<-distinct(apellidos1)
combo$lastname<-tolower(combo$lastname)
combo<-left_join(combo, apellidos1, by="lastname")
temp<-combo[which(is.na(combo$lname)),]
temp2<-unique(temp$lastname)
temp<-combo[which(!is.na(combo$lname)),]
combo$lastname<-ifelse(!is.na(combo$lname), combo$lname, combo$lastname)
temp<-combo[which(is.na(combo$lastname)),]
combo$firstname<-ifelse(!is.na(combo$firstname), combo$firstname, combo$middlename)
combo$middlename<-ifelse(combo$middlename==combo$firstname, NA, combo$middlename)
for(i in 1:4){
  combo$firstname<-ifelse(nchar(combo$firstname)>1, combo$firstname, 
                          ifelse((combo$lastname==lag(combo$lastname))&
                                   (!is.na(lag(combo$firstname)))&
                                   nchar(lag(combo$firstname))>1&
                                   (str_extract(combo$firstname, "^(\\w){1}")==str_extract(lag(combo$firstname), "^(\\w){1}")), lag(combo$firstname),
                                 ifelse((combo$lastname==lead(combo$lastname))&
                                          (!is.na(lead(combo$firstname)))&
                                          nchar(lead(combo$firstname))>1&
                                          (str_extract(combo$firstname, "^(\\w){1}")==str_extract(lead(combo$firstname), "^(\\w){1}")), lead(combo$firstname), 
                                        combo$firstname)))
  combo$middlename<-ifelse(nchar(combo$middlename)>1, combo$middlename, 
                           ifelse((combo$lastname==lag(combo$lastname))&
                                    nchar(lag(combo$middlename))>1&
                                    nchar(lag(combo$middlename))>1&
                                    (str_extract(combo$middlename, "^(\\w){1}")==str_extract(lag(combo$middlename), "^(\\w){1}")), lag(combo$middlename),
                                  ifelse((combo$lastname==lead(combo$lastname))&
                                           nchar(lead(combo$middlename))>1&
                                           nchar(lead(combo$middlename))>1&
                                           (str_extract(combo$middlename, "^(\\w){1}")==str_extract(lead(combo$middlename), "^(\\w){1}")), lead(combo$middlename), 
                                         combo$middlename)))
  combo<-combo[order(combo$lastname, combo$firstname, combo$simpname),]
}
combo$simpname<-sname(combo)
combo<-combo[order(combo$lastname, combo$firstname, combo$simpname),]
for(i in 1:100){
  combo$middlename<-ifelse(is.na(combo$middlename)&(combo$lastname==lag(combo$lastname))&(combo$firstname==lag(combo$firstname)), lag(combo$middlename), combo$middlename)
  combo$middlename<-ifelse(is.na(combo$middlename)&(combo$lastname==lead(combo$lastname))&(combo$firstname==lead(combo$firstname)), lead(combo$middlename), combo$middlename)
}
combo$simpname<-sname(combo)
combo<-combo[order(combo$lastname, combo$firstname, combo$middlename),]
write.csv(combo, "combo2.csv", row.names = F)
#### II. 3. repeat####
combo<-read.csv("combo2.csv")
combo$lastname<-trimws(combo$lastname, which=c("both"))
combo$firstname<-trimws(combo$firstname, which=c("both"))
combo$middlename<-trimws(combo$middlename, which=c("both"))
combo$cond<-ifelse((combo$firstname==lag(combo$middlename,3))&(combo$lastname==lag(combo$lastname, 3)), 3, 
                   ifelse((combo$firstname==lag(combo$middlename,2))&(combo$lastname==lag(combo$lastname, 2)), 2, 
                          ifelse((combo$firstname==lag(combo$middlename,1))&(combo$lastname==lag(combo$lastname, 1)), 1, NA)))          
combo$middlename<-ifelse(!is.na(combo$middlename), combo$middlename, 
                         ifelse(combo$cond==3, lag(combo$middlename, 3),
                                ifelse(combo$cond==2, lag(combo$middlename, 2),
                                       ifelse(combo$cond==1, lag(combo$middlename, 1), combo$middlename))))
combo$firstname<-ifelse(is.na(combo$cond), combo$firstname, 
                        ifelse((combo$cond==3)&(combo$middlename==lag(combo$middlename, 3)), lag(combo$firstname, 3),
                               ifelse((combo$cond==2)&(combo$middlename==lag(combo$middlename, 2)), lag(combo$firstname, 2),
                                      ifelse((combo$cond==1)&(combo$middlename==lag(combo$middlename, 1)), lag(combo$firstname, 1),
                                             combo$firstname))))
combo<-combo[order(combo$lastname, combo$firstname, combo$middlename),]
combo$simpname<-sname(combo)
combo$simpname<-repetir(combo$simpname, rfin, 10)
combo$org<-tolower(combo$org)
combo$cond<-NULL
combo$org<-str_replace_all(combo$org, "[(]subsidiary(\\s)of(\\s)", "")
combo$org<-str_replace_all(combo$org, "[)]$", "")
combo$org<-trimws(combo$org)
combo$org<-str_replace_all(combo$org, "[^[:alpha:]]", " ")
combo$org<-str_replace_all(combo$org, "(\\s+)", " ")
cias<-unique(combo$org)
edu<-subset(combo, select=c("id", "edu"))
write.csv(unique(edu$edu), "temp.csv", row.names = F)
write.csv(cias, "cias.csv", row.names = F)
write.csv(combo, "combo3.csv", row.names = F)
####II. 4. After cleaning on open refine####
combo<-read.csv("combo3.csv")
combo$X<-NULL
cias<-read.csv("cias2.csv")
edu<-read.csv("edub.csv")
correct<-read.csv("correcion.csv")
correct$X<-NULL
correct$key<-NULL
correct$position<-correct$prof
correct$prof<-NULL
cias$cias1<-str_replace_all(cias$cias1, "(\\b)co(\\b)", "company")
cias$cias1<-ifelse(!is.na(str_extract(cias$cias1, "(.*?)company")), 
                   str_extract(cias$cias1, "(.*?)company"), cias$cias1)
cias$cias1<-ifelse(!is.na(str_extract(cias$cias1, "american smelting")), 
                   str_extract(cias$cias1, "american smelting refining co"), cias$cias1)
combo<-left_join(combo, subset(cias, select=c("org", "cias1")), by="org")
combo$org<-ifelse(!is.na(combo$cias1), combo$cias1, combo$org)
combo$cias1<-NULL
combo<-distinct(combo)
combo<-left_join(combo, edu, by="edu")
combo$edu<-ifelse(!is.na(combo$edu1), combo$edu1, combo$edu)
combo$edu1<-NULL
combo<-distinct(combo)
combo$notes<-NULL
combo$topic<-NULL
combo<-combo[which((nchar(combo$lastname)>2)&combo$source!="worcester"&combo$source!="umn"),]
combo<-rbind.fill(combo, correct)
mistakes0<-paste(c("resident", "miscellaneous(\\s+)(\\w+)", "(\\b)rector(\\b)"), collapse="|")
combo$org<-str_replace_all(combo$org, mistakes0, "")
combo$firstname<-str_replace_all(combo$firstname, mistakes0, "")
combo$middlename<-str_replace_all(combo$middlename, mistakes0, "")
combo$lastname<-str_replace_all(combo$lastname, mistakes0, "")
combo$lastname<-combo$lastname%>%str_replace_all("[:digit:]|[:punct:]", "")%>%str_replace_all("^(\\s+)$", "")
combo$firstname<-combo$firstname%>%str_replace_all("[:digit:]|[:punct:]", "")%>%str_replace_all("^(\\s+)$", "")
combo$middlename<-combo$middlename%>%str_replace_all("[:digit:]|[:punct:]", "")%>%str_replace_all("^(\\s+)$", "")
combo$firstname[which(combo$firstname=="")]<-NA
combo$lastname[which(combo$lastname=="")]<-NA
combo$middlename[which(combo$middlename=="")]<-NA
combo$firstname<-ifelse(is.na(combo$firstname)&!is.na(combo$middlename), combo$middlename, combo$firstname)
combo$middlename<-ifelse((combo$firstname==combo$middlename)|(combo$lastname==combo$middlename), NA, combo$middlename)
combo<-combo[which(!is.na(combo$firstname)&!is.na(combo$lastname)),]
combo$simpname<-sname(combo)
mistakes<-c("ltd", "milling","r[e|c]gistered", "smelters", "company", "(\\b)box(\\b)","foundation","developmen", 
            "school", "aime", "prospectin", "(\\b)and(\\b)", "incorpo", "limite", "explora", "(\\b)mine(\\b)",
            "(\\b)mines(\\b)", "produc", "metal", "compan[y|i]", "valley", "africa", "o[f|l][f|l]ice", "(\\b)co(\\b)", 
            "chair", "manage", "reader(\\b)", "mited(\\b)", "(\\b)each(\\b)", "altern", "registr", "fully", 
            "local", "mineral", "share", "paid(\\b)", "trustee", "phone", "synd(\\b)", "district", "except")%>%paste(collapse = "|")
combo2<-combo[which(!is.na(combo$simpname)&!str_detect(combo$lastname, mistakes)&!str_detect(combo$firstname, mistakes)&(is.na(combo$middlename)|!str_detect(combo$middlename, mistakes))),]
length(unique(combo$simpname))
for(i in 1:10){
  combo$edu<-ifelse(!is.na(combo$edu), combo$edu, 
                    ifelse((combo$simpname==lag(combo$simpname, 1))&!is.na(lag(combo$edu,1)), lag(combo$edu, 1), 
                           ifelse((combo$simpname==lead(combo$simpname, 1))&!is.na(lead(combo$edu,1)), lead(combo$edu, 1),   
                                  combo$edu)))
}
combo$year<-ifelse(combo$year==515, 1815, combo$year)
combo$year<-ifelse(combo$year<20, combo$year+1900, combo$year)
combo$year<-ifelse(combo$year<100, combo$year+1800, combo$year)
combo$year<-str_extract(combo$year, "(\\d){4}")
combo$year<-str_replace(combo$year, "^7", "1")
combo$year<-str_replace(combo$year, "^1[2|3]", "18")
combo$id<-str_replace(combo$id, "ammamm", "amm")
combonoc<-subset(combo, is.na(combo$country))
combonoc$country<-coords2country(combonoc)
combo<-rbind(subset(combo, !is.na(combo$country)), combonoc)
combo<-combo[order(combo$simpname, combo$year),]
combo$org<-str_replace(combo$org, "subsidiary(\\s)of(\\s)", "")
combo$org<-ifelse(str_detect(tolower(combo$org), "(\\b)freiberg(\\b)"), "bergakademie freiberg", combo$org)
write.csv(combo, "combo4.csv", row.names=F)

##### II. 4.b further cleaning####
amm<-read.csv("amm2.csv")
mmm<-read.csv("mmm2.csv")
myb<-rbind(read.csv("myb1.csv"), read.csv("myb2.csv"), read.csv("myb3.csv"))
myb$X<-NULL
amm$X<-NULL
mmm$X<-NULL
combo<-read.csv("combo4.csv")

crp<-plyr::rbind.fill(
  subset(myb, select=c("id","year", "company", "capital", "loc", "lon", "lat", "text")), 
  subset(amm, select=c("id","year", "company", "capital", "loc", "lon", "lat", "text")),
  subset(mmm, select=c("id","year", "company", "capital", "loc", "lon", "lat", "text", "technology")))
for( i in 2:nrow(crp)){
  if(!is.na(crp$company[i])&str_detect(crp$company[i], "^Company$|^Mines(\\s)Company|^Mining(\\s)Company")){
    crp$company[i]<-crp2$company[i-1]
  }
}
crp2<-left_join(crp, 
                subset(combo, select=c("id", "org")))
crp2<-subset(crp2, !is.na(crp2$org), select=c("company", "org"))
crp2<-distinct(crp2)
crp2$org<-str_to_title(crp2$org)
crp<-left_join(crp, crp2)
crp$company<-ifelse(!is.na(crp$org)&(!str_detect(crp$org, "^Company$|^Mines(\\s)Company|^Mining(\\s)Company")), 
                    crp$org, crp$company)
crp3<-subset(crp, 
             str_detect(crp$org, "^Company$|^Mines(\\s)Company|^Mining(\\s)Company"), 
             select=c("id", "org", "company"))
crp3<-distinct(crp3)
write.csv(crp3, "correcorg.csv", row.names = F)
#####II. 4. c. Manually cleaning####
combo<-read.csv("combo4.csv")
correc<-read.csv("correcion2.csv")
combo<-left_join(combo, correc)
combo$org<-ifelse(!is.na(combo$company), combo$company, combo$org)
correc<-read.csv("correcorg2.csv")
correc$org2<-correc$company
correc<-subset(correc, select=c("id", "org2"))
crc<-unique(correc$org2)
combo<-left_join(combo, correc)
prueba<-combo[which(!is.na(combo$org2)),]
combo$org<-ifelse(!is.na(combo$org2), tolower(combo$org2), tolower(combo$org))
combo$org2<-NULL
combo$org<-str_replace(combo$org, "^(\\s+)", "")%>%trimws(which="right")
combo$org<-str_replace(combo$org, "^(\\s+)", "")%>%trimws(which="right")
combo$org<-ifelse(str_detect(combo$org, "^(\\w)$|^co$"), NA, combo$org)
org<-subset(combo, select=c("id", "org"))
org<-distinct(org)
org$org<-tolower(org$org)
table<-count(org, "org")
write.csv(table, "orgistemp.csv", row.names=F)
correc<-read_csv("correcorg3.csv")
colnames(correc)<-c("org", "org2", "freq")
correc$org2<-ifelse(str_detect(correc$org2, "mass(\\s)institute"), "mit", correc$org2)
combo<-left_join(combo, correc, by=c("org"))
combo$org<-ifelse(!is.na(combo$freq), tolower(combo$org2), tolower(combo$org))
combo$org<-str_to_title(combo$org)
combo$org2<-NULL
combo$org<-str_replace_all(combo$org, "Tympani", "Company")
combo$org<-str_replace_all(combo$org, "Sited", "Limited")
combo$org<-str_replace_all(combo$org, "Min(\\s)Ingw", "Mining")
combo$freq<-NULL
write.csv(combo, "combo5a.csv", row.names = F)
#####II. 4. d.adding new sources####
combo<-read.csv("combo5a.csv")
camborne<-read.csv("alcamborne.csv", na.strings=c("", " ", "NA"))
camborne$id<-paste("cam", row.names(camborne), sep="")
camborne$year<-str_extract(camborne$Date, "^(\\d){4}")
org<-c("(?<=c/o).*?(?=[,])", "(?<=[,]).*?(\\b)Co(\\b)", "(?<=[,]).*?(\\b)Ltd(\\b)")%>%
  paste(collapse = "|")
camborne$org<-str_extract(camborne$Location, org)
loc<-c("(\\w+)[,](\\s+)(\\w+)$", 
       "(\\w+)[,](\\s+)(\\w+)(\\s+)(\\w+)$", 
       "(\\w+)[,](\\s+)(\\w+)(\\s+)(\\w+)(\\s+)(\\w+)$", 
       "(\\w+),.*?$")%>%
  paste(collapse="|")
camborne$loc<-str_extract(camborne$Location, loc)
for(i in 1:6){
  camborne$loc<-ifelse(str_detect(camborne$loc,"(?<=[,]).*?,.*?$"), str_extract(camborne$loc,"(?<=[,]).*?,.*?$"), camborne$loc)%>%
    trimws(which="both")
  camborne$loc<-ifelse(!str_detect(camborne$Location,"[,]"), camborne$Location, camborne$loc)%>%
    trimws(which="both")
  camborne$loc<-str_replace_all(camborne$loc, "(\\b)Ltd(\\b)|(\\b)Co(\\b)|(\\s+)|^[:punct:]", " ")%>%
    trimws(which="both")
}
camborne$loc<-ifelse(is.na(camborne$loc), camborne$Country, camborne$loc)
llave<-c("engineer", "manager", "superintendent", "foreman", "president", "student", "geolog", "consulting", 
         "attorney", "prof", "chemist", "draftsman", "salesman", "architect", "inspector", "clerk", "analyst", 
         "electrician", "metallurg", "hydrographer", "civil", "council", "clergy", "designer", "specialist", 
         "supervisor", "analyst", "deputy", "editor", "electric", "broker", "experiment", "govern", "dealer", 
         "merchant", "owner", "proprietor","contractor", "retired", "died", "teacher", "manufacturer", "machinist",
         "administ", "academ", "graduate", "builder", "assayer", "assistant", "accountant", "adjuster") %>%
  paste(collapse="|")
camborne$position<-str_extract(tolower(camborne$Location), llave)
camborne$text<-camborne$Location
camborne$org<-str_replace(camborne$org, "c/o|(\\s+)", " ")%>%trimws(which="both")
camborne$lastname<-str_extract(camborne$Name, ".*(?=[,])")
camborne$firstname<-str_extract(camborne$Name, "(?<=[,](\\s))(\\w+)")
camborne$names<-str_extract(camborne$Name, "(?<=[,](\\s)).*")
camborne$middlename<-str_extract(camborne$names, "(?<=(\\w)(\\s))(\\w+)|(?<=(\\w)[.])(\\w+)")
camborne$simpname<-sname(camborne)
camborne$type<-"alumni"
camborne$source<-"cam"
camborne$edu<-"mining engineer"
camborne<-buscar2(camborne)
camborne$country<-camborne$Country
camborne$nationality<-NA
combo$lname<-NULL
combo$company<-NULL
camborne2<-subset(camborne, select=colnames(combo))
combo2<-rbind(combo, camborne2)
write.csv(combo2, "combo5.csv", row.names = F)
#####II. 4. e. homogeinizing with corpis####
combo<-read.csv("combo5.csv")
together<-read.csv("together.csv")
cias<-subset(together, select=c("id", "company"))%>%distinct()
combo<-left_join(combo, cias)
combo$org<-ifelse(!is.na(combo$company), combo$company, combo$org)
write.csv(combo, "combo5_1.csv")

#### II. 5. Test####
#### II. 5.1 Levehnstein####
combo<-read.csv("combo4.csv")
length(unique(combo$simpname))
combotest<-muestra(combo)
combotest$number<-str_replace(combotest$id, ".*[a-z]", "")
combotest$number<-as.numeric(combotest$number)
combotest$code<-str_extract(combotest$id, ".*[a-z]")
combotest<-combotest[order(combotest$type, combotest$source, combotest$code, combotest$number),]
write.csv(combotest, "comboprueba.csv", row.names = F)

#### II. 5.2  Testing matching structured and semistructured####
officers<-read.csv("officers.csv")
officers<-errores(officers, officers$company)
officers2$edu<-officers2$role
officers2<-distinct(officers2, simpname, .keep_all=T)
officers2$source<-"myb"
mengi<-read.csv("imm6.csv")
mengi$id<-paste("imm", rownames(mengi), sep="")
mengi$role<-mengi$prof
mengi$lastname<-str_extract(mengi$simpname, ".*(?=[,])")
mengi$firstname<-str_extract(mengi$simpname, "(?<=[,](\\s))(\\w)")
mengi$middlename<-str_extract(mengi$simpname, "(?<=[,](\\s)(\\w)(\\s))(\\w)")
mengi<-errores(mengi, mengi$org)
mengi<-mengi[order(mengi$org),]
profis<-read.csv("combo4.csv")
profis<-subset(profis, profis$type!="corpis", select=c("id", "year","type", "simpname", "lastname","firstname", "middlename", "position", "edu","source", "loc", "lon", "lat"))
ama<-read.csv("ama.csv")
ama$id<-paste("ama", rownames(ama), sep="")
ama$simpname<-sname(ama)
mengi$edu<-mengi$prof
ama$edu<-"engineer"
profesionales<-rbind(subset(mengi, select=c("simpname", "edu", "source", "id")), 
                     subset(profis, select=c("simpname", "edu", "source", "id")), 
                     subset(ama, select=c("simpname", "edu", "source", "id")), 
                     subset(officers2, select=c("simpname", "edu", "source", "id")))
profesionales<-distinct(profesionales, simpname, .keep_all = T)
together<-read.csv("together.csv")
toge2<-subset(together, !is.na(together$simpname))
profesionales<-read.csv("profesionales11.csv")
matchf<-left_join(
  toge2, 
  profesionales, by="simpname")
matcht<-matchf[which(!is.na(matchf$simpname)&!is.na(matchf$source)&(matchf$id.x!=matchf$id.y)),]
testmat<-muestra(matcht)
testmat<-testmat[order(testmat$id.x, testmat$id.y),]
write.csv(testmat, "testmat.csv", row.names=F)

####II. 6. Divide####
combo<-read.csv("combo5_1.csv")
combo$prof<-combo$position
combob<-combo[which((combo$simpname==lag(combo$simpname))|(combo$simpname==lead(combo$simpname))),]
write.csv(combob, "combob.csv")
comboc<-combo[which(
  (combo$type!="corpis")|
    ((combo$simpname==lag(combo$simpname,1))&(combo$type!=lag(combo$type,1)))|
    ((combo$simpname==lead(combo$simpname,1))&(combo$type!=lead(combo$type,1)))|
    ((combo$simpname==lag(combo$simpname,2))&(combo$type!=lag(combo$type,2)))|
    ((combo$simpname==lead(combo$simpname,2))&(combo$type!=lead(combo$type,2)))|
    ((combo$simpname==lag(combo$simpname,3))&(combo$type!=lag(combo$type,3)))|
    ((combo$simpname==lead(combo$simpname,3))&(combo$type!=lead(combo$type,3)))|
    ((combo$simpname==lag(combo$simpname,4))&(combo$type!=lag(combo$type,4)))|
    ((combo$simpname==lead(combo$simpname,4))&(combo$type!=lead(combo$type,4)))|
    ((combo$simpname==lag(combo$simpname,5))&(combo$type!=lag(combo$type,5)))|
    ((combo$simpname==lead(combo$simpname,5))&(combo$type!=lead(combo$type,5)))|
    ((combo$simpname==lag(combo$simpname,6))&(combo$type!=lag(combo$type,6)))|
    ((combo$simpname==lead(combo$simpname,6))&(combo$type!=lead(combo$type,6)))|
    ((combo$simpname==lag(combo$simpname,7))&(combo$type!=lag(combo$type,7)))|
    ((combo$simpname==lead(combo$simpname,7))&(combo$type!=lead(combo$type,7)))|
    ((combo$simpname==lag(combo$simpname,8))&(combo$type!=lag(combo$type,8)))|
    ((combo$simpname==lead(combo$simpname,8))&(combo$type!=lead(combo$type,8)))|
    ((combo$simpname==lag(combo$simpname,9))&(combo$type!=lag(combo$type,9)))|
    ((combo$simpname==lead(combo$simpname,9))&(combo$type!=lead(combo$type,9)))|
    ((combo$simpname==lag(combo$simpname,10))&(combo$type!=lag(combo$type,10)))|
    ((combo$simpname==lead(combo$simpname,10))&(combo$type!=lead(combo$type,10)))|   
    ((combo$simpname==lag(combo$simpname,11))&(combo$type!=lag(combo$type,11)))|
    ((combo$simpname==lead(combo$simpname,11))&(combo$type!=lead(combo$type,11)))|
    ((combo$simpname==lag(combo$simpname,12))&(combo$type!=lag(combo$type,12)))|
    ((combo$simpname==lead(combo$simpname,12))&(combo$type!=lead(combo$type,12)))|
    ((combo$simpname==lag(combo$simpname,13))&(combo$type!=lag(combo$type,13)))|
    ((combo$simpname==lead(combo$simpname,13))&(combo$type!=lead(combo$type,13)))|
    ((combo$simpname==lag(combo$simpname,14))&(combo$type!=lag(combo$type,14)))|
    ((combo$simpname==lead(combo$simpname,14))&(combo$type!=lead(combo$type,14)))|
    ((combo$simpname==lag(combo$simpname,15))&(combo$type!=lag(combo$type,15)))|
    ((combo$simpname==lead(combo$simpname,15))&(combo$type!=lead(combo$type,15)))
),]
length(unique(comboc$simpname))
length(unique(comboc$org))
alumni<-combo[which(combo$type=="alumni"),]
alumni<-alumni[order(alumni$year, alumni$simpname),]
profis<-combo[which(combo$type=="professional"),]
profis<-profis[order(profis$year, profis$simpname),]
corpis<-combo[which(combo$type=="corpis"),]
corpis<-corpis[order(corpis$year, corpis$simpname),]
combod<-merge(alumni, profis, by="simpname", match="all")
combod<-merge(combod, corpis, by="simpname", match="all")
combod1<-subset(combod, select=c("id", "type", "source", "year", "lastname", "firstname", "middlename", "simpname","prof", "org", "loc", "lon", "lat", "country"))
combod2<-subset(combod, select=c("id.x", "type.x", "source.x", "year.x", "lastname.x", "firstname.x", "middlename.x", "simpname","prof.x", "org.x", "loc.x", "lon.x", "lat.x", "country.x"))
combod3<-subset(combod, select=c("id.y", "type.y", "source.y", "year.y", "lastname.y", "firstname.y", "middlename.y", "simpname","prof.y", "org.y", "loc.y", "lon.y", "lat.y", "country.y"))
colnames(combod2)<-c("id", "type", "source", "year", "lastname", "firstname", "middlename", "simpname","prof", "org", "loc", "lon", "lat", "country")
colnames(combod3)<-c("id", "type", "source", "year", "lastname", "firstname", "middlename", "simpname","prof", "org", "loc", "lon", "lat", "country")
combod<-rbind(combod1, combod2, combod3)
combod<-distinct(combod)
combo<-distinct(combo)
length(unique(combod$simpname))
length(unique(combod$org))
length(unique(combod$lon))
write.csv(comboc, "comboc.csv", row.names = F)
write.csv(combod, "combod.csv", row.names = F)
write.csv(alumni, "calumni.csv", row.names = F)
write.csv(profis, "cprofis.csv", row.names = F)
write.csv(corpis, "ccorpis.csv", row.names = F)

####III. transformation to db####
#####IIII. 1. homologation#####
combo<- read_csv("combo5_3.csv")
crp<- read_csv("crp3.csv")
emj<-read_csv("emjdbase1_3.csv")
names<-read_csv("../ML/ensayo4/ocr/integration/nombc.csv")
names$simpname<-sname(names)
emj<-left_join(emj, subset(names, select=c(fname, simpname)), multiple="first")
combo$locn<-stri_trans_general(combo$locn, 'latin-ascii')
crp$locn<-stri_trans_general(crp$locn, 'latin-ascii')
crp$text<-stri_trans_general(crp$text, 'latin-ascii')
crp$text<-str_replace_all(crp$text, '[^[:graph:]]', " ")
crp$text<-str_replace_all(crp$text, '(?<=(\\s))(\\s)|\\\\', "")
crp$text2<-stri_trans_general(crp$text2, 'latin-ascii')
crp$text2<-str_replace_all(crp$text2, '[^[:graph:]]', " ")
crp$text2<-str_replace_all(crp$text2, '(?<=(\\s))(\\s)|\\\\', "")
locn<-rbind(subset(crp, !is.na(crp$locn), select=c(lon, lat, locn)), 
            subset(combo, !is.na(combo$locn), select=c(lon, lat, locn))) %>%
  distinct()
emj$text<-str_replace_all(emj$text, "[\"]", "")
emj2<-left_join(emj, locn, multiple="first")
temp<-subset(emj2, !is.na(emj2$lon)&is.na(emj2$locn), select=c(lon, lat)) %>% distinct()
temp$locn<-coordstoloc(temp)
temp2<-subset(emj2, !is.na(emj2$lon)&is.na(emj2$locn), select=-c(locn))
temp2<-left_join(temp2, temp)
emj3<-rbind(subset(emj2,is.na(emj2$lon)|!is.na(emj2$locn)), temp2)
emj3$org<-str_replace(emj3$company, "(?<=(\\s))Co$", "Company")
emj3$text<-str_replace_all(emj3$text, "[/]|\\\\|\\n|[\"]|\\'", "")
emj3$locn<-stri_trans_general(emj3$locn, 'latin-ascii')
crp$org<-str_replace(crp$company, "(?<=(\\s))Co$", "Company")
for(i in 1:20){
  combo$company<-ifelse(nchar(combo$company)<40, combo$company, 
                        ifelse(str_detect(combo$company, "[.].*?(\\s)[C|c]o(\\s)*$"), 
                               str_extract(combo$company, "(?<=[.]).*?(\\s)[C|c]o(\\s)*$"),
                               ifelse(str_detect(combo$company, "^.*?(\\s)[C|c]o(\\s)*$"), 
                                      str_extract(combo$company, "^.*?(\\s)[C|c]o(\\s)*$"),
                                      str_extract(combo$company, "^(\\w+)(\\s+)(\\w+)(\\s+)(\\w+)(\\s+)(\\w+)(\\s+)(\\w+)"))))
}
combo$org<-ifelse(!is.na(combo$company), combo$company, combo$org)
write_csv(emj3, "emjdbase1_4.csv")
write_csv(combo,"combo5_3a.csv" )
write_csv(crp,"crp3_1.csv")

#####III. 2. transformation to format sql#####
combo<-read_csv("combo5_3a.csv")
crp<-read_csv("crp3_1.csv")
emj<-read_csv("emjdbase1_4.csv")
emj<-emj[order(emj$id),]
combo$name<-paste(combo$firstname, combo$middlename, combo$lastname)
combo$name<-str_replace(combo$name, "(\\s)NA(?=(\\s))", "")
emj$simpname<-ifelse(!is.na(emj$name), 
                     paste(emj$lastname, ",",
                           ifelse(!is.na(emj$middlename), paste(" ",emj$middlename, sep=""), " "), str_extract(emj$firstname, ".{1}"), sep=""), 
                     NA)
emj$text<-str_replace_all(emj$text, "\\\\", "")
emj$text<-str_replace_all(emj$text, "\\n", " ")
emj$title<-str_replace_all(emj$title, "\\\\", "")
emj$date<-str_replace_all(emj$date, "\\\\", "")

careers<- combo %>% group_by(simpname) %>% summarize(miny=min(year, na.rm=T), maxy=max(year, na.rm=T)) #added to double check after fuzzymatch
emj<- emj %>% left_join(careers)
emj<- emj %>% mutate(across(c(name, lastname, fname, simpname), ~ifelse(year>maxy|year<miny, NA, .x))) 

crp$name<-ifelse(!is.na(crp$simpname), paste(crp$firstname, crp$middlename, crp$lastname), NA)
crp$name<-ifelse(!is.na(crp$simpname), str_replace(crp$name, "(\\s)NA(?=(\\s))", ""), NA)
crp$country<-coords2country(crp)
indi1<-subset(combo, !is.na(combo$simpname),select=c(lastname, firstname, middlename, name, simpname)) %>% distinct(simpname, .keep_all=T)
indi2<-subset(crp, !is.na(crp$simpname),select=c(lastname, firstname, middlename, name, simpname)) %>% distinct(simpname, .keep_all=T)
indi3<-subset(emj, !is.na(emj$simpname),select=c(lastname, firstname, middlename, name, simpname)) %>% distinct(simpname, .keep_all=T)
indi<-rbind(indi1, indi2, indi3) %>% distinct(simpname, .keep_all=T)
indi$name<-str_to_title(indi$name)
org1<-subset(combo, !is.na(combo$org),select=c(org, company)) %>% distinct(org, .keep_all=T)
org2<-subset(crp, select=c(company)) %>% distinct(company, .keep_all=T) %>% mutate(org=company)
org3<-subset(emj, !is.na(emj$org),select=c(org, company)) %>% distinct(org, .keep_all=T)
orgs<-rbind(org1, org2, org3) %>% distinct(org, .keep_all=T)
orgs<-subset(orgs, !is.na(orgs$org)) %>% distinct()
locn1<-subset(combo, !is.na(combo$locn), select=c(locn, lon, lat, country)) %>% distinct(locn, .keep_all=T)
locn2<-subset(crp,!is.na(crp$locn), select=c(locn, lon, lat, country)) %>% distinct(locn, .keep_all=T)
locn3<-subset(emj, !is.na(emj$locn),select=c(locn, lon, lat, country)) %>% distinct(locn, .keep_all=T)
locn<-rbind(locn1, locn2, locn3) %>% distinct(locn, .keep_all=T)
locn[(nrow(locn)+1),]<-list(NA, 0, 0, NA)
indi[(nrow(indi)+1),]<-list(NA, NA, NA, NA, NA)
orgs[(nrow(orgs)+1),]<-list(NA, NA)
combo2<-subset(combo, select=-c(firstname, middlename, lastname, name, lon, lat, country, company))
crp2<-crp %>% 
  subset(select=-c(firstname, middlename, lastname, name, lon, lat, country))
emj2<-subset(emj, select=-c(firstname, middlename, lastname, name, lon, lat, country, company, crp, reg, fname, cname))
indi$in_id<-rownames(indi)
orgs$og_id<-rownames(orgs)
locn$lc_id<-rownames(locn)
combo2$cm_id<-rownames(combo2)
crp2$cr_id<-rownames(crp2)
emj2$ej_id<-rownames(emj2)
combo3<-left_join(combo2, indi)
combo3<-left_join(combo3, orgs)
combo3<-left_join(combo3, locn)
combo3<-subset(combo3, select=-c(firstname, middlename, lastname, name, lon, lat, country, company, locn, simpname, org))
crp3<-left_join(crp2, indi) 
crp3<-left_join(crp3, orgs)
crp3<-left_join(crp3, locn)
crp3<-subset(crp3, select=-c(firstname, middlename, lastname, name, lon, lat, country, locn, simpname))
emj3<-left_join(emj2, indi)
emj3<-left_join(emj3, orgs)
emj3<-left_join(emj3, locn)
emj3<-subset(emj3, select=-c(firstname, middlename, lastname, name, lon, lat, country, company, locn, simpname, org))

write_csv(combo3,"individuals.csv" )
write_csv(crp3,"firms.csv")
write_csv(emj3, "journals.csv")
write_csv(indi, "indidx.csv")
write_csv(orgs, "orgidx.csv")
write_csv(locn, "locidx.csv")
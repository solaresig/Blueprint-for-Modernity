#### Engines of globalization####
## the American Institute##
#Author: Israel G Solares#
#contact:isgarcia at colmex.mx#
#Licence: CC by 4#
#language: English#

#### I. defining, extracting and cleaning####
####I.0.1 installing packages####
install.packages("countrycode")
install.packages("dplyr")
install.packages("gganimate")
install.packages("ggmap")
install.packages("ggplot2")
install.packages("gifski")
install.packages("grid")
install.packages("gridExtra")
install.packages("hunspell")
install.packages("igraph")
install.packages("magick")
install.packages("magrittr")
install.packages("network")
install.packages("networkD3")
install.packages("plotly")
install.packages("plyr")
install.packages("RColorBrewer")
install.packages("readr")
install.packages("refinr")
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
install.packages("viridis")
install.packages("digest")
install.packages('fansi')
devtools::install_github("stefanieschneider/viafr")

####I.0. 2 defining working directory####
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#### I.0.3 loading packages####
library(countrycode)
library(dplyr)
library(gganimate)
library(ggmap)
library(ggplot2)
library(gifski)
library(grid)
library(gridExtra)
library(hunspell)
library(igraph)
library(magick)
library(magrittr)
library(network)
library(networkD3)
library(plyr)
library(plotly)
library(RColorBrewer)
library(readr)
library(refinr)
library(rworldmap)
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
cargar<-function(x){
  y<-read.csv(x,na.strings=c(""," ","NA"))
  y$X<-NULL
  return(y)
}
buscar<-function(x){
  register_google(key = "")
  y<-geocode(as.character(x$loc))
  y<-as.data.frame(y)
  return(y)
}
buscar2<-function(x){
  y<-subset(x[which(!is.na(x$loc)),], select=c("loc"))
  y<-unique(y$loc)
  y<-as.data.frame(y)
  colnames(y)<-"loc"
  z<-buscar(y)
  y$lon<-z$lon
  y$lat<-z$lat
  return(y)
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
  #x<-str_replace_all(x, "S", "[S|5]")
  #x<-str_replace_all(x, "O", "[O|0]")
  #x<-str_replace_all(x, "G", "[G|6]")
  #x<-str_replace_all(x, "F", "[F|7]")
  #x<-str_replace_all(x, "I", "[I|l|1]")
  #x<-str_replace_all(x, "B", "[B|8|3]")
  #x<-str_replace_all(x, "Z", "[Z|2]")
  #x<-str_replace_all(x, "A", "[A|4]")
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
  countriesSP <- getMap(resolution='low')
  pointsSP <- SpatialPoints(ll, proj4string=CRS(proj4string(countriesSP)))  
  indices <- over(pointsSP, countriesSP, use="complete.obs")
  indices$ADMIN<-tolower(indices$ADMIN)
  indices$ADMIN
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
repeat10<-function(x){
  y<-x
}

rfin<-function(x){
  x<-key_collision_merge(as.character(x))
  x<-n_gram_merge(as.character(x), numgram=2)
  x<-key_collision_merge(as.character(x))
  x<-n_gram_merge(as.character(x), numgram=2)
  x<-key_collision_merge(as.character(x))
  x<-n_gram_merge(as.character(x), numgram=2)
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
  y$id<-as.numeric(as.character((str_replace_all(y$id, "[^[:digit:]]", ""))))
  y<-y[order(y$id),]
  y$id<-paste(source, y$id, sep="")
  return(y)
}
viafbuscar<-function(x){
  y<-map(viaf_search(x), function(z) pluck(z$text[z$name_type=="Corporate Names"],1))
  z<-map(y,function(y) pluck(y$a, 1))
  z[sapply(z, is.null)] <- NA
  y<-unlist(z)
  return(y)}
hunsorto<-function(x){
  y<-hunspell_suggest(x)
  z<-map(y, function(y) pluck(y,1))
  z[sapply(z, is.null)] <- NA
  x<-unlist(z)
  return(x)
}
mode1<- function(x) {
  ux <- unique(x)
  ux<-ux[which(!is.na(ux))]
  ux[which.max(tabulate(match(x, ux)))]
}
mode2<-function(x) {
  ux <- unique(x)
  ux<-ux[which(!is.na(ux))]
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}
makevector<-function(lista, n){
  x<-NA
  for(i in 1:length(lista)){
    x[i]<-lista[[i]][n]
  }
  return(x)
}
####II. Extraction. ####
####II.MMYB####
####II.2. capital####
mmyb<-rbind(read.csv("mmybe3.csv"), read.csv("mmybe4.csv"))
mmyb<-mmyb[which(!is.na(mmyb$text)),]
mmyb<-mmyb[which(nchar(mmyb$text)>5),]
mmyb<-subset(mmyb, select=c("id", "year", "text", "company"))
mmyb$text<-trimws(str_replace_all(mmyb$text,  "[^[:alnum:].,$&?;:|-]", " "))
mmyb$text<-trimws(str_replace_all(mmyb$text,  "(\\s+)", " "))
mmyb$text<-trimws(str_replace_all(mmyb$text,  "^$", "NA"))
mmyb$text<-trimws(str_replace_all(mmyb$text,  "^(\\s)$", "NA"))
mmyb$text<-trimws(str_replace_all(mmyb$text,  "0riginu1ly", "originally"))
mmyb$text1<-mmyb$text
mmyb$text1<-trimws(str_replace_all(mmyb$text1,  "(?<=\\d),", ""))
mmyb$text1<-str_replace_all(mmyb$text1,  "apital is 00", "apital is 60")
mmyb$text1<-str_replace_all(mmyb$text1,  "apital 00", "apital is 60")
mmyb$text1<-repetir(mmyb$text1, trimdigi, 2)
mmyb$text1<-trimws(str_replace_all(mmyb$text1,  "(\\s)(\\d+)(?=[s])", ""))
mmyb$text1<-trimws(str_replace_all(mmyb$text1,  "(?<=\\d)(\\s+),", ""))
mmyb$text1<-trimws(str_replace_all(mmyb$text1,  ",(\\s+)(?=\\d)", ""))
mmyb$text1<-trimws(str_replace_all(mmyb$text1,  "(?<=\\d)(\\s+)(?=\\d)", ""))
mmyb$text1<-trimws(str_replace_all(mmyb$text1,  "(?<=\\d),(?=\\d)", ""))
mmyb$text1<-trimws(str_replace_all(mmyb$text1,  "(?<=\\d)[;.:](?=\\d)", ""))
mmyb$text1<-trimws(str_replace_all(mmyb$text1,  "[$](\\s+)(?=\\d)", "[$]"))
mmyb$text1<-trimws(str_replace_all(mmyb$text1,  "?(\\s+)(?=\\d)", "?"))
mmyb$text1<-trimws(str_replace_all(mmyb$text1,  "(?=\\d+)yen", "(\\s)yen"))
mmyb$text1<-trimws(str_replace_all(mmyb$text1,  "(?=\\d+)fr", "(\\s)fr"))
mmyb$text1<-trimws(str_replace_all(mmyb$text1,  "(\\s+)", " "))
mmyb$capitalusd<-ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)is(\\s)[$](\\d+)")),
                        str_extract(mmyb$text1, "capital(\\s)is(\\s)[$](\\d+)"),
                        ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)was(\\s)[$](\\d+)")),
                               str_extract(mmyb$text1, "capital(\\s)was(\\s)[$](\\d+)"),
                               ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)was(\\s)originally(\\s)[$](\\d+)")),
                                      str_extract(mmyb$text1, "capital(\\s)was(\\s)originally(\\s)[$](\\d+)"),
                                      ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)of(\\s)[$](\\d+)")),
                                             str_extract(mmyb$text1, "capital(\\s)of(\\s)[$](\\d+)"),
                                             ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)to(\\s)[$](\\d+)")),
                                                    str_extract(mmyb$text1, "capital(\\s)to(\\s)[$](\\d+)"),
                                                    ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)[$](\\d+)")),
                                                           str_extract(mmyb$text1, "capital(\\s)[$](\\d+)"),
                                                           ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)is(\\s)[$](\\d+)")),
                                                                  str_extract(mmyb$text1, "Capital(\\s)is(\\s)[$](\\d+)"),
                                                                  ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)was(\\s)[$](\\d+)")),
                                                                         str_extract(mmyb$text1, "Capital(\\s)was(\\s)[$](\\d+)"),
                                                                         ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)was(\\s)originally(\\s)[$](\\d+)")),
                                                                                str_extract(mmyb$text1, "Capital(\\s)was(\\s)[$](\\d+)"),
                                                                                ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)of(\\s)[$](\\d+)")),
                                                                                       str_extract(mmyb$text1, "Capital(\\s)of(\\s)[$](\\d+)"),
                                                                                       ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)to(\\s)[$](\\d+)")),
                                                                                              str_extract(mmyb$text1, "[$](\\d+)"),
                                                                                              ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)[$](\\d+)")),
                                                                                                     str_extract(mmyb$text1, "Capital(\\s)[$](\\d+)"),   
                                                                                                     NA))))))))))))
mmyb$capitalyen<-ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)is(\\s)(\\d+)(\\s)yen")),
                        str_extract(mmyb$text1, "(\\d+)(\\s)yen"),
                        ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)was(\\s)(\\d+)(\\s)yen")),
                               str_extract(mmyb$text1, "(\\d+)(\\s)yen"),
                               ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)was(\\s)originally(\\s)(\\d+)(\\s)yen")),
                                      str_extract(mmyb$text1, "(\\d+)(\\s)yen"),
                                      ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)to(\\s)(\\d+)(\\s)yen")),
                                             str_extract(mmyb$text1, "(\\d+)(\\s)yen"),
                                             ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)of(\\s)(\\d+)(\\s)yen")),
                                                    str_extract(mmyb$text1, "(\\d+)(\\s)yen"),
                                                    ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)(\\d+)(\\s)yen")),
                                                           str_extract(mmyb$text1, "(\\d+)(\\s)yen"),
                                                           ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)(\\d+)(\\s)yen")),
                                                                  str_extract(mmyb$text1, "(\\d+)(\\s)yen"),
                                                                  ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)is(\\s)(\\d+)(\\s)yen")),
                                                                         str_extract(mmyb$text1, "(\\d+)(\\s)yen"),
                                                                         ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)was(\\s)(\\d+)(\\s)yen")),
                                                                                str_extract(mmyb$text1, "(\\d+)(\\s)yen"),
                                                                                ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)was(\\s)originally(\\d+)(\\s)yen")),
                                                                                       str_extract(mmyb$text1, "(\\d+)(\\s)yen"),
                                                                                       ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)to(\\s)(\\d+)(\\s)yen")),
                                                                                              str_extract(mmyb$text1, "(\\d+)(\\s)yen"),
                                                                                              ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)of(\\s)(\\d+)(\\s)yen")),
                                                                                                     str_extract(mmyb$text1, "(\\d+)(\\s)yen"),
                                                                                                     ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)(\\d+)(\\s)yen")),
                                                                                                            str_extract(mmyb$text1, "(\\d+)(\\s)yen"), 
                                                                                                            NA)))))))))))))
mmyb$capitalfr<-ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)is(\\s)(\\d+)(\\s)fr")),
                       str_extract(mmyb$text1, "(\\d+)(\\s)fr"),
                       ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)was(\\s)(\\d+)(\\s)fr")),
                              str_extract(mmyb$text1, "(\\d+)(\\s)fr"),
                              ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)was(\\s)originally(\\s)(\\d+)(\\s)fr")),
                                     str_extract(mmyb$text1, "(\\d+)(\\s)fr"),
                                     ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)to(\\s)(\\d+)(\\s)fr")),
                                            str_extract(mmyb$text1, "(\\d+)(\\s)fr"),
                                            ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)of(\\s)(\\d+)(\\s)fr")),
                                                   str_extract(mmyb$text1, "(\\d+)(\\s)fr"),
                                                   ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)(\\d+)(\\s)fr")),
                                                          str_extract(mmyb$text1, "(\\d+)(\\s)fr"),
                                                          ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)(\\d+)(\\s)fr")),
                                                                 str_extract(mmyb$text1, "(\\d+)(\\s)fr"),
                                                                 ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)is(\\s)(\\d+)(\\s)fr")),
                                                                        str_extract(mmyb$text1, "(\\d+)(\\s)fr"),
                                                                        ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)was(\\s)(\\d+)(\\s)fr")),
                                                                               str_extract(mmyb$text1, "(\\d+)(\\s)fr"),
                                                                               ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)was(\\s)originally(\\d+)(\\s)fr")),
                                                                                      str_extract(mmyb$text1, "(\\d+)(\\s)fr"),
                                                                                      ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)to(\\s)(\\d+)(\\s)fr")),
                                                                                             str_extract(mmyb$text1, "(\\d+)(\\s)fr"),
                                                                                             ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)of(\\s)(\\d+)(\\s)fr")),
                                                                                                    str_extract(mmyb$text1, "(\\d+)(\\s)fr"),
                                                                                                    ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)(\\d+)(\\s)fr")),
                                                                                                           str_extract(mmyb$text1, "(\\d+)(\\s)fr"), 
                                                                                                           NA)))))))))))))
mmyb$capitalboliv<-ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)is(\\s)(\\d+)(\\s)boliv")),
                          str_extract(mmyb$text1, "(\\d+)(\\s)boliv"),
                          ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)was(\\s)(\\d+)(\\s)boliv")),
                                 str_extract(mmyb$text1, "(\\d+)(\\s)boliv"),
                                 ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)was(\\s)originally(\\s)(\\d+)(\\s)boliv")),
                                        str_extract(mmyb$text1, "(\\d+)(\\s)boliv"),
                                        ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)to(\\s)(\\d+)(\\s)boliv")),
                                               str_extract(mmyb$text1, "(\\d+)(\\s)boliv"),
                                               ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)of(\\s)(\\d+)(\\s)boliv")),
                                                      str_extract(mmyb$text1, "(\\d+)(\\s)boliv"),
                                                      ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)(\\d+)(\\s)boliv")),
                                                             str_extract(mmyb$text1, "(\\d+)(\\s)boliv"),
                                                             ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)(\\d+)(\\s)boliv")),
                                                                    str_extract(mmyb$text1, "(\\d+)(\\s)boliv"),
                                                                    ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)is(\\s)(\\d+)(\\s)boliv")),
                                                                           str_extract(mmyb$text1, "(\\d+)(\\s)boliv"),
                                                                           ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)was(\\s)(\\d+)(\\s)boliv")),
                                                                                  str_extract(mmyb$text1, "(\\d+)(\\s)boliv"),
                                                                                  ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)was(\\s)originally(\\d+)(\\s)boliv")),
                                                                                         str_extract(mmyb$text1, "(\\d+)(\\s)boliv"),
                                                                                         ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)to(\\s)(\\d+)(\\s)boliv")),
                                                                                                str_extract(mmyb$text1, "(\\d+)(\\s)boliv"),
                                                                                                ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)of(\\s)(\\d+)(\\s)boliv")),
                                                                                                       str_extract(mmyb$text1, "(\\d+)(\\s)boliv"),
                                                                                                       ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)(\\d+)(\\s)boliv")),
                                                                                                              str_extract(mmyb$text1, "(\\d+)(\\s)boliv"), 
                                                                                                              NA)))))))))))))
mmyb$capitalpeso<-ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)is(\\s)(\\d+)(\\s)peso")),
                         str_extract(mmyb$text1, "(\\d+)(\\s)peso"),
                         ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)was(\\s)(\\d+)(\\s)peso")),
                                str_extract(mmyb$text1, "(\\d+)(\\s)peso"),
                                ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)was(\\s)originally(\\s)(\\d+)(\\s)peso")),
                                       str_extract(mmyb$text1, "(\\d+)(\\s)peso"),
                                       ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)to(\\s)(\\d+)(\\s)peso")),
                                              str_extract(mmyb$text1, "(\\d+)(\\s)peso"),
                                              ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)of(\\s)(\\d+)(\\s)peso")),
                                                     str_extract(mmyb$text1, "(\\d+)(\\s)peso"),
                                                     ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)(\\d+)(\\s)peso")),
                                                            str_extract(mmyb$text1, "(\\d+)(\\s)peso"),
                                                            ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)(\\d+)(\\s)peso")),
                                                                   str_extract(mmyb$text1, "(\\d+)(\\s)peso"),
                                                                   ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)is(\\s)(\\d+)(\\s)peso")),
                                                                          str_extract(mmyb$text1, "(\\d+)(\\s)peso"),
                                                                          ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)was(\\s)(\\d+)(\\s)peso")),
                                                                                 str_extract(mmyb$text1, "(\\d+)(\\s)peso"),
                                                                                 ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)was(\\s)originally(\\d+)(\\s)peso")),
                                                                                        str_extract(mmyb$text1, "(\\d+)(\\s)peso"),
                                                                                        ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)to(\\s)(\\d+)(\\s)peso")),
                                                                                               str_extract(mmyb$text1, "(\\d+)(\\s)peso"),
                                                                                               ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)of(\\s)(\\d+)(\\s)peso")),
                                                                                                      str_extract(mmyb$text1, "(\\d+)(\\s)peso"),
                                                                                                      ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)(\\d+)(\\s)peso")),
                                                                                                             str_extract(mmyb$text1, "(\\d+)(\\s)peso"), 
                                                                                                             NA)))))))))))))
mmyb$capitalpound<-ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)is(\\s)?(\\d+)")),
                          str_extract(mmyb$text1, "capital(\\s)is(\\s)?(\\d+)"),
                          ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)was(\\s)?(\\d+)")),
                                 str_extract(mmyb$text1, "capital(\\s)was(\\s)?(\\d+)"),
                                 ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)was(\\s)originally(\\s)?(\\d+)")),
                                        str_extract(mmyb$text1, "capital(\\s)was(\\s)originally(\\s)?(\\d+)"),
                                        ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)to(\\s)?(\\d+)")),
                                               str_extract(mmyb$text1, "capital(\\s)to(\\s)?(\\d+)"),
                                               ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)of(\\s)?(\\d+)")),
                                                      str_extract(mmyb$text1, "capital(\\s)of(\\s)?(\\d+)"),
                                                      ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)?(\\d+)")),
                                                             str_extract(mmyb$text1, "capital(\\s)?(\\d+)"),
                                                             ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)is(\\s)?(\\d+)")),
                                                                    str_extract(mmyb$text1, "Capital(\\s)is(\\s)?(\\d+)"),
                                                                    ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)was(\\s)?(\\d+)")),
                                                                           str_extract(mmyb$text1, "Capital(\\s)was(\\s)?(\\d+)"),
                                                                           ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)was(\\s)originally(\\s)?(\\d+)")),
                                                                                  str_extract(mmyb$text1, "Capital(\\s)was(\\s)originally(\\s)?(\\d+)"),
                                                                                  ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)to(\\s)?(\\d+)")),
                                                                                         str_extract(mmyb$text1, "Capital(\\s)to(\\s)?(\\d+)"),
                                                                                         ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)of(\\s)?(\\d+)")),
                                                                                                str_extract(mmyb$text1, "Capital(\\s)of(\\s)?(\\d+)"),
                                                                                                ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)was(\\s)originally(\\s)?(\\d+)")),
                                                                                                       str_extract(mmyb$text1, "Capital(\\s)was(\\s)?(\\d+)"),
                                                                                                       ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)of(\\s)?(\\d+)")),
                                                                                                              str_extract(mmyb$text1, "Capital(\\s)of(\\s)?(\\d+)"),
                                                                                                              ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)?(\\d+)")),
                                                                                                                     str_extract(mmyb$text1, "Capital(\\s)?(\\d+)"),
                                                                                                                     ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)is(\\s)(\\d+)")),
                                                                                                                            str_extract(mmyb$text1, "capital(\\s)is(\\s)(\\d+)"),
                                                                                                                            ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)was(\\s)(\\d+)")),
                                                                                                                                   str_extract(mmyb$text1, "capital(\\s)was(\\s)(\\d+)"),
                                                                                                                                   ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)was(\\s)originally(\\s)(\\d+)")),
                                                                                                                                          str_extract(mmyb$text1, "capital(\\s)was(\\s)originally(\\s)(\\d+)"),
                                                                                                                                          ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)to(\\s)(\\d+)")),
                                                                                                                                                 str_extract(mmyb$text1, "capital(\\s)to(\\s)(\\d+)"),
                                                                                                                                                 ifelse(!is.na(str_extract(mmyb$text1, "capital(\\s)of(\\s)(\\d+)")),
                                                                                                                                                        str_extract(mmyb$text1, "capital(\\s)of(\\s)(\\d+)"),
                                                                                                                                                        ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)is(\\s)(\\d+)")),
                                                                                                                                                               str_extract(mmyb$text1, "Capital(\\s)is(\\s)(\\d+)"),
                                                                                                                                                               ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)was(\\s)(\\d+)")),
                                                                                                                                                                      str_extract(mmyb$text1, "Capital(\\s)was(\\s)(\\d+)"),
                                                                                                                                                                      ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)to(\\s)(\\d+)")),
                                                                                                                                                                             str_extract(mmyb$text1, "Capital(\\s)to(\\s)(\\d+)"),
                                                                                                                                                                             ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)was(\\s)originally(\\s)(\\d+)")),
                                                                                                                                                                                    str_extract(mmyb$text1, "Capital(\\s)was(\\s)originally(\\s)(\\d+)"),
                                                                                                                                                                                    ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)of(\\s)(\\d+)")),
                                                                                                                                                                                           str_extract(mmyb$text1, "Capital(\\s)of(\\s)(\\d+)"),
                                                                                                                                                                                           ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)(\\d+)")),
                                                                                                                                                                                                  str_extract(mmyb$text1, "Capital(\\s)(\\d+)"),
                                                                                                                                                                                                  ifelse(!is.na(str_extract(mmyb$text1, "Capital(\\s)(\\d+)")),
                                                                                                                                                                                                         str_extract(mmyb$text1, "Capital(\\s)(\\d+)"),
                                                                                                                                                                                                         NA))))))))))))))))))))))))))
mmyb$capitalusd<-trimws(str_replace_all(mmyb$capitalusd,"[$]0", "[$]6"))
mmyb$capitalyen<-as.numeric(trimws(str_replace_all(mmyb$capitalyen,"^0", "6")))
mmyb$capitalfr<-as.numeric(trimws(str_replace_all(mmyb$capitalfr,"^0", "6")))
mmyb$capitalboliv<-as.numeric(trimws(str_replace_all(mmyb$capitalboliv,"^0", "6")))
mmyb$capitalpeso<-as.numeric(trimws(str_replace_all(mmyb$capitalpeso,"^0", "6")))
mmyb$capitalpound<-trimws(str_replace_all(mmyb$capitalpound,"?0", "?6"))
mmyb$capitalusd<-as.numeric(trimws(str_replace_all(mmyb$capitalusd,"[^[:digit:]]", "")))
mmyb$capitalyen<-as.numeric(trimws(str_replace_all(mmyb$capitalyen,"[^[:digit:]]", ""))) 
mmyb$capitalfr<-as.numeric(trimws(str_replace_all(mmyb$capitalfr,"[^[:digit:]]", ""))) 
mmyb$capitalboliv<-as.numeric(trimws(str_replace_all(mmyb$capitalboliv,"[^[:digit:]]", ""))) 
mmyb$capitalpeso<-as.numeric(trimws(str_replace_all(mmyb$capitalpeso,"[^[:digit:]]", ""))) 
mmyb$capitalpound<-as.numeric(trimws(str_replace_all(mmyb$capitalpound,"[^[:digit:]]", "")))  
exchange<-read.csv("exchange.csv")
mmyb<-merge(mmyb, exchange, by="year")
mmyb$capital<-ifelse(!is.na(mmyb$capitalusd), (mmyb$capitalusd), 
                     ifelse(!is.na(mmyb$capitalyen), (mmyb$capitalyen*mmyb$yentousd), 
                            ifelse(!is.na(mmyb$capitalboliv), (mmyb$capitalboliv*mmyb$bolivusd),
                                   ifelse(!is.na(mmyb$capitalfr), (mmyb$capitalfr*mmyb$frusd), 
                                          ifelse(!is.na(mmyb$capitalpeso), (mmyb$capitalpeso*mmyb$mxtousd),
                                                 ifelse(!is.na(mmyb$capitalpound), (mmyb$capitalpound*mmyb$poundtousd), 
                                                        NA))))))
write.csv(mmyb[which(mmyb$id<700000),], "mmybf1.csv")
write.csv(mmyb[which(mmyb$id>700000),], "mmybf2.csv")
#repetir modificando errores. 

####II.3. Location####
####II.3.1 Incorporated#### 
mmyb<-rbind(read.csv("mmybf1.csv"), read.csv("mmybf2.csv"))
mmyb<-mmyb[which(!is.na(mmyb$text)),]
mmyb<-mmyb[which(nchar(mmyb$text)>5),]
mmyb<-subset(mmyb, select=c("id", "year","company", "text", "capital"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(?<=\\d)[-]", ""))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "[-](?=\\d)", ""))
mmyb$text2<-trimws(str_replace_all(mmyb$text,  "(\\s)1(\\s)", "(\\s)1"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1S0", "(\\s)190"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1S1", "(\\s)191"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1S2", "(\\s)192"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1S", "(\\s)18"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1P0", "(\\s)190"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1P1", "(\\s)191"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1P2", "(\\s)192"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1P", "(\\s)18"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1C0", "(\\s)190"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1C1", "(\\s)191"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1C2", "(\\s)192"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1C", "(\\s)18"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1U0", "(\\s)190"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1U1", "(\\s)191"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1U2", "(\\s)192"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1U", "(\\s)18"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1H0", "(\\s)190"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1H1", "(\\s)191"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1H2", "(\\s)192"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1H", "(\\s)18"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1?0", "(\\s)190"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1?1", "(\\s)191"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1?2", "(\\s)192"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1?", "(\\s)18"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1W0", "(\\s)190"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1W1", "(\\s)191"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1W2", "(\\s)192"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1W", "(\\s)18"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1M0", "(\\s)190"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1M1", "(\\s)191"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1M2", "(\\s)192"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1M", "(\\s)18"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1e0", "(\\s)190"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1e1", "(\\s)191"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1e2", "(\\s)192"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1e", "(\\s)18"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(\\s)1-", "(\\s)1"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(?<=\\d)O", "0"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "O(?=\\d)", "0"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "U(?=\\d)", "0"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(?<=\\d)U", "0"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(?<=\\d)S", "8"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "S(?=\\d)", "8"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(?<=\\d)r", "7"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "r(?=\\d)", "7"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(?<=\\d)C", "0"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "C(?=\\d)", "0"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(?<=\\d)u", "0"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "u(?=\\d)", "0"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "l(?=\\d)", "1"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(?<=\\d)l", "1"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "J(?=\\d)", "1"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(?<=\\d)J", "1"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "I(?=\\d)", "1"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(?<=\\d)I", "1"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "y(?=\\d)", "7"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(?<=\\d)y", "7"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "f(?=\\d)", "7"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(?<=\\d)f", "7"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "G(?=\\d)", "6"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(?<=\\d)G", "6"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "o(?=\\d)", "0"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(?<=\\d)o", "0"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "i(?=\\d)", "1"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(?<=\\d)i", "1"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "s(?=\\d)", "5"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(?<=\\d)s", "5"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "A(?=\\d)", "5"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(?<=\\d)A", "5"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "B(?=\\d)", "8"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(?<=\\d)B", "8"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "H(?=\\d)", "8"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(?<=\\d)H", "8"))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(?<=\\d)[;.:](?=\\d)", ""))
mmyb$text2<-trimws(str_replace_all(mmyb$text2,  "(?<=\\d)(\\s)(?=\\d)", ""))
mmyb$registered<-trimws(
  ifelse(!is.na(str_extract(mmyb$text2, "registered(\\s)on(\\s)(\\w+)(\\s)(\\d+)th,(\\s)(\\d+)")),
         str_extract(mmyb$text2, "on(\\s)(\\w+)(\\s)(\\d+)th,(\\s)(\\d+)"),
         ifelse(!is.na(str_extract(mmyb$text2, "registered(\\s)(\\w+)(\\s)(\\d+)th,(\\s)(\\d+)")),
                str_extract(mmyb$text2, "(\\w+)(\\s)(\\d+)th,(\\s)(\\d+)"),
                ifelse(!is.na(str_extract(mmyb$text2, "registered(\\s)on(\\s)(\\w+)(\\s)(\\d+)st,(\\s)(\\d+)")),
                       str_extract(mmyb$text2, "on(\\s)(\\w+)(\\s)(\\d+)st,(\\s)(\\d+)"),
                       ifelse(!is.na(str_extract(mmyb$text2, "registered(\\s)(\\w+)(\\s)(\\d+)st,(\\s)(\\d+)")),
                              str_extract(mmyb$text2, "(\\w+)(\\s)(\\d+)st,(\\s)(\\d+)"),
                              ifelse(!is.na(str_extract(mmyb$text2, "registered(\\s)on(\\s)(\\w+)(\\s)(\\d+)nd,(\\s)(\\d+)")),
                                     str_extract(mmyb$text2, "on(\\s)(\\w+)(\\s)(\\d+)nd,(\\s)(\\d+)"),
                                     ifelse(!is.na(str_extract(mmyb$text2, "registered(\\s)(\\w+)(\\s)(\\d+)nd(\\s)(\\d+)")),
                                            str_extract(mmyb$text2, "(\\w+)(\\s)(\\d+)nd,(\\s)(\\d+)"),
                                            ifelse(!is.na(str_extract(mmyb$text2, "registered(\\s)on(\\s)(\\w+)(\\s)(\\d+)th(\\s)(\\d+)")),
                                                   str_extract(mmyb$text2, "on(\\s)(\\w+)(\\s)(\\d+)th(\\s)(\\d+)"),
                                                   ifelse(!is.na(str_extract(mmyb$text2, "registered(\\s)(\\w+)(\\s)(\\d+)th(\\s)(\\d+)")),
                                                          str_extract(mmyb$text2, "(\\w+)(\\s)(\\d+)th(\\s)(\\d+)"),
                                                          ifelse(!is.na(str_extract(mmyb$text2, "registered(\\s)on(\\s)(\\w+)(\\s)(\\d+)st(\\s)(\\d+)")),
                                                                 str_extract(mmyb$text2, "on(\\s)(\\w+)(\\s)(\\d+)st(\\s)(\\d+)"),
                                                                 ifelse(!is.na(str_extract(mmyb$text2, "registered(\\s)(\\w+)(\\s)(\\d+)st(\\s)(\\d+)")),
                                                                        str_extract(mmyb$text2, "(\\w+)(\\s)(\\d+)st(\\s)(\\d+)"),
                                                                        ifelse(!is.na(str_extract(mmyb$text2, "registered(\\s)on(\\s)(\\w+)(\\s)(\\d+)nd(\\s)(\\d+)")),
                                                                               str_extract(mmyb$text2, "on(\\s)(\\w+)(\\s)(\\d+)nd(\\s)(\\d+)"),
                                                                               ifelse(!is.na(str_extract(mmyb$text2, "registered(\\s)(\\w+)(\\s)(\\d+)nd(\\s)(\\d+)")),
                                                                                      str_extract(mmyb$text2, "(\\w+)(\\s)(\\d+)nd(\\s)(\\d+)"),
                                                                                      ifelse(!is.na(str_extract(mmyb$text2, "registered(\\s)on(\\s)the(\\s)(\\d+)th(\\s)(\\w+),(\\s)(\\d+)")),
                                                                                             str_extract(mmyb$text2, "on(\\s)the(\\s)(\\d+)th(\\s)(\\w+),(\\s)(\\d+)"),
                                                                                             ifelse(!is.na(str_extract(mmyb$text2, "registered(\\s)the(\\s)(\\d+)th(\\s)(\\w+),(\\s)(\\d+)")),
                                                                                                    str_extract(mmyb$text2, "the(\\s)(\\d+)th(\\s)(\\w+),(\\s)(\\d+)"),
                                                                                                    ifelse(!is.na(str_extract(mmyb$text2, "registered(\\s)on(\\s)the(\\s)(\\d+)st(\\s)(\\w+),(\\s)(\\d+)")),
                                                                                                           str_extract(mmyb$text2, "on(\\s)the(\\s)(\\d+)st(\\s)(\\w+),(\\s)(\\d+)"),
                                                                                                           ifelse(!is.na(str_extract(mmyb$text2, "registered(\\s)the(\\s)(\\d+)st(\\w+)(\\s),(\\s)(\\d+)")),
                                                                                                                  str_extract(mmyb$text2, "the(\\s)(\\d+)st(\\w+)(\\s),(\\s)(\\d+)"),
                                                                                                                  ifelse(!is.na(str_extract(mmyb$text2, "registered(\\s)on(\\s)the(\\s)(\\d+)nd(\\s)(\\w+),(\\s)(\\d+)")),
                                                                                                                         str_extract(mmyb$text2, "on(\\s)the(\\s)(\\d+)nd(\\s)(\\w+),(\\s)(\\d+)"),
                                                                                                                         ifelse(!is.na(str_extract(mmyb$text2, "registered(\\s)the(\\s)(\\d+)nd(\\s)(\\w+)(\\s)(\\d+)")),
                                                                                                                                str_extract(mmyb$text2, "the(\\s)(\\d+)nd(\\s)(\\w+),(\\s)(\\d+)"),
                                                                                                                                ifelse(!is.na(str_extract(mmyb$text2, "registered(\\s)on(\\s)the(\\s)(\\d+)th(\\s)(\\w+)(\\s)(\\d+)")),
                                                                                                                                       str_extract(mmyb$text2, "on(\\s)the(\\s)(\\d+)th(\\s)(\\w+)(\\s)(\\d+)"),
                                                                                                                                       ifelse(!is.na(str_extract(mmyb$text2, "registered(\\s)the(\\s)(\\d+)th(\\s)(\\w+)(\\s)(\\d+)")),
                                                                                                                                              str_extract(mmyb$text2, "the(\\s)(\\d+)th(\\s)(\\w+)(\\s)(\\d+)"),
                                                                                                                                              ifelse(!is.na(str_extract(mmyb$text2, "registered(\\s)on(\\s)the(\\s)(\\d+)st(\\s)(\\w+)(\\s)(\\d+)")),
                                                                                                                                                     str_extract(mmyb$text2, "on(\\s)the(\\s)(\\d+)st(\\s)(\\w+)(\\s)(\\d+)"),
                                                                                                                                                     ifelse(!is.na(str_extract(mmyb$text2, "registered(\\s)the(\\s)(\\d+)st(\\w+)(\\s)(\\s)(\\d+)")),
                                                                                                                                                            str_extract(mmyb$text2, "the(\\s)(\\d+)st(\\w+)(\\s)(\\s)(\\d+)"),
                                                                                                                                                            ifelse(!is.na(str_extract(mmyb$text2, "registered(\\s)on(\\s)the(\\s)(\\d+)nd(\\s)(\\w+)(\\s)(\\d+)")),
                                                                                                                                                                   str_extract(mmyb$text2, "on(\\s)the(\\s)(\\d+)nd(\\s)(\\w+)(\\s)(\\d+)"),
                                                                                                                                                                   ifelse(!is.na(str_extract(mmyb$text2, "registered(\\s)the(\\s)(\\d+)nd(\\s)(\\w+)(\\s)(\\d+)")),
                                                                                                                                                                          str_extract(mmyb$text2, "the(\\s)(\\d+)nd(\\s)(\\w+)(\\s)(\\d+)"),
                                                                                                                                                                          NA)))))))))))))))))))))))), 
  which=c("both"))
mmyb$yearg<-trimws(str_extract(mmyb$registered, "(\\d+)$"), which=c("both"))
mmyb$yearg<-as.numeric(as.character(mmyb$yearg))
write.csv(mmyb[which(mmyb$id<700000),], "mmybg1.csv")
write.csv(mmyb[which(mmyb$id>700000),], "mmybg2.csv")
##after this, still the registered data has around 2% of clear mistakes
####II.3.2 Offices#### 
mmyb<-rbind(read.csv("mmybg1.csv"), read.csv("mmybg2.csv"))
mmyb<-subset(mmyb, select=c("id", "year","yearg", "company", "capital", "text"))
mmyb$textemp<-tolower(mmyb$text)
mmyb$textemp<-str_replace_all(mmyb$textemp, "e[.]c", "london")
mmyb$textemp<-str_replace_all(mmyb$textemp, "e[.]o[.]", "london")
mmyb$textemp<-str_replace_all(mmyb$textemp, "[,]", " ")
mmyb$textemp<-str_replace_all(mmyb$textemp, "[[:digit:]]", " ")
mmyb$textemp<-str_replace_all(mmyb$textemp, "(\\s+)", " ")
street<-c("street", "road", "square", "avenue", "plaza", "viaduct", "terrace", "lane")
calle1<-as.vector(paste0("(\\w+)(\\s)", street, "(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)"))
calle2<-as.vector(paste0("(\\w+)(\\s)", street, "(\\s)(\\w+)(\\s)(\\w+)"))
calle3<-as.vector(paste0("(\\w+)(\\s)", street, "(\\s)(\\w+)"))
calle3<-as.vector(paste0("(\\w+)(\\s)", street, "(\\s)(\\w+)"))
calle<-c(calle1, calle2, calle3)
calle<-paste(calle, collapse="|")
mmyb$calle<-str_extract(mmyb$textemp, calle)
mmyb$office<-ifelse(!is.na(str_extract(lag(mmyb$textemp,1), "offices$")), 
                    mmyb$textemp,mmyb$calle)
length(unique(mmyb$office))
mmyb<-subset(mmyb, select=c("id", "year","yearg", "company", "capital", "text", "office"))
write.csv(mmyb[which(mmyb$id<450000),], "mmybh1.csv")
write.csv(mmyb[which((mmyb$id>450000)&mmyb$id<900000),], "mmybh2.csv")
write.csv(mmyb[which(mmyb$id>900000),], "mmybh3.csv")
####II.3.3 situated####
mmyb<-rbind(read.csv("mmybh1.csv"), read.csv("mmybh2.csv"), read.csv("mmybh3.csv"))
mmyb<-subset(mmyb, select=c("id", "year","yearg", "company", "capital", "text", "office"))
mmyb$textemp<-tolower(mmyb$text)
mmyb$textemp<-str_replace_all(mmyb$textemp, "e[.]c", "london")
mmyb$textemp<-str_replace_all(mmyb$textemp, "e[.]o[.]", "london")
mmyb$textemp<-str_replace_all(mmyb$textemp, "square miles", " ")
mmyb$textemp<-str_replace_all(mmyb$textemp, "(\\\s+)", " ")
mmyb$situated<-trimws(
  ifelse(!is.na(str_extract(mmyb$textemp, "situated(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)")),
         str_extract(mmyb$textemp, "situated(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)"),
         ifelse(!is.na(str_extract(mmyb$textemp, "situated(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)")),
                str_extract(mmyb$textemp, "situated(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)"),
                ifelse(!is.na(str_extract(mmyb$textemp, "situated(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)")),
                       str_extract(mmyb$textemp, "situated(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)"),
                       ifelse(!is.na(str_extract(mmyb$textemp, "situated(\\s)(\\w+)(\\s)(\\w+)")),
                              str_extract(mmyb$textemp, "situated(\\s)(\\w+)(\\s)(\\w+)"),
                              NA)))), which=c("both"))
mmyb$situated<-str_replace_all(mmyb$situated, "situated(\\s)in", "")
mmyb$situated<-str_replace_all(mmyb$situated, "situated(\\s)at", "")
mmyb$situated<-str_replace_all(mmyb$situated, "situated(\\s)on", "")
mmyb$situated<-str_replace_all(mmyb$situated, "situated(\\s)about", "")
mmyb$situated<-str_replace_all(mmyb$situated, "situated", "")
mmyb$situated<-trimws(mmyb$situated)
length(unique(mmyb$situated))
mmyb<-subset(mmyb, select=c("id", "year","yearg", "company", "capital", "text", "office", "situated"))
write.csv(mmyb[which(mmyb$id<450000),], "mmybj1.csv")
write.csv(mmyb[which((mmyb$id>450000)&mmyb$id<900000),], "mmybj2.csv")
write.csv(mmyb[which(mmyb$id>900000),], "mmybj3.csv")
####II.3.4 Mines####
mmyb<-rbind(read.csv("mmybj1.csv"), read.csv("mmybj2.csv"), read.csv("mmybj3.csv"))
mmyb<-subset(mmyb, select=c("id", "year","yearg", "company", "capital", "text", "office", "situated"))
mmyb$textemp<-tolower(mmyb$text)
mmyb$textemp<-str_replace_all(mmyb$textemp, "e[.]c", "london")
mmyb$textemp<-str_replace_all(mmyb$textemp, "e[.]o[.]", "london")
mmyb$textemp<-str_replace_all(mmyb$textemp, "[^[:alpha:]]", " ")
mmyb$textemp<-str_replace_all(mmyb$textemp, "(\\s+)", " ")
mine<-c("mines", "smelters", "plants", "refineries", "fields", "mine", "smelter", "plant", "refinery", "field")
mina1<-as.vector(paste0(mine, "(\\s)in(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)"))
mina2<-as.vector(paste0(mine, "(\\s)in(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)"))
mina3<-as.vector(paste0(mine, "(\\s)in(\\s)(\\w+)(\\s)(\\w+)"))
mina4<-as.vector(paste0(mine, "(\\s)in(\\s)(\\w+)"))
mina1<-as.vector(paste0(mine, "(\\s)at(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)"))
mina2<-as.vector(paste0(mine, "(\\s)at(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)"))
mina3<-as.vector(paste0(mine, "(\\s)at(\\s)(\\w+)(\\s)(\\w+)"))
mina4<-as.vector(paste0(mine, "(\\s)at(\\s)(\\w+)"))
mina<-c(mina1, mina2, mina3, mina4)
mina<-paste(mina, collapse="|")
mmyb$mine<-str_extract(mmyb$textemp, mina)
length(unique(mmyb$mine))
mmyb<-subset(mmyb, select=c("id", "year","yearg", "company", "capital", "text", "office", "situated", "mine"))
write.csv(mmyb[which(mmyb$id<450000),], "mmybk1.csv")
write.csv(mmyb[which((mmyb$id>450000)&mmyb$id<900000),], "mmybk2.csv")
write.csv(mmyb[which(mmyb$id>900000),], "mmybk3.csv")
####II.3.4 locations####
mmyb<-rbind(read.csv("mmybk1.csv"), read.csv("mmybk2.csv"), read.csv("mmybk3.csv"))
mmyb<-subset(mmyb, select=c("id", "year","yearg", "company", "capital", "text", "office", "situated", "mine"))
mmyb$textemp<-tolower(mmyb$text)
mmyb$textemp<-str_replace_all(mmyb$textemp, "e[.]c", "london")
mmyb$textemp<-str_replace_all(mmyb$textemp, "e[.]o[.]", "london")
mmyb$textemp<-str_replace_all(mmyb$textemp, "[^[:alpha:]]", " ")
mmyb$textemp<-str_replace_all(mmyb$textemp, "(\\s+)", " ")
location <- read_csv("locations.csv")
location<-paste(location$place, collapse=" |")
mmyb$location<-str_extract(mmyb$textemp, location)
length(unique(mmyb$location))
mmyb<-subset(mmyb, select=c("id", "year","yearg", "company", "capital", "text", "office", "situated", "mine", "location"))
mmyb$loc<-trimws(
  ifelse(!is.na(mmyb$location), mmyb$location, 
         ifelse(!is.na(mmyb$office), mmyb$office, 
                ifelse(!is.na(mmyb$situated), mmyb$situated, 
                       ifelse(!is.na(mmyb$mine), mmyb$mine,
                              NA)))), which=c("both"))
mmyb$loc<-repetir(mmyb$loc, rfin, 100)
write.csv(mmyb[which(mmyb$id<450000),], "mmybl1.csv")
write.csv(mmyb[which((mmyb$id>450000)&mmyb$id<900000),], "mmybl2.csv")
write.csv(mmyb[which(mmyb$id>900000),], "mmybl3.csv")
####II.3.4 locs####
loclon<-read.csv("loclon.csv")
mmyb<-rbind(read.csv("mmybl4.csv"), read.csv("mmybl5.csv"), read.csv("mmybl6.csv"))
mmyb$loc<-trimws(str_replace_all(mmyb$loc,  "^$", "NA"))
mmyb$loc<-trimws(str_replace_all(mmyb$loc,  "^(\\s)$", "NA"))
mmyb<-subset(mmyb, select=c("id", "year","yearg", "company", "capital", "text","office", "situated", "mine", "loc"))
loc<-as.data.frame(unique(mmyb$loc))
loc<-as.data.frame(loc[which(!is.na(loc$`unique(mmyb$loc)`)), ])
colnames(loc)<-"loc"
loclon$loc<-trimws(loclon$loc)
loc<-unique(merge(loc, loclon, all.x=TRUE))
loc1<-loc[which(!is.na(loc$lon)), ]
loc2<-loc[which(is.na(loc$lon)), ]
loc2<-loc2[1]
lloc2<-buscar(loc2$loc)
loc2$lon<-lloc2$lon
loc2$lat<-lloc2$lat
loc<-rbind(loc1, loc2)
loc<-loc[which(!is.na(loc$lon)), ]
write.csv(loc, "loc.csv")
#after corrections
loc<-read.csv("loc.csv")
mmyb<-left_join(mmyb, loc, by="loc", match="all")
mmyb<-mmyb[order(mmyb$id), ]
mmyb<-subset(mmyb, select=c("id", "year","yearg", "company", "capital", "text", "office", "situated", "mine","loc", "lon", "lat"))
mmyb$loc<-ifelse(!is.na(mmyb$lon), mmyb$loc, NA)
write.csv(mmyb[which(mmyb$id<450000),], "mmybm1.csv")
write.csv(mmyb[which((mmyb$id>450000)&mmyb$id<900000),], "mmybm2.csv")
write.csv(mmyb[which(mmyb$id>900000),], "mmybm3.csv")
####II.3.5 properties ####
mmyb<-rbind(read.csv("mmybm1.csv"), read.csv("mmybm2.csv"), read.csv("mmybm3.csv"))
mmyb$loc<-trimws(str_replace_all(mmyb$loc,  "^$", "NA"))
mmyb$loc<-trimws(str_replace_all(mmyb$loc,  "^(\\s)$", "NA"))
mmyb<-subset(mmyb, select=c("id", "year","yearg", "company", "capital", "text", "office", "situated", "mine","loc", "lon", "lat"))
mmyb1<-mmyb[which(!is.na(mmyb$loc)), ]
mmyb2<-mmyb[which(is.na(mmyb$loc)), ]
mmyb2<-subset(mmyb2, select=c("id", "year","yearg", "company", "capital", "text", "office", "mine"))
temp<-subset(mmyb2, select=c("id", "text"))
temp$text<-tolower(temp$text)
temp$text<-str_replace_all(temp$text, "e[.]c", "london")
temp$text<-str_replace_all(temp$text, "e[.]o[.]", "london")
temp$text<-str_replace_all(temp$text, "square miles", " ")
temp$text<-str_replace_all(temp$text, "(\\s+)", " ")
temp$situated<-trimws(
  ifelse(!is.na(str_extract(temp$text, "properties(\\s)on(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)")),
         str_extract(temp$text, "properties(\\s)on(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)"),
         ifelse(!is.na(str_extract(temp$text, "properties(\\s)on(\\s)(\\w+)(\\s)(\\w+)")),
                str_extract(temp$text, "properties(\\s)on(\\s)(\\w+)(\\s)(\\w+)"),
                ifelse(!is.na(str_extract(temp$text, "properties(\\s)on(\\s)(\\w+)")),
                       str_extract(temp$text, "properties(\\s)on(\\s)(\\w+)"),
                       ifelse(!is.na(str_extract(temp$text, "properties(\\s)in(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)")),
                              str_extract(temp$text, "properties(\\s)in(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)"),
                              ifelse(!is.na(str_extract(temp$text, "properties(\\s)in(\\s)(\\w+)(\\s)(\\w+)")),
                                     str_extract(temp$text, "properties(\\s)in(\\s)(\\w+)(\\s)(\\w+)"),
                                     ifelse(!is.na(str_extract(temp$text, "properties(\\s)in(\\s)(\\w+)")),
                                            str_extract(temp$text, "properties(\\s)in(\\s)(\\w+)"),
                                            NA)))))), which=c("both"))
temp$situated<-str_replace_all(temp$situated, "properties(\\s)on", "")
temp$situated<-str_replace_all(temp$situated, "properties(\\s)in", "")
temp$situated<-str_replace_all(temp$situated, "properties", "")
temp$situated<-trimws(str_replace_all(temp$situated, "[[:digit:]]", ""))
temp$situated<-trimws(str_replace_all(temp$situated, "^$", "NA"))
temp$situated<-trimws(temp$situated, which=c("both"))
temp$loc<-temp$situated
loc<-unique((as.data.frame(unique(temp$loc))))
colnames(loc)<-"loc"
loc<-unique(merge(loc, read.csv("loc.csv"), all.x=TRUE))
write.csv(loc, "loc2.csv")
loc1<-loc[which(!is.na(loc$lon)|is.na(loc$loc)), ]
loc2<-loc[which(is.na(loc$lon)&!is.na(loc$loc)), ]
loc2<-loc2[1]
lloc2<-buscar(loc2)
loc2$lon<-lloc2$lon
loc2$lat<-lloc2$lat
loc<-rbind(loc1, loc2)
loc<-loc[which(!is.na(loc$lon)), ]
temp<-left_join(temp, loc, by="loc", match="all")
temp<-temp[order(temp$id), ]
temp<-subset(temp, select=c("id","situated", "loc", "lon", "lat" ))
mmyb2<-left_join(mmyb2, temp, by="id", match="all")
mmyb<-rbind(mmyb1, mmyb2)
mmyb<-mmyb[order(mmyb$id), ]
mmyb<-subset(mmyb, select=c("id", "year","yearg", "company", "capital", "text", "office", "situated", "mine","loc", "lon", "lat"))
mmyb$loc<-rep(mmyb$loc, mmyb$company, rellenar,100)
mmyb$lon<-rep(mmyb$lon, mmyb$company, rellenar,100)
mmyb$lat<-rep(mmyb$lat, mmyb$company, rellenar,100)
write.csv(mmyb[which(mmyb$id<450000),], "mmybn1.csv")
write.csv(mmyb[which((mmyb$id>450000)&mmyb$id<900000),], "mmybn2.csv")
write.csv(mmyb[which(mmyb$id>900000),], "mmybn3.csv")
####II. 3. 6. Repetition of the extraction of name of the company####
mmyb<-rbind(read.csv("mmybn1.csv"), read.csv("mmybn2.csv"), read.csv("mmybn3.csv"))
mmyb$X<-NULL
mmyb$text2<-ifelse(!is.na(str_extract(mmyb$text, "^(\\d)|^MI[N|X]I[N|X]|SCALE(\\b)|^ANNUAL|(\\s+)ANNUAL(\\s+)R|YEARLY|REPOR|[M|N]A[M|N]UA|BOOK|continued|^AND(\\s+)|^THE.*ING$|ADVERTIS|MADE(\\b)|DISTANCE|^THE.*AL$|PAPER(\\b)|CATALOGUE")), NA, mmyb$text)
mmyb$id2<-rownames(mmyb)
mmyb<-separate_rows(mmyb, text2, sep="(?<=(//s))[Il[|]](?=(//s))")
mmyb$text2<-str_replace_all(mmyb$text2, "(?<=[[A-Z]])0(?=[[A-Z]])", "O")
mmyb$text2<-str_replace_all(mmyb$text2, "(?<=[[A-Z]])1(?=[[A-Z]])", "I")
mmyb$text2<-str_replace_all(mmyb$text2, "(?<=[[A-Z]])2(?=[[A-Z]])", "Z")
mmyb$text2<-str_replace_all(mmyb$text2, "(?<=[[A-Z]])3(?=[[A-Z]])", "B")
mmyb$text2<-str_replace_all(mmyb$text2, "(?<=[[A-Z]])4(?=[[A-Z]])", "A")
mmyb$text2<-str_replace_all(mmyb$text2, "(?<=[[A-Z]])5(?=[[A-Z]])", "S")
mmyb$text2<-str_replace_all(mmyb$text2, "(?<=[[A-Z]])6(?=[[A-Z]])", "G")
mmyb$text2<-str_replace_all(mmyb$text2, "(?<=[[A-Z]])7(?=[[A-Z]])", "T")
mmyb$text2<-str_replace_all(mmyb$text2, "(?<=[[A-Z]])8(?=[[A-Z]])", "B")
mmyb$text2<-str_replace_all(mmyb$text2, "[^[:alpha:]]", " ")
mmyb$text2<-ifelse(!is.na(str_extract(mmyb$text2, "^(\\w+)$")), NA, mmyb$text2)
mmyb$text2<-str_replace_all(mmyb$text2, "(\\s+)", " ")
mmyb$text2<-trimws(str_replace_all(mmyb$text2, "(\\s+)$|^(\\s+)", ""))
mmyb$text2<-str_replace_all(mmyb$text2, "(?<=[A-Z]{2})[a-z]", toupper(str_extract(mmyb$text2, "(?<=[A-Z]{2})[a-z]")))
mmyb$text2<-str_replace_all(mmyb$text2, "[a-z](?=[A-Z]{2})", toupper(str_extract(mmyb$text2, "[a-z](?=[A-Z]{2})")))
mmyb$text2<-str_replace_all(mmyb$text2, "(?<=[A-Z]{2})[a-z]", toupper(str_extract(mmyb$text2, "(?<=[A-Z]{2})[a-z]")))
mmyb$text2<-str_replace_all(mmyb$text2, "[a-z](?=[A-Z]{2})", toupper(str_extract(mmyb$text2, "[a-z](?=[A-Z]{2})")))
mmyb$text2<-str_replace_all(mmyb$text2, "[^[:upper:]]", " ")
mmyb$text2<-trimws(str_replace_all(mmyb$text2, "(\\s+)", " "), which=c("both"))
for(i in 1:10){
  mmyb$text2<-str_replace_all(mmyb$text2, "(^[A-Z]{1,2}(\\s))|((\\s)[A-Z]{1,2}(\\s))|((\\s)[A-Z]{1,2}$)", " ")
  mmyb$text2<-trimws(str_replace_all(mmyb$text2, "(\\s+)", " "), which=c("both"))}
mmyb$company<-ifelse(!is.na(str_extract(mmyb$text2, "^[[A-Z]]{2,}"))&
                       !is.na(str_extract(mmyb$text2, "[[A-Z]]{2,}$"))&
                       !is.na(str_extract(mmyb$text2, "^[[A-Z]]{2,}(\\s+)[[A-Z]]{2,}")), 
                     mmyb$text2, 
                     str_extract(mmyb$text2, "[[A-Z]]{2,}(.*?)LIMITED|[[A-Z]]{2,}(.*)CORPORATION[[A-Z]]{2,}(.*)SYNDICATE|[[A-Z]]{2,}(.*)COMPANY|[[A-Z]]{2,}(.*)LTD|[[A-Z]]{2,}(.*)CORP|[[A-Z]]{2,}(.*)SYND|[[A-Z]]{2,} (.*)CO"))
mmyb$company<-ifelse(!is.na(mmyb$company)&!is.na(lead(mmyb$company)), paste(mmyb$company, lead(mmyb$company), sep=" "), 
                     ifelse(!is.na(lag(mmyb$company)), NA, mmyb$company))
mmyb$company<-str_extract(mmyb$company, "((\\w+)(\\+)(\\w+)(\\s+)(\\w+)(\\s+)(\\w+)(\\s+)(\\w+)(\\s+)(\\w+))|((\\w+)(\\s+)(\\w+)(\\s+)(\\w+)(\\s+)(\\w+)(\\s+)(\\w+))|((\\w+)(\\s+)(\\w+)(\\s+)(\\w+)(\\s+)(\\w+))|((\\w+)(\\s+)(\\w+)(\\s+)(\\w+))")

temp<-subset(mmyb, select=c("id", "year", "text","company"), !is.na(company))
temp$company<-str_replace_all(temp$company, "LTD(\\b)", "LIMITED")
temp$company<-str_replace_all(temp$company, "CORP(\\b)", "CORPORATION")
temp$company<-str_replace_all(temp$company, "SYND(\\b)", "SYNDICATE")
temp$company<-str_replace_all(temp$company, "CO$", "COMPANY")
temp$company<-str_replace_all(temp$company, "VV|VX|VW", "W")
for(i in 1:5){
  temp$company<-ifelse(((str_extract(temp$company, "^.{10}")==str_extract(lead(temp$company), "^.{10}")))&
                         (((!is.na(str_extract(lead(temp$company), "LIMITED|CORPORATION|SYNDICATE|COMPANY")))&(is.na(str_extract(temp$company, "LIMITED|CORPORATION|SYNDICATE|COMPANY"))))|
                            ((!is.na(str_extract(lead(temp$company), "LIMITED|CORPORATION|SYNDICATE|COMPANY")))&(!is.na(str_extract(temp$company, "LIMITED|CORPORATION|SYNDICATE|COMPANY")))&(nchar(lead(temp$company))>=nchar(temp$company)))|
                            ((is.na(str_extract(lead(temp$company), "LIMITED|CORPORATION|SYNDICATE|COMPANY")))&(is.na(str_extract(temp$company, "LIMITED|CORPORATION|SYNDICATE|COMPANY")))&(nchar(lead(temp$company))>=nchar(temp$company)))), 
                       lead(temp$company),
                       ifelse(((str_extract(temp$company, "^.{10}")==str_extract(lag(temp$company), "^.{10}")))&
                                (((!is.na(str_extract(lag(temp$company), "LIMITED|CORPORATION|SYNDICATE|COMPANY")))&(is.na(str_extract(temp$company, "LIMITED|CORPORATION|SYNDICATE|COMPANY"))))|
                                   ((!is.na(str_extract(lag(temp$company), "LIMITED|CORPORATION|SYNDICATE|COMPANY")))&(!is.na(str_extract(temp$company, "LIMITED|CORPORATION|SYNDICATE|COMPANY")))&(nchar(lag(temp$company))>nchar(temp$company)))|
                                   ((is.na(str_extract(lag(temp$company), "LIMITED|CORPORATION|SYNDICATE|COMPANY")))&(is.na(str_extract(temp$company, "LIMITED|CORPORATION|SYNDICATE|COMPANY")))&(nchar(lag(temp$company))>nchar(temp$company)))), 
                              lag(temp$company),
                              temp$company))
  temp$company<- ifelse(str_extract(temp$company, "^(\\w){1}")==str_extract(lag(temp$company), "^(\\w){1}")|
                          str_extract(temp$company, "^(\\w){1}")==str_extract(lead(temp$company), "^(\\w){1}")|
                          str_extract(temp$company, "^(\\w){1}")==str_extract(lag(temp$company,2), "^(\\w){1}")|
                          str_extract(temp$company, "^(\\w){1}")==str_extract(lead(temp$company,2), "^(\\w){1}"),
                        temp$company, 
                        ifelse((match(str_extract(temp$company, "^(\\w){1}"), LETTERS[1:26])-match(str_extract(lag(temp$company), "^(\\w){1}"), LETTERS[1:26])>=0)&
                                 (match(str_extract(temp$company, "^(\\w){1}"), LETTERS[1:26])-match(str_extract(lag(temp$company), "^(\\w){1}"), LETTERS[1:26])<=2), 
                               temp$company, 
                               ifelse(temp$year!=lag(temp$year), temp$company, NA)))
  temp<-subset(temp, select=c("id", "year", "text","company"), !is.na(company))}
temp$ciarfn<-temp$company
temp2<-temp[order(temp$ciarfn),]
for(i in 1:10){
  temp2$ciarfn<-ifelse(((str_extract(temp2$ciarfn, "^.{10}")==str_extract(lead(temp2$ciarfn), "^.{10}")))&
                       (temp2$year!=lead(temp2$year))&
                       (((!is.na(str_extract(lead(temp2$ciarfn), "LIMITED|CORPORATION|SYNDICATE|COMPANY")))&(is.na(str_extract(temp2$ciarfn, "LIMITED|CORPORATION|SYNDICATE|COMPANY"))))|
                          ((!is.na(str_extract(lead(temp2$ciarfn), "LIMITED|CORPORATION|SYNDICATE|COMPANY")))&(!is.na(str_extract(temp2$ciarfn, "LIMITED|CORPORATION|SYNDICATE|COMPANY")))&(nchar(lead(temp2$ciarfn))>=nchar(temp2$ciarfn)))|
                          ((is.na(str_extract(lead(temp2$ciarfn), "LIMITED|CORPORATION|SYNDICATE|COMPANY")))&(is.na(str_extract(temp2$ciarfn, "LIMITED|CORPORATION|SYNDICATE|COMPANY")))&(nchar(lead(temp2$ciarfn))>=nchar(temp2$ciarfn)))), 
                     lead(temp2$ciarfn),
                     ifelse(((str_extract(temp2$ciarfn, "^.{10}")==str_extract(lag(temp2$ciarfn), "^.{10}")))&
                              (temp2$year!=lag(temp2$year))&
                              (((!is.na(str_extract(lag(temp2$ciarfn), "LIMITED|CORPORATION|SYNDICATE|COMPANY")))&(is.na(str_extract(temp2$ciarfn, "LIMITED|CORPORATION|SYNDICATE|COMPANY"))))|
                                 ((!is.na(str_extract(lag(temp2$ciarfn), "LIMITED|CORPORATION|SYNDICATE|COMPANY")))&(!is.na(str_extract(temp2$ciarfn, "LIMITED|CORPORATION|SYNDICATE|COMPANY")))&(nchar(lag(temp2$ciarfn))>nchar(temp2$ciarfn)))|
                                 ((is.na(str_extract(lag(temp2$ciarfn), "LIMITED|CORPORATION|SYNDICATE|COMPANY")))&(is.na(str_extract(temp2$ciarfn, "LIMITED|CORPORATION|SYNDICATE|COMPANY")))&(nchar(lag(temp2$ciarfn))>nchar(temp2$ciarfn)))), 
                            lag(temp2$ciarfn),
                            temp2$ciarfn))
}
temp2$ciarfn<-str_to_title(temp2$ciarfn)
#write.csv(distinct(temp2, ciarfn, .keep_all=T), "cias4eva3.csv")
temp2b<-distinct(temp2, ciarfn, .keep_all=T)
temp3<-read.csv("cias4evab.csv")
temp3$ciarfn<-trimws(str_replace_all(temp3$ciarfn, "^(\\w+)(\\s+)Companies", ""), which=c("both"))
#write.csv(temp3, "cias4evac.csv")
#temp3<-read.csv("cias4evac.csv")
temprfin<-read.csv("cias4evad.csv", na.strings = c("", " ", "NA", "Na"))
temp3b<-left_join(temp3, subset(temprfin, select=c("ciarfn", "ciarfn2")), by="ciarfn")
temp2c<-left_join(subset(temp2b, select=c("id", "year", "text","company", "ciarfn")), subset(temp3b, select=c("id","ciarfn2")), by="id")
temporal<-left_join(temp2, subset(temp2c, select=c("ciarfn", "ciarfn2")), by="ciarfn")
write.csv(subset(temporal,is.na(temporal$ciarfn2)), "temporalna.csv")
temporalna<-read.csv("temporalna.csv")
temporal2<-rbind(subset(temporal,!is.na(temporal$ciarfn2)), temporalna)
#### II. 3.7. apply viafr####
temporal0<-distinct(temp, ciarfn, .keep_all=T)
cias<-as.vector(unique(temporal2$ciarfn2))
cons<-viaf_suggest(cias)
cons<-map(cons, function(z) pluck(z$text[z$name_type=="Corporate Names"],1))
cons[sapply(cons, is.null)] <- NA
cons2<-as.data.frame(unlist(cons))
cons2<-rownames_to_column(cons2, "ciarfn2")
colnames(cons2)<-c("ciarfn2", "viafsugg")
temporal3<-left_join(temporal2, cons2, by="ciarfn2")
temporal3$viafsugg[temporal3$viafsugg=="Nations Unies"]<-NA
temporal3$company<-ifelse(!is.na(temporal3$viafsugg), temporal3$viafsugg, temporal3$ciarfn2)
temporal3$company<-str_replace_all(temporal3$company, "[^[:alpha:]]", " ")
temporal3$company<-str_replace_all(temporal3$company, "(\\s+)", " ")
temporal3$company<-str_to_title(temporal3$company)
write.csv(temporal3, "temporal3.csv", row.names = F)
temporal4<-read.csv("temporal4.csv")

mmyb2<-left_join(
            subset(mmyb, select=c("id","year","yearg","capital","text","office","situated","mine","loc","lon","lat")), 
            subset(temporal4, select=c("id", "company")), 
            by="id", match="all")

write.csv(mmyb2[which(mmyb2$id<450000),], "mmybo1.csv", row.names = F)
write.csv(mmyb2[which((mmyb2$id>450000)&mmyb2$id<900000),], "mmybo2.csv", row.names = F)
write.csv(mmyb2[which(mmyb2$id>900000),], "mmybo3.csv", row.names = F)



####II. 3. 8 integrate names and test ####
mmyb<-rbind(read.csv("mmybo1.csv"), read.csv("mmybo2.csv"), read.csv("mmybo3.csv"))
mmyb$X<-NULL
mmyb<-fill(mmyb, company, .direction=c("down"))
mybt<-mmyb[sample(nrow(mmyb), 100, replace=T), ]
mybt$id<-as.numeric(as.character((str_replace_all(mybt$id, "[^[:digit:]]", ""))))
mybt<-mybt[order(mybt$id),]
mybt<-subset(mybt, select=c("id", "year", "text", "company"))
write.csv(mybt, "mybprueba.csv", row.names = F)

####II.4. people####
####II.4.0. International mining manual####
imm1<-read.csv("immorig.csv")
imm1$text<-trimws(str_to_title(imm1$text), which=c("both"))
imm1$loc<-str_extract(str_replace_all(imm1$text, "[.,]$", ""), "(?<=[,]).*?[,].*?$")
imm1$loc<-ifelse(imm1$type=="person", imm1$loc, NA)
imm1$loc<-trimws(sub("(.*[,])(.*?[,].*?)$", "\\2", imm1$loc), which=c("both"))
imm1$text2<-ifelse(imm1$type=="person", 
                   str_replace(imm1$text, "^(\\w+)(\\s)", paste(str_extract(imm1$text, "^(\\w+)"), ", ", sep="")), 
                   NA)
imm1$text2<-str_replace(imm1$text2, "[.]", "")
imm1$name<-ifelse(!is.na(str_extract(imm1$text2, "^(\\w+)[,](\\s+)(\\w+)(\\s+)(\\w+)")), 
                   str_extract(imm1$text2, "^(\\w+)[,](\\s+)(\\w+)(\\s+)(\\w+)"), 
                   str_extract(imm1$text2, "^(\\w+)[,](\\s+)(\\w+)"))
imm1$text2<-NULL
imm1$lastname<-str_extract(imm1$name, "^(\\w+)")
imm1$firstname<-str_extract(imm1$name, "(?<=[,](\\s))(\\w+)")
imm1$middlename<-ifelse(!is.na(str_extract(imm1$name, "(?<=[,](\\s))(\\w+)(\\s+)(\\w+)")), 
                        str_extract(imm1$name, "(\\w+)$"), NA)
imm1$company<-ifelse(imm1$type=="cia", imm1$text, NA)
imm1$lastname<-rfin(imm1$lastname)
imm1$firstname<-rfin(imm1$firstname)
imm1$middlename<-rfin(imm1$middlename)
imm1$simpname<-str_to_title(sname(imm1))
for(i in 1:20){
imm1$middlename<-ifelse((is.na(imm1$middlename))&(!is.na(lag(imm1$middlename)))&(imm1$name==lag(imm1$name)), 
                        lag(imm1$middlename), imm1$middlename)
}
write.csv(imm1, "imm2.csv", row.names = F)
write.csv(distinct(subset(imm1, imm1$type=="person"), name, .keep_all=T), "immnames.csv", row.names = F)
write.csv(distinct(subset(imm1, imm1$type=="cia"), name, .keep_all=T), "immcias.csv", row.names = F)
###
imm2<-read.csv("imm2.csv")
imm3<-read.csv("imm6.csv")
imm3$simpname<-str_to_title(imm3$simpname)
imm4<-left_join(imm2, subset(imm3, select=c("year","simpname","nationality", "edu", "prof", "org")), by=c("simpname", "year"))
imm4$org<-str_to_title(imm4$org)

#imm1<-fill(imm1, lastname, firstname, name, loc, .direction = c("down"))


####II.4.1 directors####
mmyb<-rbind(read.csv("mmybo1.csv"), read.csv("mmybo2.csv"), read.csv("mmybo3.csv"))
mmyb<-subset(mmyb, select=c("id", "year","yearg", "company", "capital", "text", "office", "situated", "mine","loc", "lon", "lat"))
mmyb<-fill(mmyb, company, .direction="down")
official<-c("banker", "solicitor", "secretary", "auditor", "manager", "superintendent", "chemist", "metallurgist","auditor", "Banker", "Solicitor", "Secretary", "Auditor", "Manager", "Superintendent", "Chemist", "Metallurgist", "Auditor", "Office", "office")
official<-paste(official, collapse="|")
temp<-subset(mmyb, select=c("id", "year","text"))
temp$text<-trimws(temp$text, which=c("both"))
temp$text<-str_replace_all(temp$text, "[??f?]", "")
temp$text<-str_replace_all(temp$text, "[[:digit:]]", "")
temp$text<-str_replace_all(temp$text, "Qualification", "")
temp$text<-str_replace_all(temp$text, "(\\s+)", " ")
temp$text<-str_replace_all(temp$text, " , ", " ")
temp$text<-str_replace_all(temp$text, " ,", ",")
temp$text<-str_replace_all(temp$text, ",[.]", ".")
temp$text<-str_replace_all(temp$text, " [.]", ".")
temp$text<-str_replace_all(temp$text, "(\\s+)", " ")
temp$text<-str_replace_all(temp$text, " , ", " ")
temp$text<-str_replace_all(temp$text, " ,", ",")
temp$text<-str_replace_all(temp$text, ",[.]", ".")
temp$text<-str_replace_all(temp$text, " [.]", ".")
temp$text<-str_replace_all(temp$text, "[.]{2}", ".")
temp$text<-str_replace_all(temp$text, "[,]{2}", ",")
temp$text<-str_replace_all(temp$text, "Directors-", "Directors.")
temp$text<-str_replace_all(temp$text, "(\\s)I(\\s)", " | ")
temp$text<-str_replace_all(temp$text, "(\\s)i(\\s)", " | ")
temp$text<-str_replace_all(temp$text, "(?<=(\\s)(\\w){1})[.](?=(\\w){1}[.])", "")
temp$text<-str_replace_all(temp$text, "(?<=(\\s)(\\w){1})[.](?=(\\s))", "")
temp$text<-str_replace_all(temp$text, "(?<=(\\s)(\\w){2})[.](?=(\\s))", "")
temp$text<-str_replace_all(temp$text, "(?<=^(\\w){1})[.](?=(\\s))", "")
temp$text<-str_replace_all(temp$text, "(?<=^(\\w){2})[.](?=(\\s))", "")
#first extracting the directors
temp$director<-ifelse(is.na(str_extract(temp$text, official))&(!is.na(str_extract(temp$text, "Directors."))), 
                      temp$text, 
                      ifelse(is.na(str_extract(temp$text, official))&(!is.na(str_extract(lag(temp$text, 1), "Directors."))), 
                             temp$text, 
                             ifelse(is.na(str_extract(temp$text, official))&is.na(str_extract(lag(temp$text), official))&(!is.na(str_extract(lag(temp$text, 2), "Directors."))), 
                                    temp$text, 
                                    ifelse(is.na(str_extract(temp$text, official))&is.na(str_extract(lag(temp$text), official))&(!is.na(str_extract(lag(temp$text, 3), "Directors."))), 
                                           temp$text, 
                                           ifelse(is.na(str_extract(temp$text, official))&is.na(str_extract(lag(temp$text), official))&(!is.na(str_extract(lag(temp$text, 4), "Directors."))), 
                                                  temp$text, NA))))) 
temp$director<-trimws(tolower(temp$director), which=c("both"))
sep1<-c("consulting", "(\\s)[|:;](\\s)", "man[.](\\s)")
sep1<-paste(sep1, collapse="|")
del1<-c("^hon.", "^the(\\s)hon", "^colonel", "^geo(\\s)","^sir","col[.]","lt[.]" ,"lieut","directors", "trustees", "presidency", "captain",
        "^[^[:alpha:],]", "commiffee", "colonel", "major", "general" , "committee", "commiftee", "commitfee", "shares", "(\\s)in(\\s)", "[.-]", "capt")
del1<-paste(del1, collapse="|")
direc<-as.data.frame(str_split_fixed(temp$director, sep1, n=10))
direc$id<-temp$id
direc$text<-temp$text
direc<-melt(direc, id.vars =c("id", "text"),na.rm=TRUE)
direc<-subset(direc, select=c("id", "text", "value"))
direc$value<-str_replace_all(direc$value, del1, "")
direc<-direc[which(nchar(trimws(direc$value))>6), ]
direc$value<-trimws(tolower(direc$value), which=c("both"))
direc$role<-ifelse(!is.na(str_extract(direc$value, "engineer|chair|managing director|manager")), 
                   str_extract(direc$value, "engineer|chair|managing director|manager"), 
                   ifelse(!is.na(str_extract(direc$value, "limited|works|mfg|company|consolidated|building|property|contract|(\\s)part"))
                          , NA, 
                          "board"))
direc<-direc[which(!is.na(direc$role)), ]
direc$value<-str_replace_all(direc$value, "engineer|chairman|managing director|manager|director", "")
direc$value<-str_replace_all(direc$value, "(\\s+)", " ")
direc$value<-trimws(direc$value, which=c("both"))
direc$name<-ifelse(!is.na(str_extract(direc$value, "^(\\w+)(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)")), 
                   str_extract(direc$value, "^(\\w+)(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)"), 
                   ifelse(!is.na(str_extract(direc$value, "^(\\w+)(\\s)(\\w+)(\\s)(\\w+)")), 
                          str_extract(direc$value, "^(\\w+)(\\s)(\\w+)(\\s)(\\w+)"), 
                          ifelse(!is.na(str_extract(direc$value, "^(\\w+)(\\s)(\\w+)")), 
                                 str_extract(direc$value, "^(\\w+)(\\s)(\\w+)"),
                                 NA)))
direc<-direc[which(!is.na(direc$name)|nchar(direc$name)<20), ]  
del4<-c("^the(\\s)", "(\\s)the(\\s)", "^with(\\s)", "(\\s)with", "time", "progress", "they(\\s)", "mining", "company")
del4<-paste(del4, collapse="|")
direc$lastname<-ifelse(!is.na(str_extract(direc$name, del4)), NA, 
                       ifelse(!is.na(str_extract(direc$name, "(\\w){4,}$")),str_extract(direc$name, "(\\w){4,}$"), 
                              ifelse(!is.na(str_extract(direc$name, "(\\w){4,}(?=(\\w+)$)")),str_extract(direc$name, "(\\w){4,}(?=(\\w+)$)"),
                                     ifelse(!is.na(str_extract(direc$name, "(\\w){4,}(?=(\\w+)(\\s)(\\w+)$)")),str_extract(direc$name, "(\\w){4,}(?=(\\w+)(\\s)(\\w+)$)"), 
                                            NA))))
direc<-direc[which(!is.na(direc$lastname)), ]
direc$firstname<-tolower(str_extract(direc$name, "^(\\w+)"))
direc$middlename<-ifelse(!is.na(str_extract(direc$name, "^(\\w+)(\\s)(\\w+)(\\s)(\\w+)")), 
                         tolower(str_extract(direc$name, "^(\\w+)(\\s)(\\w+)")), NA)
direc$middlename<-str_extract(direc$middlename, "(\\w+)$")
direc$simpname<-tolower(trimws(paste(direc$lastname, ", ", 
                                     substr(direc$firstname, 1, 1), " " ,
                                     ifelse(!is.na(substr(direc$middlename, 1, 1)), substr(direc$middlename, 1, 1),""), sep="")))
direc<-subset(direc, select=c("id", "role","name", "lastname", "middlename", "firstname", "simpname"))
direc<-left_join(direc, mmyb, by="id",match="all")
write.csv(direc, "direc.csv")
####II.4.2 technicians####
mmyb<-rbind(read.csv("mmybo1.csv"), read.csv("mmybo2.csv"), read.csv("mmybo3.csv"))
mmyb<-subset(mmyb, select=c("id", "year","yearg", "company", "capital", "text", "office", "situated", "mine","loc", "lon", "lat"))
mmyb<-fill(mmyb, company, .direction="down")
temp<-subset(mmyb, select=c("id", "year","text"))
temp$text<-trimws(temp$text, which=c("both"))
temp$text<-str_replace_all(temp$text, "[??f?]", "")
temp$text<-str_replace_all(temp$text, "[[:digit:]]", "")
temp$text<-str_replace_all(temp$text, "(\\s+)", " ")
temp$text<-str_replace_all(temp$text, " , ", " ")
temp$text<-str_replace_all(temp$text, " ,", ",")
temp$text<-str_replace_all(temp$text, ",[.]", ".")
temp$text<-str_replace_all(temp$text, " [.]", ".")
temp$text<-str_replace_all(temp$text, "(\\s+)", " ")
temp$text<-str_replace_all(temp$text, " , ", " ")
temp$text<-str_replace_all(temp$text, " ,", ",")
temp$text<-str_replace_all(temp$text, ",[.]", ".")
temp$text<-str_replace_all(temp$text, " [.]", ".")
temp$text<-str_replace_all(temp$text, "[.]{2}", ".")
temp$text<-str_replace_all(temp$text, "[,]{2}", ",")
temp$text<-str_replace_all(temp$text, "(\\s)I(\\s)", " | ")
temp$text<-str_replace_all(temp$text, "(\\s)i(\\s)", " | ")
temp$text<-str_replace_all(temp$text, "(\\s)[.](\\s)", " ")
temp$text<-str_replace_all(temp$text, "(?<=(\\s)(\\w){1})[.](?=(\\w){1}[.])", "")
temp$text<-str_replace_all(temp$text, "(?<=(\\s)(\\w){1,2})[.](?=(\\s))", "")
temp$text<-str_replace_all(temp$text, "(?<=^(\\w){1,2})[.](?=(\\s))", "")
temp$text<-str_replace_all(temp$text, "(\\s+)", " ")
technician1<-c("manager[.]", "superintendent[.]", "chemist[.]", "metallurgist[.]","engineer[.]","managers[.]", "superintendents[.]", "chemists[.]", "metallurgists[.]","engineers[.]")
technician2<-c("Manager[.]", "Superintendent[.]", "Chemist[.]", "Metallurgist[.]", "Engineer[.]","Managers[.]", "Superintendents[.]", "Chemists[.]", "Metallurgists[.]", "Engineers[.]")
tech0<-str_replace_all(technician1, ".{3}$", "")
tech0<-paste(tech0, collapse="|")
tech1<-paste(technician2, collapse="|")
tech2<-str_replace_all(technician2, ".{3}$", "")
tech2<-paste(tech2, "-", sep="")
tech2<-paste(tech2, collapse="|")
tech3<-str_replace_all(technician2, ".{3}$", "")
tech3<-paste(tech3, "(\\s)", sep="")
tech3<-paste(tech3, collapse="|")
temp$technician<-ifelse(temp$year<1900, 
                        ifelse(!is.na(str_extract(temp$text, tech1)), temp$text, 
                               ifelse(!is.na(str_extract(lag(temp$text), tech1)), 
                                      temp$text,NA)),  
                        ifelse(!is.na(str_extract(temp$text, tech2)),temp$text,
                               ifelse(!is.na(str_extract(temp$text, tech3)),temp$text,
                                      ifelse(!is.na(str_extract(temp$text, tech1)),temp$text,NA))))
temp$technician<-str_replace_all(temp$technician, "(?<=(\\s)(\\w){1})[.](\\s)", " ")
temp$technician<-str_replace_all(temp$technician, "(?<=^(\\w){1})[.](\\s)", " ")
temp$technician<-trimws(temp$technician, which=c("both"))
sep2<-c("[:|;.]{1,}(\\s)")
del2<-c("^hon[.]", "^the(\\s)hon", "^colonel", "^sir","col[.]","lt[.]" ,"^lt(\\s)","(\\s)lt(\\s)","lieut","directors", "trustees", 
        "Captain", "Dr[.]", "Prof[.]", "^Dr(\\s)","(\\s)Dr(\\s)", "^dr(\\s)","(\\s)dr(\\s)","^[^[:alpha:],]", "presidency", "Colonel",
        "commiffee", "committee", "commiftee", "commitfee", "shares", "(\\s)in(\\s)", "[.-]", "capt", "dr[.]", "prof[.]", "colonel")
del2<-paste(del2, collapse="|")
tek<-as.data.frame(str_split_fixed(temp$technician, sep2, n=6))
tek$id<-temp$id
tek$text<-temp$text
tek$year<-temp$year
#before 1900
tek1<-tek[which(tek$year<1900), ]
tek1$year<-NULL
tek1$V1<-gsub("^$", NA, tek1$V1)
tek1$V2<-gsub("^$", NA, tek1$V2)
tek1$V3<-gsub("^$", NA, tek1$V3)
tek1$V4<-gsub("^$", NA, tek1$V4)
tek1$V5<-gsub("^$", NA, tek1$V5)
tek1$V6<-gsub("^$", NA, tek1$V6)
tek1$R1<-ifelse((!is.na(str_extract(lag(tek1$V1,1), tech1)))&!is.na(tek1$V1), str_extract(lag(tek1$V1,1), tech1), NA)
tek1$R2<-ifelse((!is.na(str_extract(lag(tek1$V2,1), tech1)))&!is.na(tek1$V2), str_extract(lag(tek1$V2,1), tech1), NA)
tek1$R3<-ifelse((!is.na(str_extract(lag(tek1$V3,1), tech1)))&!is.na(tek1$V3), str_extract(lag(tek1$V3,1), tech1), NA)
tek1$R4<-ifelse((!is.na(str_extract(lag(tek1$V4,1), tech1)))&!is.na(tek1$V4), str_extract(lag(tek1$V4,1), tech1), NA)
tek1$R5<-ifelse((!is.na(str_extract(lag(tek1$V5,1), tech1)))&!is.na(tek1$V5), str_extract(lag(tek1$V5,1), tech1), NA)
tek1$R6<-ifelse((!is.na(str_extract(lag(tek1$V6,1), tech1)))&!is.na(tek1$V6), str_extract(lag(tek1$V6,1), tech1), NA)
tek1<-tek1[which(!is.na(tek1$R1)|!is.na(tek1$R2)|!is.na(tek1$R3)|!is.na(tek1$R4)|!is.na(tek1$R5)|!is.na(tek1$R6)), ]
tek1<-melt(tek1, id.vars =c("id", "text"),na.rm=TRUE)
tek1<-tek1[order(tek1$id, tek1$variable),]
tek1$value<-str_replace_all(tek1$value, "[|]", " ")
tek1$value<-str_replace_all(tek1$value, del2, " ")
tek1$value<-str_replace_all(tek1$value, "(\\s+)", " ")
tek1$role1<-ifelse(tek1$variable=="V1", 
                   ifelse((tek1$id==lead(tek1$id, 1))&(lead(tek1$variable, 1)=="R1"), lead(tek1$value,1), 
                          ifelse((tek1$id==lead(tek1$id, 2))&(lead(tek1$variable, 2)=="R1"), lead(tek1$value,2),
                                 ifelse((tek1$id==lead(tek1$id, 3))&(lead(tek1$variable, 3)=="R1"), lead(tek1$value,3),  
                                        ifelse((tek1$id==lead(tek1$id, 4))&(lead(tek1$variable, 4)=="R1"), lead(tek1$value,4), 
                                               ifelse((tek1$id==lead(tek1$id, 5))&(lead(tek1$variable, 5)=="R1"), lead(tek1$value,5),
                                                      ifelse((tek1$id==lead(tek1$id, 6))&(lead(tek1$variable, 6)=="R1"), lead(tek1$value,6),  
                                                             NA)))))),  NA)
tek1$role2<-ifelse(tek1$variable=="V2", 
                   ifelse((tek1$id==lead(tek1$id, 1))&(lead(tek1$variable, 1)=="R2"), lead(tek1$value,1), 
                          ifelse((tek1$id==lead(tek1$id, 2))&(lead(tek1$variable, 2)=="R2"), lead(tek1$value,2),
                                 ifelse((tek1$id==lead(tek1$id, 3))&(lead(tek1$variable, 3)=="R2"), lead(tek1$value,3),  
                                        ifelse((tek1$id==lead(tek1$id, 4))&(lead(tek1$variable, 4)=="R2"), lead(tek1$value,4), 
                                               ifelse((tek1$id==lead(tek1$id, 5))&(lead(tek1$variable, 5)=="R2"), lead(tek1$value,5),
                                                      ifelse((tek1$id==lead(tek1$id, 6))&(lead(tek1$variable, 6)=="R2"), lead(tek1$value,6),  
                                                             NA)))))), NA)
tek1$role3<-ifelse(tek1$variable=="V3", 
                   ifelse((tek1$id==lead(tek1$id, 1))&(lead(tek1$variable, 1)=="R3"), lead(tek1$value,1), 
                          ifelse((tek1$id==lead(tek1$id, 2))&(lead(tek1$variable, 2)=="R3"), lead(tek1$value,2),
                                 ifelse((tek1$id==lead(tek1$id, 3))&(lead(tek1$variable, 3)=="R3"), lead(tek1$value,3),  
                                        ifelse((tek1$id==lead(tek1$id, 4))&(lead(tek1$variable, 4)=="R3"), lead(tek1$value,4), 
                                               ifelse((tek1$id==lead(tek1$id, 5))&(lead(tek1$variable, 5)=="R3"), lead(tek1$value,5),
                                                      ifelse((tek1$id==lead(tek1$id, 6))&(lead(tek1$variable, 6)=="R3"), lead(tek1$value,6),  
                                                             NA)))))), NA)
tek1$role4<-ifelse(tek1$variable=="V4", 
                   ifelse((tek1$id==lead(tek1$id,1))&(lead(tek1$variable, 1)=="R4"), lead(tek1$value,1), 
                          ifelse((tek1$id==lead(tek1$id,2))&(lead(tek1$variable, 2)=="R4"), lead(tek1$value,2),
                                 ifelse((tek1$id==lead(tek1$id,3))&(lead(tek1$variable, 3)=="R4"), lead(tek1$value,3),  
                                        ifelse((tek1$id==lead(tek1$id,4))&(lead(tek1$variable, 4)=="R4"), lead(tek1$value,4), 
                                               ifelse((tek1$id==lead(tek1$id,5))&(lead(tek1$variable, 5)=="R4"), lead(tek1$value,5),
                                                      ifelse((tek1$id==lead(tek1$id,6))&(lead(tek1$variable, 6)=="R4"), lead(tek1$value,6),  
                                                             NA)))))), NA)
tek1$role5<-ifelse(tek1$variable=="V5", 
                   ifelse((tek1$id==lead(tek1$id, 1))&(lead(tek1$variable, 1)=="R5"), lead(tek1$value,1), 
                          ifelse((tek1$id==lead(tek1$id, 2))&(lead(tek1$variable, 2)=="R5"), lead(tek1$value,2),
                                 ifelse((tek1$id==lead(tek1$id, 3))&(lead(tek1$variable, 3)=="R5"), lead(tek1$value,3),  
                                        ifelse((tek1$id==lead(tek1$id, 4))&(lead(tek1$variable, 4)=="R5"), lead(tek1$value,4), 
                                               ifelse((tek1$id==lead(tek1$id, 5))&(lead(tek1$variable, 5)=="R5"), lead(tek1$value,5),
                                                      ifelse((tek1$id==lead(tek1$id, 6))&(lead(tek1$variable, 6)=="R5"), lead(tek1$value,6),  
                                                             NA)))))), NA)
tek1$role6<-ifelse(tek1$variable=="V6", 
                   ifelse((tek1$id==lead(tek1$id, 1))&(lead(tek1$variable, 1)=="R6"), lead(tek1$value,1), 
                          ifelse((tek1$id==lead(tek1$id, 2))&(lead(tek1$variable, 2)=="R6"), lead(tek1$value,2),
                                 ifelse((tek1$id==lead(tek1$id, 3))&(lead(tek1$variable, 3)=="R6"), lead(tek1$value,3),  
                                        ifelse((tek1$id==lead(tek1$id, 4))&(lead(tek1$variable, 4)=="R6"), lead(tek1$value,4), 
                                               ifelse((tek1$id==lead(tek1$id, 5))&(lead(tek1$variable, 5)=="R6"), lead(tek1$value,5),
                                                      ifelse((tek1$id==lead(tek1$id, 6))&(lead(tek1$variable, 6)=="R6"), lead(tek1$value,6),  
                                                             NA)))))), NA)
tek1$role<-ifelse(tek1$variable=="V1", tek1$role1, 
                  ifelse(tek1$variable=="V2", tek1$role2, 
                         ifelse(tek1$variable=="V3", tek1$role3, 
                                ifelse(tek1$variable=="V4", tek1$role4, 
                                       ifelse(tek1$variable=="V5", tek1$role5, 
                                              ifelse(tek1$variable=="V6", tek1$role6,
                                                     NA))))))
tek1<-tek1[which((tek1$variable!="R1")&
                   (tek1$variable!="R2")&
                   (tek1$variable!="R3")&
                   (tek1$variable!="R4")&
                   (tek1$variable!="R5")&
                   (tek1$variable!="R6")),]
tek1$role<-rellenar(tek1$role, tek1$id)
tek1$role<-rellenar(tek1$role, tek1$id)
tek1$role<-rellenar(tek1$role, tek1$id)
tek1$role<-rellenar(tek1$role, tek1$id)
tek1<-subset(tek1, select=c("id", "text", "value", "role"))           
#after 1900
tek2<-tek[which(tek$year>1900), ]
tek2$year<-NULL
tek2<-melt(tek2, id.vars =c("id", "text"),na.rm=TRUE)
tek2<-subset(tek2, select=c("id", "text", "value"))
tek2$value<-gsub("^$", NA, tek2$value)
sep3<-paste(tech2, tech3, tech0, sep="|")
sep3<-paste(sep3, collapse="|")
tek2$role<-str_extract(tek2$value, sep3)
tek2<-tek2[which(!is.na(tek2$value)&!is.na(tek2$role)), ]
tek2b<-as.data.frame(str_split_fixed(tek2$value, sep3, n=3))
tek2b$V1<-gsub("^$", NA ,tek2b$V1)
tek2b$V2<-gsub("^$", NA ,tek2b$V2)
tek2b$V3<-gsub("^$", NA ,tek2b$V3)
tek2b$name<-ifelse(!is.na(tek2b$V3)&!is.na(str_extract(tek2b$V2, "^and")), 
                   tek2b$V3, 
                   ifelse(!is.na(tek2b$V2), tek2b$V2, tek2b$V1))      
tek2$name<-tek2b$name
tek2$name<-str_replace_all(tek2$name, del2, "")
tek2$text<-NULL
colnames(tek2)<-c("id", "text", "role", "value")
tek2$role<-ifelse(!is.na(str_extract(tek2$value, "limited|works|mfg|consolidated|building|property|contract|(\\s)part"))
                  , NA, tek2$role)
tek2<-tek2[which(!is.na(tek2$role)),]
tekf<-rbind(tek1, tek2)
tekf<-tekf[which(nchar(trimws(tekf$value))>6), ]
tekf$value<-trimws(tolower(tekf$value), which=c("both"))
tekf$role<-ifelse(!is.na(str_extract(tekf$value, "limited|works|mfg|consolidated|building|property|contract|(\\s)part|bank(\\s)"))
                  , NA, tekf$role)
tekf$value<-str_replace_all(tekf$value, "mining|(\\s)and(\\s)", " ")
tekf$value<-str_replace_all(tekf$value, "(\\s+)", " ")
tekf$name<-ifelse(!is.na(str_extract(tekf$value, "^(\\w+)(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)")), 
                  str_extract(tekf$value, "^(\\w+)(\\s)(\\w+)(\\s)(\\w+)(\\s)(\\w+)"), 
                  ifelse(!is.na(str_extract(tekf$value, "^(\\w+)(\\s)(\\w+)(\\s)(\\w+)")), 
                         str_extract(tekf$value, "^(\\w+)(\\s)(\\w+)(\\s)(\\w+)"), 
                         ifelse(!is.na(str_extract(tekf$value, "^(\\w+)(\\s)(\\w+)")), 
                                str_extract(tekf$value, "^(\\w+)(\\s)(\\w+)"),
                                NA)))
tekf<-tekf[which(!is.na(tekf$role)&(!is.na(tekf$name)|nchar(tekf$name)<20)), ]
tekf$name<-str_replace(tekf$name, sep3, "")
tekf$role<-trimws(tekf$role, which=c("both"))
del4<-c("^the(\\s)", "(\\s)the(\\s)", "^with(\\s)", "(\\s)with", "time", "progress", "they(\\s)", "mining", "company")
del4<-paste(del4, collapse="|")
tekf$lastname<-ifelse(!is.na(str_extract(tekf$name, del4)), NA, 
                      ifelse(!is.na(str_extract(tekf$name, "(\\w){4,}$")),str_extract(tekf$name, "(\\w){4,}$"), 
                             ifelse(!is.na(str_extract(tekf$name, "(\\w){4,}(?=(\\w+)$)")),str_extract(tekf$name, "(\\w){4,}(?=(\\w+)$)"),
                                    ifelse(!is.na(str_extract(tekf$name, "(\\w){4,}(?=(\\w+)(\\s)(\\w+)$)")),str_extract(tekf$name, "(\\w){4,}(?=(\\w+)(\\s)(\\w+)$)"), 
                                           NA))))
tekf<-tekf[which(!is.na(tekf$lastname)), ]
tekf$firstname<-tolower(str_extract(tekf$name, "^(\\w+)"))
tekf$middlename<-ifelse(!is.na(str_extract(tekf$name, "^(\\w+)(\\s)(\\w+)(\\s)(\\w+)")), 
                        tolower(str_extract(tekf$name, "^(\\w+)(\\s)(\\w+)")), NA)
tekf$middlename<-str_extract(tekf$middlename, "(\\w+)$")
tekf$simpname<-tolower(trimws(paste(tekf$lastname, ", ", 
                                    substr(tekf$firstname, 1, 1), " " ,
                                    ifelse(!is.na(substr(tekf$middlename, 1, 1)), substr(tekf$middlename, 1, 1),""), sep="")))
tekt<-left_join(subset(tekf, select=c("id", "role", "name","lastname", "middlename", "firstname", "simpname")), mmyb, by="id",match="all")
write.csv(tekt, "tek.csv")
####II.4.3. together####
tek<-read.csv("tek.csv")
direc<-read.csv("direc.csv")
myb<-rbind(read.csv("mmybo1.csv"), read.csv("mmybo2.csv"), read.csv("mmybo3.csv"))
myb$X<-NULL
myb$id<-as.numeric(str_extract(myb$id, "(\\d+)"))
myb<-fill(myb, company, .direction=c("down"))
officials<-rbind(direc, tek)
officials<-subset(officials, select=c("id", "role","name", "lastname", "middlename", "firstname", "simpname", "year", "company", "text", "loc", "lon", "lat"))
officials$role<-tolower(trimws(str_replace_all(officials$role, "[^[:alpha:]]", " "), which=c("both")))
officials<-officials[order(officials$simpname, officials$id),]
officials$lastname<-repetir(officials$lastname, rfin, 10)
officials$simpname<-repetir(officials$simpname, rfin, 10)
consultants<-officials[str_extract(officials$role, "(\\w){1}$")=="s",]
consultants<-subset(consultants, select=c("id", "role","name", "year", "text", "loc", "lon", "lat"))
officers<-officials[which(str_extract(officials$role, "(\\w){1}$")!="s"),]
officers<-subset(officers, select=c("id", "role","name","lastname", "middlename", "firstname", "simpname", "year", "text", "loc", "lon", "lat"))
mistakes0<-paste(c("resident", "miscellaneous(\\s+)(\\w+)", "(\\b)rector(\\b)"), collapse="|")
officers$org<-str_replace_all(officers$org, mistakes0, "")
officers$firstname<-str_replace_all(officers$firstname, mistakes0, "")
officers$middlename<-str_replace_all(officers$middlename, mistakes0, "")
officers$lastname<-str_replace_all(officers$lastname, mistakes0, "")
officers$lastname<-officers$lastname%>%str_replace_all("[:digit:]|[:punct:]", "")%>%str_replace_all("^(\\s+)$", "")
officers$firstname<-officers$firstname%>%str_replace_all("[:digit:]|[:punct:]", "")%>%str_replace_all("^(\\s+)$", "")
officers$middlename<-officers$middlename%>%str_replace_all("[:digit:]|[:punct:]", "")%>%str_replace_all("^(\\s+)$", "")
officers$firstname[which(officers$firstname=="")]<-NA
officers$lastname[which(officers$lastname=="")]<-NA
officers$middlename[which(officers$middlename=="")]<-NA
myb<-subset(myb, select=c("id","company"))
consultants<-left_join(consultants, myb, by="id", match="all")
officers<-left_join(officers, myb, by="id", match="all")
write.csv(consultants, "consultants.csv")
write.csv(officers, "officers.csv")
#consultants need looking for mistakes#
####II. 4. 4. Consolidation ####
myb<-rbind(read.csv("mmybo1.csv"), read.csv("mmybo2.csv"), read.csv("mmybo3.csv"))
myb$X<-NULL
myb$id<-paste("myb",myb$id, sep="")
myb<-fill(myb, company, .direction="down")
write.csv(myb[which(myb$year<1912),], "myb1.csv")
write.csv(myb[which((myb$year>1911)&myb$year<1918),], "myb2.csv")
write.csv(myb[which(myb$year>1918),], "myb3.csv")

####III.Rest of directories####
####III. 1. AMM####
####III. 1. 1. company####
amm<-read.csv("amm.csv")
exchange<-read.csv("exchange.csv")
amm$id<-rownames(amm)
companies<-c("company", "(\\s)co[.][,]", "limited", "ltd", "consolidated")
companies<-paste(companies, collapse="|")
temp<-amm             
temp$text<-str_replace_all(temp$text, "(\\s)00.,", " CO.,")
temp$text<-str_replace_all(temp$text, "(\\s)C0.,", " CO.,")
temp$text<-str_replace_all(temp$text, "(\\s)O0.,", " CO.,")
temp$text<-str_replace_all(temp$text, "(\\s)0O.,", " CO.,")
temp$text1<-tolower(temp$text)
temp$company<-ifelse(!is.na(str_extract(temp$text1, companies)), temp$text, 
                     ifelse(!is.na(str_extract(temp$text, "MINE")), temp$text, NA))
####III. 1. 2. stock####
temp$text1<-temp$text
temp$text1<-str_replace_all(temp$text1, "(?<=[$])I", "1")
temp$text1<-str_replace_all(temp$text1, "(?<=[$])l", "1")
temp$text1<-str_replace_all(temp$text1, "(?<=[$])[|]", "1")
temp$text1<-str_replace_all(temp$text1, "(?<=[$])0", "6")
temp$text1<-str_replace_all(temp$text1, "(\\s)[,]", ",")
temp$text1<-str_replace_all(temp$text1, "(?<=(\\d))[,.](?=(\\d))", "")
temp$text1<-str_replace_all(temp$text1, "Stack|Stock|Stuck|Stusk|Stook|Stuok|S40ck|S406k|Stoek|StOck", "Capital")
usd<-c("(?<=Capital[,.:](\\s)[$])(\\d+)", "(?<=Capital(\\s)[$])(\\d+)",
       "(?<=Capital[,.:](\\s))(\\d+)", "(?<=Capital(\\s))(\\d+)", 
       "(?<=[$])(\\d+)")
pou<-c("(?<=Capital[,.:](\\s)?)(\\d+)", "(?<=Capital(\\s)?)(\\d+)")
pes<-c("(?<=Capital[,.:](\\s)P)(\\d+)", 
       "(?<=Capital(\\s)P)(\\d+)",
       "(?<=Capital(\\s))(\\d+)(\\s)(?=Pesos)", 
       "(?<=Capital[,.:](\\s))(\\d+)(\\s)(?=Pesos)")
usd<-paste(usd, collapse="|")
pou<-paste(pou, collapse="|")
pes<-paste(pes, collapse="|")
temp$capitalusd<-as.numeric(as.character(str_extract(temp$text1, usd)))
temp$capitalpound<-as.numeric(as.character(str_extract(temp$text1, pou)))
temp$capitalpeso<-as.numeric(as.character(str_extract(temp$text1, pes)))
temp$stock<-ifelse(!is.na(temp$capitalpeso), (temp$capitalpeso*temp$mxtousd),
                   ifelse(!is.na(temp$capitalpound), (temp$capitalpound*temp$poundtousd), 
                          ifelse(!is.na(temp$capitalusd), (temp$capitalusd), 
                                 NA)))
temp$text1<-str_replace_all(temp$text, "[^[:alpha:],.]", " ")
temp$text1<-str_replace_all(temp$text1, "(\\s+)", " ")
temp$text1<-str_replace_all(temp$text1, "St[.]", "St")
off<-c("(?<=^Office[,](\\s))(\\w+)(\\s)(\\w+)(\\s)(\\w+)[,](\\s)(\\w+)(\\s)(\\w+)", 
       "(?<=^Office[,](\\s))(\\w+)(\\s)(\\w+)(\\s)(\\w+)[,](\\s)(\\w+)", 
       "(?<=^Office[,](\\s))(\\w+)(\\s)(\\w+)(\\s)(\\w+)",
       "(?<=^Office[,](\\s))(\\w+)(\\s)(\\w+)[,](\\s)(\\w+)(\\s)(\\w+)", 
       "(?<=^Office[,](\\s))(\\w+)(\\s)(\\w+)[,](\\s)(\\w+)", 
       "(?<=^Office[,](\\s))(\\w+)(\\s)(\\w+)",
       "(?<=^Office[,](\\s))(\\w+)[,](\\s)(\\w+)(\\s)(\\w+)", 
       "(?<=^Office[,](\\s))(\\w+)[,](\\s)(\\w+)", 
       "(?<=^Office[,](\\s))(\\w+)")
off<-paste(off, collapse="|")
temp$office<-str_extract(temp$text1, off)
temp$text1<-tolower(temp$text)
temp$text1<-str_replace_all(temp$text1, "e[.]c", "london")
temp$text1<-str_replace_all(temp$text1, "e[.]o[.]", "london")
temp$text1<-str_replace_all(temp$text1, "[^[:alpha:]]", " ")
temp$text1<-str_replace_all(temp$text1, "(\\s+)", " ")
location <- read_csv("locations.csv")
location<-paste(location$place, collapse=" |")
temp$location<-str_extract(temp$text1, location)
####III. 1. 3. Incorporated####
incor<-temp[which(!is.na(str_extract(temp$text, "^Incorporated|^Inc.,"))),]
incor$text<-str_replace(incor$text, "(?<=^Incorporated)(\\s)in(?=(\\s))", "," )
incor$text<-str_replace_all(incor$text, "(\\s)[,](\\s)", ", ")
incor$text<-str_replace_all(incor$text, "(?<=Incorporated)(\\s)", ", ")
incor$text<-str_replace_all(incor$text, "(?<=(\\s)(\\w){1})[.]", "")
incor$text<-str_replace_all(incor$text, "(?<=(\\s)(\\w){1})(\\s)(?=(\\w){1})", "")
incor$text<-str_replace_all(incor$text, "[^[:alnum:],.$]", " ")
incor$text<-str_replace_all(incor$text, "(\\s+)", " ")
incor$locinc<-ifelse(!is.na(str_extract(incor$text, "(?<=Incorporated[,.](\\s))(\\w+)(\\s)(\\w+)(\\s)(\\w+)(?=[,.])")),
                     str_extract(incor$text, "(?<=Incorporated[,.](\\s))(\\w+)(\\s)(\\w+)(\\s)(\\w+)(?=[,.])"),
                     ifelse(!is.na(str_extract(incor$text, "(?<=Incorporated[,.](\\s))(\\w+)(\\s)(\\w+)(?=[,.])")),
                            str_extract(incor$text, "(?<=Incorporated[,.](\\s))(\\w+)(\\s)(\\w+)(?=[,.])"),
                            str_extract(incor$text, "(?<=Incorporated[,.](\\s))(\\w+)(?=[,.])")))
incor$locinc<-str_replace_all(incor$locinc, "[^[:alpha:]]", " ")
incor$locinc<-trimws(str_replace_all(incor$locinc, "(\\s+)", " "), which=c("both"))
incor$locinc<-gsub("^(\\s)$|^$", NA, incor$locinc)
incor$text<-incor$incor
incor$text<-str_replace_all(incor$text, "[^[:alnum:],.$;:]", " ")
incor$text<-str_replace_all(incor$text, "(\\s+)", " ")
incor$yinc<-str_extract(incor$text, "(?<=(\\s))(\\d){4}")
incor$yinc<-ifelse(incor$yinc<1923, incor$yreg, NA)
incor<-subset(incor, select=c("id", "yinc", "locinc"))
temp<-left_join(temp, incor, by="id", match="all")
temp<-subset(temp, select=c("id", "company", "stock", "office", "location", "yinc", "locinc"))
amm<-left_join(amm, temp, by="id")
write.csv(amm, "amm1.csv")
####III. 1. 4. People####
amm<-read.csv("amm1.csv")
amm$X<-NULL
temp<-subset(amm, select=c("id", "year", "text"))
official<-c("banker", "solicitor", "secretary", "auditor", "manager", "superintendent", "chemist", "metallurgist","auditor", "Banker", 
            "Chief", "Solicitor", "Secretary", "Auditor", "Manager", "Superintendent", "Chemist", "Metallurgist", "Supt", "Snpt", "Slipt", 
            "Mgr", "Pres", "Mining(\\s)Engineer", "General(\\s)Manager", "General(\\s)Mgr", "General(\\s)Supt", "Consulting(\\s)Engineer", 
            "General(\\s)Superintendent", "Gen[.](\\s)Mine(\\s)Supt", "Gen[.](\\s)Mine(\\s)Superintendent", "Gen(\\s)Mine(\\s)Supt", 
            "Gen(\\s)Mine(\\s)Superintendent", "Gen[.](\\s)Supt", "Gen[.](\\s)Manager", "Gen[.](\\s)Superintendent", "Gen[.](\\s)Chemist", 
            "Gen[.](\\s)Metallurgist", "Gen[.](\\s)Engineer", "Gen[.](\\s)Mine(\\s)Mgr", "Gen[.](\\s)Mine(\\s)Manager", "Gen(\\s)Mine(\\s)Mgr", 
            "Gen(\\s)Mine(\\s)Manager", "Vice", "Purchasing(\\s)Agent", "Mine(\\s)Manager", "Owner", "Chairman", "Managing(\\s)Director", 
            "Lessee", "Assistant")
off0<-paste(official, collapse="|")
off1<-paste("^", official, sep="")
off1b<-paste("^(\\w+)[,](\\s+)", official, sep="")
off1<-rbind(off1, off1b)
off1<-paste(off1, collapse="|")
rgx1<-c("(\\w+)(\\s)(\\w+)(\\s)(\\w+)(?=[,.](\\s)", 
        "(\\w+)(\\s)(\\w+)(?=[,.](\\s)", 
        "(\\w+)(\\s)(\\w+)(\\s)(\\w+)(?=[,](\\s)(\\w+)[,.](\\s)", 
        "(\\w+)(\\s)(\\w+)(?=[,](\\s)(\\w+)[,.](\\s)")
rgx2<-c("^(\\w+)(\\s)(\\w+)(\\s)(\\w+)(?=[,.])",
        "^(\\w+)(\\s)(\\w+)(?=[,.])")
rgx2<-paste(rgx2, collapse="|")
off2<-expand.grid(rgx1, official)
off2$v3<-paste(off2$Var1, off2$Var2, ")", sep="")
off2<-paste(off2$v3, collapse = "|")
temp$text1<-ifelse(!is.na(str_extract(lead(temp$text), off1)), paste(temp$text, lead(temp$text), sep=" "), temp$text)
temp$text1<-str_replace_all(temp$text1, "[^[:alpha:].,;:]", " ")
temp$text1<-initials(temp$text1)
temp$text1<-trimws(str_replace_all(temp$text1, "(\\s+)", " "), which=c("both"))
temp$text1<-str_replace_all(temp$text1, "(\\s)Jr[.]", "")
temp$text1<-ifelse(str_extract(temp$text1, "^(\\w){1}")==toupper(str_extract(temp$text1, "^(\\w){1}")), 
                   temp$text1, 
                   ifelse(!is.na(str_extract(lag(temp$text1), "(\\w+)(\\s)(\\w+)$")), 
                          paste(str_extract(lag(temp$text1), "(\\w+)(\\s)(\\w+)$"), temp$text1, sep=""), 
                          paste(str_extract(lag(temp$text1), "(\\w+)$"), temp$text1, sep="")))
temp$text1<-str_replace_all(temp$text1, "[,](\\s)[,]", ",")
temp$text1<-str_replace_all(temp$text1, "[,]{2}", ",")
temp$text1<-str_replace_all(temp$text1, "(\\s)[,](\\s)", ", ")
temp$text1<-str_replace_all(temp$text1, "(\\s)[.](\\s)", ". ")
temp$text1<-initials(temp$text1)
temp$text1<-str_replace_all(temp$text1, "[^[:alpha:].,;:]", " ")
temp$text1<-trimws(str_replace_all(temp$text1, "(\\s+)", " "), which=c("both"))
temp$role<-str_extract(temp$text1, off0)
#write.csv(temp, "temp.csv")
temp<-read.csv("temp.csv")
temp$X<-NULL
tempI<-temp[which(is.na(temp$role)),]
tempII<-temp[which(!is.na(temp$role)),]
temp1<-tempII[which(tempII$year<=1918), ]
temp2<-tempII[which(tempII$year>1918), ]
temp2$name<-ifelse(!is.na(temp2$role), str_extract(temp2$text1, rgx2), NA)
temp1$name<-NA
temp1<-rbind(temp1, temp2[which(is.na(temp2$name)), ])
temp2<-temp2[which(!is.na(temp2$name)), ]
temp1$name<-str_extract(temp1$text1, off2)
tempII<-rbind(temp1, temp2)
tempII<-tempIIa[order(tempIIa$id),]
tempa<-tempII[which(is.na(tempII$name)),]
tempb<-tempII[which(!is.na(tempII$name)),]
tempa$text1<-str_replace_all(tempa$text1, "[.](\\s)[,]", ",")
tempa$text1<-str_replace_all(tempa$text1, "[.][,]", ",")
tempa$name<-str_extract(tempa$text1, rgx2)
tempb<-rbind(tempb, tempa[which(!is.na(tempa$name)),])
tempa<-tempa[which(is.na(tempa$name)),]
tempa$name<-str_extract(tempa$text1, off2)
temp<-rbind(tempa, tempb)
temp<-temp[order(temp$id),]
temp$name<-str_replace_all(temp$name, "E M", "")
del<-c("^the(\\s)", "(\\s)the(\\s)", "^with(\\s)", "(\\s)with", "time", "progress", "they(\\s)", "mining", "company")
del<-paste(del, collapse="|")
temp$name<-str_replace_all(temp$name, "[^[:alpha:]]", " ")
temp$name<-str_replace_all(temp$name, "(\\s+)", " ")
temp$name<-str_replace_all(temp$name, "^NA", "")
temp$lastname<-lname(temp$name)
temp$firstname<-fname(temp$name)
temp$middlename<-mname(temp$name)
temp$simpname<-sname(temp)
temp$edu<-ifelse(!is.na(str_extract(temp$text1, "E(\\s)M[.,]")), "mining eng", NA)
temp<-subset(temp, select=c("id", "role", "name", "lastname", "firstname", "middlename", "simpname", "edu"))
amm<-left_join(amm, temp, by="id" , match="all")
amm$location<-repetir(amm$location, llenar, 100)
amm$company<-repetir(amm$company, llenar, 100)
amm<-subset(amm, select=c("id","source","year","company", "stock", "office", "location", "yinc", "locinc", "role" ,"simpname", "lastname", "firstname", "middlename", "edu", "text"))
colnames(amm)<-c("id","source","year","company", "stock", "office", "loc", "yinc", "locinc", "role" ,"simpname", "lastname", "firstname", "middlename", "edu", "text")
lloc<-buscar2(amm)
amm<-left_join(amm, lloc, by="loc", match="all")
amm<-amm[order(amm$id), ]
amm<-subset(amm, select=c("id","source","year","company", "stock", "office", "loc","lon","lat", "yinc", "locinc", "role" ,"simpname", "lastname", "firstname", "middlename", "edu", "text"))
offamm<-subset(amm, select=c("id","simpname", "lastname", "firstname", "middlename", "edu", "role", "company","loc","lon","lat", "text"))
offamm<-offamm[which(!is.na(offamm$simpname)),]
amm$id<-paste("amm", amm$id, sep="")
year<-subset(amm, select=c("id", "year"))
offamm$id<-paste("amm", offamm$id, sep="")
offamm<-left_join(offamm, year, by="id", match="all")
write.csv(amm, "amm2.csv")
write.csv(offamm, "offamm.csv")
####III. 2. MMM####
mmm<-cargar("mmm0.csv")
mmm<-subset(mmm, select=c("id", "place", "company", "text", "capitalusd", "capitalpound", "capitalfr", 
                          "capitalpeso", "minerals", "owns", "owned", "doutput", "technology", "incor", "name", 
                          "role", "loc"))
mmm$lastname<-lname(mmm$name)
mmm$middlename<-mname(mmm$name)
mmm$firstname<-fname(mmm$name)
mmm$simpname<-sname(mmm)
mmm$loc<-str_replace_all(mmm$loc, "[^[:alpha:]]", " ")
mmm$loc<-str_replace_all(mmm$loc, "location|mill|plant|smelter", " ")
mmm$loc<-str_replace_all(mmm$loc, "(\\s+)", " ")
mmm$loc<-str_replace_all(mmm$loc, "(\\s)po(\\s)", " ")
mmm$loc<-str_replace_all(mmm$loc, "^po(\\s)", " ")
mmm$loc<-str_replace_all(mmm$loc, "(\\s)p(\\s)o(\\s)", " ")
mmm$loc<-str_replace_all(mmm$loc, "d(\\s)f(\\s)", "df")
mmm$loc<-str_replace_all(mmm$loc, "b(\\s)c(\\s)", "bc")
mmm$loc<-str_replace_all(mmm$loc, "(\\s+)", " ")
mmm$loc<-trimws(mmm$loc)
mmm$loc<-ifelse(!is.na(str_extract(mmm$loc, "(\\w+)(\\s)(\\w+)(\\s)(\\w+)$")), str_extract(mmm$loc, "(\\w+)(\\s)(\\w+)(\\s)(\\w+)$"), 
                ifelse(!is.na(str_extract(mmm$loc, "(\\w+)(\\s)(\\w+)$")), str_extract(mmm$loc, "(\\w+)(\\s)(\\w+)$"), 
                       ifelse(!is.na(str_extract(mmm$loc, "(\\w+)$")), str_extract(mmm$loc, "(\\w+)$"),  
                              NA)))
lmm<-buscar(mmm)
mmm<-left_join(mmm, lmm, by="loc", match="all")
mmm<-subset(mmm, select=c("id", "place", "company", "text", "capitalusd", "capitalpound", "capitalfr", 
                          "capitalpeso", "minerals", "owns", "owned", "doutput", "technology", "incor", "name", 
                          "role", "lastname", "middlename", "firstname", "simpname", "loc", "lon", "lat"))
colnames(mmm)<-c("id", "loc", "company", "text", "capitalusd", "capitalpound", "capitalfr", 
                 "capitalpeso", "minerals", "owns", "owned", "doutput", "technology", "incor", "name", 
                 "role", "lastname", "middlename", "firstname", "simpname", "loc2", "lon2", "lat2")
lmm<-buscar2(mmm)
mmm<-left_join(mmm, lmm, by="loc", match="all")
mmm$loc<-ifelse(!is.na(mmm$loc2), mmm$loc2, mmm$loc)
mmm$lat<-ifelse(!is.na(mmm$lat2), mmm$lat2, mmm$lat)
mmm$lon<-ifelse(!is.na(mmm$lon2), mmm$lon2, mmm$lon)
mmm<-subset(mmm, select=c("id", "company", "text", "capitalusd", "capitalpound", "capitalfr", 
                          "capitalpeso", "minerals", "owns", "owned", "doutput", "technology", "incor", "name", 
                          "role", "lastname", "middlename", "firstname", "simpname", "loc", "lon", "lat"))
write.csv(mmm, "mmm1.csv")
####III. 2. 1. MMM capital####
exchange<-read.csv("exchange.csv")
mmm<-read.csv("mmm1.csv")
mmm$X<-NULL
mmm$year<-1926
mmm<-left_join(mmm, exchange, by="year", match="all")
mmm$capitalpound<-as.numeric(mmm$capitalpound)
mmm$capitalpeso<-as.numeric(mmm$capitalpeso)
mmm$capitalusd<-as.numeric(mmm$capitalusd)
mmm$capitalfr<-as.numeric(mmm$capitalfr)
mmm$capital<-ifelse(!is.na(mmm$capitalpound), (mmm$capitalpound*mmm$poundtousd), 
                    ifelse(!is.na(mmm$capitalusd), (mmm$capitalusd), 
                           ifelse(!is.na(mmm$capitalpeso), (mmm$capitalpeso*mmm$mxtousd), 
                                  ifelse(!is.na(mmm$capitalfr), (mmm$capitalfr*mmm$frtousd), 
                                         NA))))
mmm$id<-paste("mmm", mmm$id, sep="")
write.csv(mmm, "mmm2.csv")
#### III. 3. Individuales####
lmyb<-subset(rbind(read.csv("mmybo1.csv"), read.csv("mmybo2.csv"), read.csv("mmybo3.csv")),
             select=c("id", "loc", "lon", "lat"))
mmmoff<-subset(read.csv("mmm2.csv"), select=c("id","year", "company", "text", "role", "simpname", "lastname", "middlename", "firstname", "loc", "lon", "lat"))
mmmoff<-mmmoff[which(!is.na(mmmoff$simpname)),]
mmmoff$source<-"mmm"
mmmoff$edu<-NA
mmyboff<-subset(read.csv("officers.csv"), select=c("id", "year","company", "text", "role", "simpname", "lastname", "middlename", "firstname"))
mmyboff<-left_join(mmyboff, lmyb, by="id", match="all")
mmyboff$id<-paste("myb", mmyboff$id, sep="")
mmyboff$edu<-NA
mmyboff$source<-"myb"
ammoff<-read.csv("offamm.csv")
ammoff<-subset(ammoff, select=c("id", "year", "company", "text",  "role", "simpname", "lastname", "middlename", "firstname", "loc", "lon", "lat", "edu"))
ammoff$source<-"amm"
off<-rbind(ammoff, mmyboff, mmmoff)
off<-off[order(off$simpname, off$year, off$lastname, off$firstname),]
write.csv(off, "off.csv")
####IV. Analysis. ####
####IV.1 MMYB####
#### IV.1.a number of companies####
myb<-rbind(read.csv("myb1.csv"), read.csv("myb2.csv"), read.csv("myb3.csv"))
myb<-subset(myb, select=c("id", "year","yearg", "company", "text", "office", "situated", "mine","loc", "lon", "lat"))
myb$country<-coords2country(myb)
crp<-read.csv("crp3a.csv")
capital<-subset(crp, select=c("id", "capital"))%>% distinct()
myb<-left_join(myb, capital)
capital2<-subset(myb, select=c("year","company", "capital"))%>% distinct()
capital2<-subset(capital2, !is.na(capital2$capital))
myb$capital<-NULL
myb<-left_join(myb, capital2)
tablemyb<- ddply(unique(subset(myb, select=c("year", "company", "capital"))), 
                 c("year"), 
                 summarise, 
                 Nocompanies=length(year),
                 capital=sum(capital, na.rm = TRUE))
tablemyb2<- ddply((unique(subset(myb, select=c("yearg", "company", "capital")))), 
                  c("yearg"), 
                  summarise, 
                  Nocompanies=length(yearg), 
                  capital=sum(capital, na.rm = TRUE))
tablemyb3<- ddply((unique(subset(myb, select=c("year", "company", "capital", "country")))), 
                  c("year", "country"), 
                  summarise, 
                  Nocompanies=length(year), 
                  capital=sum(capital, na.rm = TRUE))
tablemyb4<- ddply((unique(subset(myb, select=c("year", "company", "capital", "country")))), 
                  c("country"), 
                  summarise, 
                  Nocompanies=length(year), 
                  capital=sum(capital, na.rm = TRUE))
tempmyb<-tablemyb2[which(tablemyb2$year>1830),]
tempmyb<-tempmyb[which(tempmyb$year<1930),]
tempmyb %>% 
  ggplot(aes(x = yearg, y = Nocompanies))+
  geom_point(aes(x = yearg, y = Nocompanies), color="Blue")+
  geom_line()+
  scale_x_continuous(breaks = round(seq(min(1800), max(1919), by = 1),1))+
  theme(axis.text.x = element_text(size=7, angle=90))+
  ggtitle("Number of companies by year of incorporation, 1830-1923")
  transition_reveal(yearg)

tablemyb$CapitalperCompany<-tablemyb$capital/tablemyb$Nocompanies
tablemyb %>% 
  ggplot(aes(x = year, y = Nocompanies))+
  geom_point(aes(x = year, y = Nocompanies, size=CapitalperCompany),alpha=0.5)+
  geom_line()+
  theme(axis.text.x = element_text(size=7, angle=90))+
  scale_x_continuous(breaks = round(seq(min(tablemyb$year), max(tablemyb$year), by = 1),1))+
  ggtitle("Number of companies registered ath the MMYB, 1887-1923")
  transition_reveal(year)

tablemyb3$kperfirm<-tablemyb3$capital/tablemyb3$Nocompanies

tablemyb3 %>% 
  ggplot()+
  geom_point(aes(x = capital, y = Nocompanies, size=kperfirm, color=country), alpha=0.7)+
  theme(axis.text.x = element_text(size=7, angle=90))+
  scale_size(range = c(5, 30), name="Capital per company") +
  ggtitle("Number of companies and capital by country, 1887-1923")+
  theme(legend.position = "none")+
  transition_time(year) +
  labs(title = 'Year:{frame_time}' , x = "Capital", y = "Number of companies")
anim_save("tablemyb.gif")

tablemyb4$kperfirm<-tablemyb4$capital/tablemyb4$Nocompanies
intertest<-tablemyb4 %>% 
  ggplot()+
  geom_point(aes(x = capital, y = Nocompanies, size=kperfirm, color=country), alpha=0.3)+
  scale_size(range = c(5, 30), name="Capital per company") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  #theme.ipsum()+
  theme(legend.position = "none")+
  ggtitle("Number of companies and capital by country, 1887-1923")

ggplotly(intertest, tooltip="all")


#### IV. 1. b capital####
mybk<-ddply(myb, 
            c("year"),
            summarise, 
            capital=sum(capital, na.rm = TRUE))
mybk%>%
  ggplot(aes(x=year, y=capital))+
  geom_point()+
  geom_line()
table<-merge(tablemyb, mybk, by="year")
table$mean<-table$capital/table$Nocompanies
table%>%
  ggplot(aes(x=year, y=mean))+
  geom_point()+
  geom_line()

#### IV. 1. c location####
off<-read.csv("off.csv")
off$X<-NULL
off2<-off[which(!is.na(off$lon)),]
off2<-st_as_sf(off2, coords=c("lon", "lat"))
data("World")
off2anim<-tm_shape(World)+ tm_polygons()+
  tm_shape(off2)+tm_dots()+  tm_symbols()+  tm_facets(along="year", free.coords = F)

tmap_animation(off2anim, filename = "off2_anim.gif", delay = 35)



tmap_mode("view")
tmtest<-tm_shape(off2, name="Officials in the the MMYB")+tm_dots(col="year")+tm_symbols()
tmtest
tmap_save(tmtest, "tmtest.html")


#base+ geom_point(data=off, aes(x=lon, y=lat, fill=year), color = "white", shape=21, size=3.0, alpha=0.5)+
# coord_fixed(ratio = 1.1)+
#  scale_colour_distiller(palette="Purples", name="year", guide = "colorbar")+
#  ggtitle("Locations of firms, MMYB")+
#  theme(plot.title = element_text(hjust = 0.5))

####IV. consultas####
amm<-read.csv("amm3.csv")
mmm<-read.csv("mmm2.csv")
myb<-rbind(read.csv("myb1.csv"), read.csv("myb2.csv"), read.csv("myb3.csv"))
officers<-read.csv("officers.csv")
mmm$source<-"mmm"
myb$source<-"myb"
officers$id<-paste("myb", officers$id, sep="")
amm<-subset(amm, select=c("id", "source","year", "company", "capital", "lastname","firstname","middlename","role","loc", "lon","lat","text"))
mmm<-subset(mmm, select=c("id", "source","year", "company",  "capital","lastname","firstname","middlename","role", "loc", "lon","lat","text" ))
myb<-subset(myb, select=c("id", "source","year", "company",  "capital", "loc","lon","lat",  "text"))
officers<-subset(officers, select=c("id",  "lastname", "firstname", "middlename", "role"))
myb<-left_join(myb, officers, by="id", match="all")
crp2<-rbind(amm, mmm, myb)
crp2$simpname<-sname(crp2)

heckstein<-c("H. Eckstein")
heckstein<-consulta(crp2, "company", heckstein)
heckstein<-heckstein[which((heckstein$company!="COMPANY LIMITED")&
                             (heckstein$company!="MINING COMPANY LIMITED")&
                             (heckstein$company!="EXPLORATION COMPANY LIMITED")&
                             (heckstein$company!="MINES LIMITED")),]
heckstein<-heckstein[which(!is.na(heckstein$simpname)), ]
heckstein<-heckstein[which(!is.na(heckstein$lon)), ]
hecksteinsf<-st_as_sf(heckstein, coords=c("lon", "lat"))
tmap_mode("view")
hecksteinm<-tm_shape(hecksteinsf)+tm_dots(col="company", id="simpname", palette="Blues")+tm_symbols()
hecksteinm
tmap_save(hecksteinm, "hecksteinm.html")
write.csv(hecksteinsf, "hecksteinsf.csv")
#apparajith
mysore<-c("MYSORE", "Mysore")
mysore<-consulta(crp2, "company", mysore)
mysore<-mysore[which((mysore$company!="COMPANY LIMITED")&
                       (mysore$company!="MINING COMPANY LIMITED")&
                       (mysore$company!="EXPLORATION COMPANY LIMITED")&
                       (mysore$company!="MINES LIMITED")),]
mysore<-mysore[which(!is.na(mysore$simpname)), ]
mysore<-mysore[which(!is.na(mysore$lon)), ]
mysoresf<-st_as_sf(mysore, coords=c("lon", "lat"))
tmap_mode("view")
mysorem<-tm_shape(mysoresf)+tm_dots(col="company", id="simpname", palette="Blues")+tm_symbols()
mysorem
tmap_save(mysorem, "mysorem.html")
write.csv(mysoresf, "mysoresf.csv")


####V. Test####
amm<-read.csv("amm2.csv")
mmm<-read.csv("mmm2.csv")
myb<-rbind(read.csv("myb1.csv"), read.csv("myb2.csv"), read.csv("myb3.csv"))
combo<-read.csv("combo4.csv")
amm<-subset(amm, select=c("id", "year", "company", "stock", "office", "yinc", "locinc", "loc", "text"))
mmm<-subset(mmm, select=c("id", "year", "company",  "capital", "minerals", "owns",  "owned",  "doutput", "technology", "incor", "loc", "text" ))
myb<-subset(myb, select=c("id", "year", "company",  "capital",  "office", "yearg", "situated", "mine", "loc",  "text"))
combo<-subset(combo, select=c("id",  "lastname", "firstname", "middlename", "prof"))
#sample variable
mybt<-muestra(myb, "myb")
mmmt<-muestra(mmm, "mmm")
ammt<-muestra(amm, "amm")
mybt<-left_join(mybt, combo, by="id", match="all")
mmmt<-left_join(mmmt, combo, by="id", match="all")
ammt<-left_join(ammt, combo, by="id", match="all")
write.csv(ammt, "ammt.csv")
write.csv(mmmt, "mmmt.csv")
write.csv(mybt, "mybt.csv")
#for amm



#### V. Together ####
amm<-read.csv("amm2.csv")
mmm<-read.csv("mmm2.csv")
myb<-rbind(read.csv("myb1.csv"), read.csv("myb2.csv"), read.csv("myb3.csv"))
amm<-subset(amm, select=c("id",  "text", "year", "company", "capital","role", "simpname", "lastname", "firstname","middlename", "edu", "loc","lon", "lat"))
mmm<-subset(mmm, select=c("id", "year", "company",  "capital", "minerals","doutput", "technology", "loc","lon", "lat", "text" ))
myb<-subset(myb, select=c("id", "year", "company",  "capital",  "loc",  "text"))
colnames(mmm)<-c("id", "year", "company",  "capital", "minerals","output", "technology", "loc","lon", "lat", "text" )
crp<-rbind.fill(mmm, amm, myb)
crp<-crp[which((crp$company!=lag(crp$company))|(crp$lon!=lag(crp$lon))), ]
write.csv(crp, "crp.csv")

crp<-read.csv("crp.csv")
crp$company<-str_replace_all(crp$company, "[(][s|S]ubsidiary(\\s)of(\\s)", "")
crp$company<-str_replace_all(crp$company, "[)]$", "")
crp$company<-trimws(crp$company)
crp$company<-str_replace_all(crp$company, "[^[:alpha:]]", " ")
crp$company<-str_replace_all(crp$company, "(\\s+)", " ")
crp$company<-str_replace_all(crp$company, "^(\\s+)", "")
crp$company<-ifelse(str_detect(crp$company, "MINING(\\s)DIRECTORY|MINES(\\s)HANDBOOK|MINING(\\s)MANUAL"), NA, crp$company)
crp$company<-tolower(crp$company)
crp$company<-str_replace(crp$company, "(\\b)co(\\b)", "company")
crp$company<-str_replace(crp$company, "(\\b)limited(\\b)", "ltd")
crp$company<-ifelse(!str_detect(crp$company, "(?<=^company(\\b)).*|(?<=^ltd(\\b)).*"),
  str_replace(crp$company, "(?<=(\\b)company(\\b)).*|(?<=(\\b)ltd(\\b)).*", ""),
  crp$company)
crp$company<-str_replace(crp$company, "(\\b)co(\\b)", "company")
crp$company<-str_replace(crp$company, "(\\b)limited(\\b)", "ltd")
crp$company<-str_replace_all(crp$company, "^(\\s+)", "")
errors<-subset(crp, str_detect(crp$company, "^(\\w+)$"))
crp<-subset(crp, select=c("id", "company"))
write.csv(crp, "crptemporal.csv")
crp2<-read.csv("crptemp2.csv")%>%subset(select=c("id", "company"))
crp2$comp2<-crp2$company
crp2$company<-NULL
crp<-read.csv("crp.csv")%>%subset(select=c("id", "company"))
crp$company<-str_replace_all(crp$company, "[(][s|S]ubsidiary(\\s)of(\\s)", "")
crp$company<-str_replace_all(crp$company, "[)]$", "")
crp$company<-trimws(crp$company)
crp$company<-str_replace_all(crp$company, "[^[:alpha:]]", " ")
crp$company<-str_replace_all(crp$company, "(\\s+)", " ")
crp$company<-str_replace_all(crp$company, "^(\\s+)", "")
crp$company<-ifelse(str_detect(crp$company, "MINING(\\s)DIRECTORY|MINES(\\s)HANDBOOK|MINING(\\s)MANUAL"), NA, crp$company)
crp$company<-tolower(crp$company)
crp$company<-str_replace(crp$company, "(?<=(\\b)co(\\b)).*|(?<=(\\b)company(\\b)).*|(?<=(\\b)limited(\\b)).*|(?<=(\\b)ltd(\\b)).*", "")
crp$company<-str_replace(crp$company, "(\\b)co(\\b)", "company")
crp$company<-str_replace(crp$company, "(\\b)limited(\\b)", "ltd")
correcion<-left_join(crp, crp2)
correcion$comp2<-str_replace_all(correcion$comp2, "ñ", "n")
correcion<-correcion[which(correcion$company!=correcion$comp2), ]
correcion$company<-correcion$comp2
correcion$comp2<-NULL
write.csv(correcion, "correcion2.csv", row.names = F)

####VI. Corrections####
####VI. 1. With combo####
combo<-read.csv("combo5.csv")
crp<-read.csv("crp.csv")
orgs<-subset(combo, select=c("id", "org"))
orgs$org<-ifelse(is.na(orgs$org), "NULL", orgs$org)
orgs<-distinct(orgs)
nulls<-subset(orgs, str_detect(orgs$org, "NULL"))
crp<-left_join(crp, orgs)
nulls<-subset(crp, str_detect(crp$org, "NULL"))
matches<-subset(crp, select=c("company", "org"))
matches<-distinct(matches)
matches<-subset(matches, !is.na(matches$org))
crp$org<-NULL
crp<-left_join(crp, matches)
crp$company<-ifelse(!is.na(crp$org), crp$org, crp$company)
crp$company<-str_replace_all(crp$company, "[^[:alpha:]]", " ")
for(i in 1:10){crp$company<-str_replace_all(crp$company, "(\\s+)", " ")}
write.csv(crp, "crp2a.csv", row.names = F)

####VI. 2. With capital####
amm<-read.csv("amm3.csv")
mmm<-read.csv("mmm2.csv")
myb<-rbind(read.csv("myb1.csv"), read.csv("myb2.csv"), read.csv("myb3.csv"))
amm<-subset(amm, select=c("id",  "text", "year", "company", "capital","role", "simpname", "lastname", "firstname","middlename", "edu", "loc","lon", "lat"))
mmm<-subset(mmm, select=c("id", "year", "company",  "capital", "minerals","doutput", "technology", "loc","lon", "lat", "text" ))
myb<-subset(myb, select=c("id", "year", "company",  "capital",  "loc",  "text"))
colnames(mmm)<-c("id", "year", "company",  "capital", "minerals","output", "technology", "loc","lon", "lat", "text" )
crp1<-rbind.fill(mmm, amm, myb)
combo<-read.csv("combo5.csv")
crp0<-read.csv("crp2a.csv")
crp<-left_join(crp1, 
               subset(crp0, select=c("id","company" , "capital", "technology")))
crp<-fill(crp, company,.direction = "down")
crp$company<-str_replace(tolower(crp$company), "sub(\\s)of(\\s)", "")
crp$company<-ifelse(str_detect(tolower(crp$company), "(?<=ubsidiary(\\s)of(\\s)).*(\\b)co[.]"), 
                    str_extract(tolower(crp$company), "(?<=ubsidiary(\\s)of(\\s)).*(\\b)co[.]"), crp$company)
crp$company<-ifelse(str_detect(tolower(crp$company), "(?<=ontrolled(\\s)by(\\s)).*(\\b)co[.]"), 
                    str_extract(tolower(crp$company), "(?<=ontrolled(\\s)by(\\s)).*(\\b)co[.]"), crp$company)
crp$company<-ifelse(str_detect(tolower(crp$company), "(?<=filiation(\\s)of(\\s)).*(\\b)co[.]"), 
                    str_extract(tolower(crp$company), "(?<=filiation(\\s)of(\\s)).*(\\b)co[.]"), crp$company)
crp$company<-str_replace(tolower(crp$company), "sub(\\s)of(\\s)", "")
crp$company<-ifelse(str_detect(tolower(crp$company), "(?<=ubsidiary(\\s)of(\\s)).*(\\b)ltd[.]"), 
                    str_extract(tolower(crp$company), "(?<=ubsidiary(\\s)of(\\s)).*(\\b)ltd[.]"), crp$company)
crp$company<-ifelse(str_detect(tolower(crp$company), "(?<=ontrolled(\\s)by(\\s)).*(\\b)ltd[.]"), 
                    str_extract(tolower(crp$company), "(?<=ontrolled(\\s)by(\\s)).*(\\b)ltd[.]"), crp$company)
crp$company<-ifelse(str_detect(tolower(crp$company), "(?<=filiation(\\s)of(\\s)).*(\\b)ltd[.]"), 
                    str_extract(tolower(crp$company), "(?<=filiation(\\s)of(\\s)).*(\\b)ltd[.]"), crp$company)
crp$company<-str_replace(tolower(crp$company), "sub(\\s)of(\\s)", "")
crp$company<-ifelse(str_detect(tolower(crp$company), "(?<=ubsidiary(\\s)of(\\s)).*company"), 
                    str_extract(tolower(crp$company), "(?<=ubsidiary(\\s)of(\\s)).*company"), crp$company)
crp$company<-ifelse(str_detect(tolower(crp$company), "(?<=ontrolled(\\s)by(\\s)).*company"), 
                    str_extract(tolower(crp$company), "(?<=ontrolled(\\s)by(\\s)).*company"), crp$company)
crp$company<-ifelse(str_detect(tolower(crp$company), "\\(af[:alpha:]{4,}(\\s)of(\\s)"), 
                    str_replace(tolower(crp$company), "\\(af[:alpha:]{4,}(\\s)of(\\s)", ""), crp$company)
crp$company<-ifelse(str_detect(tolower(crp$company), "affiliation(\\s)of(\\s)"), 
                    str_replace(tolower(crp$company), "affiliation(\\s)of(\\s)", ""), crp$company)
crp$company<-ifelse(str_detect(tolower(crp$company), ".*(\\b)co[.]"), 
                    str_extract(tolower(crp$company), "^.*(\\b)co[.]"), crp$company)
crp$company<-ifelse(str_detect(tolower(crp$company), ".*(\\b)company"), 
                    str_extract(tolower(crp$company), "^.*(\\b)company"), crp$company)
crp$company<-ifelse(str_detect(tolower(crp$company), ".*(\\b)ltd[.]"), 
                    str_extract(tolower(crp$company), "^.*(\\b)ltd[.]"), crp$company)
crp$company<-ifelse(str_detect(tolower(crp$company), ".*(\\b)inc[.]"), 
                    str_extract(tolower(crp$company), "^.*(\\b)inc[.]"), crp$company)
crp$company<-str_replace_all(crp$company, "\\(|\\)", "")
crp$company<-ifelse(str_detect(tolower(crp$company), "anaconda.*copper"), "Anaconda Copper Company", crp$company)
crp$company<-ifelse(str_detect(tolower(crp$company), "american.*smelting"), "American Smelting Refining Co", crp$company)
crp$company<-ifelse(str_detect(tolower(crp$company), "united(\\s)states(\\s)smelting.*refining|u[.]*(\\s)*s[.]*(\\s)smelting.*refining"), "USSRM", crp$company)
crp$company<-str_to_title(crp$company)
for( i in 2:nrow(crp)){
  if(!is.na(crp$company[i])&str_detect(crp$company[i], "^Company$|^Mines(\\s)Company|^Mining(\\s)Company")){
    crp$company[i]<-crp$company[i-1]
  }
}
crp$cat<-str_extract(crp$id, "[:alpha:]+")
crp$text2<-str_replace_all(crp$text, "(?<=(\\d))[o|O]|[o|O](?=(\\d))", "0")
crp$text2<-str_replace_all(crp$text2, "(?<=(\\d))[l|I|L]|[l|I|L](?=(\\d))", "1")
crp$text2<-str_replace_all(crp$text2, "(\\b)[s|S](?=(\\d))", "$")
crp$text2<-str_replace_all(crp$text2, "(?<=(\\d))[s|S]|[s|S](?=(\\d))", "5")
crp$text2<-str_replace_all(crp$text2, "(?<=(\\d))[z|Z]|[z|Z](?=(\\d))", "2")
crp$text2<-trimws(str_replace_all(crp$text2,"(?<=[$|?])[C|G|0]", "6"))
crp$text2<-trimws(str_replace_all(crp$text2,"(?<=at(\\s))8(?=(\\d))", "?"))
crp$text2<-ifelse(str_detect(crp$text2, "[,](\\d){3}(\\d+)(\\w)*[:punct:]"),
                  str_replace_all(crp$text2,"(?<=[,](\\d){3})(?=(\\d))", " "), 
                  crp$text2)
crp$text2<-ifelse(str_detect(crp$text2, "19(\\d){4}"),
                  str_replace(crp$text2,"(?<=19(\\d){2})(?=(\\d))", " "), 
                  crp$text2)
crp$text2<-ifelse(str_detect(tolower(crp$text2), "capital(\\s)(\\w+)(\\s)2(\\d){3}"),
                  str_replace(crp$text2,"(?<=s(\\s))2(?=(\\d){3})", "?"), 
                  crp$text2)
bign<-c("(?<=19(\\d){2})(\\s+)(?=(\\d))", 
        "(?<=18(\\d){2})(\\s+)(?=(\\d))", 
        "(?<=(\\d){3})(\\s+)(?=(\\d))", 
        "(?<=[?|$](\\d){3})[,]*(\\d)*[,]*(\\d)*[,]*(\\d)*(\\s)(\\d)",
        "(?<=[,](\\d){3})(\\s)(\\d)*(\\s)i",
        "(?<=[,](\\d){3}[,])(\\s)(\\d)*[,]*(\\d)*(\\s)i", 
        "(\\d)(\\s+)(\\d+)[d|s|p]", 
        "(\\d){3}[,](\\s)(\\d){1,2}[,](\\d)") %>%paste(collapse="|")
crp$text2<-ifelse(!str_detect(crp$text2, bign), 
                  trimws(str_replace_all(crp$text2,"(?<=(\\d))(\\s+)(?=[.|,])|(?<=(\\d))(\\s+)(?=(\\d))", "")), crp$text2)
crp$text2<-ifelse(!str_detect(crp$text2, bign), 
                  str_replace_all(crp$text2, "(?<=[,])(\\s)(?=[:digit:])", ""), crp$text2)
crp$text2<-trimws(str_replace_all(crp$text2,"(?<=[$|?])(\\s+)(?=(\\d))", ""))
crp$text2<-trimws(str_replace_all(crp$text2,"(\\s+)", " "))
temp0<-subset(crp, !is.na(crp$capital)&crp$cat=="mmm")
temp1<-subset(crp, !is.na(crp$capital)&crp$cat!="mmm"|is.na(crp$capital))
#temp2<-subset(crp, is.na(crp$capital))
temp1$text2<-str_replace_all(temp1$text2, "(?<=[$|?])I", "1")
temp1$text2<-str_replace_all(temp1$text2, "(?<=[$|?])l", "1")
temp1$text2<-str_replace_all(temp1$text2, "(?<=)(\\b)[|](?=(\\d))", "1")
temp1$text2<-str_replace_all(temp1$text2, "(?<=[$|?])0", "6")
temp1$text2<-str_replace_all(temp1$text2, "[.](?=000)", ",")
temp1$text2<-str_replace_all(temp1$text2, "\\[|\\]", "")
temp1$text2<-ifelse(!is.na(temp1$capital), 
                    str_replace_all(temp1$text2, "(?<=[$|?])(\\s)*(?=[,](\\d))", "1"), 
                    temp1$text2)
temp1$capital<-NULL
#temp2$capital<-NULL
###haciendo para temp 1###

kmyb<-c("capital(\\s)[:alpha:]*(\\s)*[:alpha:]*(\\s)*[$|?](\\d)*[,]*(\\d)*[,]*(\\d)*[,]*(\\d+)", 
        "capital(\\s)[:alpha:]*(\\s)*[:alpha:]*(\\s)*(\\d)*[,]*(\\d)*[,]*(\\d)*[,]*(\\d+)yen", 
        "capital(\\s)[:alpha:]*(\\s)*[:alpha:]*(\\s)*(\\d)*[,]*(\\d)*[,]*(\\d)*[,]*(\\d+)fr",
        "capital(\\s)[:alpha:]*(\\s)*[:alpha:]*(\\s)*(\\d)*[,]*(\\d)*[,]*(\\d)*[,]*(\\d+)boliv", 
        "capital(\\s)[:alpha:]*(\\s)*[:alpha:]*(\\s)*(\\d)*[,]*(\\d)*[,]*(\\d)*[,]*(\\d+)peso", 
        "capital(\\s)[:alpha:]*(\\s)*[:alpha:]*(\\s)*(\\d)*[,]*(\\d)*[,]*(\\d)*[,]*(\\d+)")%>%
  paste(collapse="|")
kamm<-c("(?<=capital[,.:](\\s)[$|?|p])(\\d)*[,]*(\\d)*[,]*(\\d)*[,]*(\\d+)", 
        "(?<=capital(\\s)[$|?|p])(\\d)*[,]*(\\d)*[,]*(\\d)*[,]*(\\d+)",
        "(?<=capital[,.:])(\\s)*(\\d)*[,]*(\\d)*[,]*(\\d)*[,]*(\\d+)", 
        "(?<=capital)(\\s)*(\\d)*[,]*(\\d)*[,]*(\\d)*[,]*(\\d+)", 
        "(?<=[$|?|p])(\\d)*[,]*(\\d)*[,]*(\\d)*[,]*(\\d+)",
        "(?<=capital)(\\s)*(\\d)*[,]*(\\d)*[,]*(\\d)*[,]*(\\d+)(\\s)*(?=peso)", 
        "(?<=capital[,.:])(\\s)*(\\d)*[,]*(\\d)*[,]*(\\d)*[,]*(\\d+)(\\s)*(?=peso)")%>%
  paste(collapse="|")
temp1am<-temp1[which(temp1$cat=="amm"),]
temp1my<-temp1[which(temp1$cat=="myb"),]
temp1mm<-subset(temp1, temp1$cat=="mmm")
temp1mm$capital<-NA
temp1am$text2<-ifelse(!str_detect(temp1am$text2,"[,](\\s)(?=(\\d){1,2}[,])"), 
                      str_replace_all(temp1am$text2, "(\\s)(?=[,])", ""), temp1am$text2)
temp1am$text2<-str_replace_all(temp1am$text2, "S[t|4][a|o|u|0|6][c|e|s|6]k", "Capital")
temp1am$text2<-str_replace_all(temp1am$text2, "[.](?=(\\d){3})", ",")
temp1am$text2<-str_replace_all(tolower(temp1am$text2), "(?<=capital)[,|.]*(\\s)*[3|5|9|8](?=[1-9]{1,2}[,])", "$")
temp1am$text2<-str_replace_all(tolower(temp1am$text2), "(?<=capital)[,|.][3|5|9|8](?=[1-9]{2})", "$")
temp1am$capital<-str_extract(temp1am$text2, kamm)
temp1am$capital<-ifelse(str_detect(temp1am$capital, "[$|?]"), temp1am$capital,
  str_replace_all(tolower(temp1am$capital), "[3|5|9|8](?=[1-9]{1}(\\d){2})|(\\b)[3|5|9|8](?=[1-9]{1}(\\d){0,1}[,])", "$"))
temp1my$text2<-ifelse(!str_detect(temp1my$text2,"[,](\\s)(?=(\\d){1,2}[,])"), 
                      str_replace_all(temp1my$text2, "(\\s)(?=[,])", ""), temp1my$text2)
temp1my$capital<-str_extract(tolower(temp1my$text2), kmyb)
temp1my$capital<-str_replace_all(temp1my$capital, "(?<=[:alpha:])(\\s)*[3|5|9|8](?=[123456789]{1,2}[,])|(?<=[:alpha:])(\\s)*[3|5|9|8](?=(\\d){3})|(?<=[:alpha:][,])(\\s)*[3|5|9|8](?=(\\d){3})", " $")
temp1b<-rbind(temp1my, temp1am, temp1mm)
temp1b$capital<-ifelse(str_detect(tolower(temp1b$text2), "(\\b)r[e|o]is(\\b)")&!is.na(temp1b$capital), 
                       str_extract(temp1b$text2, "[$|?](\\d)*[,]*(\\d)*[,]*(\\d)*[,]*(\\d+)"), temp1b$capital)
temp1b$currency<-ifelse(is.na(temp1b$capital), NA, 
                 ifelse(str_detect(temp1b$text2, "pounds|?"),"pounds", 
                 ifelse(str_detect(temp1b$text2, "francs"),"francs",
                 ifelse(str_detect(temp1b$text2, "yens"), "yens",  
                 ifelse(str_detect(temp1b$text2, "usd|[$]"),"usd", 
                 ifelse(str_detect(temp1b$text2, "peso"), "peso", 
                 ifelse(str_detect(temp1b$text2, "peseta"), "peseta", 
                 ifelse(str_detect(temp1b$text2, "boliv"),"boliv",
                 ifelse(str_detect(temp1b$text2, "rupee"),"rupee",
                 ifelse(str_detect(temp1b$text2, "florin"),"florin",
                 ifelse(str_detect(temp1b$text2, "(\\b)kr[.]"),"krona",
                 ifelse(temp1b$cat=="myb", "pounds", "usd"))))))))))))
temp1b$capital<-str_replace_all(temp1b$capital, "[^[:digit:].]", "")%>%as.character() %>%as.numeric()
temp0$currency<-NA
crp2<-rbind(temp0, temp1b)
crp2<-crp2[order(crp2$cat, as.numeric(as.character(str_extract(crp2$id, "(\\d+)")))),]
crp2$capital<-ifelse(crp2$capital<100, NA, crp2$capital)
###para temp 2###
temp1<-subset(crp2, !is.na(crp2$capital))
temp2<-subset(crp2, is.na(crp2$capital))
temp2$capital<-NULL
temp2$shares<-ifelse(str_detect(temp2$text2, "shares")&
                       !str_detect(temp2$text2, "[Q|q]ualific"),
                     str_extract(temp2$text2, "(\\s)(\\d)*[,|.]*(\\d)*(\\b)(\\s+)fully|(\\s)(\\d)*[,|.]*(\\d)*(\\b)(\\s+)share"), NA) %>%
  trimws()
at<-c("[$|?](\\d+)(?=(\\s)each)",
      "(\\b)(\\d+)[$|?](?=(\\s)ea(\\w)h)",
      "(?<=(\\b)at)(\\s)*(\\d+)[$|?]", 
      "(?<=(\\b)of)(\\s)*(\\d+)[$|?](\\s)*ea(\\w)h",
      "(?<=(\\b)at)(\\s)*[$|?]*(\\d)*(\\s)*(\\d)*[d|s|p](\\s)*(\\d)*[d|s|p]*(\\b)", 
      "(?<=(\\b)of)(\\s)*[$|?]*(\\d)*(\\s)*(\\d)*[d|s|p](\\s)*(\\d)*[d|s|p]*(\\b)", 
      "(?<=(\\b)at)(\\s)*[$|?]*(\\d)*(\\s)*(\\d)*(\\s)*cent", 
      "(?<=(\\b)of)(\\s)*[$|?]*(\\d)*(\\s)*(\\d)*(\\s)*cent", 
      "(?<=(\\b)at)(\\s)*[$|?](\\d){1,3}(?!=[,])", 
      "(?<=(\\b)of)(\\s)*[$|?](\\d){1,3}(\\s)*ea(\\w)h")
temp2$at<-NA
for(i in 1:length(at)){
  temp2$at<-ifelse(!is.na(temp2$shares)&is.na(temp2$at), 
                   str_extract(temp2$text, at[i]), temp2$at)  
}
temp2$at<-ifelse(str_detect(temp2$at, "^(\\s)*$"), NA, temp2$at) %>% trimws()
temp2$curr0<-ifelse(str_detect(temp2$at, "[?|p|(?!<=s)d]"), "pounds", 
                    ifelse(str_detect(temp2$at, "[$|cent]"), "usd", "error")) 
temp2$at0<-str_replace(temp2$at, "(\\b)(\\d+)[d|s|p]|(\\b)(\\d+)(\\s)*cent", "")%>%trimws()
temp2$at0<-ifelse(str_detect(temp2$at0, "^(\\s)*$"), NA, temp2$at0)
temp2$at0<-str_replace_all(temp2$at0, "[^[:digit:].]", "") %>% as.character() %>% as.numeric()
temp2$at1<-str_extract(temp2$at, "(\\b)(\\d+)d")
temp2$at2<-str_extract(temp2$at, "(\\b)(\\d+)p|(\\b)(\\d+)(\\s)*cent")
temp2$at3<-str_extract(temp2$at, "(\\b)(\\d+)s")
temp2$at1<-as.numeric(as.character(str_replace_all(temp2$at1, "[^[[:digit:].]]", "")))/240
temp2$at2<-as.numeric(as.character(str_replace_all(temp2$at2, "[^[[:digit:].]]", "")))/100
temp2$at3<-as.numeric(as.character(str_replace_all(temp2$at3, "[^[[:digit:].]]", "")))/20
temp2$at<-rowSums(cbind(temp2$at0, temp2$at1, temp2$at2, temp2$at3), na.rm=T)
temp2[,c("at0","at1", "at2", "at3")]<-list(NULL)
library(tidyverse)
prices<-temp2 %>%
  dplyr::group_by(company, year, at) %>%
  dplyr::summarise(n=length(company), .groups = "keep")
prices2<-subset(prices, !is.na(prices$at)&prices$at!=0)
prices2<-prices2[order(prices2$company, prices2$year, prices2$n, decreasing=T),]
prices3<-prices2 %>%
  dplyr::group_by(company, year) %>%
  dplyr::summarise(max=max(n), .groups = "keep")
prices2<-left_join(prices2, prices3)
prices2<-subset(prices2, prices2$n==prices2$max)
prices2$n<-NULL
temp2<-left_join(temp2, prices2)
temp2$at<-ifelse(is.na(temp2$max), NA, temp2$at)
temp2$shares<-str_replace_all(temp2$shares, "^0", "6")
temp2$shares<-str_replace_all(temp2$shares, "[.](?=(\\d){3})", "")
temp2$shares<-ifelse(str_detect(temp2$shares, "[,](\\d){4}(\\s+)share"), 
                     str_replace(temp2$shares, "(?<=[,](\\d){3})(\\d)", ""), temp2$shares)
temp2$shares<-as.numeric(as.character(str_replace_all(temp2$shares, "[^[[:digit:].]]", "")))
temp2$cap0<-temp2$shares*temp2$at
kap0<-ifelse(!is.na(temp2$shares), 
             str_extract_all(temp2$text2, "[?|$](\\d+)[,]*(\\d+)[,]*(\\d)*(\\b)"), NA)
kap1<-ifelse(
  str_detect(tolower(temp2$text2), "(\\b)capital(\\b)")&str_detect(temp2$text, "?(\\d+)[,]*(\\d+)"), 
  str_extract_all(temp2$text2, "?(\\d+)[,|.]*(\\d+)[,|.]*(\\d+)"), 
  str_extract_all(tolower(temp2$text2), "capital(\\s)*(\\w+)(\\s)*[:punct:]*(\\s)*(\\d+)[,|.]*(\\d+)(\\s)*[,|.]*(\\d*)(\\s)*(\\w)*"))
temp2$cap1<-makevector(kap0, 1)
temp2$cap2<-makevector(kap0, 2)
temp2$cap3<-makevector(kap0, 3)
temp2$cap4<-makevector(kap1, 1)
temp2$cap5<-makevector(kap1, 2)
temp2$cap6<-makevector(kap1, 3)
temp2$cap0<-as.character(temp2$cap0)
###puting it together with temmp1b###
temp1$cap7<-as.character(temp1$capital)
temp1$capital<-NULL
temp1$curr0<-temp1$currency
temp3<-rbind.fill(temp1, temp2)
temp3$cap4<-str_replace_all(temp3$cap4, "(?<=[:alpha:])(\\s)*[3|5|9|8](?=[123456789]{1,2}[,])|(?<=[:alpha:])(\\s)*[3|5|9|8](?=(\\d){3})|(?<=[:alpha:][,])(\\s)*[3|5|9|8](?=(\\d){3})", " $")
temp3<-pivot_longer(temp3, cols=c("cap0", "cap1", "cap2", "cap3", "cap4", "cap5", "cap6", "cap7"), names_to = "typek", values_to = "values")
temp3<-subset(temp3, !is.na(temp3$values))
temp3<-subset(temp3, !str_detect(temp3$values, "drachma"))
temp3$currency<-NULL
temp3$currency<-ifelse(!is.na(temp3$curr0), temp3$curr0, 
                ifelse(str_detect(temp3$values, "?")|str_detect(temp3$text2, "pounds|?"),"pounds", 
                ifelse(str_detect(temp3$values, "(\\d+)(\\s+)fr")|str_detect(temp3$text2, "francs"),"francs",
                ifelse(str_detect(temp3$values, "(\\d+)(\\s+)yen")|str_detect(temp3$text2, "yens"), "yens",  
                ifelse(str_detect(temp3$values, "[$]")|str_detect(temp3$text2, "usd|[$]"),"usd", 
                ifelse(str_detect(temp3$values, "(\\d+)(\\s+)peso")|str_detect(temp3$text2, "peso"), "peso",  
                ifelse(str_detect(temp3$values, "boliv")|str_detect(temp3$text2, "boliv"),"boliv",
                ifelse(str_detect(temp3$values, "rupee")|str_detect(temp3$text2, "rupee"),"rupee",
                NA))))))))
for( i in 1:nrow(temp3)){
  if(is.na(temp3$currency[i])&str_detect(temp3$values[i], "is(\\s)2")){
  temp3$currency[i]<-"pounds"
  temp3$values[i]<-str_replace(temp3$values[i], "(?<=is(\\s))2", "?")
    }
}
temp3$currency<-ifelse(is.na(temp3$currency), "pounds", temp3$currency)
temp3$fraction1<-str_extract(temp3$values, "(\\d+)d|(\\s)(\\d){1,2}$")
temp3$fraction2<-str_extract(temp3$values, "(\\d+)p|(\\d+)cent")
temp3$fraction3<-str_extract(temp3$values, "(\\d+)s|(?<=(\\d){3})(\\s)(\\d){1,2}(\\b)")
temp3$fraction1<-as.numeric(as.character(str_replace_all(temp3$fraction1, "[^[[:digit:].]]", "")))/240
temp3$fraction2<-as.numeric(as.character(str_replace_all(temp3$fraction2, "[^[[:digit:].]]", "")))/100
temp3$fraction3<-as.numeric(as.character(str_replace_all(temp3$fraction3, "[^[[:digit:].]]", "")))/20
temp3$values<-str_replace_all(temp3$values, "(?<=(\\d){2})(\\s)*[:alpha:].*|(\\d+)cent.*|(\\s)(\\d){1,2}$.*|(?<=(\\d){3})(\\s)(\\d){1,2}(\\b).*", "")
temp3$values<-ifelse(str_detect(temp3$typek, "cap4|cap1"), 
                     str_replace(temp3$values, "(?<=(\\d))[.](?=(\\d){3})", ","), 
                     temp3$typek)
temp3$values<-str_replace_all(temp3$values,"[^[:digit:].]", "")
temp3$values<-as.numeric(as.character(temp3$values))
temp3$values<-rowSums(cbind(temp3$values, temp3$fraction1, temp3$fraction2, temp3$fraction3), na.rm=T)
temp3[,c("fraction1", "fraction2", "fraction3")]<-list(NULL)
exchange<-read.csv("exchange2.csv")
temp3<-merge(temp3, exchange, by=c("year", "currency"))
temp3$kapital<-temp3$rate*temp3$values
temp3$max<-NULL
temp3<-subset(temp3, !is.na(temp3$kapital)&temp3$kapital>100)
kapitals<- temp3 %>%
  dplyr::group_by(company, year, kapital) %>%
  dplyr::summarise(n=length(company), .groups = "keep")
kapitals2<-kapitals %>%
  dplyr::group_by(company, year) %>%
  dplyr::summarise(max=max(n), .groups = "keep")
kapitals3<-left_join(kapitals, kapitals2)
kapitals3<-subset(kapitals3, (kapitals3$n==kapitals3$max))
kapitals3$n<-NULL
temp4<-left_join(temp3, kapitals3)
temp4<-temp4[order(temp4$year, temp4$id),]
temp4<-subset(temp4, !is.na(temp4$max)|temp4$typek=="cap7")
kapitals3<-temp4 %>%
  dplyr::group_by(company, year) %>%
  dplyr::summarise(capital=max(kapital), .groups = "keep")
temp3b<-subset(rbind.fill(temp1, temp2), select=-c(curr0, max, cap0, cap1, cap2, cap3, cap4, cap5, cap6, cap7,text2))
temp3b<-left_join(temp3b, kapitals3)
#crpb<-rbind.fill(temp1, temp3b)
crpb<-temp3b[order(temp3b$cat, 
                 as.numeric(as.character(str_extract(temp3b$id, "(\\d+)")))),]  
shares<-subset(crpb, select=c("year", "company", "shares", "at", "currency")) %>%distinct()

kapitals4<-subset(crpb,!is.na(crpb$capital)) %>%
  dplyr::group_by(company, year) %>%
  dplyr::summarise(capital=max(capital), .groups = "keep")
kapitals4<-kapitals4[order(kapitals4$company, kapitals4$year),]
kapitals4$capital<-as.numeric(as.character(kapitals4$capital))
kapitals4$growth<-ifelse(
  (kapitals4$company==lag(kapitals4$company)&((kapitals4$capital/lag(kapitals4$capital))<0.7)), 
  "error1", ifelse(
    (kapitals4$company==lead(kapitals4$company)&((kapitals4$capital/lead(kapitals4$capital))<0.4)), 
    "error2", NA))
kapitals4$capital<-ifelse(!is.na(kapitals4$growth), 
                          ifelse(kapitals4$growth=="error1", lag(kapitals4$capital), lead(kapitals4$capital)), 
                          kapitals4$capital)
kapitals4$growth<-NULL
shares<-subset(temp3, !is.na(temp3$shares)|!is.na(temp3$at), select=c("id", "shares", "at"))%>%distinct()
crp$capital<-NULL
crpfinal<-left_join(crp, kapitals4)
crpfinal2<-crpfinal[which((crpfinal$company!=lag(crpfinal$company))|(crpfinal$lon!=lag(crpfinal$lon))|is.na(crpfinal$lon)), ]
write.csv(crpfinal, "crp3bruto.csv", row.names=F)
write.csv(crpfinal2, "crp2b.csv", row.names=F)
write.csv(shares, "shares1.csv", row.names=F)

#### VI. 3.Making it together####
myb<-rbind(read.csv("myb1.csv"), read.csv("myb2.csv"), read.csv("myb3.csv"))
myb$X<-NULL
amm<-read.csv("amm3.csv")
mmm<-read.csv("mmm2.csv")
ama<-read.csv("ama.csv")
crp<-read.csv("crp2b.csv")
myb<-fill(myb, company, .direction="down")
temp<-distinct(subset(myb, select=c("lon", "lat")), lon, lat)
temp$country<-coords2country(temp)
myb<-left_join(myb, temp, by=c("lon", "lat"), match="all")
officers<-read.csv("officers11.csv")
officers$id<-paste("myb",officers$id, sep="")
myb<-left_join(myb, subset(officers, select=c("id","role", "simpname")), by="id", match="all")
ama$simpname<-sname(ama)
amm$company2<-str_extract(amm$company, "(?<=[(]Subsidiary(\\s)of(\\s))(.*?)(?=[,])")
amm$company<-ifelse(!is.na(amm$company2), amm$company2, amm$company)
amm$company<-str_replace_all(amm$company, "8[.]", "B.")
amm$company<-str_replace_all(amm$company, "[(]", "C")
amm$company<-str_replace_all(amm$company, "[^[:alpha:].&]", " ")
for (i in 1:5){
  amm$company<-str_replace_all(amm$company, "(\\s+)", " ")
  amm$company<-str_replace_all(amm$company, "^(\\s+)|^[:punct:]", "")
  amm$company<-trimws(amm$company, which=c("both"))
}
amm$company<-str_to_title(amm$company)
amm<-amm[order(amm$company, as.numeric(str_extract(amm$id, "(\\d+)"))), ]
amm<-subset(amm, select=-c(X, source, lastname,firstname, middlename, company2))
amm<-amm[order(amm$company, amm$year),]
temp<-distinct(subset(amm, select=c("lon", "lat")), lon, lat)
temp$country<-coords2country(temp)
amm<-left_join(amm, temp, by=c("lon", "lat"), match="all")
amm<-amm[order(as.numeric(str_extract(amm$id, "(\\d+)"))), ]
mmm$country<-"mexico"
mmm$year<-1926
mmm$text<-NA
together<-rbind(
  subset(myb, select=c("id", "year", "company", "text", "loc", "lon", "lat", "role", "simpname", "country")), 
  subset(amm, select=c("id", "year", "company","text", "loc", "lon", "lat", "role", "simpname", "country")), 
  subset(mmm, select=c("id", "year", "company","text", "loc", "lon", "lat", "role", "simpname", "country")))
#crp$company<-ifelse(is.na(crp$org), crp$company, crp$org)
crp2<-subset(crp, select=c("id", "company"))
llavec<-left_join(subset(together, select=c("id", "company")), crp2, by="id")
colnames(llavec)<-c("id", "company", "org")
llavec$company<-ifelse(str_detect(llavec$company, "^Company$"), NA, llavec$company)
for(i in 1:300){
  llavec$org<-ifelse(is.na(llavec$org)&!is.na(lead(llavec$org))&
                       !is.na(llavec$company)&!is.na(lead(llavec$company))&
                       (llavec$company==lead(llavec$company)), lead(llavec$org), llavec$org)
  llavec$org<-ifelse(is.na(llavec$org)&!is.na(lag(llavec$org))&
                       !is.na(llavec$company)&!is.na(lag(llavec$company))&
                       (llavec$company==lag(llavec$company)), lag(llavec$org), llavec$org)
}
llavec$company<-ifelse(!is.na(llavec$org), llavec$org, llavec$company)
llavec$org<-llavec$company
llavec$company<-NULL
together<-left_join(together, llavec)
together$errors<-ifelse(((together$id==lag(together$id))&(together$org!=lag(together$org)))|((together$id==lead(together$id))&(together$org!=lead(together$org))), 
                        1, 0)
together$org<-ifelse(together$errors==1, together$company, together$org)
together$org<-ifelse(str_detect(tolower(together$text), "amalgamated(\\s)copper")|
                       str_detect(tolower(together$company), "amalgamated(\\s)copper")|
                       str_detect(tolower(together$org), "amalgamated(\\s)copper")|
                       str_detect(tolower(together$text), "anaconda(\\s)copper")|
                       str_detect(tolower(together$company), "anaconda(\\s)copper")|
                       str_detect(tolower(together$org), "anaconda(\\s)copper"), 
                     "Anaconda Copper Company", together$org)
together$org<-ifelse(str_detect(tolower(together$text), "asarco")|
                       str_detect(tolower(together$company), "asarco")|
                       str_detect(tolower(together$org), "asarco")|
                       (str_detect(tolower(together$text), "american(\\s)smelting")&str_detect(tolower(together$text), "refining(\\s)[co|00|c0|0o]"))|
                       (str_detect(tolower(together$company), "american(\\s)smelting")&str_detect(tolower(together$company), "refining(\\s)[co|00|c0|0o]"))|
                       (str_detect(tolower(together$org), "american(\\s)smelting")&str_detect(tolower(together$org), "refining(\\s)[co|00|c0|0o]")), 
                     "American Smelting Refining Co", together$org)
together$org<-ifelse(nchar(together$company)>50, 
                     str_extract(tolower(together$company), 
                                 "^.*?company|^.*?corp|^.*?limited|^.*?(\\b)ltd(\\b)|^.*?(\\b)co(\\b)"), 
                     together$org)
together$company<-ifelse(!is.na(together$org), together$org, together$company)
largas<-c("(?<=[,]).*?company", 
          "(?<=[,]).*?corp", 
          "(?<=[,]).*?limited",
          "(?<=[,]).*?(\\b)ltd(\\b)",
          "(?<=[,]).*?(\\b)co(\\b)")%>%paste(collapse="|")
together$company<-ifelse(nchar(together$company)>50, 
                     str_extract(tolower(together$company), largas), 
                     together$company)
together$company<-ifelse(!is.na(together$org), together$org, together$company)
together$errors<-NULL
together<-distinct(together)
together$company<-str_replace_all(together$company, "(?<=Company)[.,].*$", "")
together$company<-str_replace_all(together$company, "0r0", "Oro")
for(i in 1:5){
  together$company<-str_replace(together$company, "^(\\s+)|^[:punct:]+|^(\\d)", "")}
together$company<-trimws(together$company, which="both")
together$company<-str_to_title(together$company)
cias<-subset(together, select=c("id", "company"))
cias$org<-cias$company
cias$company<-NULL
crp<-left_join(crp, cias, by="id")
capital<-subset(crp, select=c("id", "capital"))%>% distinct()
together<-left_join(together, capital)
capital2<-subset(together, select=c("year","company", "capital"))%>%distinct()
capital2<-subset(capital2, !is.na(capital2$capital))
together$capital<-NULL
together<-left_join(together, capital2)
crp$fakeid<-str_extract(crp$id, "(\\d+)")%>%as.character()%>%as.numeric()
crp<-crp[order(crp$cat, crp$fakeid),]
crp3<-crp[which((crp$company!=lag(crp$company))|(crp$lon!=lag(crp$lon))|is.na(crp$lon)), ]
crp3$fakeid<-NULL
locs<-subset(crp3, select=c("loc", "lon", "lat"))%>%distinct()
locs<-subset(locs, !is.na(locs$lon))
crp3a<-crp3[which(is.na(crp3$loc)|!is.na(crp3$lon)),]
crp3b<-crp3[which(!is.na(crp3$loc)&is.na(crp3$lon)),]
crp3b$lon<-NULL
crp3b$lat<-NULL
crp3b$loc<-str_replace_all(crp3b$loc, "(\\b)sooth(\\b)", "south")
crp3b<-left_join(crp3b, locs)
crp3<-rbind(crp3a, crp3b)
crp3a<-crp3[which(is.na(crp3$loc)|!is.na(crp3$lon)),]
crp3b<-crp3[which(!is.na(crp3$loc)&is.na(crp3$lon)),]
crp3b$lon<-NULL
crp3b$lat<-NULL
locs<-read.csv("locs.csv")
locs$lon<-ifelse(is.na(locs$lon), "error", locs$lon)
crp3b<-left_join(crp3b, locs)
crp3<-rbind(crp3a, crp3b)%>%distinct()
crp3$loc<-ifelse(crp3$lon=="error", NA, crp3$loc)
capital<-subset(crp3, select=c("year", "company", "capital"))%>%distinct()
capital<-capital[order(capital$company, capital$year),]
for (i in 1:10){
  capital$capital<-ifelse((capital$company==lead(capital$company))&!is.na(lead(capital$capital))&is.na(capital$capital), 
                          lead(capital$capital), capital$capital)
  capital$capital<-ifelse((capital$company==lag(capital$company))&!is.na(lag(capital$capital))&is.na(capital$capital), 
                          lag(capital$capital), capital$capital)
}
crp3$capital<NULL
crp3<-left_join(crp3, capital)
crpfinal<-crp3[which((crp3$company!=lag(crp3$company))|(crp3$lon!=lag(crp3$lon))|
                       (crp3$loc!=lag(crp3$loc))|!is.na(crp3$simpname)|
                       !is.na(crp3$capital)|!is.na(crp3$minerals)|
                       !is.na(crp3$technology)|!is.na(crp3$output)), ]
crpfinal$org<-NULL
write.csv(crpfinal, "crp3.csv", row.names=F)
write.csv(together, "together.csv", row.names=F)



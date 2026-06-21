library(tidyverse)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
lista<- list.files(recursive=T)
info<- list.files(recursive=T) %>% 
  file.info()
data<-data.frame(file=lista, size=info$size) 
small<-data %>%
  subset(size<=100000000)
rest<-data %>%
  subset(size>100000000)
dir.create("largefiles")
dir.create("gitfiles")


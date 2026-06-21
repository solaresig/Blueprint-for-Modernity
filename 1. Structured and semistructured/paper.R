####Appendix####
library(tidyverse)
library(ggmap)
library(rworldmap)
library(rworldxtra)
library(dplyr)
####Appendix####
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
combo<-read.csv("combo5_3.csv")
crp<-read.csv("crp3.csv")
emj<-read_csv("emjdbase1.csv")
emj<-subset(emj, emj$type=="Title"|emj$type=="Text")
combo<-combo[order(combo$simpname, combo$year),]
crp<-crp[order(crp$company, crp$year),]
crp<-distinct(crp, company, year)
crpag<-distinct(combo, org, year)
colnames(crpag)<-c("company", "year")
crp<-rbind(crp, crpag)
crp<-subset(crp, !is.na(crp$company)&!is.na(crp$year))
crp<-crp[order(crp$company, crp$year),]
combo$first<-ifelse(combo$simpname!=lag(combo$simpname), 1, 0)
combo$first[1]<-1
crp$first<-ifelse(crp$company!=lag(crp$company), 1, 0)
crp$first[1]<-1
tablecrp<-crp %>%
          dplyr::group_by(year) %>%
          dplyr::summarize(n=sum(first, na.rm=T))
tablecrp$acc<-tablecrp$n
for(i in 2:nrow(tablecrp)){
  tablecrp$acc[i]<-tablecrp$acc[i-1]+tablecrp$n[i]
}
tablecom<-combo %>% 
  group_by(year)%>%
  summarize(n=sum(first, na.rm=T))
tablecom$acc<-tablecom$n
for(i in 2:nrow(tablecom)){
  tablecom$acc[i]<-tablecom$acc[i-1]+tablecom$n[i]
}
tableemj<-dplyr::count(emj, year)
tableemj$acc<-tableemj$n
for(i in 2:nrow(tableemj)){
  tableemj$acc[i]<-tableemj$acc[i-1]+tableemj$n[i]
}

mini<-min(tablecom$year[which(!is.na(tablecom$year))])
maxi<-max(tablecom$year[which(!is.na(tablecom$year))])
tablecrp2<-data.frame(year=seq(mini, maxi, by=1))
tablecrp2<-left_join(tablecrp2, tablecrp, by="year")
tablecrp2$n[which(is.na(tablecrp2$n))]<-0
tablecrp2$acc<-tablecrp2$n
for(i in 2:nrow(tablecrp2)){
  tablecrp2$acc[i]<-tablecrp2$acc[i-1]+tablecrp2$n[i]
}

tableemj2<-data.frame(year=seq(mini, maxi, by=1))
tableemj2<-left_join(tableemj2, tableemj)
tableemj2$n[which(is.na(tableemj2$n))]<-0
tableemj2$acc<-tableemj2$n
for(i in 2:nrow(tableemj2)){
  tableemj2$acc[i]<-tableemj2$acc[i-1]+tableemj2$n[i]
}

generalt<-tablecom %>% mutate(type="Individuals") %>% rbind(
  tablecrp2 %>% mutate(type="Firms")) %>% rbind(
    tableemj2 %>% mutate(type="Journal"))


jpeg(file="../totalacc.jpg", width=600, height=400)
ggplot(generalt, aes(x=year, y=acc, fill=type))+
  geom_col()+ scale_fill_grey()+
  theme(legend.position = "bottom", plot.caption = element_text(hjust = 0.5))+
  labs(x="Year", y="Accumulated n", title="Figure A1: Individuals, firms and journals in the sample")
dev.off()

plot(tablecom$year, tablecom$acc, xlab="Year", ylab="Accumulated", type="h")
plot(tablecrp$year, tablecrp$acc, xlab="Year", ylab="Accumulated", type="h")
plot(tableemj$year, tableemj$acc, xlab="Year", ylab="Accumulated", type="h")

crp<-read.csv("crp3.csv")
emj<-read_csv("emjdbase0b.csv")
emj<-subset(emj, !is.na(emj$lon))
crp$lon<-crp$lon%>%as.character()%>%as.numeric()
crp$lat<-crp$lat%>%as.character()%>%as.numeric()
length(combo$lon[which(!is.na(combo$lon))])
length(unique(combo$simpname[which(!is.na(combo$lon))]))
length(crp$lon[which(!is.na(crp$lon))])
length(unique(crp$company[which(!is.na(crp$lon))]))
corporateloc<-subset(crp, select=c("lon", "lat"))
emjloc<-subset(emj, select=c("lon", "lat")) 
individualsloc<-subset(combo, select=c("lon", "lat"))
corporateloc$type<-"Firms"
emjloc$type<-"Journal"
individualsloc$type<-"Individuals"

totalloc<-rbind(individualsloc, corporateloc, emjloc)
totalloc<-totalloc %>% count(type, lon, lat)

base<- getMap(resolution = "high")
base2<- ggplot(data = base, aes(x=long, y = lat)) + geom_polygon(data = base, aes(x=long, y = lat, group = group),  fill = "white", color="white")
base3<-ggplot(data = base, aes(x=long, y = lat)) + geom_polygon(data = base, aes(x=long, y = lat, group = group),  fill = "grey", color="grey")

jpeg(file="../crpmap.jpg", width=600, height=300)
base2+geom_count(data=crp[which(!is.na(crp$lon)),], aes(x=lon, y=lat), color="black", fill="black", alpha=0.5)+
  theme(legend.position = "none", plot.caption = element_text(hjust = 0.5))+
  scale_size_area(max_size = 10)
dev.off()
jpeg(file="../cmbmap.jpg", width=600, height=300)
base2+geom_count(data=combo[which(!is.na(combo$lon)),], aes(x=lon, y=lat), color="black", fill="black", alpha=0.5)+
  theme(legend.position = "none", plot.caption = element_text(hjust = 0.5))+
  scale_size_area(max_size = 10)
dev.off()
jpeg(file="../emjmap.jpg", width=600, height=300)
base2+geom_count(data=emj[which(!is.na(emj$lon)),], aes(x=lon, y=lat), color="black", fill="black", alpha=0.5)+
  theme(legend.position = "none", plot.caption = element_text(hjust = 0.5))+
  scale_size_area(max_size = 10)
dev.off()

jpeg(file="../totalmap2.jpg", width=800, height=400)
base2+geom_count(data=totalloc, aes(x=lon, y=lat, size=n, color=type, fill=type), alpha=0.2)+
  scale_fill_grey(start=0, end=0.75)+
  scale_color_grey(start=0, end=0.75)+
  scale_size_area(max_size = 25)
dev.off()


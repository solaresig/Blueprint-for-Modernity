library(tidyverse)
library(stringi)
library(rworldmap)
library(rworldxtra)
library(tidyverse)
library(tmap)
library(tmaptools)
coordstoloc<-function(df){
  start<-Sys.time()
  locn<-rep(NA, nrow(df))
  for(i in 1:nrow(df)){
    x<-tryCatch(rev_geocode_OSM(x=df$lon[i], y=df$lat[i], zoom=8),
                error=function(e) NA)
    locn[i]<-ifelse(is.na(x), NA, 
                      ifelse(is.null(x[[1]]$name), NA, x[[1]]$name))
    progr<-100*i/(nrow(df))
    elap<-difftime(Sys.time(), start, units="mins")
    eta<-elap*(100-progr)/progr
    print(paste("Progress:", progr, "%"))
    print(paste("Elapsed time:", as.numeric(elap), "mins"))
    print(paste("Remaining time:", as.numeric(eta), "mins"))
  }
  return(locn)
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
sname<-function(x){
  y<-ifelse(!is.na(x$lastname), 
            paste(x$lastname, ", ", substr(x$firstname, 1, 1), " " ,
                  ifelse(!is.na(substr(x$middlename, 1, 1)), substr(x$middlename, 1, 1),""), sep=""), 
            NA)
  y<-tolower(trimws(y))
  return(y)
}

muestra<-function(df, source){
  n<-ceiling((1.96^2)*((0.5*(1-0.5)/(0.05^2)))/(1+(0.5*(1-0.5)/((0.05^2)*nrow(df)))))
  y<-df[sample(nrow(df), n, replace=F), ]
  #y$id<-as.numeric(as.character((str_replace_all(y$id, ".*[^[:digit:]]", ""))))
  y<-y[order(y$id),]
  #y$id<-paste(source, y$id, sep="")
  return(y)
}
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
####0. mining  EMJ####
#####0.1 calculating shares######
lista<- list.files("../2. Unstructured/ocr/vols/")
info<- list.files(paste(getwd(), "../2. Unstructured/ocr/vols/", sep="/"), full.names = T) %>% 
  file.info()
info<-info$size
shares<-data.frame()
for(i in 1:length(lista)){
  emj<-read_csv(paste0("../2. Unstructured/ocr/vols/",lista[i])) %>%
    select(c(x1, x2, y1, y2, wide, height, type, page))
  emj$area<-(emj$x2-emj$x1)*(emj$y2-emj$y1)
  share<-emj %>% group_by(page, type) %>% 
    summarize(area=sum(area), 
              total=mean(height)*mean(wide)) 
  share$vol<-str_replace(lista[i], "[.]csv", "")
  shares<-rbind(shares, share)
  rm(emj, share)
  progress<-100*info[i]/sum(info)
  print(progress)  
}
shares2<-shares %>% group_by(page, type) %>% 
  pivot_wider(names_from=type, values_from=area) %>%
  ungroup()  %>% group_by(vol) %>%
  summarize(Page=sum(Page, na.rm=T)/sum(total), 
            Text=sum(Text, na.rm=T)/sum(total), 
            Title=sum(Title, na.rm=T)/sum(total), 
            Date=sum(Date, na.rm=T)/sum(total), 
            Author=sum(Author, na.rm=T)/sum(total)) 
shares2$vol<-shares2$vol %>% as.character()  %>% as.numeric()
shares2 <- shares2 %>% arrange(vol) %>%
  mutate(Total=Page+Text+Title+Date+Author)
write_csv(shares2, "shares.csv")


##### 0.2 preparing datasets for extraction#####
###### 0.2. 1 reverse geocoding######
combo<-read_csv("combo5_3.csv")
crp<-read_csv("crp3.csv")
crp$company<-stri_trans_general(crp$company, 'latin-ascii')
crp$company<-str_replace_all(crp$company,"[^[:alpha:]]", " ") 
crp$company<-str_replace_all(crp$company,"^(\\s)+|(?<=(\\s))(\\s)|[:punct:](\\s)*$|^(\\s)*[:punct:](\\s)*|[.](\\s)*$|(\\d)", "")
crp$lon<-ifelse(crp$lon=="error", NA, crp$lon)
crp$lon<-as.numeric(crp$lon)
combo$org<-stri_trans_general(combo$org, 'latin-ascii')
combo$org<-str_replace_all(combo$org,"[^[:alpha:]]", " ") 
combo$org<-str_replace_all(combo$org,"^(\\s)+|(?<=(\\s))(\\s)|[:punct:](\\s)*$|^(\\s)*[:punct:](\\s)*|[.](\\s)*$|(\\d)", "")
locsa<-subset(combo, !is.na(combo$lon),select=c("lon","lat")) %>%distinct()
locsb<-subset(crp, !is.na(crp$lon),select=c("lon","lat")) %>%distinct()
locs<-rbind(locsa, locsb) %>% distinct()
locs0<-coordstoloc(locs)
locs0$locn<-locs0$loc
locs0$loc<-NULL
locs0$locn<-str_replace_all(locs0$locn, "[,](\\s)*(\\d+)[,]", "")
locs0$locn<-str_replace_all(locs0$locn, "[,](\\s)*[,]", ",")
locs0<-subset(locs0, !is.na(locs0$lon)&locs0$lon!="error")
locs0$lon<-as.numeric(locs0$lon)
combo3<-left_join(combo, locs0)
crp3<-left_join(crp, locs0)
locs<-read_csv("integration/cities2.csv")
colnames(locs)<-c("loc", "lat", "lon", "country")
locs0$locn<-stri_trans_general(locs0$locn, 'latin-ascii')
locs0$loc<-str_extract(locs0$locn, ".*?(?=[,])")
locs0$reg<-str_extract(locs0$locn, "(?<=[,](\\s)).*?(?=[,])")
locs0$country<-str_extract(locs0$locn, "(?<=[,](\\s)).*?$")
locs0$country<-ifelse(str_detect(locs0$country, "(?<=[,](\\s)).*?$"), 
                      str_extract(locs0$country, "(?<=[,](\\s)).*?$"), locs0$country)
locs0$country<-ifelse(str_detect(locs0$country, "(?<=[,](\\s)).*?$"), 
                      str_extract(locs0$country, "(?<=[,](\\s)).*?$"), locs0$country)
locs$reg<-NA
locs0$locn<-NULL
locs<-rbind(locs, locs0)
locs<-locs %>% distinct(loc, country, .keep_all = T)
locs<-subset(locs, (nchar(locs$loc)>4)&(nchar(locs$loc)<15))
locr<-subset(locs, select=c(loc)) %>% distinct()
cour<-subset(locs, select=c(country)) %>% distinct()
colnames(locr)<-"reg"
colnames(cour)<-"reg"
write_csv(locs, "locs0b.csv")
write_csv(locr, "locr.csv")
write_csv(cour, "cour.csv")
write_csv(locs0, "locs0.csv")
write_csv(combo3, "combo5_3a.csv")
write_csv(crp3, "crp3_1.csv")
######0.2.2 name data#####
combo<-read_csv("combo5_3.csv")
names<-subset(combo, select=c("firstname","middlename","lastname", "simpname")) %>% distinct()
names$name<-paste(names$firstname, names$middlename, names$lastname)
names$name<-str_replace(names$name, "(\\s)NA(?=(\\s))", "")
names$id2<-row.names(names)
nombc<-subset(combo, select=c("firstname","middlename","lastname"), nchar(combo$firstname)>2) %>% distinct()
nombc$middlename<-ifelse(is.na(nombc$middlename), NA, 
                         ifelse(nchar(nombc$middlename)==1, paste(nombc$middlename, ".", sep=""), nombc$middlename))
nombc$fname<-paste(nombc$firstname, ifelse(!is.na(nombc$middlename), nombc$middlename, ""), nombc$lastname)
nombc$fname<-str_replace_all(nombc$fname, "(\\s+)", " ") %>% str_to_title()
write_csv(nombc, "nombc.csv")

######0.2. 3 corporate data#####
library(stringi)
combo<-read_csv("combo5_3.csv")
crp<-read_csv("crp3.csv")
crps<-c("(\\s)co(\\b)", "company", "(\\s)ltd(\\b)", "limited", "(\\s)cia(\\b)", "compania", "syndicate", "(\\s)synd(\\b)", 
        "mines") %>% paste(collapse = "|")
crp0<-subset(combo, str_detect(tolower(combo$org), crps)&nchar(combo$org)>10&nchar(combo$org)<50, select=c(org)) %>% distinct()
crp$company<-stri_trans_general(crp$company, 'latin-ascii')
crp$company<-str_replace_all(crp$company,"[^[:graph:]]", " ") 
crp$company<-str_replace_all(crp$company,"^(\\s)+|(?<=(\\s))(\\s)|[:punct:](\\s)*$|^(\\s)*[:punct:](\\s)*|[.](\\s)*$|(\\d)", "") 
crp1<-subset(crp, str_detect(tolower(crp$company), crps)&nchar(crp$company)>10&nchar(crp$company)<50, select=c(company)) %>% distinct()
colnames(crp0)<-"company"
crpa<-rbind(crp0, crp1) %>% distinct()
write_csv(crpa, "crps.csv")

######0.2.4 extracting variables#####
datafr<-read_csv("../2. Unstructured/ocr/emjdbase0.csv")
datafra<-subset(datafr, !str_detect(datafr$type,"Page|Date|Author"))
datafrb<-subset(datafr, str_detect(datafr$type,"Page|Date|Author"))
datafrb$loc<-NA
datafrb$countr0<-NA
datafra$text0<-datafra$text
datafra$text<-str_replace_all(datafra$text, "(\\s)[&](\\s)", " and ")
datafra$text<-str_replace_all(datafra$text, "[^[:alpha:]]", " ")
datafra$text<-str_replace_all(datafra$text, "[:punct:]|(\\s)(?=(\\s))", "")
datafra$text<-str_replace_all(datafra$text, "(\\s)(?=(\\s))", "")
datafra<-datafra %>%
  group_by(vol)%>%
  nest()

##try with country###
cl<-makeCluster(detectCores()-2, type="SOCK")
registerDoSNOW(cl)
datafra$data<-foreach(g = datafra$data) %dopar% {
  library(tidyverse)
  countr<-read_csv("integration/cour.csv")
  cou<-paste(unique(countr$reg), "(\\b)", sep="")
  locr<-read_csv("integration/locr.csv")
  loc<-paste(unique(locr$reg), "(\\b)", sep="")
  crps<-read_csv("integration/crps.csv")
  crps$reg<-str_replace_all(crps$reg, "[^[:alpha:]]", " ")
  crps$reg<-str_replace(crps$reg, "[:punct:]|(\\s)(?=(\\s))", "")
  crps$reg<-str_replace_all(crps$reg, "(\\s)(?=(\\s))", "")
  crps<-crps %>% distinct()
  crp<-unique(crps$reg)
  g$countr0<-extra(g, cou)
  g$loc<-extra(g, loc)
  g$crp<-extra(g, crp)
  vol<-g$id[1]
  progr<-file("progress.txt")
  writeLines(print(paste("Progress:", "vol",vol)), progr) 
  return(g)
}
stopCluster(cl)

datafra1<-datafra %>% unnest(cols=data)
datafra1$text<-datafra1$text0
datafra1$text0<-NULL
library(plyr)
temp<-rbind.fill(datafra1, datafrb)
write_csv(temp, "emjdbase0_1.csv")

#####0.3 reprocessing with locations####
library(ggmap)
locs<-read_csv("locsb.csv")
locs$n<-1
library(tidyverse)
datafr<-read_csv("emjdbase0_1.csv")
datafr<- datafr[order(datafr$page, datafr$r2),]
multiple<-distinct(locs, loc, country, .keep_all = T) %>%
  dplyr::group_by(loc)%>%
  dplyr::summarize(n=sum(n))
locs$n<-NULL
locs<-left_join(locs, multiple)
states<-read_csv("states.csv")
states<-unique(states$loc)
datafr1<-left_join(datafr, distinct(subset(locs, select=c("loc", "country", "n", "reg"))), multiple="all")
datafra<-subset(datafr1, is.na(datafr1$n)|datafr1$n==1)
datafrb<-subset(datafr1, !is.na(datafr1$n)&datafr1$n!=1)
datafrus<-subset(datafrb, str_detect(datafrb$country, "United(\\s)States"))
datafrus$state<-extra(datafrus, states)
datafral<-subset(datafrb, is.na(datafrb$country)|!str_detect(datafrb$country, "United(\\s)States"))
datafrus$sc<-ifelse(!is.na(datafrus$state), 1, 0)
datafral$sc<-ifelse(is.na(datafral$country)|(datafral$country==datafral$countr0), 1, 0)
datafral$state<-NA
datafrb<-rbind(datafral, datafrus)
datafrb<-datafrb[order(datafrb$id, datafrb$sc, datafrb$country),]
datafrb$sc<-ifelse(datafrb$countr0==datafrb$country, 1, datafrb$sc)
datafrb$sc[which(is.na(datafrb$sc))]<-0
datafrb<-datafrb %>%
  group_by(id, loc) %>%
  nest()
for(i in 1:nrow(datafrb)){
  dum<-sum(datafrb[[3]][[i]]$sc)
  if(dum==0){
    datafrb[[3]][[i]]$country<-NA
    datafrb[[3]][[i]]$sc[1]<-1}
}
datafrb<- datafrb %>% unnest(cols = c(data))
datafrb<-subset(datafrb, datafrb$sc>0)
datafrb$country<-ifelse(str_detect(datafrb$country, "United(\\s)States"),"United States", datafrb$country)
datafrb<-datafrb %>% distinct(id, loc, x1, country, .keep_all = T)
datafrb$n<-NULL
datafrb$reg<-ifelse(is.na(datafrb$reg), datafrb$state, datafrb$reg)
datafrb$loc<-ifelse(is.na(datafrb$country)&!is.na(datafrb$reg), datafrb$reg, 
                    ifelse(is.na(datafrb$country), datafrb$countr0, 
                           ifelse(!is.na(datafrb$reg), paste(datafrb$loc, datafrb$reg), datafrb$loc)))
datafrb$country<-ifelse(is.na(datafrb$country), datafrb$countr0, datafrb$country)
datafrb$state<-NULL
datafrb$sc<-NULL
datafra$n<-NULL
datafrf<-rbind(datafra, datafrb)
datafrf$loc<-ifelse(!is.na(datafrf$loc), datafrf$loc, datafrf$countr0)
datafrf$n<-NULL
datafrf$countr0<-NULL
datafrf<-left_join(datafrf, subset(locs, select=-c(n)), multiple="first")
write_csv(datafrf, "emjdbase0_2.csv")

datafr<-read_csv("emjdbase0_2.csv")
names<-read_csv("nombc.csv")
aut<-subset(datafr, !is.na(datafr$author))
aut$id1<-row.names(aut)
names$id2<-row.names(names)
aut$name<-str_to_lower(aut$author)
names$name<-str_to_lower(names$fname)
nombres<-merge_plus(subset(aut, select=c("id1","name"), (nchar(aut$name)>6)), 
                    names, by="name", unique_key_1="id1", unique_key_2="id2", match_type = 'fuzzy',
                    fuzzy_settings = build_fuzzy_settings(maxDist = .055))$matches
nombres<-subset(nombres, select=-c(id1, id2))
nombres<-nombres%>%distinct()
aut<-left_join(aut, nombres, by=join_by(name==name_1))
aut$name<-aut$name_2
aut<-subset(aut, select=-c(id1, name_2, tier))
library(plyr)
datafr<-rbind.fill(subset(datafr, is.na(datafr$author)), aut)
cor<-subset(datafr, !is.na(datafr$crp))
library(stringi)
combo<-read_csv("combo5_3a.csv")
crp<-read_csv("crp3_1.csv")
crp0<-subset(combo, select=c(org)) %>% distinct()
crp$company<-stri_trans_general(crp$company, 'latin-ascii')
crp$company<-str_replace_all(crp$company,"[^[:alpha:]]", " ") 
crp$company<-str_replace_all(crp$company,"^(\\s)+|(?<=(\\s))(\\s)|[:punct:](\\s)*$|^(\\s)*[:punct:](\\s)*|[.](\\s)*$|(\\d)", "") 
crp1<-subset(crp, nchar(crp$company)>10&nchar(crp$company)<50, select=c(company)) %>% distinct()
colnames(crp0)<-"company"
crpa<-rbind(crp0, crp1) %>% distinct()
crpa<-subset(crpa, !is.na(crpa$company))
cor$id1<-row.names(cor)
crpa$id2<-row.names(crpa)
cor$cname<-tolower(cor$crp)
crpa$cname<-tolower(crpa$company)
nombres<-merge_plus(subset(cor, select=c("id1","cname")), 
                    crpa, by="cname", unique_key_1="id1", unique_key_2="id2", match_type = 'fuzzy',
                    fuzzy_settings = build_fuzzy_settings(maxDist = .1))$matches
nombres<-subset(nombres, select=-c(id1, id2))
nombres<-nombres%>%distinct()
cor<-left_join(cor, nombres, by=join_by(cname==cname_1))
cor$cname<-cor$cname_2
cor<-subset(cor, select=-c(id1, cname_2, tier))
datafr<-rbind.fill(subset(datafr, is.na(datafr$crp)), cor)
datafr<-datafr[order(datafr$id, datafr$r2),]
total2<-subset(datafr, select=-c(x1, y1,x2, y2, score, height, wide, rank, r2))
write_csv(datafr, "emjdbase0_3.csv")
write_csv(total2, "emjdbase1_3.csv")
write_csv(crp, "crp3_1.csv")

####I. homologation####
combo<- read_csv("combo5_3.csv")
crp<- read_csv("crp3.csv")
emj<-read_csv("emjdbase1_3.csv")
names<-read_csv("nombc.csv")
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

####II. transformation to format sql#####
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
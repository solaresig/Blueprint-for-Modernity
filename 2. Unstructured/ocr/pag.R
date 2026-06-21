####general definitions####
#####programs#####
library(tidyverse)
library(parallel)
library(doSNOW)
library(foreach)

#####functions#####
muestra <- function(df, seed) {
  set.seed(seed)
  n <- ceiling((1.96^2) * ((0.5 * (1 - 0.5) / (0.05^2))) / (1 + (0.5 * (1 - 0.5) / ((0.05^2) * nrow(df)))))
  y <- df[sample(nrow(df), n, replace = TRUE), ]
  y <- y[order(y$id), ]
  return(y)
}
moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


####main process####
procesar<-function(lista){
  library(plyr)
  library(tidyverse)
  library(fedmatch)
  #####Regex#####
  rauth <- c(
    "(?=\n[{|\\[]From(\\s)the)",
    "(?<=\n[{|\\[][C|c][o|o][n|N][t|T][I|l|i])",
    "(\\n)(?=By(\\s))"
  ) %>% paste(collapse = "|")
  
  rauth2<-c("(?<=(\\n)By(\\s))[A-Z].*?(?=(\\n))",
            "(?<=(\\s)By(\\s))[A-Z].*?(?=[,.])",
            "(?<=^By(\\s))[A-Z].*?(?=[,.])",
            "[:punct:]F(?i)rom(\\s)o.*?corres", 
            "[:punct:]F(?i)rom(\\s).*?journa"
  ) %>% paste(collapse = "|")
  
  rtitl <- c(
    "\n\f",
    "(?<![A-Z]{4}|[A-Z]{4}[.]|[A-z]{2}\\p{Pd}|[A-Z]{4}(\\s))[(\\n)|(\\f)](?=([A-Z]{5})|([A-Z]{4}(\\s)[A-Z]{2})|(THE(\\s)))(?![A-Z]{5}(\\s)[a-z])",
    "(?<![A-Z]{4}|[A-Z]{4}[.]|[A-z]{2}\\p{Pd}|[A-Z]{4}(\\s))[(\\n)|(\\f)].{1}(?=([A-Z]{5})|([A-Z]{4}(\\s)[A-Z]{2})|(THE(\\s)))(?![A-Z]{5}(\\s)[a-z])",
    "(?<=[A-Z]{4}[.])[(\\n)|(\\f)](?![A-Z]{3})",
    "(?<=[A-Z]{4})[(\\n)|(\\f)](?![A-Z]{3})",
    "(?<![A-Z]{4}|[A-Z]{4}[.]|[A-z]{2}\\p{Pd}|[A-Z]{4}(\\s))\\p{Pd}(?=([A-Z]{5})|([A-Z]{4}(\\s)[A-Z]{2})|(THE(\\s)))",
    "(?<=[)])(\\s)*\\p{Pd}"
  ) %>% paste(collapse = "|")
  rtit2<-c("^(\\s)*[:punct:]*(\\s)*[:punct:]*[A-Z]{4,}", "[)](\\s)*(?<=\\p{Pd})", 
           "^(\\s)*[:punct:]*(\\s)*[:punct:]*THE(\\s)", 
           "^[{|\\[]From(\\s)the|[{|\\[][C|c][o|o][n|N][t|T][I|l|i]"
  ) %>% paste(collapse = "|")
  subtit<-c("^(\\s)*[A-Z]{4,}(\\s)*[A-Z]*[.,]*\\p{Pd}", 
            "^(\\s)*[A-Z]{4,}(\\s)*[A-Z]*[a-z]*[A-Z]*[.,]*\\p{Pd}") %>% paste(collapse = "|")
  rtitn<-c("^[:lower:]{5,}"
  ) %>% paste(collapse = "|")
  erase<-c("^(?i)(\\w*)(\\s*)(\\w+)(\\s+)AND(\\s)MINING(\\s+)J(\\w*)", 
           "^(\\s+)", "\\n", "\\f", "\\p{Pd}" 
  )%>% paste(collapse = "|")
  
  ori <- paste("vols/", lista, sep = "")
  dest <- paste("processed/", lista, sep = "")
  datafr <- read_csv(ori)
  colnames(datafr)[1] <- "id"
  #####just assigning the distribution of the parts#####
  datafr$x1 <- datafr$x1 / datafr$wide
  datafr$y1 <- datafr$y1 / datafr$height
  datafr$x2 <- datafr$x2 / datafr$wide
  datafr$y2 <- datafr$y2 / datafr$height
  datafr$x <- (datafr$x1 + datafr$x2) / 2
  datafr$y <- (datafr$y1 + datafr$y2) / 2
  datafr$rx <- datafr$id
  datafr<-datafr[order(datafr$id),]
  datafr[c("vol", "page")]<-str_split_fixed(datafr$page, "[_]", 2)
  datafr$page<-as.numeric(datafr$page)
  datafr <- datafr %>%
    group_by(page) %>%
    nest()
  for (h in 1:nrow(datafr)) {
    for (i in 1:2) {
      for (j in 1:nrow(datafr[[2]][[h]])) {
        temp <- subset(datafr[[2]], (datafr[[2]][[h]]$x1 > datafr[[2]][[h]]$x2[j]) |
                         (datafr[[2]][[h]]$x1 > datafr[[2]][[h]]$x2[j] - 0.13))
        if (length(temp$id) > 0) {
          x <- min(temp$rx) - 1
          datafr[[2]][[h]]$rx[j] <- x
        }
      }
      for (j in 1:nrow(datafr[[2]][[h]])) {
        temp <- subset(datafr[[2]][[h]], (datafr[[2]][[h]]$x > datafr[[2]][[h]]$x[j] + 0.2))
        if (length(temp$id) > 0) {
          x <- min(temp$rx) - 1
          datafr[[2]][[h]]$rx[j] <- x
        } else {
          temp <- subset(datafr[[2]][[h]], (datafr[[2]][[h]]$x > datafr[[2]][[h]]$x[j] - 0.2) &
                           (datafr[[2]][[h]]$x < datafr[[2]][[h]]$x[j] + 0.2))
          x <- min(temp$rx)
          datafr[[2]][[h]]$rx[j] <- x
        }
      }
    }
    datafr[[2]][[h]]$ry <- rank(datafr[[2]][[h]]$y)
    datafr[[2]][[h]]$rx <- ifelse(str_detect(datafr[[2]][[h]]$type, "Date|Page"), 0, datafr[[2]][[h]]$rx)
    datafr[[2]][[h]]$ry <- ifelse(str_detect(datafr[[2]][[h]]$type, "Date|Page"), 0, datafr[[2]][[h]]$ry)
    datafr[[2]][[h]] <- datafr[[2]][[h]][order(datafr[[2]][[h]]$rx, datafr[[2]][[h]]$ry), ]
    datafr[[2]][[h]]$rank <- as.numeric(row.names(datafr[[2]][[h]]))
    datafr[[2]][[h]]$rank <- ifelse((datafr[[2]][[h]]$type == "Title") &
                                      (datafr[[2]][[h]]$x > 0.4) &
                                      (datafr[[2]][[h]]$x < 0.6) &
                                      (datafr[[2]][[h]]$x2 - datafr[[2]][[h]]$x1 > 0.5) &
                                      (datafr[[2]][[h]]$y > 0.7),
                                    0.5, datafr[[2]][[h]]$rank)
    datafr[[2]][[h]]$dum0 <- ifelse((datafr[[2]][[h]]$type == "Title") &
                                      (datafr[[2]][[h]]$x > 0.4) &
                                      (datafr[[2]][[h]]$x < 0.6) &
                                      (datafr[[2]][[h]]$x2 - datafr[[2]][[h]]$x1 > 0.5) &
                                      (datafr[[2]][[h]]$y > 0.7),
                                    1,0)
  }
  datafr <- datafr %>% unnest(cols = data)
  #####using regex to assign types#####
  datafr <- datafr %>% separate_rows(text, sep = rauth)
  texto<-subset(datafr, datafr$type=="Text")
  texto <- texto %>% separate_rows(text, sep = rtitl)
  texto$rt<-as.numeric(row.names(texto))
  rest<-subset(datafr, datafr$type!="Text")
  rest$rt<-as.numeric(row.names(rest))
  datafr<-rbind(rest, texto)
  datafr<-datafr[order(datafr$page, datafr$rank, datafr$rt),]
  datafr <- subset(datafr, !str_detect(datafr$text, "^((\\s)(\\n))*((\\n)(\\d))*$"))
  datafr <- datafr %>%
    group_by(page) %>%
    nest()
  
  for (h in 1:nrow(datafr)) {
    datafr[[2]][[h]] <- datafr[[2]][[h]][order(datafr[[2]][[h]]$rank,datafr[[2]][[h]]$rt), ]
    datafr[[2]][[h]]$r2 <- as.numeric(row.names(datafr[[2]][[h]]))
    datafr[[2]][[h]]$type <- ifelse((datafr[[2]][[h]]$type == "Title") &
                                      (str_detect(datafr[[2]][[h]]$text, rtitn)),
                                    "Text", datafr[[2]][[h]]$type)
    datafr[[2]][[h]]$type <- ifelse((datafr[[2]][[h]]$type != "Title") &
                                      (nchar(datafr[[2]][[h]]$text)>=4)&
                                      (str_detect(datafr[[2]][[h]]$text,
                                                  rtit2)),
                                    "Title", datafr[[2]][[h]]$type)
    datafr[[2]][[h]]$title <- ifelse(datafr[[2]][[h]]$type == "Title", datafr[[2]][[h]]$text, NA)
    datafr[[2]][[h]]$title <- ifelse(datafr[[2]][[h]]$type == "Text" &
                                       str_detect(datafr[[2]][[h]]$text,
                                                  "^[{|\\[]From(\\s)the|[{|\\[][C|c][o|o][n|N][t|T][I|l|i]"),
                                     datafr[[2]][[h]]$text, datafr[[2]][[h]]$title)
    datafr[[2]][[h]]$subt <- ifelse(sum(datafr[[2]][[h]]$dum0>0), datafr[[2]][[h]]$title, str_extract(datafr[[2]][[h]]$title, subtit))
    datafr[[2]][[h]]$author <- ifelse(datafr[[2]][[h]]$type == "Author", datafr[[2]][[h]]$text, NA)
    datafr[[2]][[h]]$pagi <- ifelse(datafr[[2]][[h]]$type == "Page", datafr[[2]][[h]]$text, NA)
    datafr[[2]][[h]]$pagi <- str_extract(datafr[[2]][[h]]$pagi, "(\\d+)") %>% as.numeric()
    datafr[[2]][[h]]$date <- ifelse(datafr[[2]][[h]]$type == "Date", datafr[[2]][[h]]$text, NA)
    datafr[[2]][[h]]$date <- str_replace_all(datafr[[2]][[h]]$date, "(\\n)", " ")
    datafr[[2]][[h]]$date <- str_replace_all(datafr[[2]][[h]]$date, "(\\s+)", " ")
    datafr[[2]][[h]]$date <- ifelse(str_detect(datafr[[2]][[h]]$date, "^(\\s)*$"), NA, datafr[[2]][[h]]$date)
    datafr[[2]][[h]]$text<-str_replace_all(datafr[[2]][[h]]$text, erase, " ")
    datafr[[2]][[h]]$text<-str_replace_all(datafr[[2]][[h]]$text, "^(\\s)|(?<=(\\s))(\\s)", "")
    datafr[[2]][[h]]$title<-str_replace_all(datafr[[2]][[h]]$title, erase, " ")
    datafr[[2]][[h]]$title<-str_replace_all(datafr[[2]][[h]]$title, "^(\\s)|(?<=(\\s))(\\s)", "")
    datafr[[2]][[h]]<-subset(datafr[[2]][[h]], !str_detect(datafr[[2]][[h]]$text, "^[^[:alpha:][:digit:]]*$"))
    datafr[[2]][[h]]$title<-ifelse((nchar(datafr[[2]][[h]]$title)> 100)&
                                     (nchar(str_extract(datafr[[2]][[h]]$title, "^.*?(?=[a-z]|â€|[A-Z][a-z]|\\p{Pd}|\\x28)"))>4),
                                   str_extract(datafr[[2]][[h]]$title, "^.{4,}?(?=[a-z]|â€|[A-Z][a-z]|\\p{Pd}|\\x28)"), 
                                   datafr[[2]][[h]]$title)
    datafr[[2]][[h]]$title<-ifelse((nchar(datafr[[2]][[h]]$title)<4), NA, datafr[[2]][[h]]$title)
    datafr[[2]][[h]]$author<-ifelse(is.na(datafr[[2]][[h]]$author), str_extract(datafr[[2]][[h]]$text, rauth2), datafr[[2]][[h]]$author)
    
    datafr[[2]][[h]]$dum1<-ifelse(is.na(datafr[[2]][[h]]$title), 0, 
                                  ifelse((datafr[[2]][[h]]$title==datafr[[2]][[h]]$text)&datafr[[2]][[h]]$type=="Title",1, 0))
    datafr[[2]][[h]]$dum2<-ifelse(is.na(datafr[[2]][[h]]$title), 0, 
                                  ifelse(str_detect(datafr[[2]][[h]]$title, "(?i)^By")&(datafr[[2]][[h]]$type=="Title"), 1,0))
    datafr[[2]][[h]]$dum3<- ifelse((datafr[[2]][[h]]$type=="Title")&str_detect(datafr[[2]][[h]]$text, "(?i)advertisem"), 
                                   1,0)
    if (nrow(datafr[[2]][[h]]) > 1) {
      for (i in 2:nrow(datafr[[2]][[h]])) {
        if((datafr[[2]][[h]]$id[i]==datafr[[2]][[h]]$id[i-1])&(datafr[[2]][[h]]$dum1[i-1]==1)&((datafr[[2]][[h]]$dum1[i]==0)|(datafr[[2]][[h]]$dum2[i]==1))){
          datafr[[2]][[h]]$title[i]<-datafr[[2]][[h]]$title[i-1]
        } 
      }
      for (i in 2:nrow(datafr[[2]][[h]])) {
        if (is.na(datafr[[2]][[h]]$author[i]) & !is.na(datafr[[2]][[h]]$author[i - 1]) &
            is.na(datafr[[2]][[h]]$title[i])) {
          datafr[[2]][[h]]$author[i] <- datafr[[2]][[h]]$author[i - 1]
        } else if (is.na(datafr[[2]][[h]]$author[i]) & !is.na(datafr[[2]][[h]]$author[i - 1]) &
                   !is.na(datafr[[2]][[h]]$title[i]) & !is.na(datafr[[2]][[h]]$title[i - 1])) {
          if (datafr[[2]][[h]]$title[i] == datafr[[2]][[h]]$title[i - 1]) {
            datafr[[2]][[h]]$author[i] <- datafr[[2]][[h]]$author[i - 1]
          }
        }
      }
    }
    
    datafr[[2]][[h]]$dum0<-NULL
    datafr[[2]][[h]]$dum1<-NULL
    datafr[[2]][[h]]$dum2<-NULL
    datafr[[2]][[h]]$dum3<-ifelse(sum(datafr[[2]][[h]]$dum3)>0, 1, 0)
    datafr[[2]][[h]]$title <- ifelse(str_detect(datafr[[2]][[h]]$type, "Page|Date"), NA, datafr[[2]][[h]]$title)
    datafr[[2]][[h]]$author <- ifelse(str_detect(datafr[[2]][[h]]$type, "Page|Date"), NA, datafr[[2]][[h]]$author)
    
    if (nrow(datafr[[2]][[h]]) > 2) {
      for(i in 1:(nrow(datafr[[2]][[h]])-1)){
        if(!is.na(datafr[[2]][[h]]$title[i])&!is.na(datafr[[2]][[h]]$title[i+1])){
          a<-str_extract(datafr[[2]][[h]]$title[i], "^.{5}")
          b<-str_extract(datafr[[2]][[h]]$title[i+1], "^.{5}")
          if(!is.na(a)&!is.na(b)){
            if(a==b){
              d<-nchar(datafr[[2]][[h]]$title[i])-nchar(datafr[[2]][[h]]$title[i+1])
              if(d>0){
                datafr[[2]][[h]]$title[i]<-datafr[[2]][[h]]$title[i+1]
              } else{
                datafr[[2]][[h]]$title[i+1]<-datafr[[2]][[h]]$title[i]
              } 
            }            
          }
        }
      }
    }
    datafr[[2]][[h]] <- datafr[[2]][[h]] %>% fill(title, pagi, date, .direction = c("down"))
  }
  
  datafr <- datafr %>% unnest(cols = data, keep_empty = T)
  datafr<-subset(datafr, !is.na(datafr$id)&(datafr$dum3==0))
  datafr$dum3<-NULL
  #####compiling different texts according to titles#####
  
  datafr$text2 <- datafr$text
  data1 <- subset(datafr, str_detect(datafr$type, "Date|Page"))
  data2 <- subset(datafr, !str_detect(datafr$type, "Date|Page"))
  data2 <- data2[order(data2$page, data2$r2), ]
  
  for (i in 4:nrow(data2)) {
    if (((data2$r2[i] < 4)) &
        (data2$type[i] == "Text") &
        is.na(data2$title[i]) & !is.na(data2$title[i - 1])) {
      data2$title[i] <- data2$title[i - 1]
    }
  }
  
  data2 <- data2 %>%
    group_by(page) %>%
    nest()
  
  for (h in 1:nrow(data2)) {
    data2[[2]][[h]] <- data2[[2]][[h]] %>% fill(title, .direction = c("down"))
  }
  
  data2 <- data2 %>% unnest(cols = data)
  data2 <- data2[order(data2$page, data2$r2), ]
  data2$beg <- 1
  
  for (i in (nrow(data2) - 1):1) {
    if (!is.na(data2$title[i + 1]) & !is.na(data2$title[i])) {
      if (data2$title[i + 1] == data2$title[i]) {
        x <- paste(data2$text[i], data2$text2[i + 1])
        data2$text2[i] <- x
        data2$beg[i + 1] <- 0
      }
    }
  }
  
  data2 <- subset(data2, data2$beg == 1)
  for (i in 2:nrow(data2)){
    if(!is.na(data2$subt[i])&data2$type[i-1]=="Title"){
      data2$title[i]<-data2$title[i-1]
    }
  }
  data2$text <- data2$text2
  datafr <- rbind(subset(data1, select = -c(text2)),
                  subset(data2, select = -c(beg, text2)))
  datafr <- datafr[order(datafr$page, datafr$r2), ]
  
  #extra step to add missing authors
  datafr$author<-ifelse(!is.na(datafr$author)&(nchar(datafr$author)>5), datafr$author, 
                        str_extract(datafr$text, "(?<=By(\\s))[A-Z]{1,}[a-z]*[.,]*(\\s)[A-Z]{1,}[a-z]*[.,]*(\\s)[A-Z]{1,}[a-z]*[.,]*(\\s)[A-Z]{1,}[a-z]*[.,]*"))
  
  #####Code for extracting date#####
  datafr$year <- str_extract(datafr$date, "1(\\d){3}")[which(!is.na(str_extract(datafr$date, "1(\\d){3}")))] %>%
    moda() %>% as.numeric()
  datafr$date<-str_to_title(datafr$date)
  datafr$month <- ifelse(str_detect(datafr$date, "[J|S]an"), 1,
                         ifelse(str_detect(datafr$date, "(?<![A-z](\\s))F"), 2,
                                ifelse(str_detect(datafr$date, "Ma[a-z]{2}"), 3,
                                       ifelse(str_detect(datafr$date, "A[p|b]"), 4,
                                              ifelse(str_detect(datafr$date, "Ma"), 5,
                                                     ifelse(str_detect(datafr$date, "[J|S]u.{1}y|[J|S]ul"), 7,
                                                            ifelse(str_detect(datafr$date, "[J|S]u[n|r]"), 6,
                                                                   ifelse(str_detect(datafr$date, "(?<![A-z](\\s))A"), 8,
                                                                          ifelse(str_detect(datafr$date, "Se"), 9,
                                                                                 ifelse(str_detect(datafr$date, "Oc"), 10,
                                                                                        ifelse(str_detect(datafr$date, "N"), 11,
                                                                                               ifelse(str_detect(datafr$date, "Dec"), 12, NA)
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
  me<-mean(datafr$month, na.rm = T)
  for (i in 1:nrow(datafr)){
    if(!is.na(datafr$month[i])){
      if((me>=6)&(datafr$month[i]<7)){
        datafr$month[i]<-NA
      }else if((me<=6)&(datafr$month[i]>6)){
        datafr$month[i]<-NA
      }else {
        tem<- datafr$month[1:i]
        tem<- subset(tem, !is.na(tem))
        tem<- tem[length(tem)]
        if((length(tem)>0)){
          if((datafr$month[i]<tem)|((datafr$month[i]-tem)>2)){
            datafr$month[i]<-NA
          }  
        }
      }
    }
  }
  rm(me, tem)
  datafr$day<-str_extract(datafr$date, "(\\b)(\\d){1,2}(\\b)") %>% as.numeric()
  datafr$day<- ifelse(datafr$day>31, NA, datafr$day)
  for (i in 2:nrow(datafr)){
    if(is.na(datafr$month[i]&!is.na(datafr$month[i-1]))){
      datafr$month[i]<- datafr$month[i-1]
    }
    if(is.na(datafr$day[i]&!is.na(datafr$day[i-1]))){
      datafr$day[i]<- datafr$day[i-1]
    }
  }
  datafr <- datafr %>% 
    tidyr::fill(month, day, .direction = "downup")
  #####adding missing pages####
  pagi<-unique(datafr$pagi)
  pagi<-pagi[!is.na(pagi)]
  datafr$pagi<-ifelse((datafr$pagi>datafr$page)|((datafr$page-datafr$pagi)>(3*(datafr$page[1]-pagi[1]))), 
                      NA, datafr$pagi)
  datafr$difer <- ifelse(!is.na(datafr$pagi)&(datafr$page>datafr$pagi), datafr$page - datafr$pagi, NA)
  mod <- moda(datafr$difer[which(!is.na(datafr$difer))])
  datafr$difer <- ifelse((datafr$difer > (1.25 * mod)) | (datafr$difer < (0.75 * mod)),
                         NA, datafr$difer)
  datafr$difer <- ifelse(!is.na(datafr$difer), datafr$difer, mod)
  datafr$pagi <- ifelse(is.na(datafr$pagi), datafr$page - datafr$difer, datafr$pagi)
  #####author data#####
  profs<-c("[D|M][a-z][.|,]*", "Pr(\\w){2}[.|,]*", "Doctor", "Pro[a-z]es[a-z]+", "C[.|,]*(\\s)*E[.|,]*", "Geo[a-z]*", "Lieut[a-z]*",
           "LL[.|,]*(\\s)*D[.|,]*", "E[.|,]*(\\s)*M[.|,]*", "M[.|,]*(\\s)*E[.|,]*","LL[.|,]", "A[.|,]*(\\s)*M[.|,]*","Mech[a-z]+", 
           "Teach[a-z]+", "Ph.(\\s)*D", "Metal[a-z]+", "Treas[a-z]+", "[C|O]hief", "Assist[a-z]+", "Archi[a-z]+", "Engi[a-z]+")
  del<-c("Practic[a-z]+", "Jr[.|,]", "The", "Extrac[a-z]+", "N[a-z]w\b", "United",
         "(\\s)[A-Z][a-z]{0,1}[.|,]$", "[:punct:]*(\\s)*[:punct:]*$", "[:digit:]")%>% paste(collapse="|")
  p1<-paste(profs, "(\\s)", sep="")
  p2<-paste(profs, "$", sep="")
  p3<-paste(profs, "(\\b)", sep="")
  profs<-c(p1, p2, p3) %>% paste(collapse="|")
  datafr$temp<-nchar(str_replace_all(datafr$author, "\\S", ""))
  datafr$author<-ifelse(!is.na(datafr$author)&(nchar(datafr$author)>5)&!str_detect(datafr$author, "(?i)From")&
                          (datafr$temp>0)&(datafr$temp<4), 
                        datafr$author, 
                        str_extract(datafr$text, 
                                    "(?<=By(\\s))[A-Z]{1,}[a-z]*[.,]*(\\s+)[A-Z]{1,}[a-z]*[.,]*(\\s+)[A-Z]{1,}[a-z]*[.,]*(\\s)[A-Z]{1,}[a-z]*[.,]*(\\s)[A-Z]{1,}[a-z]*[.,]*"))
  aut<-subset(datafr, !is.na(datafr$author))
  aut$edu<-str_extract(aut$author, profs)
  aut$author<-str_replace_all(aut$author, profs, "")
  aut$author<-str_replace(aut$author, del, "")
  aut$author<-str_replace(aut$author, del, "")
  aut$temp<-str_replace(aut$author, "[.,](\\s)*(\\w)*(\\s)*(\\w)*[.,]*$", "")
  aut$temp2<-nchar(str_replace_all(aut$temp, "\\S", ""))
  aut$temp<-ifelse((aut$temp2>2)&(nchar(aut$temp)>6), str_replace(aut$author, "[.|,](\\s)*(\\w)*(\\s)*(\\w)*[:punct:]*$", ""), aut$temp)
  aut$temp2<-nchar(str_replace_all(aut$temp, "\\S", ""))  
  aut$temp<-ifelse((aut$temp2>2)&(nchar(aut$temp)>6), 
                   str_replace(aut$temp, "[.|,](\\s)*(\\w)*(\\s)*(\\w)*(\\s)*(\\w)*[:punct:]*$", ""), aut$temp)
  aut$temp2<-nchar(str_replace_all(aut$temp, "\\S", ""))  
  aut$author<-ifelse((aut$temp2>0)&(nchar(aut$temp)>6), aut$temp, aut$author)
  aut$temp2<-nchar(str_replace_all(aut$author, "\\S", ""))
  aut$author<-str_replace_all(aut$author, "[^[A-z]]", " ")
  aut$author<-str_replace_all(aut$author, "(\\s+)", " ")
  aut$author<-str_replace_all(aut$author, "^(\\s+)", "")
  aut$temp<-NULL
  aut$temp2<-NULL
  datafr<-rbind.fill(subset(datafr, is.na(datafr$author)), aut)
  rm(aut)
  datafr<- datafr[order(datafr$page, datafr$r2),]
  ####final cleaning####
  datafr<- subset(datafr, select=-c(rx, ry, x, y))
  datafr<- datafr[order(datafr$page, datafr$r2),]
  write_csv(datafr, dest)
  info<- list.files(paste(getwd(), "vols/", sep="/"), full.names = T) %>% 
    file.info()
  info<-sum(info$size)
  info2<- list.files(paste(getwd(), "processed/", sep="/"), full.names = T) %>% 
    file.info()
  info2<-sum(info2$size)
  prog<-100*info2/info
  progr<-file("progress.txt")
  writeLines(print(paste("Progress:", prog, "%")), progr)  
  close(progr)
}

#####paths#####
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
lista<- list.files("vols/")
info<- list.files(paste(getwd(), "vols/", sep="/"), full.names = T) %>% 
  file.info()
info<-info$size
info2<- list.files(paste(getwd(), "processed/", sep="/"), full.names = T) %>% 
  file.info()
info2<-info2$size
lista2<- list.files("processed/")

####parallel processing####
cl<-makeCluster(detectCores()-2, type="SOCK")
registerDoSNOW(cl)
foreach(g = lista) %dopar% {procesar(g)}
stopCluster(cl)
####compiling####
lista2<- list.files("processed/")
info2<- list.files(paste(getwd(), "processed/", sep="/"), full.names = T) %>% 
  file.info()
info2<-info2$size
total <- data.frame()
start<-Sys.time()
for(g in 1:length(lista2)){
  ori <- paste("processed/", lista2[g], sep = "")
  datafr <- read_csv(ori)
  total <- rbind(total, datafr)
  progr<-100*sum(info2[1:g])/sum(info2[1:length(info2)])
  elap<-difftime(Sys.time(), start, units="mins")
  eta<-elap*(sum(info2[g:length(info2)])/sum(info2[1:g]))
  print(paste("Number of rows:", nrow(total)))
  print(paste("Progress:", progr, "%"))
  print(paste("Elapsed time:", as.numeric(elap), "mins"))
  print(paste("Remaining time:", as.numeric(eta), "mins"))
}
total$id <- paste(total$vol, "_", total$id, sep = "")
total$vol<-as.numeric(total$vol)
total <- total[order(total$vol, total$page), ]
total<-subset(total, 
              select=c("id", "vol", "page", "x1", "y1", "x2", "y2","height", "wide", "score", "rank", "r2",
                       "type", "pagi", "date", "day", "month", "year","author", "title", "subt","text", 
                       "edu"))
total$subt<-str_replace(total$subt, "^(\\s+)", "")
total2<-subset(total, select=-c(x1, y1,x2, y2, score, height, wide, rank, r2))
muest <- muestra(total2, 105)
muest <- muest[order(muest$vol, muest$page), ]
muest$lastt<-str_extract(muest$text, ".{20}$")
write_csv(muest, "../../testtotal.csv")
write_csv(total, "emjdbase0.csv")
write_csv(total2, "emjdbase.csv")

####calculating shares#####
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
lista<- list.files("vols/")
info<- list.files(paste(getwd(), "vols/", sep="/"), full.names = T) %>% 
  file.info()
info<-info$size
shares<-data.frame()
for(i in 1:length(lista)){
  emj<-read_csv(paste0("vols/",lista[i])) %>%
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
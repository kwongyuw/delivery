rm(list=ls())
library(DBI)
library(tidyverse)
library(stringr)
library(openxlsx)
library(data.table)

Sys.setlocale(locale="en_us.UTF-8")
source('/Users/kwongyu/Google Drive/dwb/cred.txt')
# source('~/Documents/eScience/projects/delivery/baidu_geo_func.R')

#Grab data 

data_dir <- '/Users/kwongyu/Google Drive/dwb/dwb_Data'


temp = list.files(path = data_dir, pattern="*.xlsx$")
#temp = list.files(path = data_dir, pattern="*C3_.*&*.xlsx$", full.names = T)
temp

list2env(
  lapply(setNames(file.path(data_dir ,temp), make.names(gsub("*.xlsx$", "", temp))), 
         read.xlsx), envir = .GlobalEnv)
#########

names(Aug_C3_P1)
names(Aug_C3_P2)
names(G_Order_Sep_City3.Condensed.)
names(Jul_C3_P1)
names(Jul_C3_P2)

data <- bind_rows(Aug_C3_P1, Aug_C3_P2,Jul_C3_P1,Jul_C3_P2,G_Order_Sep_City3.Condensed.)


sup_data <- data %>%
  select(sup_id, from_addr, from_tel) %>% 
  distinct(.) #%>%  slice(1:20)

#ensure sensible numbers to throw in
sup_data <- separate(sup_data, from_tel, c("tel1", "tel2", "tel3"), sep="[, —]", remove=FALSE)
sup_data <- separate(sup_data, tel1, c("tel1", "int1"), sep="转") %>%
  separate(tel2, c("tel2", "int2"), sep="转")
sup_data$tel3[(grep("[^0-9\\(\\)-E]+", sup_data$tel3))] <- sub("[^0-9]*([0-9]+)[^0-9]*", "\\1", (sup_data$tel3[(grep("[^0-9\\(\\)-E]+", sup_data$tel3))]))
#length(grep("^[^0-9]*021", sup_data$tel1))

write.csv(sup_data,'sup_list.csv')

#===========================
#after sup info scraped
#===========================
setwd('~/Google Drive/dwb/dwb_Data')
sup_info <- read.csv('sup_info3.csv', stringsAsFactors = FALSE)
#supplementing tag info (tags have all words inseparably tgt)
addtag_a <- read.csv("sup_info.csv", stringsAsFactors = FALSE) %>% select(details:tag4)
addtag_b <- read.csv("sup_info1b.csv", stringsAsFactors = FALSE) %>% select(details:tag4)
addtag <- rbind(addtag_a, addtag_b)
sup_info <- left_join(sup_info, addtag, by=c("sup_id", "from_addr", "from_tel", "details")) 

temp <- sup_info
temp$details <- sub('\n详情\n标签:','\n标签:', temp$details)
temp <- separate(temp, details, c('name', 'd1','tags', 'd2', 'from_addr_xms',  'd3', 'avgexp', 
                                      'taste', 'envir', 'service','d7', 'comment', 'remarks'),
                 sep = "\n", remove=FALSE, extra="merge") %>%
  select(-'d1', -'d2', -'d3', -'d7') %>%
  separate(avgexp,c("avgexp_lo", 'avgexp_up'), sep='-') %>%
  filter(!is.na(taste))
temp$avgexp_lo <- as.numeric(sub('^¥','', temp$avgexp_lo))
temp$taste <- sub('^.*:','', temp$taste)
temp$envir <- sub('^.*:','', temp$envir)
temp$service <- sub('^.*:','', temp$service)
temp$from_tel[temp$from_tel==""] <- temp$from_.tel[temp$from_tel==""]
temp$from_.tel <- NULL
temp[,8:12] <- lapply(temp[,8:12],as.numeric)

# ready for merging back (not yet manually checking which is the best)
df <- temp
df$adist_long <- diag(adist((df$from_addr), (df$from_addr_xms)) / 
                       pmax(unlist(lapply((df$from_addr_xms),nchar)),unlist(lapply((df$from_addr),nchar))))
df$adist_short <- diag(adist((df$from_addr), (df$from_addr_xms)) / 
                        pmin(unlist(lapply((df$from_addr_xms),nchar)),unlist(lapply((df$from_addr),nchar))))
df$match_manual <- NA
df$match_rule <- 1
df$match_rule[(df$adist_long>0.7 & df$adist_short>=1)] <- 0

df <- df[!duplicated(select(df, sup_id, from_tel, from_addr,from_addr_xms)),]



#===Manual filter====================================
#manual <- function(id_tel_addr) {
#  drop <- c()
#  #count <- 0
#  for (i in 1:dim(id_tel_addr)[1]){
#    temp <- filter(id_tel_addr, from_addr==id_tel_addr$from_addr[i], from_tel==id_tel_addr$from_tel[i])
#    if (dim(temp)[1] >1) {
#      print(i)
#      #count <- count +1
#      print(temp$from_addr) #temp$from_addr_xms
#      print(temp$from_addr_xms)
#      print(temp$X)
#      input <- readline(prompt='which one to drop')
#      drop <- c(drop, input)
#      id_tel_addr <- filter(id_tel_addr,!(X %in% drop))
#      print(dim(id_tel_addr)[1])
#    }
#  }
#  #print(count)
#  out <- id_tel_addr
#}
#df <- manual(df)
#=======================================

#instead of manual check (>5mins for 20 records), just keeping the one with lowest adist_long?
df <- group_by(df, sup_id, from_tel, from_addr) %>%
  arrange(adist_long) %>%
  top_n(1)

write.csv(df,'sup.csv')

#df$sup_name <- NA
#via haoma.baidu.com 
#df$sup_name[grepl("18217774279",df$from_tel)] <- "CoCo都可茶饮(浦电路店)"
#df$sup_name[grepl("18621672116",df$from_tel)] <- "上海联通"   #no info
#df$sup_name[grepl("15721077747",df$from_tel)] <- "鲜芋仙(日月光店)"

#via Baidu place information api
#http://api.map.baidu.com/place/v2/search?query=%E6%B5%A6%E4%B8%9C%E6%96%B0%E5%8C%BA%E9%BE%99%E6%B1%87%E8%B7%AF338%E5%8F%B7%E6%96%B0%E9%83%BD%E6%B1%87%E7%BE%8E%E9%A3%9F%E5%B9%BF%E5%9C%BA&region=%E4%B8%8A%E6%B5%B7&scope=2&output=json&ak=
#http://map.baidu.com/detail?qt=ninf&uid=e85e25272a109d4a94f08cee&detail=cater
#but failed to find 鮮芋仙 from api
#http://api.map.baidu.com/place/v2/search?query=%E5%BE%90%E6%B1%87%E5%8C%BA%E5%BE%90%E5%AE%B6%E6%B1%87%E8%B7%AF618%E5%8F%B7%E6%97%A5%E6%9C%88%E5%85%89%E4%B8%AD%E5%BF%83%E5%B9%BF%E5%9C%BA&region=%E4%B8%8A%E6%B5%B7&scope=2&output=json&ak=
#documentation: http://lbsyun.baidu.com/index.php?title=webapi/guide/webservice-placeapi





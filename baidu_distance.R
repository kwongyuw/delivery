rm(list=ls())
library(DBI)
library(tidyverse)
library(stringr)
library(openxlsx)
library(data.table)

source('~/Google Drive/dwb/cred.txt')
# source('~/Documents/eScience/projects/delivery/baidu_geo_func.R')


baidu_distance <- function(origins, destinations) {
  require(RJSONIO)
  origins <- paste(origins)
  destinations <- paste(destinations)
  full <- 'http://api.map.baidu.com/routematrix/v2/driving?output=json'
  url <- URLencode(paste(full, '&origins=', origins, '&destinations=', destinations, '&ak=', ak_bd_serv, sep=''))
  x <- fromJSON(url,simplify=FALSE)
  
  if (x$status == 0) {
    out <- c(x$result[[1]]$distance$value,
             x$result[[1]]$distance$text,
             x$result[[1]]$duration$value,
             x$result[[1]]$duration$text)
  } else {
    out <- NA
  }
  Sys.sleep(0.0167) # Verified developer now 160/s (API only allows 50 requests per second for non-verified user)
  out
  }

df_from <- fread(file = '~/Google Drive/dwb/dwb_Data/data_full_from_addr.csv', 
                        na.strings = c(""),
                        stringsAsFactors = F)

df_to <- fread(file = '~/Google Drive/dwb/dwb_Data/data_full.csv', 
                 na.strings = c(""),
                 stringsAsFactors = F)
### create df of vendor and individual addresses by userID

names(df_to)
names(df_from)

df_ocords <- df_to %>%
              select(id, LAT, LON) %>%
              mutate(o_cords = paste(LAT, LON, sep = ",")) %>%
              select(-LAT,-LON)

df_dcords <- df_from %>%
              select(id, LAT, LON) %>%
              mutate(d_cords = paste(LAT, LON, sep = ",")) %>%
              select(-LAT,-LON)

df <- inner_join(df_ocords,df_dcords, by = c("id"))
              

df <- df %>% slice(299981:329980) #you can change to 30,000 for each run 
#Initialize
df$dist<-NA
df$dist_txt<-NA
df$time<-NA
df$time_txt<-NA
drive.temp=list()


for(i in 1:nrow(df)){
  
  origins <- df$o_cords[i]
  destinations <- df$d_cords[i]
  
  drive.temp <- try(baidu_distance(origins=origins, destinations=destinations), TRUE)
  df$dist[i] <- drive.temp[1]
  df$dist_txt[i] <- drive.temp[2]
  df$time[i] <- drive.temp[3]
  df$time_txt[i] <- drive.temp[4]
  
  if ((i %% 100) ==0) cat(paste(i," "))
  if (i == nrow(df)) cat("Done!\n")

}

write.csv(df, 'dist11.csv')

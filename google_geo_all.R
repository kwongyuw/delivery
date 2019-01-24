rm(list=ls())
library(DBI)
library(tidyverse)
library(stringr)
library(openxlsx)
library(data.table)

source('/Volumes/GoogleDrive/Team Drives/delivery/data/cred.txt')


#Grab data 

data_dir <- '/Volumes/GoogleDrive/Team Drives/delivery/data'


temp = list.files(path = data_dir, pattern="*.xlsx$")
#temp = list.files(path = data_dir, pattern="*C3_.*&*.xlsx$", full.names = T)
temp

list2env(
  lapply(setNames(file.path(data_dir ,temp), make.names(gsub("*.xlsx$", "", temp))), 
         read.xlsx), envir = .GlobalEnv)
#########

names(Aug_C3_P2)
names(G_Order_Sep_City3.Condensed.)
names(Jul_C3_P1)
names(Jul_C3_P2)

data <- bind_rows(Aug_C3_P2,Jul_C3_P1,Jul_C3_P2,G_Order_Sep_City3.Condensed.)

geo_data <- data %>%
  select(address) %>% 
  distinct(.)

# test <- data %>%
#   group_by(address) %>%
#   summarise(n = n()) %>%
#   mutate(ad_id = 1:nrow(.))
# 
# test2 <-left_join(data, test %>%
#                           select(-n), by = c("address"))
##

#Baidu API call
geocodeAddress <- function(address) {
  require(RJSONIO)
  full <- paste(address)
  url <- "http://api.map.baidu.com/geocoder/v2/?address="
  url <- URLencode(paste(url, full, '&output=json&ak=', ak_bd_brow, sep=''))
  x <- fromJSON(url,simplify=FALSE)
  if (x$status == 0) {
    out <- c(x$result$location$lat, 
             x$result$location$lng)
  } else {
    out <- NA
  }
  Sys.sleep(0.0167) # Verified developer now 160/s (API only allows 50 requests per second for non-verified user)
  out
}
#Google API call
#geocodeAddress <- function(address) {
#  require(RJSONIO)
#  full <- paste(address)
#  url <- "https://maps.google.com/maps/api/geocode/json?address="
#  url <- URLencode(paste(url, full, '&sensor=false&key=',apikey, sep = ""))
#  x <- fromJSON(url, simplify = FALSE)
#  if (x$status == "OK") {
#    
#      out <- c(x$results[[1]]$geometry$location$lat,
#               x$results[[1]]$geometry$location$lng)
#    } else {
#      out <- NA
#  }
#  Sys.sleep(0.05)  # API only allows 50 requests per second
#  out
#}

#Initialize
geo_data$LAT<-NA
geo_data$LON<-NA
g_add=list()

for (i in 1:nrow(geo_data)) {
  g_add <- geocodeAddress(geo_data$address[i])
  geo_data$LAT[i] <- g_add[1]
  geo_data$LON[i] <- g_add[2]

  if ((i %% 100) ==0) cat(paste(i," "))
  if (i == nrow(geo_data)) cat("Done!\n")
}

#add addresses w/geo_cords back to main dataset
data_result <- left_join(data, geo_data, by = c("address"))

# from saved on whole data set the things that were already ran
geo_aug1 <- fread(file = '/Volumes/GoogleDrive/Team Drives/delivery/data/geo_aug1.csv', 
                   na.strings = c(""),
                   stringsAsFactors = F)

geo_test <- geo_aug1 %>%
  select(-V1, -X, -address) %>%
  mutate(LAT = as.numeric(LAT),
         LON = as.numeric(LON))

data_geo_aug1 <- bind_cols(Aug_C3_P1,geo_test)

#append to overall
data_geo_all <- bind_rows(data_result, data_geo_aug1)



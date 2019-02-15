rm(list=ls())
library(DBI)
library(tidyverse)
library(stringr)
library(openxlsx)
library(data.table)

source('/Volumes/GoogleDrive/Team Drives/delivery/data/cred.txt')
source('~/Documents/eScience/projects/delivery/baidu_geo_func.R')


data_dir <- '/Volumes/GoogleDrive/Team Drives/delivery/data'

sup_df <- fread(file = file.path(data_dir,'sup_addr.csv'), 
                  na.strings = c(""),
                  stringsAsFactors = F,
                header = T)
### Text Clean Up

geo_data <- sup_df %>%
  mutate(cleaned2 = trimws(cleaned)) %>%
  select(cleaned2) %>%
  slice(1:20)



#Initialize
geo_data$LAT<-NA
geo_data$LON<-NA
g_add=list()

for (i in 1:nrow(geo_data)) {
  g_add <- geocodeAddress(geo_data[,1][i])
  geo_data$LAT[i] <- g_add[1]
  geo_data$LON[i] <- g_add[2]
  
  if ((i %% 100) ==0) cat(paste(i," "))
  if (i == nrow(geo_data)) cat("Done!\n")
}



g_add <- geocodeAddress("长宁区天山路88号3号楼1502室")


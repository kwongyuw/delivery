rm(list=ls())
library(DBI)
library(tidyverse)
library(stringr)
library(openxlsx)
library(data.table)

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

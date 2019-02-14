rm(list=ls())

library(tidyverse)
library(stringr)
library(openxlsx)
library(data.table)

data_dir <- '/Volumes/GoogleDrive/Team Drives/delivery/data'

f_df <- fread(file = file.path(data_dir,'data_full_from_addr.csv'), 
                na.strings = c(""),
                stringsAsFactors = F,
                header = T)

# Load everything that has the names dist and append 
temp = list.files(path = data_dir, pattern="dist")
temp

dist_df <- do.call(bind_rows,
  lapply(file.path(data_dir ,temp), read.csv, stringsAsFactors = FALSE)) %>%
  unique(.)

# Join to main data using "id" 
a_df <- left_join(f_df, dist_df, by = "id")
  

rm(list=ls())

library(tidyverse)
library(stringr)
library(openxlsx)
library(data.table)
library(readxl)

data_dir <- '/Volumes/GoogleDrive/Team Drives/delivery/data'


temp = list.files(path = data_dir, pattern="*.xlsx$")


or_df <- do.call(bind_rows,
        lapply(file.path(data_dir ,temp), read_excel, range = cell_cols(1:21)))

# Load everything that has the names dist and append 
temp = list.files(path = data_dir, pattern="dist")
temp

dist_df <- do.call(bind_rows,
  lapply(file.path(data_dir ,temp), read.csv, stringsAsFactors = FALSE)) %>%
  unique(.)


# Join to main data using "id" 
a_df <- left_join(or_df, dist_df, by = "id")

# restaurant information 
sup <- read.csv(file.path(data_dir ,'sup.csv'), stringsAsFactors = F) %>% select(-X.1,-X)

# join
df_all <- left_join(a_df, sup, by=c('sup_id', 'from_tel', 'from_addr'))

# features 
df_feat <- read.csv(file.path(data_dir ,'data_derived.csv'), stringsAsFactors = F)

#join
df_all <- left_join(df_all , df_feat, by='id')



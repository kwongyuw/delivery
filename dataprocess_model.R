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
sup <- read.csv(file.path(data_dir ,'sup.csv'), stringsAsFactors = F)

# join
df_all <- left_join(a_df, sup, by=c('sup_id', 'from_tel', 'from_addr'))

# features 
df_feat <- read.csv(file.path(data_dir ,'data_derived.csv'), stringsAsFactors = F)

#join
df_all <- left_join(df_all , df_feat, by='id')

#######
# left over time is calculated after subtracting cooking time and travel time from prereq
df_all <- df_all %>% 
  mutate(left_m = left1/60,
         left_20 = ifelse(left_m >= 20, 1, 0))

sum(is.na(df_all$prereq))
sum(is.na(df_all$left1))
table(df_all$left_20)
##### Look at descriptives for features 

# require time is a bucket of values 
df_all <- df_all %>% 
  mutate(tmref_cat = case_when(require_tmref >= 11.5 &  require_tmref <= 14.5 ~ "lunch",
                               require_tmref >= 17.5 &  require_tmref <= 20.5 ~ "dinner",
                               TRUE ~ "other"))

prop.table(table(df_all$tmref_cat))

### Do proper subsets 
crt_df <- df_all %>%
  filter(prereq > 2100 & prereq < 3600,
         left_m >= -120 & left_m < 120,
         dist <= 140000,
         user_exp < 200)

rm(a_df)
rm(df_feat)
rm(dist_df)
rm(or_df)
rm(sup)
rm(temp)
library(tidyverse)
library(stringr)
library(openxlsx)
library(data.table)
library(readxl)

#data_dir <- '/Volumes/GoogleDrive/Team Drives/delivery/data'
data_dir <- 'dwb_Data'


temp = list.files(path = data_dir, pattern="*.xlsx$")


or_df <- do.call(bind_rows,
                 lapply(file.path(data_dir ,temp), read_excel, range = cell_cols(1:21)))

# Load everything that has the names dist and append 
temp = list.files(path = data_dir, pattern="dist")
temp

dist_df <- do.call(bind_rows,
                   lapply(file.path(data_dir ,temp), read.csv, stringsAsFactors = FALSE)) %>%
  unique(.)
# many orders in Aug 1-17 are missing in distance .csv files (options: 1) generalize from collected addresses, 2) collect again)


# Join to main data using "id" 
a_df <- left_join(or_df, 
                  mutate(dist_df, time=time/60), # std to mins
                  by = "id")

# restaurant information 
sup <- read.csv(file.path(data_dir ,'sup.csv'), stringsAsFactors = F)

# join
df_all <- left_join(a_df, sup, by=c('sup_id', 'from_tel', 'from_addr'))

# features 
df_feat <- read.csv(file.path(data_dir ,'data_derived.csv'), stringsAsFactors = F) %>%
            type_convert() # ensure the date in correct type for weather match

# IV by Weather: NCDC_NOAA on Hongqiao, hourly ####
source("git/delivery/weather.R")
df_feat_iv <- left_join(df_feat, hq, by = c("place_date"="date", "place_tmref"="tmref"), 
                suffix=c("","_hqp"))
df_feat_iv <- left_join(df_feat_iv, hq, by = c("require_date"="date", "require_tmref"="tmref"), 
                suffix=c("","_hqr"))
df_feat_iv <- left_join(df_feat_iv, pd, by = c("place_date"="date", "place_tmref"="tmref"), 
                suffix=c("","_pdp"))
df_feat_iv <- left_join(df_feat_iv, pd, by = c("require_date"="date", "require_tmref"="tmref"), 
                suffix=c("","_pdr"))
hqp_cols <- (names(df_feat_iv) %in% names(hq)) # coz hqp uses no suffix as 1st set
names(df_feat_iv)[hqp_cols] <- paste(names(df_feat_iv)[hqp_cols], "hqp", sep="_")
remove(hqp_cols)

#join
df_all <- left_join(df_all , df_feat_iv, by='id')

#######
# left over time is calculated after subtracting cooking time and travel time from prereq
df_all <- df_all %>% 
  mutate(left_m = left1, # eqv when std to mins 
         left2 = prereq - (prepare+time), left2_m = left2, # eqv when std to mins
         left_20 = ifelse(left_m >= 20, 1, 0), left2_20 = ifelse(left2_m >= 20, 1, 0))

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

# put cords back to numeric lat lon
df_all <- separate(df_all, o_cords, c("o_lat", "o_lon"), sep=",", remove=FALSE, convert=TRUE) %>%
          separate(d_cords, c("d_lat", "d_lon"), sep=",", remove=FALSE, convert=TRUE)

### Do proper subsets 
crt_df <- df_all %>%
  filter(delay < 100, prereq > 0 & prereq < 100, # in minutes #prereq>2100, prereq<3600
         left_m < 240,
         price>0, price <= 15000, paid <= 15000, rider_income <= 1300,
         ride <= 1500, dist <= 140000, time <= 200,
         user_exp < 75, u_price_avg<15000,
         !is.infinite(ow_ratio), ow_ratio<3)
#remove non-Shanghai coordinates 
crt_df$o_lat[crt_df$o_lat>31.5] <- NA
crt_df$o_lat[crt_df$o_lat<31.1] <- NA
crt_df$d_lat[crt_df$d_lat>31.5] <- NA
crt_df$d_lat[crt_df$d_lat<31.1] <- NA
crt_df$o_lon[crt_df$o_lon>121.7] <- NA
crt_df$o_lon[crt_df$o_lon<121.3] <- NA
crt_df$d_lon[crt_df$d_lon>121.7] <- NA
crt_df$d_lon[crt_df$d_lon<121.3] <- NA

rm(a_df, df_feat, df_feat_iv,dist_df, or_df, sup, temp)

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

# prereq is a raw time value 
df_all %>% 
  mutate(prereq_m = prereq/60) %>%
  select(prereq_m) %>%
  filter(prereq_m >= 0 & prereq_m < 500) %>%
  ggplot(., aes(x=prereq_m)) + 
  geom_histogram(stat = "count")


df_all %>% mutate(prereq_m = prereq/60) %>% 
  select(prereq_m) %>%
  filter(prereq_m > 39 & prereq_m < 41) %>%
  ggplot(., aes(x=prereq_m)) + 
  geom_histogram(stat = "count")


df_all %>% mutate(prereq_m = prereq/60) %>%
  select(prereq_m) %>%
  filter(prereq_m >= 0 & prereq_m <= 40) %>%
  ggplot(., aes(x=prereq_m)) + 
  geom_histogram(stat = "count")
#######
# left over time is calculated after subtracting cooking time and travel time from prereq
names(df_all)

df_all %>% select(left1) %>% 
  mutate(left_m = left1/60) %>%
  filter(left_m >= 0 & left_m < 60) %>% 
  ggplot(., aes(x=left_m)) + 
  geom_histogram(stat = "count")

df_all <- df_all %>% 
  mutate(left_m = left1/60,
         left_20 = ifelse(left_m >= 20, 1, 0))

sum(is.na(df_all$prereq))
sum(is.na(df_all$left1))
table(df_all$left_20)
##### Look at descriptives for features 

df_all %>% select(require_tm, require_tmref,user_exp, u_price_avg, left_m) %>%
  summary(.)

summary(df_all$left_m)

df_all %>% select(left1, require_tmref) %>% 
  mutate(left_m = left1/60) %>% 
  filter(left_m >= 0 & left_m < 60) %>% 
  ggplot(., aes(x=require_tmref)) + 
  geom_histogram(stat = "count")

 summary(df_all$require_tmref)

table(df_all$require_tmref)

# require time is a bucket of values 
df_all <- df_all %>% 
  mutate(tmref_cat = case_when(require_tmref >= 11.5 &  require_tmref <= 14.5 ~ "lunch",
                               require_tmref >= 17.5 &  require_tmref <= 20.5 ~ "lunch",
                               TRUE ~ "other"))

prop.table(table(df_all$tmref_cat))

######User experiance
df_all %>% select(left1, require_tmref, user_exp, u_price_avg) %>% 
  filter(user_exp < 200) %>%
  ggplot(., aes(x=user_exp)) + 
  geom_histogram(stat = "count")

#
df_all %>% select(left1, require_tmref, user_exp, u_price_avg) %>% 
  mutate(left_m = left1/60) %>%
  filter(left_m >= 0 & left_m < 60) %>% 
  filter(user_exp < 200) %>% nrow()

df_all %>% nrow()

### Do proper subsets 
crt_df <- df_all %>%
  filter(prereq >2400 & prereq < 3600,
        left_m >= -120 & left_m < 120,
         dist <= 140000,
         user_exp < 200)

ggplot(crt_df, aes(x=left_m)) + 
  geom_histogram(stat = "count")

summary(crt_df$left_m)

table(crt_df$left_20)

####SEE comparison of 20min leftover 
crt_df %>%
  select(u_price_avg, tmref_cat,time, user_exp, dist, delay) %>%
  summary()

crt_df %>%
  mutate(delay_m = delay/60) %>%
  select(left_20, u_price_avg, time, user_exp, dist, delay_m) %>%
  group_by(left_20) %>%
  summarise_all(funs(median))

crt_df %>%
  mutate(delay_m = delay/60) %>%
  select(left_20, u_price_avg, time, user_exp, dist, delay_m) %>%
  group_by(left_20) %>%
  summarise_all(funs(mean))

crt_df %>%
  select(left_20,tmref_cat) %>%
  group_by(left_20, tmref_cat) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n))
#############
## outcome 
crt_df %>% 
  mutate(delay_m = delay/60) %>%
  ggplot(., aes(x=delay_m)) + 
  geom_histogram(stat = "count")
####




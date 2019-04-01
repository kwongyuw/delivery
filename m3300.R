library(tidyverse)
library(stargazer)

source('/Users/kwongyu/Google Drive/dwb/git/delivery/dataprocess_model.R')

rider <- group_by(df) %>% group_by(rider_id, read_date, r_shift) %>% 
  summarize(r_inshift_n=n()) %>%
  arrange(rider_id, read_date, r_shift) %>%
  group_by(rider_id) %>%
  mutate(r_exp_n=cumsum(r_inshift_n))
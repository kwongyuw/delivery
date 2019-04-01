library(tidyverse)
library(stargazer)

source('/Users/kwongyu/Google Drive/dwb/git/delivery/dataprocess_model.R')

rider_m <- group_by(crt_df, rider_id, month = month(finish_tm)) %>% 
  summarise(orig_inc = sum(rider_org_income), inc = sum(rider_income))

ggplot(filter(rider_m, inc/100 > 10), aes(x=inc/100)) + 
  stat_bin(binwidth = 10, alpha=0.1) 

plot(as.factor(rider_m$month), rider_m$inc/100, cex=0.5)
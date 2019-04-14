library(tidyverse)
library(stargazer)

source('/Users/kwongyu/Google Drive/dwb/git/delivery/dataprocess_model.R')

# if use crt_df (after cleaning) only 142 ppl left
rider_m <- mutate(crt_df, roi_subs=rider_org_income+300, ri_subs=rider_income+300) %>%
  group_by(rider_id, month = month(finish_tm)) %>%
  summarise(orig_inc = sum(rider_org_income), inc = sum(rider_income), 
            orig_inc_subs=sum(roi_subs), inc_subs=sum(ri_subs),
            num=n())
ggplot(filter(rider_m, inc/100 > 10), aes(x=inc/100)) + 
  stat_bin(binwidth = 10, alpha=0.1) 
plot(as.factor(rider_m$month), rider_m$inc/100, cex=0.5)


# if use df_all (no cleaning), 866 riders
rider_ma <- mutate(df_all, roi_subs=rider_org_income+300, ri_subs=rider_income+300) %>%
  group_by(rider_id, month = month(finish_tm)) %>%
  summarise(orig_inc = sum(rider_org_income), inc = sum(rider_income), 
            orig_inc_subs=sum(roi_subs), inc_subs=sum(ri_subs),
            num=n())

ggplot(filter(rider_ma, inc/100 > 10), aes(x=inc_subs/100, fill=month)) + geom_histogram(binwidth = 5)
plot(as.factor(rider_ma$month), rider_ma$inc_subs/100, cex=0.5)

plus3ka <- filter(rider_ma,inc_subs/100>3000)


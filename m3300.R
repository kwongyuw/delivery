library(tidyverse)
library(stargazer)

source('/Users/kwongyu/Google Drive/dwb/git/delivery/dataprocess_model.R')

df <- group_by(df_all, rider_id, month=month(finish_tm)) %>%
  select(rider_id, month, ends_with("_tm"), rider_income, dist, time) %>%
  mutate(ri_subs=rider_income+300,  count_atm=order(order(finish_tm)), 
         inc_atm=cumsum(rider_income),sinc_atm=cumsum(ri_subs),
         excel_month=(max(inc_atm)/100>3000), sexcel_month=(max(sinc_atm)/100>3000), 
         speed = dist/ifelse(finish_tm!=leave_tm, as.numeric(finish_tm - leave_tm), NA),
         bdunexp = ifelse(finish_tm!=leave_tm, as.numeric(finish_tm - leave_tm), NA)/time) %>%
  filter(dist<14000, speed<30, time>0)

summary(filter(df, inc_atm/100<=3000)$speed)
summary(filter(df, inc_atm/100>3000)$speed)

# Exceling month X before/after $3000
## speed
summary(filter(df, !excel_month, inc_atm/100<3000)$speed)
summary(filter(df, excel_month, inc_atm/100<3000)$speed)
summary(filter(df, excel_month, inc_atm/100>=3000)$speed)
table(df$excel_month, (df$inc_atm/100>=3000))
summary(filter(df, !sexcel_month, sinc_atm/100<3000)$speed)
summary(filter(df, sexcel_month, sinc_atm/100<3000)$speed)
summary(filter(df, sexcel_month, sinc_atm/100>=3000)$speed)
table(df$sexcel_month, (df$sinc_atm/100>=3000))
## complication in reality (deliver time vs baidu-map expected time)
summary(filter(df, !excel_month, inc_atm/100<3000)$bdunexp)
summary(filter(df, excel_month, inc_atm/100<3000)$bdunexp)
summary(filter(df, excel_month, inc_atm/100>=3000)$bdunexp)
table(df$excel_month, (df$inc_atm/100>=3000))
summary(filter(df, !sexcel_month, sinc_atm/100<3000)$bdunexp)
summary(filter(df, sexcel_month, sinc_atm/100<3000)$bdunexp)
summary(filter(df, sexcel_month, sinc_atm/100>=3000)$bdunexp)
table(df$sexcel_month, (df$sinc_atm/100>=3000))



# if use df_all (no cleaning), 866 riders >$3000
rider_ma <- mutate(df, ri_subs=rider_income+300) %>%
  group_by(rider_id, month) %>%
  summarise(inc = sum(rider_income), inc_subs=sum(ri_subs),
            num=n(), speed_avg=mean(speed, na.rm=TRUE), speed_sd=sd(speed, na.rm=TRUE))
summary(filter(rider_ma, inc_subs/100<=3000)$speed_avg)
summary(filter(rider_ma, inc_subs/100>3000)$speed_avg)


ggplot(filter(rider_ma, inc/100 > 10), aes(x=inc_subs/100, fill=month)) + geom_histogram(binwidth = 5)
plot(as.factor(rider_ma$month), rider_ma$inc_subs/100, cex=0.5)

plus3ka <- filter(rider_ma,inc_subs/100>3000)

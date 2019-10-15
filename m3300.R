
library(tidyverse)
library(stargazer)

source('/Users/kwongyu/Google Drive/dwb/git/delivery/dataprocess_model.R')

df <- group_by(df_all, rider_id, month=month(finish_tm)) %>%
  select(rider_id, id, month, ends_with("_tm"), rider_income, dist, time, onhand) %>%
  mutate(ri_subs=rider_income+300,  count_atm=order(order(finish_tm)), 
         inc_atm=cumsum(rider_income),sinc_atm=cumsum(ri_subs),
         excel_month=(max(inc_atm)/100>3000), sexcel_month=(max(sinc_atm)/100>3000), 
         speed = dist/ifelse(finish_tm!=leave_tm, as.numeric(finish_tm - leave_tm), NA),
         speed_wt = speed*onhand, # magnify speed by multi-orders on hand
         bdunexp = ifelse(finish_tm!=leave_tm, as.numeric(finish_tm - leave_tm), NA)/time,
         lunch_hr = (hour(require_tm)>=10 & hour(require_tm)<14), 
         dinner_hr= (hour(require_tm)>=17 & hour(require_tm)<20)) %>%
  filter(dist<14000, speed<30, time>0) %>%
  ungroup()
df$month_end <- 31
df$month_end[df$month==9] <- 30
df$remain_days <- df$month_end - day(df$finish_tm)

summary(filter(df, inc_atm/100<=3000)$speed)
summary(filter(df, inc_atm/100>3000)$speed)

# when approaching 3000, seems to slow down (reaching $2400 before 25th day)
thres<-3000
for (i in c(3,5,7)) {
  print(paste0(i,"-day window"))
  focus <- unique(filter(df,remain_days<=i, inc_atm/100>=(thres-100*i), inc_atm/100<(thres+100*i))$rider_id)
  succeed <- unique(filter(df,inc_atm/100>=thres)$rider_id)
  
  # speed_wt (weight the speed to handle multi-order issue)
  ## descriptive doesn't immediately supportive
  print(summary(filter(df,rider_id %in% focus, rider_id %in% succeed, 
                       remain_days<=i, inc_atm/100<thres,inc_atm/100>=(thres-100*i))$speed_wt))
  print(summary(filter(df,rider_id %in% focus, rider_id %in% succeed, 
                       remain_days<=i,inc_atm/100>=thres, inc_atm/100<(thres+100*i))$speed_wt))
  filter(df,rider_id %in% focus, rider_id %in% succeed, remain_days<=i) %>%
    ggplot(aes(x=speed,fill=as.factor(inc_atm/100>=thres))) + 
    stat_density(alpha=0.4, position="identity")
  
  ## regression shows support
  ### stat sign slower >$3000
  temp <- filter(df,rider_id %in% focus, rider_id %in% succeed, 
                 remain_days<=i, inc_atm/100<(thres+100*i),inc_atm/100>=(thres-100*i)) %>%
    mutate(inc_atm2=inc_atm^2)
  temp_lm <- lm(speed_wt ~ as.factor(inc_atm/100>thres) + inc_atm + inc_atm2 +
                  lunch_hr + as.factor(rider_id), 
                data=temp) %>%
    summary()
  print(head(temp_lm$coefficients))
  #### placebo check: insign for 2600..3500, except stat sign at 3200
  
  # number of orders (avoid multi-order issue)
  ## before threshold
  temp <- filter(df,rider_id %in% focus, rider_id %in% succeed,
                 remain_days<=i, inc_atm/100<thres,inc_atm/100>=(thres-100*i))
  table(temp$lunch_hr,temp$dinner_hr) %>%
    print()
  ## after threshold
  temp <- filter(df,rider_id %in% focus, rider_id %in% succeed,
                 remain_days<=i, inc_atm/100>=thres, inc_atm/100<(thres+100*i))
  table(temp$lunch_hr,temp$dinner_hr) %>%
    print()
  ### when i=3, opposite result if chg the bar to 2700, and diff if 2800 
  ### placebo check: supportive for 2700,2800,2300)
}


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
            num=n(), speed_avg=mean(speed, na.rm=TRUE), speed_sd=sd(speed, na.rm=TRUE),
            bdunexp_avg=mean(bdunexp,na.rm=TRUE), bdunexp_sd=sd(bdunexp,na.rm=TRUE))
summary(filter(rider_ma, inc_subs/100<=3000)$speed_avg)
summary(filter(rider_ma, inc_subs/100>3000)$speed_avg)


ggplot(filter(rider_ma, inc/100 > 10), aes(x=inc_subs/100, fill=month)) + geom_histogram(binwidth = 5)
plot(as.factor(rider_ma$month), rider_ma$inc_subs/100, cex=0.5)

plus3ka <- filter(rider_ma,inc_subs/100>3000)
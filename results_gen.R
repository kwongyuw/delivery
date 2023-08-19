library(caret)
library(tidyverse)
# detach("package:tidyverse", unload=TRUE)
library(stargazer)
library(AER)
library(hms)
library(ggpubr)
#source('~/Documents/eScience/projects/delivery/dataprocess_model.R') # data from control groups
setwd("/Users/kwongyu/OneDrive - UW/Projects/dwb")
source('git/delivery/dataprocess_model.R')

# Descriptive statistics ####
df_all %>% 
  ggplot(aes(x=require_tm)) + geom_histogram()

sg_ss <- function(df, stats=c("n", "mean", "sd", 
                              "p25", "median", "p75", "max"),
                  disp="text") {
  as.data.frame(df) %>%
    stargazer(summary.stat = stats, type=disp) %>%
    return()
}

## order level
select(crt_df, prereq, prepare, ride, delay, rider_income, price) %>% # avoid dist & time for now
  mutate(rider_income=rider_income/100, price=price/100) %>%
  as.data.frame() %>%
  sg_ss(disp="latex")

## user level
select(crt_df, user_id, u_delay_avg, u_lunch_avg, u_n_tt, u_prereq_avg, u_price_avg, u_price_sd, u_rinc_avg, u_span) %>%
  distinct() %>%
  select(-user_id) %>%
  sg_ss(disp="latex")

## rider level
select(crt_df, rider_id, r_exp_n, r_inshift_n, r_shift) %>%
  group_by(rider_id) %>%
  summarize(exp_max=max(r_exp_n, na.rm=TRUE), exp_min=min(r_exp_n, na.rm=TRUE),
            orders_max=max(r_inshift_n, na.rm=TRUE), orders_min=min(r_inshift_n, na.rm=TRUE)) %>%
  ungroup() %>%
  select(-rider_id) %>%
  sg_ss(disp="latex")

## restaurant level
select(crt_df, sup_id, sup_exp) %>%
  group_by(sup_id) %>%
  summarize(exp_max=max(sup_exp, na.rm=TRUE), exp_min=min(sup_exp, na.rm=TRUE)) %>%
  ungroup() %>%
  select(-sup_id) %>%
  sg_ss(disp="latex")
# why the min exp can be as many as 716 orders in restaurant and 226 in riders?
# <- because they accumulate exp in Aug 1-17

# descriptive graphs ####
## peak hours (use as.hms())
mutate(crt_df, require_hms = as_hms(require_tm)) %>%
  ggplot(aes(x=require_hms, y=delay)) + 
  geom_point(size=0.1, alpha=0.1) +
  geom_smooth()

## follow through users
temp <- group_by(crt_df, user_id, sup_id) %>%
  filter(tmref_cat=='lunch') %>%
  arrange(user_id, sup_id, place_tm) %>% # same user same restaurant
  mutate(delay_lag1 = lag(delay), require_tm_lag1=lag(require_tm), place_tm_lag1=lag(place_tm), prereq_lag1=lag(prereq), 
         tt_order=n(), occ=order(place_tm, decreasing=FALSE)) %>%
  ungroup() %>%
  select(user_id, sup_id:price, place_tm, require_tm, dipatch_tm, finish_tm, 
         tmref_cat, delay, delay_lag1, require_tm_lag1, place_tm_lag1, prereq, prereq_lag1, tt_order, occ) %>%
  arrange(desc(tt_order), user_id, sup_id, place_tm) %>%
  filter(!is.na(delay_lag1))

### given user and restaurant, place_tm variation dominates prereq variation
temp1 <- filter(temp, occ==10, hour(require_tm)==12, minute(require_tm)<=30) %>%
  mutate(pair_id=paste(user_id,'_', sup_id))
temp1 <- mutate(temp, pair_id=paste(user_id, '_', sup_id)) %>%
  filter(pair_id %in% temp1$pair_id)
i = 0
g1 <- filter(temp1, sup_id==names(sort(table(temp1$sup_id))[length(table(temp1$sup_id))-i])) %>% 
  ggplot(aes(x=occ, color=as.character(user_id), y=as_hms(place_tm))) + geom_line() +
  geom_line(aes(y=as_hms(require_tm))) +
  coord_cartesian(xlim=c(0,30), ylim=c(as_hms('10:00:00'), as_hms('14:00:00')))
g2 <- filter(temp1, sup_id==names(sort(table(temp1$sup_id))[length(table(temp1$sup_id))-i])) %>% 
  ggplot(aes(x=occ, color=as.character(user_id), y=prereq)) + geom_line() +
  coord_cartesian(xlim=c(0,30), ylim=c(0,240)) 
ggarrange(g1,g2, ncol=1, align = 'v')

# More competitive days, place the orders earlier ####
temp <- crt_df %>% mutate(require_tm_min=minute(require_tm)) %>%
  group_by(require_date, tmref_cat) %>%
  mutate(day_orders = n()) %>% 
  group_by(require_date, tmref_cat, day_orders) %>%
  summarize(prereq_avg=mean(prereq), prereq_med=median(prereq),
            place_avg=mean(as_hms(place_tm)), place_med=median(as_hms(place_tm))) %>%
  ungroup()
filter(temp, tmref_cat %in% c('lunch', 'dinner')) %>%
  ggplot(aes(x=day_orders, y=prereq_avg)) + geom_point() + geom_smooth(method='lm')

temp <- crt_df %>% mutate(require_tm_min=minute(require_tm)) %>%
  group_by(require_date, tmref_cat) %>%
  mutate(day_orders = n()) %>% 
  group_by(require_date, tmref_cat, day_orders, require_tmref) %>%
  summarize(prereq_avg=mean(prereq), prereq_med=median(prereq),
            place_avg=mean(as_hms(place_tm)), place_med=median(as_hms(place_tm))) %>%
  ungroup()
filter(temp, tmref_cat %in% c('lunch', 'dinner')) %>%
  ggplot(aes(x=day_orders, y=prereq_avg)) + geom_point() + geom_smooth(method='lm')
filter(temp, tmref_cat %in% c('lunch', 'dinner')) %>%
  ggplot(aes(x=day_orders, y=place_med, color=as.character(require_tmref))) + geom_point() + geom_smooth(method='lm')


# for individual user, if prev order too late or too early, increase or decrease prereq ####
temp <- group_by(crt_df, user_id, sup_id) %>%
  filter(tmref_cat=='lunch') %>%
  arrange(user_id, sup_id, place_tm) %>% # same user same restaurant
  mutate(delay_lag1 = lag(delay), require_tm_lag1=lag(require_tm), place_tm_lag1=lag(place_tm), prereq_lag1=lag(prereq), 
         tt_order=n(), occ=order(place_tm, decreasing=FALSE)) %>%
  ungroup() %>%
  select(user_id, sup_id:price, place_tm, require_tm, dipatch_tm, finish_tm, 
         tmref_cat, delay, delay_lag1, require_tm_lag1, place_tm_lag1, prereq, prereq_lag1, tt_order, occ) %>%
  arrange(desc(tt_order), user_id, sup_id, place_tm) %>%
  filter(!is.na(delay_lag1))
## by the number of repeat order
### focus on the 2nd order
temp %>%  # small sample of previous order for 12:05
  filter(hour(require_tm_lag1)==12, minute(require_tm_lag1)==5, occ == 2) %>% 
  ggplot(aes(x=delay_lag1, y=(prereq-prereq_lag1))) + geom_point(alpha=0.1) + geom_smooth(method='lm') +
  coord_cartesian(ylim=c(-60,60))

temp %>% 
  filter(abs(as.numeric(require_tm - require_tm_lag1, units='mins'))<=15, occ == 2) %>% 
  ggplot(aes(x=delay_lag1, y=(prereq-prereq_lag1))) + geom_point(alpha=0.1) + geom_smooth(method='lm') +
  coord_cartesian(ylim=c(-60,60))

temp %>%
  filter(occ == 2) %>% 
  ggplot(aes(x=delay_lag1, y=(prereq-prereq_lag1))) + geom_point(alpha=0.1) + geom_smooth(method='lm') +
  coord_cartesian(ylim=c(-60,60))
## by any previous order
temp %>% # small sample of previous order for 12:05
  filter(hour(require_tm_lag1)==12, minute(require_tm_lag1)==5) %>% 
  ggplot(aes(x=delay_lag1, y=(prereq-prereq_lag1))) + geom_point(alpha=0.1) + geom_smooth(method='lm')
temp %>% # all data
  ggplot(aes(x=delay_lag1, y=(prereq-prereq_lag1))) + geom_point(alpha=0.1) + geom_smooth(method='lm')

### colored first 6 orders 
### (90% data, sharpest adj in 2nd order)
temp %>% # small sample of previous order for 12:05
  filter(hour(require_tm_lag1)==12, minute(require_tm_lag1)==5, occ < 7) %>% 
  ggplot(aes(x=delay_lag1, y=(prereq-prereq_lag1), color=as.character(occ))) + geom_point(alpha=0.1) + geom_smooth(method='lm')

## qualitatively similar result in place_tm as in prereq
temp %>% # small sample of previous order for 12:05
  filter(hour(require_tm_lag1)==12, minute(require_tm_lag1)==5, occ==2) %>% #, prereq-prereq_lag1!=0
  ggplot(aes(x=delay_lag1, y=as.numeric(as_hms(place_tm) - as_hms(place_tm_lag1), units="mins"))) + 
  geom_point(alpha=0.1) + geom_smooth(method='lm')

# how many percentile raised for median lunch user calling 1 minute earlier ####
## for lunch across all 73 days
temp <- filter(crt_df, tmref_cat=='lunch') %>%
  mutate(place_hms=as_hms(place_tm), place_med=median(place_hms)) %>%
  arrange(place_hms) %>%
  mutate(place_rank=rank(place_hms, ties.method='min')) %>%
  select(user_id, sup_id, rider_id, place_tm, require_tm, finish_tm, 
         place_hms, place_med, place_rank)
temp %>% mutate(place_pct=place_rank/nrow(temp)) %>%
  filter(place_hms==place_med | place_hms==(place_med-60)) %>% View()
(0.4999115-0.4936143)*nrow(temp)/length(unique(as.Date(temp$require_tm)))
# 14.6 users earlier
## for lunch by the median-busy lunch date, 2015-08-30
temp <- filter(crt_df, tmref_cat=='lunch', require_date==ymd("2015-08-30")) %>%
  mutate(place_hms=as_hms(place_tm), place_med=median(place_hms)) %>%
  arrange(place_hms) %>%
  mutate(place_rank=rank(place_hms, ties.method='min')) %>%
  select(user_id, sup_id, rider_id, place_tm, require_tm, finish_tm, 
         place_hms, place_med, place_rank)
temp %>% mutate(place_pct=place_rank/nrow(temp)) %>%
  filter( abs(place_hms - place_med)<60) %>% View()
# 1134 -> 1110, 0.5002206 -> 0.4896339


# regressions ####
temp <- crt_df %>%
  mutate(disap = delay+10, disap = ifelse(disap >-5 & disap <5, 0, disap), # not helpful looking at range
         delay_cens = pmax(0, delay), # only looking at the time being late
         sudden_rain_pdp = ifelse(is.na(pcp_begin_pdp), FALSE, pcp_begin_pdp==tm_pdp),
         sudden_rain_hqp = ifelse(is.na(pcp_begin_hqp), FALSE, pcp_begin_hqp==tm_hqp))

## plain OLS ####
lm1 <- lm((delay) ~ prereq +prepare+price +  
            rider_exp + user_exp + onhand + ow_ratio + rider_income + dist,
          data=temp)
lm1i <- lm((delay) ~ prereq +prepare+price +  
             rider_exp + user_exp + rider_income,
          data=temp)
lm1ii <- lm((delay) ~ prereq + price + rider_income,
           data=temp)
lm1iii <- lm((delay) ~ prereq +prepare+price +  
            rider_exp + user_exp + onhand + rider_income,
          data=temp)
lm1iv <- lm((delay) ~ prereq +prepare+price +  
            rider_exp + user_exp + onhand + ow_ratio + rider_income,
          data=temp)
### include few more different specifications of plain OLS
stargazer(lm1ii, lm1i, lm1iii, lm1iv,lm1, report="cvt*", omit.stat=c("ser", "rsq"),
          no.space=TRUE, type="latex")


## IVs (temperature & sudden rain) ####
temp %>% filter(!is.na(temp_pdp)) %>%
  ggplot(aes(x=as.factor(round((temp_pdp-32)*5/9)), y=prereq)) + 
  geom_boxplot(varwidth=TRUE) + 
  labs(x="Temperate at Pudong when placing orders (Celsius)", y="Pre-require time")
temp %>% filter(!is.na(temp_hqp)) %>%
  ggplot(aes(x=as.factor(round((temp_hqp-32)*5/9)), y=prereq)) + 
  geom_boxplot(varwidth=TRUE) + 
  labs(x="Temperate at Hongqiao when placing orders (Celsius)", y="Pre-require time")
lm2 <- lm(prereq ~ temp_hqp + temp_pdp +prepare+price +  
            rider_exp + user_exp + onhand + ow_ratio + rider_income + dist,
          data=temp)
lm3 <- lm(prereq ~ temp_hqp + temp_pdp + sudden_rain_hqp + sudden_rain_pdp + prepare+price +  
            rider_exp + user_exp + onhand + ow_ratio + rider_income + dist,
          data=temp)
lm4 <- ivreg((delay) ~ prereq + 
               prepare+price +  
               rider_exp + user_exp + onhand + ow_ratio + rider_income + dist
             | temp_hqp + temp_pdp + # temp comes from data_derived2.csv
               prepare+price +  
               rider_exp + user_exp + onhand + ow_ratio + rider_income + dist,
             data=temp)
lm5 <- ivreg((delay) ~ prereq +
               prepare+price +  
               rider_exp + user_exp + onhand + ow_ratio + rider_income + dist
             | temp_hqp + temp_pdp + sudden_rain_hqp + sudden_rain_pdp +
               prepare+price +  
               rider_exp + user_exp + onhand + ow_ratio + rider_income + dist,
             data=temp)
stargazer(lm4, lm5, lm2, lm3, report="cvt*", omit.stat=c("ser", "rsq"),
          no.space=TRUE, type="latex")

## only the time being late
lm1b <- lm((delay_cens) ~ prereq +prepare+price +  
            rider_exp + user_exp + onhand + ow_ratio + rider_income + dist,
          data=temp)
lm4b <- ivreg((delay_cens) ~ prereq + 
               prepare+price +  
               rider_exp + user_exp + onhand + ow_ratio + rider_income + dist
             | temp_hqp + temp_pdp + # temp comes from data_derived2.csv
               prepare+price +  
               rider_exp + user_exp + onhand + ow_ratio + rider_income + dist,
             data=temp)
lm5b <- ivreg((delay_cens) ~ prereq +
               prepare+price +  
               rider_exp + user_exp + onhand + ow_ratio + rider_income + dist
             | temp_hqp + temp_pdp +
               prepare+price +  
               rider_exp + user_exp + onhand + ow_ratio + rider_income + dist,
             data=temp)
stargazer(lm1b, lm4b, lm5b, report="cvt*", omit.stat=c("ser", "rsq"),
          no.space=TRUE, type="latex")

### fixed required_tm=12:05, use the following can yield the same OLS and IV qualitative result ####
temp <- crt_df %>%
  mutate(disap = delay+10, disap = ifelse(disap >-5 & disap <5, 0, disap), # not helpful looking at range
         delay_cens = pmax(0, delay), # only looking at the time being late
         sudden_rain_pdp = ifelse(is.na(pcp_begin_pdp), FALSE, pcp_begin_pdp==tm_pdp),
         sudden_rain_hqp = ifelse(is.na(pcp_begin_hqp), FALSE, pcp_begin_hqp==tm_hqp)) %>%
  filter((minute(require_tm) == 5), hour(require_tm)==12) # multiple of 5min

## Channels ####
### more are completed and on-tim
# among 30min slots, slots w/ earlier call in does have more orders delivered (and on-time)
slots_30m <- ungroup(temp) %>%
  group_by(require_date, require_tmref, tmref_cat) %>%
  summarize(avg_prereq=mean(prereq, na.rm=TRUE), 
            count=n(), count_ontime=sum(delay < 0, na.rm=TRUE)) %>%
  mutate(Lunch=(tmref_cat=="lunch")) %>%
  ungroup()

#### all orders
slots_30m %>% filter(avg_prereq<60) %>%
  ggplot(aes(x=avg_prereq, y=count, color=Lunch)) + geom_point(alpha=0.1) + geom_smooth() +
  labs(x="Average Pre-require time within 30-min slots", y="Orders Completed") 

#### delay<0 orders
slots_30m %>% filter(avg_prereq<60) %>%
  ggplot(aes(x=avg_prereq, y=count_ontime, color=Lunch)) + geom_point(alpha=0.1) + geom_smooth() +
  labs(x="Average Pre-require time within 30-min slots", y="Orders Completed")


sample_n(temp, 10000) %>%
  mutate(rec=as.numeric(require_tm - receive_tm, units="mins"), 
         lea=as.numeric(require_tm - leave_tm, units="mins")) %>%
  ggplot(aes(x=prereq, y=lea)) + geom_point(alpha=0.1) + geom_smooth()

# categorize prereq to see the difference in trends
call_05m <- mutate(temp, prereq_05m = as.factor((prereq %/% 5)*5),
                   rece = as.numeric(require_tm - receive_tm, units="mins"),
                   leav = as.numeric(require_tm - leave_tm, units="mins"))

## calling in earlier has slightly more time after leaving restaurants
call_05m %>%
  ggplot(aes(x=prereq_05m, y=leav)) + # restaurant receive info earlier
  geom_boxplot(varwidth = TRUE) + 
  coord_cartesian(ylim=c(0,50)) +
  labs(x="Pre-require time (in 5-min slots)", y="Time left after leaving restaurant")

## more time when restaurant receives
call_05m %>%
  #  filter(rece >= 0) %>% # 0.1% observations 
  ggplot(aes(x=prereq_05m, y=rece)) + # restaurant receive info earlier
  geom_boxplot(varwidth = TRUE) +
  coord_cartesian(ylim=c(25,75)) +
  labs(x="Pre-require time (in 5-min slots)", y="Time left after restaurant receives")


## delay pushes to -15mins (ie arrive 15min earlier)
call_05m %>%
  ggplot(aes(x=prereq_05m, y=delay)) + 
  geom_boxplot(varwidth = TRUE) + 
  coord_cartesian(ylim=c(-30, 0))  +
  labs(x="Pre-require time (in 5-min slots)", y="Delay time")



## Require time at whole number minutes (each multiple of 5min)
mutate(crt_df, require_tm_min=minute(require_tm)) %>%
  ggplot(aes(x=require_tm_min, y=prereq, group=require_tm_min)) + 
  geom_boxplot(varwidth=TRUE) + 
  scale_x_continuous(breaks=seq(0, 60, 5))

temp <- crt_df %>%
  mutate(disap = delay+10, disap = ifelse(disap >-5 & disap <5, 0, disap), # not helpful looking at range
         delay_cens = pmax(0, delay), # only looking at the time being late
         sudden_rain_pdp = ifelse(is.na(pcp_begin_pdp), FALSE, pcp_begin_pdp==tm_pdp),
         sudden_rain_hqp = ifelse(is.na(pcp_begin_hqp), FALSE, pcp_begin_hqp==tm_hqp)) %>%
  filter((minute(require_tm) %% 5 == 0)) # multiple of 5min
lm1 <- lm((delay) ~ prereq +prepare+price +  
            rider_exp + user_exp + onhand + ow_ratio + rider_income + dist,
          data=temp)
lm1i <- lm((delay) ~ prereq +prepare+price +  
             rider_exp + user_exp + rider_income,
           data=temp)
lm1ii <- lm((delay) ~ prereq + price + rider_income,
            data=temp)
lm1iii <- lm((delay) ~ prereq +prepare+price +  
               rider_exp + user_exp + onhand + rider_income,
             data=temp)
lm1iv <- lm((delay) ~ prereq +prepare+price +  
              rider_exp + user_exp + onhand + ow_ratio + rider_income,
            data=temp)
stargazer(lm1ii, lm1i, lm1iii, lm1iv,lm1, report="cvt*", omit.stat=c("ser", "rsq"),
          no.space=TRUE, type="text")


# when dipatch_tm-place_tm>50 seems weird because many are dipatched after the required_tm...
ggplot(temp, aes(y=delay, x=(left1))) + geom_point(alpha=0.1) + geom_smooth() # take 12:05 required as an example
View(filter(arrange(temp, desc(delay)), left1>50))


# follow through users and see if they order earlier next time?
## if prev order too late or too early, increase or decrease prereq
temp <- group_by(crt_df, user_id, sup_id) %>%
  filter(tmref_cat=='lunch') %>%
  arrange(user_id, sup_id, place_tm) %>% # same user same restaurant
  mutate(delay_lag1 = lag(delay), require_tm_lag1=lag(require_tm), place_tm_lag1=lag(place_tm), prereq_lag1=lag(prereq), 
         tt_order=n(), occ=order(place_tm, decreasing=FALSE)) %>%
  ungroup() %>%
  select(user_id, sup_id:price, place_tm, require_tm, dipatch_tm, finish_tm, 
         tmref_cat, delay, delay_lag1, require_tm_lag1, place_tm_lag1, prereq, prereq_lag1, tt_order, occ) %>%
  arrange(desc(tt_order), user_id, sup_id, place_tm) %>%
  filter(!is.na(delay_lag1))

temp %>% 
  filter(abs(as.numeric(require_tm - require_tm_lag1, units='mins'))<=15, occ == 2) %>% 
  ggplot(aes(x=delay_lag1, y=as.numeric(as_hms(place_tm) - as_hms(place_tm_lag1), units="mins"))) + 
  geom_point(alpha=0.1) + geom_smooth(method='lm') + coord_cartesian(ylim=c(0,20))





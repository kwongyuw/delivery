library(caret)
library(tidyverse)
# detach("package:tidyverse", unload=TRUE)
library(stargazer)
library(psych)
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

# graphs ####
## peak hours (use as.hms())
mutate(crt_df, require_hms = as_hms(require_tm)) %>%
  ggplot(aes(x=require_hms, y=delay)) + 
  geom_point(size=0.1, alpha=0.1) +
  geom_smooth()

# regressions ####
temp <- crt_df %>%
  mutate(disap = delay+10, disap = ifelse(disap >-5 & disap <5, 0, disap), # not helpful looking at range
         delay_cens = pmax(0, delay), # only looking at the time being late
         sudden_rain_pdp = ifelse(is.na(pcp_begin_pdp), FALSE, pcp_begin_pdp==tm_pdp),
         sudden_rain_hqp = ifelse(is.na(pcp_begin_hqp), FALSE, pcp_begin_hqp==tm_hqp))

## plain OLS
lm1 <- lm((delay) ~ prereq +prepare+price +  
            rider_exp + user_exp + onhand + ow_ratio + rider_income + dist,
          data=temp)
### include few more different specifications of plain OLS
stargazer(lm1, report="cvt*", omit.stat=c("ser", "rsq"),
          no.space=TRUE, type="latex")


## IVs (temperature & sudden rain)
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
stargazer(lm2, lm3, lm4, lm5, report="cvt*", omit.stat=c("ser", "rsq"),
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

## Channels ####
### more are completed and on-tim
# among 30min slots, slots w/ earlier call in does have more orders delivered (and on-time)
slots_30m <- ungroup(temp) %>%
  group_by(require_date, require_tmref) %>%
  summarize(avg_prereq=mean(prereq, na.rm=TRUE), count=n(), count_ontime=sum(delay < -10, na.rm=TRUE)) %>%
  ungroup()

slots_30m %>%
  ggplot(aes(x=avg_prereq, y=count)) + geom_point(alpha=0.1) + geom_smooth() +
  geom_point(aes(y=count_ontime), color="red", alpha=0.1) + geom_smooth(aes(y=count_ontime), color="red")

# calling in earlier has slightly more time after leaving restaurants
sample_n(temp, 10000) %>%
  mutate(rec=as.numeric(require_tm - receive_tm, units="mins"), 
         lea=as.numeric(require_tm - leave_tm, units="mins")) %>%
  filter(rec > -80) %>%
  ggplot(aes(x=prereq, y=lea)) + geom_point(alpha=0.1) + geom_smooth()

# categorize prereq to see the difference
call_05m <- mutate(temp, prereq_05m = as.factor(prereq %/% 5),
                   rece = as.numeric(require_tm - receive_tm, units="mins"))
call_05m %>%
  ggplot(aes(x=prereq_05m, y=delay)) + # delay decreases to -20mins
  geom_boxplot(varwidth = TRUE) + 
  coord_cartesian(ylim=c(-30, 0))

call_05m %>%
  filter(rece >= 0) %>% # 0.1% observations 
  ggplot(aes(x=prereq_05m, y=rece)) + # restaurant receive info earlier
  geom_boxplot(varwidth = TRUE)

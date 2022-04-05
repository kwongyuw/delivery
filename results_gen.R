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

## order level
select(crt_df, prereq, prepare, ride, delay, rider_income, price) %>% # avoid dist & time for now
  as.data.frame() %>%
  stargazer(summary.stat = c("n", "mean", "sd", 
                             "p25", "median", "p75", "max"),
            type="latex")

## user level
select(crt_df, user_id, u_delay_avg, u_lunch_avg, u_n_tt, u_prereq_avg, u_price_avg, u_price_sd, u_rinc_avg, u_span) %>%
  distinct() %>%
  select(-user_id) %>%
  describe(omit=TRUE) %>%
  select(n, mean, sd, median, min, max)

# rider level
select(crt_df, rider_id, r_exp_n, r_inshift_n, r_shift) %>%
  group_by(rider_id) %>%
  summarize(exp_max=max(r_exp_n, na.rm=TRUE), exp_min=min(r_exp_n, na.rm=TRUE),
            orders_max=max(r_inshift_n, na.rm=TRUE), orders_min=min(r_inshift_n, na.rm=TRUE)) %>%
  ungroup() %>%
  select(-rider_id) %>%
  describe(omit=TRUE) %>%
  select(n, mean, sd, median, min, max)

# restaurant level
select(crt_df, sup_id, sup_exp) %>%
  group_by(sup_id) %>%
  summarize(exp_max=max(sup_exp, na.rm=TRUE), exp_min=min(sup_exp, na.rm=TRUE)) %>%
  ungroup() %>%
  select(-sup_id) %>%
  describe(omit=TRUE) %>%
  select(n, mean, sd, median, min, max)
# why the min exp can be as many as 716 orders in restaurant and 226 in riders?


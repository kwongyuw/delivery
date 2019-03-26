rm(list=ls())
library(tidyverse)
library(readxl)
library(lubridate)
library(hms)

#loading all xlsx
setwd('~/Google Drive/dwb/dwb_Data')
data_dir <- '~/Google Drive/dwb/dwb_Data'
temp = list.files(path = data_dir, pattern="*.xlsx$")
df <- do.call(bind_rows,
                 lapply(file.path(data_dir ,temp), read_excel, range = cell_cols(1:21)))

#features wanted:
#(from raw) require_tm, price, delivery fee, sup_id
#(from API) dest neighborhood, sup neighborhood
#(manip raw) order features
#            store frequency (i.e. how experience is restaurant), supplier volume by time
#            user frequency (i.e. how familiar w/ the app), rider frequency (i.e. how experienced is rider)


#(manip raw) order features
#timing info
df <- mutate(df, delay=as.numeric(finish_tm)-as.numeric(require_tm), 
             prereq=as.numeric(require_tm)-as.numeric(place_tm), 
             prepare=as.numeric(leave_tm)-as.numeric(dipatch_tm), 
             ride=as.numeric(finish_tm)-as.numeric(leave_tm), 
             allow_cook=prereq-ride, #cooka=as.numeric(cookable_tm)-as.numeric(cook_tm), 
             left1=as.numeric(dipatch_tm)-as.numeric(place_tm)) #left3=prereq-(dish_longer*60 + ride), #left2=prereq-(cooka+ride), 
              #can't use prereq-(prepare+ride) = (dipatch - place) - delay -> 0.9 R2 for delay regression

#weather when placing order (non-run-able)
#pcp_dur=as.numeric(place_tmref)-as.numeric(pcp_beginref) + as.numeric(as.Date(place_tm)-as.Date(shw_begin))*24, 
#shw_dur=as.numeric(place_tmref)-as.numeric(shw_beginref) + as.numeric(as.Date(place_tm)-as.Date(shw_begin))*24,
#rel_tm=ymd_h(ifelse(hour(place_tm)>=6, (paste(date(place_tm),6)), (paste((date(place_tm)-1),6)))), 
#shw_rdur=pmin(shw_dur,(hour(place_tm)-hour(rel_tm))+(minute(place_tm)-minute(rel_tm))/60)) #shw_rel should always be + , shw_rdur1=pmin(shw_dur,as.numeric(require_tm)-as.numeric(rel_tm))

#(manip raw)rider frequency (i.e. how experienced is rider)
#rider-order ratio (#riders active, #orders to be delivered every 30mins)
###too large to manipulate for work-order ratio(ETA: 5hrs), load from roster30.RData instead 
load("roster30.RData")
### or run the bunch below
#==========detailed 1 day example (non-run-able)=============================================
# make up rider availability
#t <- min(df$read_tm, na.rm=TRUE)
#filter to day-size for easier manipulation
#temp <- filter(df, as.Date(read_tm)==as.Date(t))
#rider <- group_by(temp, rider_id, as.Date(read_tm)) %>%
#  arrange(read_tm) %>%
#new shift if last order is 1hr (3600s) ago
#  mutate(new_shift=ifelse(is.na(lag(finish_tm)),FALSE,((as.numeric(read_tm)-as.numeric(lag(finish_tm)))/3600)>1), shift=cumsum(new_shift)) %>%
# begin time & end time per shift in a day per rider
#  group_by(rider_id, read_date=as.Date(read_tm), shift) %>%
#  summarize(begin=min(read_tm), end=max(finish_tm))
# count active rider at time t: 
# 1) same date, 2) shift has begun, 3) shift will not end in 5mins
#avai <- function(t) {
#  print(t)
#  if (hour(t)==9) {print(Sys.time()-tl30)}
#  size <- filter(rider, as.Date(t)==read_date) %>% # var "rider" determines which file of shift to use
#    filter(as.hms(as.hms(t)+hms(hours=7))>as.hms(as.hms(begin)+hms(hours=7))) %>%
#    filter(as.hms(as.hms(t)+hms(hours=7)+hms(minutes=5)) < as.hms(as.hms(end)+hms(hours=7)))%>%
#    #looking at the 5-min window regardless of frequency per (i.e. same for 30mins or 5min or 1hr)
#    dim()
#  return(size[1])
#}
# rough 30-min monitoring
#min30 <- as.hms(hms(minutes=30*c(0:(13*2))) + hms(((9*60)+30)*60))
#schedule30 <- ymd_hms(paste(rep(unique(rider$read_date),each=length(min30)), min30, sep=" "))
#tl30 <- Sys.time()
#start counting
#avai_n30 <- lapply(schedule30, avai) %>% unlist()
#print(Sys.time()-tl30)
#roster30 <- as.data.frame(avai_n30)
#roster30$tm <- schedule30
#roster30$tmref <- hour(roster30$tm)+((minute(roster30$tm)>=15)&(minute(roster30$tm)<45))*0.5+(minute(roster30$tm)>=45)*1
# we can also do high quality 5-min monitoring...
#order30 <- group_by(df, read_date=as.Date(read_tm), require_tmref) %>%
#  summarise(order_n=n())
#===============================================================================================
remove(roster30_7, roster30_8, roster30_9, worker)
#Merge order & roster to df
df$require_tmref <- hour(df$require_tm)+((minute(df$require_tm)>=15)&(minute(df$require_tm)<45))*0.5+(minute(df$require_tm)>=45)*1
df$require_date <- as.Date(df$require_tm)
roster30$date <- as.Date(roster30$tm)
df <- left_join(df,roster30, by = c("require_date"="date", "require_tmref"="tmref")) %>%
  left_join(order30, by=c("require_date"="read_date", "require_tmref")) %>%
  mutate(ow_ratio = order_n/avai_n30, tm=NULL)

#(manip raw)user frequency (i.e. how familiar w/ the app)
df <- arrange(df, user_id, place_tm) %>% group_by(user_id) %>% mutate(count=1, user_exp=cumsum(count)) %>% group_by()
#rider experience (in # orders finished under data period)
df <- arrange(df, rider_id, read_tm) %>% group_by(rider_id) %>% mutate(rider_exp=cumsum(count)) %>% group_by()
#(manip raw) store frequency (i.e. how experience is restaurant), supplier volume by time
df <- arrange(df, sup_id, read_tm) %>% group_by(sup_id) %>% mutate(sup_exp=cumsum(count)) %>% group_by()
df <- select(df, -count)


#Number of orders finished in each shift
df <- group_by(df, rider_id, read_date=as.Date(read_tm)) %>%
  arrange(read_tm) %>%
  mutate(r_new_shift=ifelse(is.na(lag(finish_tm)),FALSE,((as.numeric(read_tm)-as.numeric(lag(finish_tm)))/3600)>1), 
         r_shift=cumsum(r_new_shift)) %>%
  group_by()
rider <- group_by(df) %>% group_by(rider_id, read_date, r_shift) %>% 
  summarize(r_inshift_n=n()) %>%
  arrange(rider_id, read_date, r_shift) %>%
  group_by(rider_id) %>%
  mutate(r_exp_n=cumsum(r_inshift_n))

df <- left_join(df, rider, by = c("rider_id", "read_date", "r_shift"))


#user features
user <- group_by(df, user_id) %>%
  arrange(user_id, finish_tm) %>%
  summarize(u_n_tt=n(), u_rinc_avg=mean(rider_income), u_delay_avg=mean(delay),
            u_lunch_avg=as.factor(mean(hour(finish_tm))>10 & mean(hour(finish_tm))<14), u_prereq_avg = mean(require_tm - place_tm), 
            u_price_avg=mean(price), u_price_sd=sd(price), u_span=max(finish_tm)-min(finish_tm))

df <- left_join(df, user, by = c("user_id"))

#Weather: NCDC_NOAA on Hongqiao, hourly
df <- mutate(df, place_date=as.Date(place_tm),
             place_tmref = hour(place_tm)+((minute(place_tm)>=15)&(minute(place_tm)<45))*0.5+(minute(place_tm)>=45)*1)
df <- left_join(df,hq, by = c("place_date"="date", "place_tmref"="tmref"), 
                suffix=c("","_hqp"))
df <- left_join(df,hq, by = c("require_date"="date", "require_tmref"="tmref"), 
                suffix=c("","_hqr"))
df <- left_join(df,pd, by = c("place_date"="date", "place_tmref"="tmref"), 
                suffix=c("","_pdp"))
df <- left_join(df,pd, by = c("require_date"="date", "require_tmref"="tmref"), 
                suffix=c("","_pdr"))

#tedious check
#(filter(select(df,place_tm,place_tmref,precipitating,pcp_begin),precipitating==TRUE))[860:870,]
#filter((select(hq, ymdhm, year,month, day, precipitating, switch_to, regime, pcp_begin)),month==11, day==6)[34:40,]


df <- select(df, id, delay:sup_exp, r_shift:shw_begin_pdr)

write.csv(df, 'data_derived.csv')

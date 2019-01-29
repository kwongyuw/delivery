rm(list=ls())
library(tidyverse)
library(readxl)
library(lubridate)
library(hms)

Sys.setlocale(locale="en_us.UTF-8")
setwd('~/Google Drive/dwb/dwb_Data')

data_fn <- '~/Google Drive/dwb/dwb_Data/Aug_C3_P1.xlsx'
df <- read_xlsx(data_fn)

#features wanted:
#(from raw) require_tm, price, delivery fee, sup_id
#(from API) dest neighborhood, sup neighborhood
#(manip raw) order features
#            store frequency (i.e. how experience is restaurant), supplier volume by time
#            user frequency (i.e. how familiar w/ the app), rider frequency (i.e. how experienced is rider)


#(manip raw) order features
#timing info
df <- mutate(df, delay=as.numeric(finish_tm)-as.numeric(require_tm), prereq=as.numeric(require_tm)-as.numeric(place_tm), 
             prepare=as.numeric(leave_tm)-as.numeric(receive_tm), ride=as.numeric(finish_tm)-as.numeric(leave_tm), 
             allow_cook=prereq-ride, #cooka=as.numeric(cookable_tm)-as.numeric(cook_tm), 
             left1=prereq-(prepare+ride)) #left3=prereq-(dish_longer*60 + ride), #left2=prereq-(cooka+ride), 


#weather when placing order (non-run-able)
pcp_dur=as.numeric(place_tmref)-as.numeric(pcp_beginref) + as.numeric(as.Date(place_tm)-as.Date(shw_begin))*24, 
shw_dur=as.numeric(place_tmref)-as.numeric(shw_beginref) + as.numeric(as.Date(place_tm)-as.Date(shw_begin))*24,
rel_tm=ymd_h(ifelse(hour(place_tm)>=6, (paste(date(place_tm),6)), (paste((date(place_tm)-1),6)))), 
shw_rdur=pmin(shw_dur,(hour(place_tm)-hour(rel_tm))+(minute(place_tm)-minute(rel_tm))/60)) #shw_rel should always be + , shw_rdur1=pmin(shw_dur,as.numeric(require_tm)-as.numeric(rel_tm))

#(manip raw)rider frequency (i.e. how experienced is rider)
#worker-order ratio (#workers active, #orders to be delivered every 30mins)
###too large to manipulate for work-order ratio(ETA: 5hrs), load from roster30.RData instead 
load("roster30.RData")
### or run the bunch below
#==========detailed 1 day example (non-run-able)=============================================
# make up worker availability
#t <- min(df$read_tm, na.rm=TRUE)
#filter to day-size for easier manipulation
#temp <- filter(df, as.Date(read_tm)==as.Date(t))
#worker <- group_by(temp, rider_id, as.Date(read_tm)) %>%
#  arrange(read_tm) %>%
#new shift if last order is 1hr (3600s) ago
#  mutate(new_shift=ifelse(is.na(lag(finish_tm)),FALSE,((as.numeric(read_tm)-as.numeric(lag(finish_tm)))/3600)>1), shift=cumsum(new_shift)) %>%
# begin time & end time per shift in a day per rider
#  group_by(rider_id, read_date=as.Date(read_tm), shift) %>%
#  summarize(begin=min(read_tm), end=max(finish_tm))
# count active worker at time t: 
# 1) same date, 2) shift has begun, 3) shift will not end in 5mins
#avai <- function(t) {
#  print(t)
#  if (hour(t)==9) {print(Sys.time()-tl30)}
#  size <- filter(worker, as.Date(t)==read_date) %>% # var "worker" determines which file of shift to use
#    filter(as.hms(as.hms(t)+hms(hours=7))>as.hms(as.hms(begin)+hms(hours=7))) %>%
#    filter(as.hms(as.hms(t)+hms(hours=7)+hms(minutes=5)) < as.hms(as.hms(end)+hms(hours=7)))%>%
#    #looking at the 5-min window regardless of frequency per (i.e. same for 30mins or 5min or 1hr)
#    dim()
#  return(size[1])
#}
# rough 30-min monitoring
#min30 <- as.hms(hms(minutes=30*c(0:(13*2))) + hms(((9*60)+30)*60))
#schedule30 <- ymd_hms(paste(rep(unique(worker$read_date),each=length(min30)), min30, sep=" "))
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



#Number of orders finished in each shift
df <- group_by(df, rider_id, read_date=as.Date(read_tm)) %>%
  arrange(read_tm) %>%
  mutate(new_shift=ifelse(is.na(lag(finish_tm)),FALSE,((as.numeric(read_tm)-as.numeric(lag(finish_tm)))/3600)>1), shift=cumsum(new_shift)) %>%
  group_by()
worker <- group_by(df) %>% group_by(rider_id, read_date, shift) %>% 
  summarize(finished_n=n()) %>%
  arrange(rider_id, read_date, shift) %>%
  group_by(rider_id) %>%
  mutate(exp_n=cumsum(finished_n))

df <- left_join(df, worker, by = c("rider_id", "read_date", "shift"))


#user features
user <- group_by(df, user_id) %>%
  arrange(user_id, finish_tm) %>%
  summarize(n=n(), rinc_avg=mean(rider_income), #, delay_avg=mean(delay)
            lunch=as.factor(mean(hour(finish_tm))>10 & mean(hour(finish_tm))<14), prereq_avg = mean(require_tm - place_tm), 
            price_avg=mean(price), price_sd=sd(price), span=max(finish_tm)-min(finish_tm))


#store info 
df$sup_name <- NA
#via haoma.baidu.com 
df$sup_name[grepl("18217774279",df$from_tel)] <- "CoCo都可茶饮(浦电路店)"
df$sup_name[grepl("18621672116",df$from_tel)] <- "上海联通"   #no info
df$sup_name[grepl("15721077747",df$from_tel)] <- "鲜芋仙(日月光店)"

#via Baidu place information api
#http://api.map.baidu.com/place/v2/search?query=%E6%B5%A6%E4%B8%9C%E6%96%B0%E5%8C%BA%E9%BE%99%E6%B1%87%E8%B7%AF338%E5%8F%B7%E6%96%B0%E9%83%BD%E6%B1%87%E7%BE%8E%E9%A3%9F%E5%B9%BF%E5%9C%BA&region=%E4%B8%8A%E6%B5%B7&scope=2&output=json&ak=
#http://map.baidu.com/detail?qt=ninf&uid=e85e25272a109d4a94f08cee&detail=cater
#but failed to find 鮮芋仙 from api
#http://api.map.baidu.com/place/v2/search?query=%E5%BE%90%E6%B1%87%E5%8C%BA%E5%BE%90%E5%AE%B6%E6%B1%87%E8%B7%AF618%E5%8F%B7%E6%97%A5%E6%9C%88%E5%85%89%E4%B8%AD%E5%BF%83%E5%B9%BF%E5%9C%BA&region=%E4%B8%8A%E6%B5%B7&scope=2&output=json&ak=
#documentation: http://lbsyun.baidu.com/index.php?title=webapi/guide/webservice-placeapi

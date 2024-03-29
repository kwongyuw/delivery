rm(list=ls())
library(tidyverse)
library(readxl)
library(lubridate)
library(hms)

#loading all xlsx
setwd('/Users/kwongyu/OneDrive - UW/Projects/dwb')
data_dir <- 'dwb_Data'
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
#timing info ####
df <- mutate(df, delay=as.numeric(finish_tm - require_tm, units="mins"), 
             prereq=as.numeric(require_tm - place_tm, units="mins"), 
             prepare=as.numeric(leave_tm- dipatch_tm, units="mins"), 
             ride=as.numeric(finish_tm - leave_tm, units="mins"), 
             allow_cook=prereq-ride, #cooka=as.numeric(cookable_tm)-as.numeric(cook_tm), 
             left1=as.numeric(dipatch_tm - place_tm, units="mins")) #left3=prereq-(dish_longer*60 + ride), #left2=prereq-(cooka+ride), 
              #can't use prereq-(prepare+ride) = (dipatch - place) - delay -> 0.9 R2 for delay regression

#weather when placing order (non-run-able)
#pcp_dur=as.numeric(place_tmref)-as.numeric(pcp_beginref) + as.numeric(as.Date(place_tm)-as.Date(shw_begin))*24, 
#shw_dur=as.numeric(place_tmref)-as.numeric(shw_beginref) + as.numeric(as.Date(place_tm)-as.Date(shw_begin))*24,
#rel_tm=ymd_h(ifelse(hour(place_tm)>=6, (paste(date(place_tm),6)), (paste((date(place_tm)-1),6)))), 
#shw_rdur=pmin(shw_dur,(hour(place_tm)-hour(rel_tm))+(minute(place_tm)-minute(rel_tm))/60)) #shw_rel should always be + , shw_rdur1=pmin(shw_dur,as.numeric(require_tm)-as.numeric(rel_tm))

#(manip raw)rider frequency (i.e. how experienced is rider)
#rider-order ratio (#riders active, #orders to be delivered every 30mins) ####
###too large to manipulate for work-order ratio(ETA: 5hrs), load from roster30.RData instead 
load(paste(data_dir, "roster30.RData", sep="/"))
### or run the bunch below
#==========detailed 1 day example (non-run-able)
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
# ===
remove(roster30_7, roster30_8, roster30_9, worker)
#Merge order & roster to df (by 30min slot)
df <- mutate(df, place_date=as.Date(place_tm), require_date=as.Date(require_tm), # time reference for 30min slots (ow ratio & weather IV)
             place_tmref = hour(place_tm)+((minute(place_tm)>=15)&(minute(place_tm)<45))*0.5+(minute(place_tm)>=45)*1,
             require_tmref = hour(require_tm)+((minute(require_tm)>=15)&(minute(require_tm)<45))*0.5+(minute(require_tm)>=45)*1)
roster30$date <- as.Date(roster30$tm)
df <- left_join(df,roster30, by = c("require_date"="date", "require_tmref"="tmref")) %>%
  left_join(ungroup(order30), by=c("require_date"="read_date", "require_tmref")) %>% # avoid group error in order30
  mutate(ow_ratio = order_n/avai_n30, tm=NULL)

#(manip raw)user frequency (i.e. how familiar w/ the app) ####
df <- arrange(df, user_id, place_tm) %>% group_by(user_id) %>% 
        mutate(count=1, user_exp=cumsum(count)) %>% group_by()
#rider experience (in # orders finished under data period) ####
df <- arrange(df, rider_id, read_tm) %>% group_by(rider_id) %>% 
        mutate(rider_exp=cumsum(count)) %>% group_by()
#(manip raw) store frequency (i.e. how experience is restaurant), supplier volume by time ####
df <- arrange(df, sup_id, read_tm) %>% group_by(sup_id) %>% 
        mutate(sup_exp=cumsum(count)) %>% group_by()
df <- select(df, -count)

#Number of orders on-hand ####
multi_order <- drop_na(df,arrive_tm) %>%
          select(rider_id,id, arrive_tm,finish_tm) %>%
          gather(key="action",value="tm",arrive_tm,finish_tm) %>%
          arrange(rider_id,tm) %>%
          mutate(addorder=if_else(action=="arrive_tm",1,-1)) %>%
          group_by(rider_id) %>%
          mutate(onhand=cumsum(addorder), 
                 onhandXdur=onhand*as.numeric(lead(tm)-tm, units="mins")) %>%
          ungroup()
##also, maybe time consuming? 0.3*60=18mins
rids <- unique(multi_order$rider_id)
tl <- Sys.time()
print(tl)
print(paste("expect:", length(unique(multi_order$id))/6879*23/3600))
onhandXdur <- list()
for (r in c(1:length(rids))) {
  short <- filter(multi_order,rider_id==rids[r])
  ids <- unique(short$id)
  temp <- list()
  for (i in c(1:length(ids))) {
    id<-ids[i]
    temp[[i]] <- short[which(short$id==id & short$action=="arrive_tm"):(which(short$id==id & short$action=="finish_tm")-1),] %>% 
                        # beginning of this order                       drop last row as it concerns after finishing this order
      summarize(id=first(id), onhandXdur=sum(onhandXdur))
  }
  onhandXdur[[r]] <- bind_rows(temp)
}
onhandXdur <- bind_rows(onhandXdur)
print(Sys.time()-tl)
#runtime est: length(unique(multi_order$id))/6879*23/3600

df <- left_join(df,select(onhandXdur,id,onhandXdur),
                by="id") %>%
        mutate(onhand = onhandXdur/as.numeric(finish_tm-arrive_tm, units="mins"))

#Number of orders finished in each shift ####
df <- group_by(df, rider_id, read_date=as.Date(read_tm)) %>%
  arrange(read_tm) %>%
  mutate(r_new_shift=ifelse(is.na(lag(finish_tm)),FALSE,((as.numeric(read_tm - lag(finish_tm), units="secs"))/3600)>1), 
         r_shift=cumsum(r_new_shift)) %>%
  group_by()
rider <- group_by(df) %>% group_by(rider_id, read_date, r_shift) %>% 
  summarize(r_inshift_n=n()) %>%
  arrange(rider_id, read_date, r_shift) %>%
  group_by(rider_id) %>%
  mutate(r_exp_n=cumsum(r_inshift_n))

df <- left_join(df, rider, by = c("rider_id", "read_date", "r_shift"))


#user features ####
user <- group_by(df, user_id) %>%
  arrange(user_id, finish_tm) %>%
  summarize(u_n_tt=n(), u_rinc_avg=mean(rider_income), u_delay_avg=mean(delay),
            u_lunch_avg=as.factor(mean(hour(require_tm))>10 & mean(hour(require_tm))<14), # a lunch user on avg? (thou dataprocess_model uses require_tm 11.5-14.5 for lunch & 17.5-20.5 for dinner)
            u_prereq_avg = mean(as.numeric(require_tm - place_tm, units="mins")), 
            u_price_avg=mean(price), u_price_sd=sd(price), 
            u_span=as.numeric(max(finish_tm)-min(finish_tm), units="days")) # u_span: the span user has used for ordering

df <- left_join(df, user, by = c("user_id"))



#tedious check####
#(filter(select(df,place_tm,place_tmref,precipitating,pcp_begin),precipitating==TRUE))[860:870,]
#filter((select(hq, ymdhm, year,month, day, precipitating, switch_to, regime, pcp_begin)),month==11, day==6)[34:40,]

# choice ouput ####
df <- select(df, id, delay:onhand, r_shift:names(df)[ncol(df)]) # names(df)[ncol(df)] = to the last col

write.csv(df, file.path(data_dir, 'data_derived.csv'))


#compete in pre-require time under rationing by waiting
rm(list=ls())
library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(tidyr)
library(stargazer)
library(AER)

Sys.setlocale(locale="en_us.UTF-8")
setwd("~/Google Drive/dwb/dwb_Data")

#Load data first
#peripheral
#1) weather
weather <- read_excel("~/Google Drive/dwb/weather/houbao_daily_20152H.xlsx")
## spliting for day-night condition & max-min temperature
weather <- separate(weather, 天气状况, c("daytime", "nighttime"), sep=" /") %>%
  separate(气温, c("temp_max", "temp_min"), sep=" / ", convert=TRUE)
weather$day_bad <- (weather$daytime %in% c("中到大雨", "阵雨", "中雨", "暴雨", "雷阵雨", "小到中雨", "小雨"))
weather$night_bad <- (weather$nighttime %in% c("阵雨", "中雨", "小雨", "暴雨", "大雨", "雷阵雨", "中雪"))
weather$temp_max <- as.numeric(gsub("℃","", weather$temp_max))
weather$temp_min <- as.numeric(gsub("℃","", weather$temp_min))
##extract year-month-day
weather <- separate(weather, 日期, c("year", "rest"), sep=c("年"), remove=FALSE, convert=TRUE) %>%
  separate(rest, c("month", "day"), sep=c("月"), convert=TRUE)
weather$day <- as.numeric(gsub("日","", weather$day))
#head(weather)


#replicate in Larger files
#load individual files
jul1 <- read_excel("Jul_C3_P1.xlsx")
jul2 <- read_excel("Jul_C3_P2.xlsx")
aug1 <- read_excel("Aug_C3_P1.xlsx")
aug2 <- read_excel("Aug_C3_P2.xlsx")
sep3c <- read_excel("G_Order_Sep_City3(Condensed).xlsx")
dfl <- rbind(jul1,jul2)
dfl <- rbind(dfl, aug1, aug2)
dfl <- rbind(dfl, sep3c)
remove(jul1, jul2, aug1, aug2, sep3c)

#cleaning
dfl <- select(dfl, -c(verify_tm, cooked_tm))
dfl$year <- year(dfl$require_tm)
dfl$month <- month(dfl$require_tm)
dfl$day <- day(dfl$require_tm)


#Weather: houbao (matched by require_date)
dfl <- left_join(dfl,weather, by = c("year", "month","day"))
dfl$date <- paste(dfl$year, dfl$month, dfl$day)
#Weather: xiaoshan (matched by place_hour)
dfl <- mutate(dfl, pl_year=year(place_tm), pl_month=month(place_tm), pl_day=day(place_tm))
dfl$place_tmref <- hour(dfl$place_tm)+((minute(dfl$place_tm)>=15)&(minute(dfl$place_tm)<45))*0.5+(minute(dfl$place_tm)>=45)*1
dfl <- left_join(dfl,xs, by = c("pl_year"="year", "pl_month"="month", "pl_day"="day", "place_tmref"="tmref"))

#adapt data
dfl <- mutate(dfl, delay=as.numeric(finish_tm)-as.numeric(require_tm), prereq=as.numeric(require_tm)-as.numeric(place_tm), 
              prepare=as.numeric(leave_tm)-as.numeric(receive_tm), ride=as.numeric(finish_tm)-as.numeric(leave_tm), 
              allow_cook=prereq-ride, #cooka=as.numeric(cookable_tm)-as.numeric(cook_tm), 
              left1=prereq-(prepare+ride), #left3=prereq-(dish_longer*60 + ride), #left2=prereq-(cooka+ride), 
              pcp_dur=as.numeric(place_tmref)-as.numeric(pcp_beginref) + as.numeric(as.Date(place_tm)-as.Date(shw_begin))*24, 
              shw_dur=as.numeric(place_tmref)-as.numeric(shw_beginref) + as.numeric(as.Date(place_tm)-as.Date(shw_begin))*24,
              rel_tm=ymd_h(ifelse(hour(place_tm)>=6, (paste(date(place_tm),6)), (paste((date(place_tm)-1),6)))), 
              shw_rdur=pmin(shw_dur,(hour(place_tm)-hour(rel_tm))+(minute(place_tm)-minute(rel_tm))/60)) #shw_rel should always be + , shw_rdur1=pmin(shw_dur,as.numeric(require_tm)-as.numeric(rel_tm))

#prune data
dfl <- filter(dfl, price>0, prereq>0)



#call in earlier reduces delay, same for 500k file
gl1 <- ggplot(dfl, aes(x=prereq/60, y=delay/60)) + geom_point(size=0.1, alpha=0.3)
gl1 + geom_smooth() + coord_cartesian(xlim=c(0,1000), ylim=c(-150,150)) + labs(x="minutes ordered in advance", y="delay(mins)", title="Advance ordering and delay")
temp = filter(dfl, prereq>2400, prereq<240*60)
summary(lm(as.numeric(delay)~prereq + price,data=temp))
#lm0<- lm(as.numeric(delay)~ prereq + price,data=dfl)
#lm<- lm(as.numeric(delay)~ prereq ,data=temp)
#lm1<- lm(as.numeric(delay)~ prereq + price,data=temp)
#stargazer(lm0, lm, lm1, type="text", report="cvt*")

#system usually dipatch 50-75mins earlier
#ggplot(dfl, aes(x=prereq/60, y=(as.numeric(require_tm) - as.numeric(dipatch_tm))/60)) + geom_point(size=0.1, alpha=0.3) + geom_smooth() + geom_abline(slope=1,intercept=0) + coord_cartesian(ylim=c(-200,200),xlim=c(0,2500))

#even if those call in too early ones
#lm0<- lm(as.numeric(delay)~ prereq + price,data=filter(temp, prereq>2400, prereq<3600))
#lm<- lm(as.numeric(delay)~ prereq +price,data=filter(temp, prereq>2400, prereq<3600, left1>1200))
#lm1<- lm(as.numeric(delay)~ prereq + price,data=filter(temp,prereq>2400, prereq<3600,left1>2400))
#stargazer(lm0, lm, lm1, type="text", report="cvt*")

#to run in cluster
#set 1 (same effect w/ rider FE)
#temp <- filter(dfl, prereq>2400, prereq<240*60)
#lm0<- lm(as.numeric(delay)~ prereq + price + as.factor(rider_id),data=temp)
#lm<- lm(as.numeric(delay)~ prereq + as.factor(rider_id),data=temp)
#lm1<- lm(as.numeric(delay)~ prereq + price + as.factor(rider_id),data=temp)
#stargazer(lm0, lm, lm1, type="text", omit="^as.factor(rider_id)*", report="cvt*")
toy <- sample_n(dfl, 3000)
temp = filter(toy, prereq>2400, prereq<240*60)



#hour-by-hour effect
#for (i in 0:23) {
#  temp_part <- filter(temp,hour(require_tm)==i)
#  if (dim(temp_part)[1]>0) {
#    print(summary(lm(as.numeric(delay)~prereq + price,data=filter(temp,hour(require_tm)==i))))
#  }
#  print(i)
#}


#IV
ggplot(filter(df, prereq>2400, shw_rdur/60<70),aes(x=shw_rdur/60,y=prereq/60)) + geom_point(size=0.1) + geom_smooth()
summary(ivreg(as.numeric(delay)~prereq|shw_dur, data=temp))
summary(lm(prereq~shw_dur, data=temp))
#summary(ivreg(as.numeric(delay)~prereq|shw_rdur, data=temp))
#summary(lm(prereq~shw_rdur, data=temp))


#also, delay and discrete pricing zone
ggplot(df, aes(y=delay,x=distance, color=as.factor(rider_income))) + geom_point(size=0.1) + coord_cartesian(xlim=c(-100,7500), ylim=c(0,6000))
summary(RDestimate(as.numeric(delay)~distance,cutpoint=2000, filter(df,distance<3000)))
df$m_100<- (df$distance>1950 & df$distance<2050)|(df$distance>2950 & df$distance<3050)|(df$distance>3950 & df$distance<4050)
summary(lm(as.numeric(delay)~cpm,data=filter(df,m_100==TRUE)))
summary(lm(as.numeric(delay)~cpm + prereq + price + menu_num,data=filter(df,m_100==TRUE)))






#HZ data - random 5 days
#individual files
sep1 <- read_excel("G_Order_Sep_City1.xlsx")
oct3 <- read_excel("G_Order_Oct_City3.xlsx")
nov <- read_excel("G_Order_Nov.xlsx")
#combine individual files
df <- rbind(sep1, oct3, nov)
#reduce memory usage
remove(sep1, oct3, nov)



#merge with weather
#Weather: houbao, daily
df$year <- year(df$require_tm)
df$month <- month(df$require_tm)
df$day <- day(df$require_tm)
df <- left_join(df,weather, by = c("year", "month","day")) %>%
  filter(year==2015)
df$date <- paste(df$year, df$month, df$day)
#day vs night weather
df$weather <- df$daytime
df$weather[hour(df$require_tm)>18] <- df$nighttime[hour(df$require_tm)>18]
#Weather: NCDC_NOAA on Xiaoshan, hourly
df <- mutate(df, pl_year=year(place_tm), pl_month=month(place_tm), pl_day=day(place_tm))
df$place_tmref <- hour(df$place_tm)+((minute(df$place_tm)>=15)&(minute(df$place_tm)<45))*0.5+(minute(df$place_tm)>=45)*1
df <- left_join(df,xs, by = c("pl_year"="year", "pl_month"="month", "pl_day"="day", "place_tmref"))
#tedious check
#(filter(select(df,place_tm,place_tmref,precipitating,pcp_begin),precipitating==TRUE))[860:870,]
#filter((select(xs, ymdhm, year,month, day, precipitating, switch_to, regime, pcp_begin)),month==11, day==6)[34:40,]


#adapt data
df <- mutate(df, delay=as.numeric(finish_tm)-as.numeric(require_tm), prereq=as.numeric(require_tm)-as.numeric(place_tm), 
             prepare=as.numeric(leave_tm)-as.numeric(receive_tm), ride=as.numeric(finish_tm)-as.numeric(leave_tm), 
             cooka=as.numeric(cookable_tm)-as.numeric(cook_tm), allow_cook=prereq-ride,
             left1=prereq-(prepare+ride), left2=prereq-(cooka+ride), left3=prereq-(dish_longer*60 + ride),
             pcp_dur=as.numeric(place_tmref)-as.numeric(pcp_beginref) + as.numeric(as.Date(place_tm)-as.Date(shw_begin))*24, 
             shw_dur=as.numeric(place_tmref)-as.numeric(shw_beginref) + as.numeric(as.Date(place_tm)-as.Date(shw_begin))*24,
             rel_tm=ymd_h(ifelse(hour(place_tm)>=6, (paste(date(df$place_tm),6)), (paste((date(df$place_tm)-1),6)))), 
             shw_rdur=pmin(shw_dur,as.numeric(place_tm)-as.numeric(rel_tm)), shw_rdur1=pmin(shw_dur,as.numeric(require_tm)-as.numeric(rel_tm))) #shw_rel should always be +
#ggplot(df,aes((MW))) + stat_count(na.rm=TRUE) + 
#  stat_count(aes((MW.1)), color="red", position="stack", na.rm=TRUE) + 
#  stat_count(aes((MW.2)), color="blue", position="stack", na.rm=TRUE)
#consider $pm on average?
df$cpm <- df$rider_income/df$distance
df$dlv_perc <- df$rider_income/df$price
df$memo <- !(is.na(df$rider_memo))

#Pruning data
#summary(select(df, place_tm:predict_tm))
df <- select(df, -c(verify_tm, predict_tm))
df <- filter(df, price>0)
temp <- filter(df,!is.infinite(cpm), prereq>2400, prereq<3600)

#CLAIM: Under rationing, people compete to call in earlier, which enhances welfare by reducing delay
#Under rationing: 
ggplot(filter(df,yday(require_tm)==244),aes(x=require_tm, y=delay/60)) + geom_point(size=0.1, alpha=0.4) + geom_smooth() + coord_cartesian(ylim=c(-50,50)) + labs(x="deliver time required", y="delay(mins)", title="Delay pattern over a day (Sept 1, 2015)")
#people compete to call in earlier: 
ggplot(df,aes(x=left2/60)) + stat_bin(aes(y=..density..), binwidth=1) + labs(x="mins", title="Time left excluding cooking & delivery time") + coord_cartesian(xlim=c(-50,50))
#   ggplot(filter(df, left2/60>-50, left2/60<50, left1/60>-50, left1/60<50, left3/60>-50, left3/60<50),aes(x=left2/60)) + stat_bin() + stat_bin(aes(x=left1/60), color="red", alpha=0.3) + stat_bin(aes(x=left3/60),color="yellow",alpha=0.3)
#   ggplot(filter(df, left2/60>-50, left2/60<50, left1/60>-50, left1/60<50, left3/60>-50, left3/60<50),aes(x=left1/60, color=(weather=="小雨"))) + stat_bin(aes(y=..density..),position="identity", alpha=0.3)
#which enhances welfare by reducing delay
ggplot(df, aes(x=prereq/60, y=delay/60)) + geom_point(size=0.1, alpha=0.3) + geom_smooth() + coord_cartesian(xlim=c(0,240), ylim=c(-120,120)) + labs(x="minutes ordered in advance", y="delay(mins)", title="Advance ordering and delay")
#   reduce headache in restaurant: ggplot(df, aes(x=prereq/60, y=(prepare - dish_longer*60)/60)) + geom_point(size=0.1, alpha=0.3) + geom_smooth() + coord_cartesian(xlim=c(0,240), ylim=c(-120,120))
#   reduce headache in delivery person: ? 


#more general context that just this particular food mkt can have good effect
#textbook examples in rationing <- handbook has demand uncertainty only
#DU: price before uncertainty & cannot choose quantity -> diff P in stocks
#Andrew Ching 's limited nursing house <- read
#barzel's paper? <- no clear examples
# what do consumers possess in surgery, public housing, movie theater?

#cannot inc easily but demand uncertainty makes them costly
#is there any inefficiency? not sure
#general: 
#product: serving food in timely fashion
#in limited capacity (cannot increase all of a sudden)
#not increasing early, because wasted the capcity
#if info comes, then cook early
#Is this mechanism good?
#pro: reduce producer's uncertainty
#con: call in earlier <- cost of making decision early







#NumericalResults
#calling in earlier helps reduce delay
summary(lm(as.numeric(delay)~prereq + price*menu_num,data=temp))

#even when calling in earlier than needed
#using left2=prereq-((cookable_tm-cook_tm) + ride)
#hist(as.numeric(filter(df, left2<3000, left2>-1500)$left2),20)
lm0<- lm(as.numeric(delay)~cpm + prereq + price*menu_num,data=filter(temp, left2>600))
lm<- lm(as.numeric(delay)~cpm + prereq + price*menu_num,data=filter(temp, left2>1200))
lm1<- lm(as.numeric(delay)~cpm + prereq + price*menu_num,data=filter(temp, left2>1800))
stargazer(lm0, lm, lm1, type="text", report="cvt*")
#                         -0.550                    -0.517                  -0.528         
#prereq                   t = -21.493***            t = -18.009***           t = -7.072*** 

#IV: shower beginning time 
ggplot(filter(df, prereq>2400, shw_rdur/60<70),aes(x=shw_rdur/60,y=prereq/60)) + geom_point(size=0.1) + geom_smooth()
#shower duration correlate w/ prereq, but strange: 1) bounded in a tilted column, 2) same for dur,rdur,rdur1
summary(ivreg(as.numeric(delay)~prereq|shw_rdur, data=temp))
summary(lm(prereq~shw_rdur, data=temp))
#and shower should increase delay, against our hypothesis (if shower reduces traffic on road, reduce delay)

#robustness check - other leftover time measure
#same for left1=prereq-(prepare+ride) = prereq - ((leave_tm-receive_tm) + (finish_tm-leave_tm)),
#left1 underestimates the actual cooking time
#hist(as.numeric(filter(df, left1<3000, left1>-1500)$left1),20)
lm0<- lm(as.numeric(delay)~cpm + prereq + price*menu_num,data=filter(temp, left1>1200))
lm<- lm(as.numeric(delay)~cpm + prereq + price*menu_num,data=filter(temp, left1>1800))
lm1<- lm(as.numeric(delay)~cpm + prereq + price*menu_num,data=filter(temp, left1>2400))
stargazer(lm0, lm, lm1, type="text", report="cvt*")

#same for left3=prereq-(dish_longer*60 + ride)
#hist(as.numeric(filter(df, left3<3000, left3>-1500)$left3),20)
lm0<- lm(as.numeric(delay)~cpm + prereq + price*menu_num,data=filter(temp, left3>600))
lm<- lm(as.numeric(delay)~cpm + prereq + price*menu_num,data=filter(temp, left3>1200))
lm1<- lm(as.numeric(delay)~cpm + prereq + price*menu_num,data=filter(temp, left3>1800))
stargazer(lm0, lm, lm1, type="text", report="cvt*")

#largely same for old==1
#largely same for lunch & dinner
#     (i.e. hour(require_tm)>=11, hour(require_tm)<14) & (ie hour(require_tm)>=18, hour(require_tm)<19)
#largely same after censoring
#effect coming from ~18mins early arrival, 
#lm0<- lm(as.numeric(delay)~cpm + prereq + price*menu_num,data=filter(temp, delay>-1200))
#lm<- lm(as.numeric(delay)~cpm + prereq + price*menu_num,data=filter(temp, delay>-1100))
#lm1<- lm(as.numeric(delay)~cpm + prereq + price*menu_num,data=filter(temp, delay>-1000))
#stargazer(lm0, lm, lm1, type="text", report="cvt*")
#but it's almost half of one order diff (mean38,median34) for any particular rider
#df_rider <- arrange(df, rider_id, finish_tm) %>% group_by(rider_id) %>% mutate(next_gap=lead(as.numeric(finish_tm))-as.numeric(finish_tm)) %>% filter(next_gap<7200)
#ggplot(df_rider,aes(x=next_gap/60)) + stat_bin() + geom_vline(xintercept=1100/60)




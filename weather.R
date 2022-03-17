#cleaning xiaoshan hourly weather
library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate)

xs_hourly <- read.table("weather/xiaoshan_15H2.csv", 
                        head=TRUE, sep=",")
hq_hourly <- read.table("weather/hongqiao_15H2.txt", 
                        head=TRUE)
pd_hourly <- read.table("weather/pudong_15H2.txt", 
                        head=TRUE)
# see surface_hourly.txt for code reference

xs_hourly <- rename(xs_hourly, ymdhm=YR..MODAHRMN, wind_dir=DIR, wind_spd=SPD, cloud_ceil=CLG, sky_cov=SKC, L_cloud=L, M_cloud=M, H_cloud=H,
                    # SPD & GUS = WIND SPEED & GUST IN MILES PER HOUR 
                    visib=VSB, temp=TEMP, dewp=DEWP, sealv_pres=SLP, alti=ALT, station_pres=STP, tmax=MAX, tmin=MIN,
                    # VSB = VISIBILITY IN STATUTE MILES TO NEAREST TENTH 
                    precip_1h=PCP01, precip_6h=PCP06, precip_24h=PCP24, precip_xxh=PCPXX, snow_depth=SD)
hq_hourly <- rename(hq_hourly, ymdhm=YR..MODAHRMN, wind_dir=DIR, wind_spd=SPD, cloud_ceil=CLG, sky_cov=SKC, L_cloud=L, M_cloud=M, H_cloud=H,
                    visib=VSB, temp=TEMP, dewp=DEWP, sealv_pres=SLP, alti=ALT, station_pres=STP, tmax=MAX, tmin=MIN,
                    precip_1h=PCP01, precip_6h=PCP06, precip_24h=PCP24, precip_xxh=PCPXX, snow_depth=SD)
pd_hourly <- rename(pd_hourly, ymdhm=YR..MODAHRMN, wind_dir=DIR, wind_spd=SPD, cloud_ceil=CLG, sky_cov=SKC, L_cloud=L, M_cloud=M, H_cloud=H,
                    visib=VSB, temp=TEMP, dewp=DEWP, sealv_pres=SLP, alti=ALT, station_pres=STP, tmax=MAX, tmin=MIN,
                    precip_1h=PCP01, precip_6h=PCP06, precip_24h=PCP24, precip_xxh=PCPXX, snow_depth=SD)
#summary(xs_hourly)

#replace for NA
xs_hourly$precip_1h[xs_hourly$precip_1h=="*****"]<-NA
xs_hourly$precip_6h[xs_hourly$precip_6h=="*****"]<-NA
xs_hourly$precip_24h[xs_hourly$precip_24h=="*****"]<-NA
xs_hourly$precip_xxh[xs_hourly$precip_xxh=="*****"]<-NA
xs_hourly$precip_24h[xs_hourly$precip_24h=="T*****"]<-NA
xs_hourly$precip_xxh[xs_hourly$precip_xxh=="T*****"]<-NA
xs_hourly$precip_xxh[xs_hourly$precip_xxh=="T 0.00"]<-NA
xs_hourly$visib[xs_hourly$visib=="****"]<-NA
xs_hourly$MW[xs_hourly$MW=="**"]<-NA
xs_hourly$MW.1[xs_hourly$MW.1=="**"]<-NA
xs_hourly$MW.2[xs_hourly$MW.2=="**"]<-NA
xs_hourly$tmax[xs_hourly$tmax=="***"]<-NA
xs_hourly$tmin[xs_hourly$tmin=="***"]<-NA

hq_hourly$precip_1h[hq_hourly$precip_1h=="*****"]<-NA
hq_hourly$precip_6h[hq_hourly$precip_6h=="*****"]<-NA
hq_hourly$precip_24h[hq_hourly$precip_24h=="*****"]<-NA
hq_hourly$precip_xxh[hq_hourly$precip_xxh=="*****"]<-NA
hq_hourly$precip_24h[hq_hourly$precip_24h=="T*****"]<-NA
hq_hourly$precip_xxh[hq_hourly$precip_xxh=="T*****"]<-NA
hq_hourly$precip_xxh[hq_hourly$precip_xxh=="T 0.00"]<-NA
hq_hourly$visib[hq_hourly$visib=="****"]<-NA
hq_hourly$MW[hq_hourly$MW=="**"]<-NA
hq_hourly$MW.1[hq_hourly$MW.1=="**"]<-NA
hq_hourly$MW.2[hq_hourly$MW.2=="**"]<-NA
hq_hourly$tmax[hq_hourly$tmax=="***"]<-NA
hq_hourly$tmin[hq_hourly$tmin=="***"]<-NA

pd_hourly$precip_1h[pd_hourly$precip_1h=="*****"]<-NA
pd_hourly$precip_6h[pd_hourly$precip_6h=="*****"]<-NA
pd_hourly$precip_24h[pd_hourly$precip_24h=="*****"]<-NA
pd_hourly$precip_xxh[pd_hourly$precip_xxh=="*****"]<-NA
pd_hourly$precip_24h[pd_hourly$precip_24h=="T*****"]<-NA
pd_hourly$precip_xxh[pd_hourly$precip_xxh=="T*****"]<-NA
pd_hourly$precip_xxh[pd_hourly$precip_xxh=="T 0.00"]<-NA
pd_hourly$visib[pd_hourly$visib=="****"]<-NA
pd_hourly$MW[pd_hourly$MW=="**"]<-NA
pd_hourly$MW.1[pd_hourly$MW.1=="**"]<-NA
pd_hourly$MW.2[pd_hourly$MW.2=="**"]<-NA
pd_hourly$tmax[pd_hourly$tmax=="***"]<-NA
pd_hourly$tmin[pd_hourly$tmin=="***"]<-NA



xs <- select(xs_hourly, ymdhm, wind_spd, visib, MW:MW.2, temp, tmax, tmin, precip_6h:precip_xxh) %>%
  separate(ymdhm,c("year", "month", "day", "hr", "min"), sep=c(4,6,8,10), remove=FALSE,convert=TRUE) %>%
  # MW: MANUALLY OBSERVED PRESENT WEATHER
  mutate(precipitating=((MW %in% c(50:99))|(MW.1 %in% c(50:99))|(MW.2 %in% c(50:99))), # 50-99  Precipitation at the station at the time of observation
         drizzle=((MW %in% c(50:59))|(MW.1 %in% c(50:59))|(MW.2 %in% c(50:59))), #50-59  Drizzle
         rain=((MW %in% c(60:69))|(MW.1 %in% c(60:69))|(MW.2 %in% c(60:69))), # 60-69  Rain
         srain=((MW %in% c(70:79))|(MW.1 %in% c(70:79))|(MW.2 %in% c(70:79))), #70-79  Solid precipitation not in showers
         shower=((MW %in% c(80:99))|(MW.1 %in% c(80:99))|(MW.2 %in% c(80:99))), #80-99  Showery precipitation, or precipitation with current or recent thunderstorm
         shower_sl=((MW %in% c(80))|(MW.1 %in% c(80))|(MW.2 %in% c(80))), #80: Rain shower(s), slight
         rain_sl_cnt=((MW %in% c(61))|(MW.1 %in% c(61))|(MW.2 %in% c(61))), #61: Rain, not freezing, continuous, slight at time of observation
         rain_sl_itm=((MW %in% c(60))|(MW.1 %in% c(60))|(MW.2 %in% c(60))), #60: Rain, not freezing, intermittent, slight at time of observation
         mist=((MW %in% c(10))|(MW.1 %in% c(10))|(MW.2 %in% c(10))), # 10: Mist
         haze=((MW %in% c(5))|(MW.1 %in% c(5))|(MW.2 %in% c(5)))) %>% # 05: Haze
  type.convert() %>%
  mutate(tm=ymd_hm(ymdhm), date=as.Date(tm))
xs$ymdhm <- as.character(xs$ymdhm)

hq <- select(hq_hourly, ymdhm, wind_spd, visib, MW:MW.2, temp, tmax, tmin, precip_6h:precip_xxh) %>%
  separate(ymdhm,c("year", "month", "day", "hr", "min"), sep=c(4,6,8,10), remove=FALSE,convert=TRUE) %>%
  mutate(precipitating=((MW %in% c(50:99))|(MW.1 %in% c(50:99))|(MW.2 %in% c(50:99))), 
         drizzle=((MW %in% c(50:59))|(MW.1 %in% c(50:59))|(MW.2 %in% c(50:59))), 
         rain=((MW %in% c(60:69))|(MW.1 %in% c(60:69))|(MW.2 %in% c(60:69))), 
         srain=((MW %in% c(70:79))|(MW.1 %in% c(70:79))|(MW.2 %in% c(70:79))),
         shower=((MW %in% c(80:99))|(MW.1 %in% c(80:99))|(MW.2 %in% c(80:99))),
         shower_sl=((MW %in% c(80))|(MW.1 %in% c(80))|(MW.2 %in% c(80))), 
         rain_sl_cnt=((MW %in% c(61))|(MW.1 %in% c(61))|(MW.2 %in% c(61))),
         rain_sl_itm=((MW %in% c(60))|(MW.1 %in% c(60))|(MW.2 %in% c(60))),
         mist=((MW %in% c(10))|(MW.1 %in% c(10))|(MW.2 %in% c(10))),
         haze=((MW %in% c(5))|(MW.1 %in% c(5))|(MW.2 %in% c(5)))) %>%
  type.convert() %>%
  mutate(tm=ymd_hm(ymdhm), date=as.Date(tm))
hq$ymdhm <- as.character(hq$ymdhm)

pd <- select(pd_hourly, ymdhm, wind_spd, visib, MW:MW.2, temp, tmax, tmin, precip_6h:precip_xxh) %>%
  separate(ymdhm,c("year", "month", "day", "hr", "min"), sep=c(4,6,8,10), remove=FALSE,convert=TRUE) %>%
  mutate(precipitating=((MW %in% c(50:99))|(MW.1 %in% c(50:99))|(MW.2 %in% c(50:99))), 
         drizzle=((MW %in% c(50:59))|(MW.1 %in% c(50:59))|(MW.2 %in% c(50:59))), 
         rain=((MW %in% c(60:69))|(MW.1 %in% c(60:69))|(MW.2 %in% c(60:69))), 
         srain=((MW %in% c(70:79))|(MW.1 %in% c(70:79))|(MW.2 %in% c(70:79))),
         shower=((MW %in% c(80:99))|(MW.1 %in% c(80:99))|(MW.2 %in% c(80:99))),
         shower_sl=((MW %in% c(80))|(MW.1 %in% c(80))|(MW.2 %in% c(80))), 
         rain_sl_cnt=((MW %in% c(61))|(MW.1 %in% c(61))|(MW.2 %in% c(61))),
         rain_sl_itm=((MW %in% c(60))|(MW.1 %in% c(60))|(MW.2 %in% c(60))),
         mist=((MW %in% c(10))|(MW.1 %in% c(10))|(MW.2 %in% c(10))),
         haze=((MW %in% c(5))|(MW.1 %in% c(5))|(MW.2 %in% c(5)))) %>%
  type.convert() %>%
  mutate(tm=ymd_hm(ymdhm), date=as.Date(tm))
pd$ymdhm <- as.character(pd$ymdhm)

#summary(hq)
ggplot(xs,aes(as.factor(MW))) + stat_count() + 
  stat_count(aes(as.factor(MW.1)), color="red", position="stack") + 
  stat_count(aes(as.factor(MW.2)), color="blue", position="stack")
ggplot(hq,aes(as.factor(MW))) + stat_count() + 
  stat_count(aes(as.factor(MW.1)), color="red", position="stack") + 
  stat_count(aes(as.factor(MW.2)), color="blue", position="stack")
ggplot(pd,aes(as.factor(MW))) + stat_count() + 
  stat_count(aes(as.factor(MW.1)), color="red", position="stack") + 
  stat_count(aes(as.factor(MW.2)), color="blue", position="stack")


#prepare referencing columns
xs$tmref <- xs$hr + (xs$min/60)
xs <- mutate(xs, switch_pcp=(1-precipitating)*lag(precipitating) + (precipitating)*(1-lag(precipitating)),
             switch_shw=(1-shower)*lag(shower) + (shower)*(1-lag(shower)))
#note: if changing above lines, caution if any grouping implied
xs$switch_pcp[1]<-1             
xs$switch_shw[1]<-1             
xs <- mutate(group_by(xs), regime_pcp=cumsum(switch_pcp), regime_shw=cumsum(switch_shw)) %>% #cum_pcp=as.numeric(precipitating) + ifelse(lag(precipitating),1,0)*precipitating + ifelse(is.na(lag(cum_pcp)),0,lag(cum_pcp)-1), 
  group_by(regime_pcp) %>%
  mutate(pcp_begin=first(ymd_hm(ymdhm)), pcp_beginref= hour(pcp_begin)+(minute(pcp_begin)/60)) %>%
  group_by(regime_shw) %>%
  mutate(shw_begin=first(ymd_hm(ymdhm)), shw_beginref= hour(shw_begin)+(minute(shw_begin)/60))
#prepare referencing columns
hq$tmref <- hq$hr + (hq$min/60)
hq <- mutate(hq, switch_pcp=(1-precipitating)*lag(precipitating) + (precipitating)*(1-lag(precipitating)),
             switch_shw=(1-shower)*lag(shower) + (shower)*(1-lag(shower)))
#note: if changing above lines, caution if any grouping implied
hq$switch_pcp[1]<-1             
hq$switch_shw[1]<-1             
hq <- mutate(group_by(hq), regime_pcp=cumsum(switch_pcp), regime_shw=cumsum(switch_shw)) %>% #cum_pcp=as.numeric(precipitating) + ifelse(lag(precipitating),1,0)*precipitating + ifelse(is.na(lag(cum_pcp)),0,lag(cum_pcp)-1), 
  group_by(regime_pcp) %>%
  mutate(pcp_begin=first(ymd_hm(ymdhm)), pcp_beginref= hour(pcp_begin)+(minute(pcp_begin)/60)) %>%
  group_by(regime_shw) %>%
  mutate(shw_begin=first(ymd_hm(ymdhm)), shw_beginref= hour(shw_begin)+(minute(shw_begin)/60))
#prepare referencing columns
pd$tmref <- pd$hr + (pd$min/60)
pd <- mutate(pd, switch_pcp=(1-precipitating)*lag(precipitating) + (precipitating)*(1-lag(precipitating)),
             switch_shw=(1-shower)*lag(shower) + (shower)*(1-lag(shower)))
#note: if changing above lines, caution if any grouping implied
pd$switch_pcp[1]<-1             
pd$switch_shw[1]<-1             
pd <- mutate(group_by(pd), regime_pcp=cumsum(switch_pcp), regime_shw=cumsum(switch_shw)) %>% #cum_pcp=as.numeric(precipitating) + ifelse(lag(precipitating),1,0)*precipitating + ifelse(is.na(lag(cum_pcp)),0,lag(cum_pcp)-1), 
  group_by(regime_pcp) %>%
  mutate(pcp_begin=first(ymd_hm(ymdhm)), pcp_beginref= hour(pcp_begin)+(minute(pcp_begin)/60)) %>%
  group_by(regime_shw) %>%
  mutate(shw_begin=first(ymd_hm(ymdhm)), shw_beginref= hour(shw_begin)+(minute(shw_begin)/60))



#remove non-raining regime
xs$shw_begin[!(xs$shower)] <- NA
xs$shw_beginref[!(xs$shower)] <- NA
xs$pcp_begin[!(xs$precipitating)] <- NA
xs$pcp_beginref[!(xs$precipitating)] <- NA
#remove non-raining regime
hq$shw_begin[!(hq$shower)] <- NA
hq$shw_beginref[!(hq$shower)] <- NA
hq$pcp_begin[!(hq$precipitating)] <- NA
hq$pcp_beginref[!(hq$precipitating)] <- NA
#remove non-raining regime
pd$shw_begin[!(pd$shower)] <- NA
pd$shw_beginref[!(pd$shower)] <- NA
pd$pcp_begin[!(pd$precipitating)] <- NA
pd$pcp_beginref[!(pd$precipitating)] <- NA

xs <- select(group_by(xs), tm:tmref, temp, wind_spd, visib, precipitating, rain:rain_sl_cnt,
             mist, pcp_begin, shw_begin)
hq <- select(group_by(hq), tm:tmref, temp, wind_spd, visib, precipitating, rain:rain_sl_cnt,
             mist, pcp_begin, shw_begin)
pd <- select(group_by(pd), tm:tmref, temp, wind_spd, visib, precipitating, rain:rain_sl_cnt,
             mist, pcp_begin, shw_begin)

#switch_time <- select(filter(xs,switch_pcp==1),ymdhm, year, month, day, hr,min, place_tmref, precipitating, switch_pcp) %>%
#  mutate()
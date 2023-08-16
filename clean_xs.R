#cleaning xiaoshan hourly weather
library(dplyr)
library(tidyr)
library(lubridate)

xs_hourly <- read.table("~/Google Drive/dwb/weather/xiaoshan_15H2.csv", 
                        head=TRUE, sep=",")
#summary(xs_hourly)
xs_hourly <- rename(xs_hourly, ymdhm=YR..MODAHRMN, wind_dir=DIR, wind_spd=SPD, cloud_ceil=CLG, sky_cov=SKC, L_cloud=L, M_cloud=M, H_cloud=H,
                    visib=VSB, temp=TEMP, dewp=DEWP, sealv_pres=SLP, alti=ALT, station_pres=STP, tmax=MAX, tmin=MIN,
                    precip_1h=PCP01, precip_6h=PCP06, precip_24h=PCP24, precip_xxh=PCPXX, snow_depth=SD)
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

xs <- select(xs_hourly, ymdhm, wind_spd, visib, MW:MW.2, temp, tmax, tmin, precip_6h:precip_xxh) %>%
  separate(ymdhm,c("year", "month", "day", "hr", "min"), sep=c(4,6,8,10), remove=FALSE,convert=TRUE) %>%
  mutate(tm=ymd_hm(ymdhm),
         precipitating=((MW %in% c(50:99))|(MW.1 %in% c(50:99))|(MW.2 %in% c(50:99))), 
         drizzle=((MW %in% c(50:59))|(MW.1 %in% c(50:59))|(MW.2 %in% c(50:59))), 
         rain=((MW %in% c(60:69))|(MW.1 %in% c(60:69))|(MW.2 %in% c(60:69))), 
         srain=((MW %in% c(70:79))|(MW.1 %in% c(70:79))|(MW.2 %in% c(70:79))),
         shower=((MW %in% c(80:99))|(MW.1 %in% c(80:99))|(MW.2 %in% c(80:99))),
         shower_sl=((MW %in% c(80))|(MW.1 %in% c(80))|(MW.2 %in% c(80))), 
         rain_sl_cnt=((MW %in% c(61))|(MW.1 %in% c(61))|(MW.2 %in% c(61))),
         rain_sl_itm=((MW %in% c(60))|(MW.1 %in% c(60))|(MW.2 %in% c(60))),
         mist=((MW %in% c(10))|(MW.1 %in% c(10))|(MW.2 %in% c(10))),
         haze=((MW %in% c(5))|(MW.1 %in% c(5))|(MW.2 %in% c(5)))) %>%
  type.convert()
xs$ymdhm <- as.character(xs$ymdhm)

#summary(xs)
ggplot(xs,aes(as.factor(MW))) + stat_count() + 
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

#remove non-raining regime
xs$shw_begin[!(xs$shower)] <- NA
xs$shw_beginref[!(xs$shower)] <- NA
xs$pcp_begin[!(xs$precipitating)] <- NA
xs$pcp_beginref[!(xs$precipitating)] <- NA


#switch_time <- select(filter(xs,switch_pcp==1),ymdhm, year, month, day, hr,min, place_tmref, precipitating, switch_pcp) %>%
#  mutate()
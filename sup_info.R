rm(list=ls())
library(DBI)
library(tidyverse)
library(stringr)
library(openxlsx)
library(data.table)

source('/Users/kwongyu/Google Drive/dwb/cred.txt')
# source('~/Documents/eScience/projects/delivery/baidu_geo_func.R')

#Grab data 

data_dir <- '/Users/kwongyu/Google Drive/dwb/dwb_Data'


temp = list.files(path = data_dir, pattern="*.xlsx$")
#temp = list.files(path = data_dir, pattern="*C3_.*&*.xlsx$", full.names = T)
temp

list2env(
  lapply(setNames(file.path(data_dir ,temp), make.names(gsub("*.xlsx$", "", temp))), 
         read.xlsx), envir = .GlobalEnv)
#########

names(Aug_C3_P1)
names(Aug_C3_P2)
names(G_Order_Sep_City3.Condensed.)
names(Jul_C3_P1)
names(Jul_C3_P2)

data <- bind_rows(Aug_C3_P1, Aug_C3_P2,Jul_C3_P1,Jul_C3_P2,G_Order_Sep_City3.Condensed.)


sup_data <- data %>%
  select(sup_id, from_addr, from_tel) %>% 
  distinct(.) #%>%  slice(1:20)

#ensure sensible numbers to throw in
sup_data <- separate(sup_data, from_tel, c("tel1", "tel2", "tel3"), sep="[, —]", remove=FALSE)
sup_data <- separate(sup_data, tel1, c("tel1", "int1"), sep="转") %>%
  separate(tel2, c("tel2", "int2"), sep="转")
sup_data$tel3[(grep("[^0-9\\(\\)-E]+", sup_data$tel3))] <- sub("[^0-9]*([0-9]+)[^0-9]*", "\\1", (sup_data$tel3[(grep("[^0-9\\(\\)-E]+", sup_data$tel3))]))
#length(grep("^[^0-9]*021", sup_data$tel1))

write.csv(sup_data,'sup_list.csv')

#===========================
#after sup info scraped
#===========================
sup_info <- read.csv('sup_info.csv')

temp <- separate(sup_info, details, c('name', 'd1','tags', 'd2', 'from_addr_xms',  'd3', 'avgexp', 
                                      'taste', 'envir', 'service','d7', 'comment', 'remarks'),
                 sep = "\n", remove=FALSE, extra="merge") %>%
  select(-'d1', -'d2', -'d3', -'d7') %>%
  separate(avgexp,c("avgexp_lo", 'avgexp_up'), sep='-')
temp$avgexp_lo <- sub('^¥','', temp$avgexp_lo)
temp$taste <- sub('^.*:','', temp$taste)
temp$envir <- sub('^.*:','', temp$envir)
temp$service <- sub('^.*:','', temp$service)

#{'sup_id': 11805, 'from_addr': '上海市长宁区天山西路568号1幢101室(近地铁2号线淞虹路站)', '
#from_tel': '02152190867,18202163818,', 'details': '四海游龙 天山西路店\n标签:\n中式快餐
#简餐北新泾淞虹路站\n地址:\n上海长宁区 天山西路568号1幢101室(近地铁2号线淞虹路站)\n人均:\n
#¥17-35\n口味:3.5\n环境:3.5\n服务:3.5\n点评:\n最喜欢原味锅贴，皮很薄，不过馅料不多，吃口正
#好！酸辣汤是老公最爱，可惜现在越来越…\n粉丝汤(2) 香酥鸡(2) 02152190867 021-52190867...'
#, 'bulk': '默认\n↓\n| 人均⬆ | 人均⬇ |\n口味 | 环境 | 服务\n四海游龙 天山西路店\n标签:\n中
#式快餐简餐北新泾淞虹路站\n地址:\n上海长宁区 天山西路568号1幢101室(近地铁2号线淞虹路站)\n
#人均:\n¥17-35\n口味:3.5\n环境:3.5\n服务:3.5\n点评:\n最喜欢原味锅贴，皮很薄，不过馅料不多
#，吃口正好！酸辣汤是老公最爱，可惜现在越来越…\n粉丝汤(2) 香酥鸡(2) 02152190867 021-521908
#67...\n四海游龙 剑河店 OVERSEAS DRAGON\n标签:\n中式快餐简餐快餐简餐北新泾西郊百联\n地址:\
#n上海长宁区 剑河路512号(近西郊百联)\n人均:\n¥10-20\n口味:3.5\n环境:3.3\n服务:3.3\n点评:\n
#外卖回家吃的，不错，赞一个\n蛮实惠的，生意自然很好。 提供在线菜单 02152190867 021-5219086
#7...\n+添加餐厅\n«1»\n没有找到想要的餐厅？立即添加，可获30-50枚秘币。\n您对搜索结果： 满
#意 不满意', 'comments_full': '匿名用户\n口味: 4 环境: 4 服务: 3\n最喜欢原味锅贴，皮很薄，
#不过馅料不多，吃口正好！酸辣汤是老公最爱，可惜现在越来越稀了~\n2012-11-08 17:10 四海游龙(
#天山西路店)'}
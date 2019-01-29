rm(list=ls())
library(DBI)
library(tidyverse)
library(stringr)
library(openxlsx)
library(data.table)

Sys.setlocale(locale="en_us.UTF-8")
setwd('~/Google Drive/dwb/dwb_Data')

data_fn <- '~/Google Drive/dwb/dwb_Data/Aug_C3_P1.xlsx'
df <- read_xlsx(data_fn)

sup_addr <- data.frame(from_addr = unique(df$from_addr), stringsAsFactors = FALSE)
sup_addr$end_1 <- substring(sup_addr$from_addr, nchar(sup_addr$from_addr), nchar(sup_addr$from_addr))
sort(table(sup_addr$end_1)) #(   )  ）   #  A   b   B   C   D   E   F   K   M 室 层 楼  樓 铺
sup_addr$end_2 <- substring(sup_addr$from_addr, nchar(sup_addr$from_addr)-1, nchar(sup_addr$from_addr))
table(filter(sup_addr, end_1 %in% c("楼","樓"))$end_2)
sup_addr$end_3 <- substring(sup_addr$from_addr, nchar(sup_addr$from_addr)-2, nchar(sup_addr$from_addr))
table(filter(sup_addr, end_1 %in% c("楼","樓"))$end_3) # - B 

#Rules:
sup_addr$cleaned <- sup_addr$from_addr

#(     )    ）
sup_addr <- separate((sup_addr),cleaned,c("cleaned", "supple"), sep="[\\(（]")

#樓
#remove last 2 digits. if - B, remove last 3 digits
#search for [0-9]樓
sup_addr$cleaned[grep("(.*)[0-9一二三四]楼$", sup_addr$cleaned)] <- 
  sub("(.*).{2}$", "\\1", sup_addr$cleaned[grep("(.*)[0-9一二三四]楼$", sup_addr$cleaned)])
#.{2} for repeating . twice
sup_addr$cleaned[grep("(.*)[0-9一二三四]楼$", sup_addr$cleaned)] <-
  sub("(.*)[B-]+$", "\\1", sup_addr$cleaned[grep("(.*)[0-9一二三四]楼$", sup_addr$cleaned)])

#层



#debug review
table(substring(sup_addr$from_addr, nchar(sup_addr$from_addr), nchar(sup_addr$from_addr)))
table(substring(sup_addr$cleaned, nchar(sup_addr$cleaned), nchar(sup_addr$cleaned)))

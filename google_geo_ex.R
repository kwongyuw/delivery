library(svMisc)
library(DBI)
library(aws.s3)
library(tidyverse)
library(stringr)

source('/Volumes/GoogleDrive/My Drive/eScience/projects/dssg2018/cred.txt')
#Might need to set systemenv

rawAWSdata <- function(filename) {
  s3read_using(FUN = read.csv, object = paste(location, filename, sep=''))
}

data <- rawAWSdata('DPS_Choice_1314-1718.csv')

str(data)

is.na(data) <- data == ""
names(data)

geo_data <- data %>%
  select(StudentNumber,Address,CityStateZip) %>%
  distinct(.) %>%
  filter(!is.na(Address)) %>%
  unite(Address,Address, CityStateZip, sep = " ", remove = FALSE) #%>%
  #slice(1:10)
#test$comb <- full <- paste(test$Address, test$CityStateZip, sep=" ")


geo_data$Address <- as.character(geo_data$Address)

str(geo_data)
##
#Google API call
geocodeAddress <- function(address) {
  require(RJSONIO)
  full <- paste(address)
  url <- "https://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, full, '&sensor=false&key=',apikey, sep = ""))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {
    
      out <- c(x$results[[1]]$geometry$location$lat,
               x$results[[1]]$geometry$location$lng)
    } else {
      out <- NA
  }
  Sys.sleep(0.05)  # API only allows 50 requests per second
  out
}
#Initialize
geo_data$LAT<-NA
geo_data$LON<-NA
g_add=list()

old <- Sys.time() # get start time
for (i in 1:nrow(geo_data)) {
  g_add <- geocodeAddress(geo_data$Address[i])
  geo_data$LAT[i] <- g_add[1]
  geo_data$LON[i] <- g_add[2]

  if (i == nrow(geo_data)) cat("Done!\n")
}
new <- Sys.time() - old # calculate difference
print(new)

#crapped out at 57602

geo_data2 <- data %>%
  select(StudentNumber,Address,CityStateZip) %>%
  distinct(.) %>%
  filter(!is.na(Address)) %>%
  unite(Address,Address, CityStateZip, sep = " ", remove = FALSE) %>%
  slice(57602:73261)
dim(geo_data2)
 #Initialize
geo_data2$LAT<-NA
geo_data2$LON<-NA
g_add=list()

old <- Sys.time() # get start time
for (i in 1:nrow(geo_data2)) {
  g_add <- geocodeAddress(geo_data2$Address[i])
  geo_data2$LAT[i] <- g_add[1]
  geo_data2$LON[i] <- g_add[2]

  if (i == nrow(geo_data2)) cat("Done!\n")
}
new <- Sys.time() - old # calculate difference
print(new)
#write.csv(geo_data2, file="geo_loc_2.csv")

geo_data_1 <- geo_data %>%
                  slice(1:57601)
dim(geo_data_1)
dim(geo_data2)

geo_data_full <- bind_rows(geo_data_1, geo_data2)

write.csv(geo_data_full, file = "geo_choicedata.csv")
############
names(geo_data_full)
miss_ll <- geo_data_full %>%
            filter(is.na(LAT) | is.na(LON) ) %>% 
            mutate_at(vars(Address), funs(str_replace_all(., "#", "")))
#write.csv(miss_ll, file = "missing.csv")

g_add=list()

old <- Sys.time() # get start time
for (i in 1:nrow(miss_ll)) {
  g_add <- geocodeAddress(miss_ll$Address[i])
  miss_ll$LAT[i] <- g_add[1]
  miss_ll$LON[i] <- g_add[2]

  if (i == nrow(miss_ll)) cat("Done!\n")
}
new <- Sys.time() - old # calculate difference
print(new)
write.csv(miss_ll, file = "missing_complete.csv")
names(geo_data_full)
##########WHAT ARE YOU DOING HERE????
missing_s <- miss_ll$StudentNumber
tail(geo_data_full)
test_missing <- geo_data_full %>%
                filter(!StudentNumber %in% missing_s)
dim(test_missing)
head(miss_ll)
tail(miss_ll)

student_location = bind_rows(test_missing,miss_ll)
write.csv(student_location, file="student_l.csv")
#################

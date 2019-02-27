library(DBI)
library(stringr)


#Baidu API call
geocodeAddress <- function(address) {
  require(RJSONIO)
  full <- paste(address)
  url <- "http://api.map.baidu.com/geocoder/v2/?address="
  url <- URLencode(paste(url, full, '&output=json&ak=', ak_bd_brow, sep=''))
  x <- fromJSON(url,simplify=FALSE)
  if (x$status == 0) {
    out <- c(x$result$location$lat, 
             x$result$location$lng)
  } else {
    out <- NA
  }
  Sys.sleep(0.0167) # Verified developer now 160/s (API only allows 50 requests per second for non-verified user)
  out
}


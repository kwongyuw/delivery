
baidu_distance <- function(origins, destinations) {
  require(RJSONIO)
  origins <- paste(origins)
  destinations <- paste(destinations)
  url <- 'http://api.map.baidu.com/routematrix/v2/driving?output=json'
  url <- URLencode(paste(full, '&origins=', origins, '&destinations=', destinations, '&ak=', ak_bd_serv, sep=''))
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
origins = '31.23836,121.3887'
destinations = '31.22166,121.38697'

origins <- paste(origins)
destinations <- paste(destinations)
url <- 'http://api.map.baidu.com/routematrix/v2/driving?output=json'
url <- URLencode(paste(full, '&origins=', origins, '&destinations=', destinations, '&ak=', ak_bd_serv, sep=''))
x <- fromJSON(url,simplify=FALSE)

x




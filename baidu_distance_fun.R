baidu_distance <- function(origins, destinations) {
  require(RJSONIO)
  origins <- paste(origins)
  destinations <- paste(destinations)
  full <- 'http://api.map.baidu.com/routematrix/v2/driving?output=json'
  url <- URLencode(paste(full, '&origins=', origins, '&destinations=', destinations, '&ak=', ak_bd_serv, sep=''))
  x <- fromJSON(url,simplify=FALSE)
  
  if (x$status == 0) {
    out <- c(x$result[[1]]$distance$value,
             x$result[[1]]$distance$text,
             x$result[[1]]$duration$value,
             x$result[[1]]$duration$text)
  } else {
    out <- NA
  }
  Sys.sleep(0.0167) # Verified developer now 160/s (API only allows 50 requests per second for non-verified user)
  out
}
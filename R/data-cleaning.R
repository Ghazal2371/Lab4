data_cleaning = function(link) {
  library(httr)
  library(tidyverse)
  library(lubridate)
  library(dplyr)
  url = "https://data.iowa.gov/resource/m3tr-qhgy.json"
  html = GET(url)
  a = content(html)
  b = unlist(unlist(a))
  c = as.data.frame(do.call(cbind, a))
  aa = as.data.frame(t(c))
  cc = glimpse(aa)
  cc$sale_dollars = as.numeric(cc$sale_dollars)
  cc$date = gsub("T00:00:00.000", "", cc$date)
  cc$date = ymd(cc$date)
  dd = glimpse(cc)
  cc$store_location = sub(
    pattern = ".*list\\((.*))\\).*",
    replacement = "\\1",
    x = cc$store_location
  )
  cc$stor_long_location = as.numeric(sub(
    pattern = "(.*)\\,.*",
    replacement = "\\1",
    x = cc$store_location
  ))
  cc$stor_lati_location = as.numeric(sub(
    pattern = ".*\\,(.*)",
    replacement = "\\1",
    x = cc$store_location
  ))
  cc$na = as.numeric(cc$store_location)
  dd = na.omit(cc)
  w = which(!is.na(cc$na))
  cc <- cc[-c(w), ]
}

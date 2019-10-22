library(rvest)
library(tidyverse)
library(magrittr)
library(scales)
library(knitr)
library(lubridate)
library(tibble)


#constants to get the charts only in sweden
url <- "https://spotifycharts.com/regional/se/daily/"
#period of time to extract data
timeframe <- seq(as.Date("2019/06/01"), as.Date("2019/08/31"), by = "day")
timeframe[1:3]
#union of data function 
pasteurl<- function(x){
  full_url <- paste0(url, x)
  full_url
}
#create all the urls for the time period
workingurl <- pasteurl(timeframe)
#create the scraper function, to get the values of the variables to extract you use selectorgadget
Scrape <- function(x){
  page <- x 
  url <- page %>% read_html() %>% html_nodes('.chart-table-image') %>% html_nodes('a') %>% html_attr('href') %>% as.data.frame()
  dates <- page %>% read_html() %>% html_nodes('.responsive-select~ .responsive-select+ .responsive-select .responsive-select-value') %>% html_text() %>% as.data.frame()
  rank <- page %>% read_html() %>% html_nodes('.chart-table-position') %>% html_text() %>% as.data.frame()
  artist <- page %>% read_html() %>% html_nodes('.chart-table-track span') %>% html_text() %>% as.data.frame()
  track <- page %>% read_html() %>% html_nodes('strong') %>% html_text() %>% as.data.frame()
  streams <- page %>% read_html() %>% html_nodes('td.chart-table-streams') %>% html_text() %>% as.data.frame()
  
  #combine, name, and make it a tibble
  df <- cbind(rank, url, track, artist, streams, dates)
  names(df) <- c("Rank","Url", "Track", "Artist", "Streams", "Date")
  df <- as.tibble(df)
  return(df)
}
spotifydata <- map_df(workingurl, Scrape)
#Fixing the values of the table
spotifydata %<>% 
  mutate(Artist = gsub("by ", "", Artist), 
         Streams = gsub(",", "", Streams), 
         Streams = as.numeric(Streams), 
         Date = as.Date(spotifydata$Date, "%m/%d/%Y"))
#save tibble into a .csv for analysis
write.csv(spotifydata,"spotifytop200summer.csv") 



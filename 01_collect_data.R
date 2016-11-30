# Collect data from People`s Daily 

# Import general packages:
library(rvest)
library(tidyverse)
library(stringr)

# Function: front_page() to scrape the content of the front page of People`s Daily on the specified date.
# Note: the format of data needs to be "XXXX(year)-XX(month)-XX(date)", type: string. 
front_page<-function (date){
  # url address
  baseurl<-"http://www.ziliaoku.org/rmrb/"
  url<-str_c(baseurl, date,"-1")
  #print(url)
  # extract the front page content of the specific date of People`s Daily
  article<-read_html(url)
  text<-article %>% html_nodes(".article") %>% html_text()
  # save text coupus as a .txt, in the directory of "/rawdata/"
  filedirct<- str_c("rawdata/",date, ".txt")
  write.table (text, file = filedirct, row.names = F, quote = F)
}

# Function: collection() to scrape the the content of the front page of People`s Daily in a time period.
# Note: the format of data needs to be "XXXX(year)-XX(month)-XX(date)", type: string.
collection<-function (date_start, date_end) {
  # create a date sequence based on the date_start and date_end
  dates<-seq(as.Date(date_start), as.Date(date_end), by=1)
  for (d in seq_along(dates)) {
    front_page(dates[d])
    #print(dates[d])
  }
}

# scrape the text corpus of the front page of People`s Daily from 1989-05-04 to 1989-07-04
# save all txt files into /rawdata
collection ("1989-05-04", "1989-07-04")



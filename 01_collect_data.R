# Scrape text data from People`s Daily 

# Import general packages:
library(rvest)
library(tidyverse)
library(stringr)

##############################################################
# Functions to scrape data and handle the subfolder
# 0. clean_folder()
# 1. create_folder()
# 2. scraping()
# 3. collecting()
##############################################################


# FUNCTION: clean_folder()
# Input: 
# date_start and date_end =  string, the format of date needs to be "XXXX(year)-XX(month)-XX(date)".
# Output: No return value, only delet the contents of each subfolder
clean_folder <- function(date_start, date_end){
  basedirct <- "data/rawdata/"
  dates<-seq(as.Date(date_start), as.Date(date_end), by=1)
  for (d in seq_along(dates)) {
    path <-str_c(basedirct, dates[d])
    unlink(path, recursive = TRUE)
  }
}



# FUNCTION: create_folder()
# input=string date XXXX-XX-XX, 
# output: generate a new filefolder if the folder does not exist. 
# under the data/rawdata/
create_folder <- function (date){
  basedirct <- "data/rawdata/"
  path<- str_c(basedirct, date)
  # create the subfolder for each day, if the folder does not exist, create a folder for it
  if (!file.exists(path)){
    dir.create(path)
  } 
}


# FUNCTION: scraping()
# Associated function: create_folder()
# input = string date XXXX-XX-XX; pagenumber: int. 
# ouput = None; 
# File generated: write the scraped texts of artiles, whose \t\t and \t\t\n have been striped,
# into a folder created by create_folder() as a .txt file. 

scraping <- function (date, pagenumber) {
  # create a folder based on the date under the data/rawdata/
  create_folder(date)
  # initiate the base-directory and base url
  basedirct <- "data/rawdata/"
  baseurl <- "http://www.ziliaoku.org/rmrb/"
  
  # generate the url for scraping
  url<-str_c(baseurl, date, "-", toString(pagenumber))
  # extract the content of the specific page of People`s Daily on a certain date
  article<-read_html(url)
  text<-article %>% html_nodes(".article") %>% html_text()
  
  # in the text (a list), all the artilces will be saved as an element of this list
  for (i in seq_along(text)){
    # strip the space at the begining and the end of one of the article from text
    rawtext<-strsplit(text[i], '\\s')
    # after splitting one of the text to a lists of words (sep='space'), the title is the 6th element.
    # save this string and add the date, page number
    title <- rawtext[[1]][6]
    
    # if title is none, replace it by the other paresd string as title or if it is one sentence news (means it has no title)
    # return Null.
    if (title == ""){
      title <- rawtext[[1]][5]
    }
    
    filename <- str_c(date, "-", pagenumber, ".", toString(i), ".", title)
    # pre-processing of the text of the article
    text[i]<-str_trim(text[i], side='both')
    # save the text of one artile
    filedirct<- str_c(basedirct, date, "/", filename,".txt")
    # write into a .txt file into the assigned directory
    writeLines(text[i], filedirct)
  }
}


# FUNCTION: collecting() to scrape the the content of the pages of People`s Daily in a time period.
# Associated funciton: scraping()
# Input: 
# date_start and date_end =  string, the format of date needs to be "XXXX(year)-XX(month)-XX(date)".
# pagelist = a list of pagenumbers 
# Output:
# save the texts of each artile of each date in specified page of People`s Daily 
# into the corresponding subfolder in the data/rawdata/

collecting <-function (date_start, date_end, pagelist) {
  # create a date sequence based on the date_start and date_end
  dates<-seq(as.Date(date_start), as.Date(date_end), by=1)
  for (d in seq_along(dates)) {
    for (p in seq_along(pagelist))
      scraping(dates[d], pagelist[p])
  }
}

##############################################################
# Scraping Data from "http://www.ziliaoku.org/rmrb/"
##############################################################


# clean_folder ("1989-05-04","1989-05-05")

# Fundamental parameters:
# basedirct <- "data/rawdata/"
# baseurl <- "http://www.ziliaoku.org/rmrb/"

pagelist=c(1)
collecting ("1989-05-04","1989-05-05", pagelist)




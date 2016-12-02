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
# Input: folder, date_start, date_end
# date_start and date_end =  string, the format of date needs to be "XXXX(year)-XX(month)-XX(date)".
# Output: No return value, only delet the contents of each subfolder
clean_folder <- function(folder, date_start, date_end){
  basedirct <- str_c("data/", folder, "/")
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

# FUNCRION find_title() 
# Input: a list of the string splitted article 
# Outbut: title = str, the title of the article
# Note: in the scragped data, the first element is always the description of the news
# like, the frontpage-important news, or the frontpage-photograph
# some news may not have a title. this function will detect all the scraping articel
# to use the second non-space element as its title, if this element has character more than 20 char
# then consider this new does not a title, assign notilte to this artle.
find_title <- function(list) {
  j <- 0
  for (i in seq_along (list)){
    if (list[i] !=""){
      j <- j + 1
      if (j == 2) {
        if (nchar(list[i]) < 20 ){
          title <- list [i]
          break() 
        } else {
          title <- 'notitle'
        } 
      } else {title <- "empty_article"}
    } else {title <- "empty_article"}
  }
  return (title)
}

    

    


# FUNCTION: scraping()
# Associated function: create_folder(), find_title()
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
  for (i in seq_along(text)) {
    # strip the space at the begining and the end of one of the article from text
    rawtext<-unlist(strsplit(text[i], '\\s'))
    # after splitting one of the text to a lists of words (sep='space'), 
    # call the function find_title to assign the second piece of non-null char as the title. 
    title <- find_title(rawtext)
    filename <- str_c(date, "-", pagenumber, "_", toString(i), "_", title)
    # pre-processing of the text of the article
    text[i]<-str_trim(text[i], side='both')
    # save the text of one artile
    filedirct<- str_c(basedirct, date, "/", filename,".txt")
    # write into a .txt file into the assigned directory
    #write.table (text[i], file = filedirct, row.names = F, quote = F)
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

# Note: to scrape the data will take 10-20 min for two yrs` data. 

# clean_folder ("segtext","1989-01-01","1990-12-31")

# Fundamental parameters:
# basedirct <- "data/rawdata/"
# baseurl <- "http://www.ziliaoku.org/rmrb/"

#pagelist=c(1)
#collecting ("1986-02-02","1987-12-31", pagelist)

# encounter problem for 1986-01-06 1986-02-02,..., because in these days, there are lots of empty artilces.





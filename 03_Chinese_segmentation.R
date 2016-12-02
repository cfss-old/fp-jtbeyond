# Chinese Word Segmentation:
# purpose: to segmentize Chinese sentences to separated words, according to certain pattern. 

###########################################################
# Important affiliated files
# create a small dictionary for certain words "user.utf8"
# create a small dictionary for stop words "stopwords.txt"
###########################################################

library(feather)
library(stringr)
#install.packages("jiebaR")
library(jiebaR)
#install.packages("tm") 
#install.packages("NLP") # "tm" is attached with library NLP
library(NLP)
library(tm) # text mining library
library(tidyverse)
# install.packages("tidytext")
library(tidytext)


#################################################
# Steps to process Chinese texts 
# 1. Segmenentation of rawdata
# 2. Vectorize the segmentized text
# 3. create the corpus of text files (TDM)
# 4. Further clean the text corpus
# 5. Conversion of TDM to a dataframe, 
# tidy the dataframe
#################################################


####################################################################
# FUNCTIONS created by this Rscript
# 1. one_txt_segment(date), segment the articles 
# of one day
# 2. text_segment(start_date, end_daate), segment all the articles 
# during a time period
# 3. corpus_to_TDM(text corpus), convert the corpus of
# articles of one day to a TDM
# 4. one_create_dataframe (date), read in the segmented text of 
# one day, to create a dataframe with columns: date_page(chr),
# document(chr), title (chr), term(chr), count(dbl) 
# 5. create_dataframe (start_date, end_daate)
####################################################################



# 1. Segmentize the texts

# FUNCTION: one_txt_segment()
# # Associated libraries: library(jiebaR), library(stringr)
# input: date = str.
# output: no return value. segment the text files in a folder and save them into a new dirc. 


one_txt_segment <- function (date) {
  # assign the basedirc 
  input_basedir <- "./data/rawdata/"
  output_basedir <- "./data/segtext/"
  input_folder_dir <- str_c (input_basedir, date)
  output_folder_dir <- str_c (output_basedir, date)
  
  # creat the output folder, if it does not exist
  if (!file.exists(output_folder_dir)){
    dir.create(output_folder_dir) 
  }
  
  # create the list for the article in an input folder
  txt_list <- list.files(input_folder_dir, pattern = '*.txt')
  # if there is no file in this folder, then raise a message
  if (!length(txt_list)) {
    message ("There is no file in:", input_folder_dir)
  } else {
    # iterate the input folder to segmentize the .txt document by using mixseg() function from JiebaR library
    for (txt_file in txt_list) {
      # set the input path for .txt
      input_txt_dir <- str_c(input_folder_dir, "/", txt_file) 
      # set the output path for .txt
      output_txt_dir <- str_c(output_folder_dir, "/", txt_file)
      # initializing the segmentation function, save the segmentized text into subfolder
      # using the user`s dictionary, delete the stop words, for the file from the op
      # and save the segment result into mixseg ("Worker Type: Jieba Segment")
      mixseg = worker (bylines = T, 
                       user = './11_seg_dirc.utf8',
                       stop_word = './10_stopwords.txt', 
                       output = output_txt_dir)
      # "<=" is the operator in jiebaR library to do the segmentation. 
      mixseg <= input_txt_dir
    }
  }
}

# FUNCTION: text_segment(), to segment the article on a newspaper in a time period.
# Associated funciton: one_text_segment()
# Input: 
# date_start and date_end =  string, the format of date needs to be "XXXX(year)-XX(month)-XX(date)".
# Output: no return value
# save the segmentized texts of each artile of each date in specified page of People`s Daily 
# into the corresponding subfolder in the data/segtext/

text_segment <-function (date_start, date_end) {
  # create a date sequence based on the date_start and date_end
  dates<-seq(as.Date(date_start), as.Date(date_end), by=1)
  # initiate the basedirc 
  basedir <- "./data/rawdata/"
  for (d in seq_along(dates)) {
    # segmentize the raw data by calling the one_txt_segment() function
    input_folder_dir <- str_c (basedir, dates[d])
    if (file.exists(input_folder_dir)){
      one_txt_segment(dates[d])  
    } else {
      message (input_folder_dir, " does not exist.")
    }
  }
}


# 2, 3, 4, 5: cluster the individual articles, create the text corpus, and dataframe for each day`s aricles

# FUNCTION: corpus_to_TDM(), convert corpus of text to a clean TDM
# Input: corpus = tm`s Vcorpus object
# Output: txt_tdm a cleaned TDM
corpus_to_TDM <-function (corpus){
  # set the control parameters
  # 1. remove numbers, punctuations, and whitespace
  # 2. minDocFreq: minimal frequency of word in a doc 
  # 3. wordLengths: minimal legnth of word
  # 4. weighting: defaulted weighting by the word frequency (TF), if weightTfIdf is to normalize the weighting
  # weightTfIdf: https://www.rdocumentation.org/packages/tm/versions/0.6-2/topics/weightTfIdf
  control<-list(removePunctuation=T,removeNumbers=T, stripWhitespace=T, 
                minDocFreq=5, wordLengths =c(2,Inf))
  # 5. create the TDM
  txt_tdm<-TermDocumentMatrix(corpus, control)  
  # 6. further remove the sparse terms
  txt_tdm <- removeSparseTerms(txt_tdm, 0.80)
  
  return (txt_tdm)
}


# FUNCRION: one_create_dataframe (date), to create a dataframe for a specific date 
# Associated functions: corpus_to_TDM()
# Associated libraries: library(tidyverse), library(tm)
# Input: date = str
# Output: ddf = a dataframe for text of articles in one day.
# if there is file in the folder do the creating dataframe job
# if not, raise a message and return value 0

one_create_dataframe <- function (date){
  input_basedir <- "./data/segtext/"
  input_folder_dir <- str_c (input_basedir, date)
  txtname_list <- list.files(input_folder_dir, pattern = '*.txt')
  
  # an empty list for the text corpus
  aslist<- list()
  
  # check whether the file folder is empty or not 
  if (!length(txtname_list)) {
    message ("there is no file in:", input_folder_dir)
    return (0)
  } else {
    for (txt in txtname_list){
      txt_dir<-str_c(input_folder_dir,'/', txt) 
      # read in each txt file
      txt_segged<- readLines(txt_dir, encoding="UTF-8")
      # combine all the elements (genearted by each line) into one string, separated by space 
      txt_seggged<-paste(txt_segged, collapse = '')
      # separate the segged terms by space, and form a list of terms
      txt_list<-strsplit(as.character(txt_seggged),split=" ")
      # append this list to "aslist", to form a list of list of segged terms.
      aslist<-append(aslist, txt_list)
    }
    # vectorize all the text files.
    text_corpus=Corpus(VectorSource(aslist))
    
    # call corpus_to_TDM () function to covert the text_corpus of a day to a TDM
    txt_tdm<-corpus_to_TDM(text_corpus) 
    # transpose the tdm to dtm (each row: one doc/one term) and create a dataframe
    df <- tidy(t(txt_tdm))
    
    # incorporate the meta-information from the name of text to the created dataframe.
    # 1. delete the last four characters '.txt' for the list of text name.
    txtname_list<- gsub('.{4}$', '', txtname_list)
    # 2. form the dataframe by using the text name (separating one column to 'date_page', 'document', 'title')
    # join the "df" (dataframe of the DTM) to this meta-data dataframe by key="document"
    # (NOTE: must use "as_data_frame" rather than the "as.data.frame", 
    # otherwise the tibble will not be shown)
    df <- as_data_frame(txtname_list) %>%
      separate(value, into = c('date_page', 'document', 'title'), sep= '_') %>%
      left_join (df, by="document") 
    return (df)
  }
}




# FUNCTION: create_dataframe (start_date, end_daate)
# Associated libraries: library(dplyr), library(stringr)
# Associated functions: one_create_dataframe
# Input: start_date, end_date = str of the date
# Output a dataframe for all the terms in artilces during the time period above.

create_dataframe <-function (date_start, date_end){
  # initiate an empty dataframe
  df <- data.frame(date_page = character(),
                   document = character(), 
                   title = character(),
                   term = character(),
                   count = double(),
                   stringsAsFactors=FALSE)
  # create a date sequence based on the date_start and date_end
  dates<-seq(as.Date(date_start), as.Date(date_end), by=1)
  for (d in seq_along(dates)) {
    # readin the articles of each day in this period, create dataframe, 
    # combine the dataframe with the previous one
    df_temp <- one_create_dataframe(dates[d])
    # if there is no file in that folder of the date, a 0 will be returned by one_create_dataframe
    if (typeof(df_temp) == "list") {
      df <- bind_rows (df, df_temp)
    } else {message ("The date of ", dates[d], " is ignored when the dataframe is created.")}
  }
  return (as_data_frame(df))
}

####################################################################################################
# Debuging 
# found: when join dataframe with "1989-09-16" 
# to any dataframe, an error raized as
# "Error: Can't join on 'document' x 'document' because of incompatible types (integer / character) 
# since on the data source: this day`s newspaper is missing!
# so need to add control process for all the above functions, if there is no  .txt  (in this case,
# for the txt_list generated by the list.files() in a folder, will be character(0), then 
# length (character(0)) will be 0), raise a message or ignore this folder and continue to 
# do the dataframe combine
####################################################################################################




#################################################
# Running above functions to segmentize 
# the texts from "1989-01-01" to "1990-12-31"
#################################################
# to do the text_segment will take 1.5 h for 2 yrs` data, Be patient.

# text_segment("1989-01-01", "1989-12-31")
# dataframe1989<- create_dataframe("1989-01-01", "1989-12-31")
# object.size(dataframe1989)


#tidydata_1989 <- dataframe1989 %>%

# since we only scrape the front page, so there is no need to have page number in "date_page"
# transmute this column to "date", convert the date to date format from character

#mutate (date = as.Date( gsub('.{2}$', '', date_page)))  %>%
#select (date, document, title, term, count)

#write.csv(tidydata_1989, file = "dataframe/tidydata_1989")

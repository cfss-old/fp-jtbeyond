# Chinese Word Segmentation:
# purpose: to segmentize Chinese sentences to separated phrases, according to certain pattern

# Important:
# create a small dictionary for certain words "user.utf8"
# create a small dictionary for stop words "stopwords.txt"
library(feather)
library(stringr)
#install.packages("jiebaR")
library(jiebaR)
#install.packages("tm") 
#install.packages("NLP") # "tm" is attached with library NLP
library(NLP)
library(tm) # text mining library

#################################################
# A. Segmenentation of rawdata
#################################################

# list all the files in rawdata
ff <- list.files("./rawdata",pattern = '*.txt')

# iterate the rawdata folder to segmentize all the texts
for (f in ff) {
  # initializing the segmentation function, save the segmentized text into subfolder 
  op<-str_c("./segdata/",f)
  # using the user`s dictionary, delete the stop words, save the file into rawdata/seg/ 
  mixseg = worker (bylines=T, user='./user.utf8', stop_word = "./stopwords.txt", output = op)
  filename<-str_c("./rawdata/",f)
  mixseg<=filename
}

#################################################
# B.vectorize the segmentized txt file and
# create the corpus for text files. 
#################################################

segf<-list.files("./segdata",pattern = '*.txt')
# an empty list for the 
aslist<- list()

for (f in segf) {
  dirc<-str_c("./segdata/", f)
  # read in each txt file
  txt_segged<- readLines(dirc, encoding="UTF-8")
  # combine all the elements (genearted by each line) into one string, separated by space 
  txt_seggged<-paste(txt_segged, collapse = '')
  # separate the segged terms by space, and form a list of terms
  txt_list<-strsplit(as.character(txt_seggged),split=" ")
  # append this list to "aslist", to form a list of list of segged terms.
  aslist<-append(aslist, txt_list )
}

### Preparation for the term-document-matrix  (TDM)
# vectorize all the text files.
text_corpus=Corpus(VectorSource(aslist))
# check the corpus
# inspect(text_corpus)
# writelines() to print part of text (to confirm all the texts have been vectorized)
# writeLines(as.character(text_corpus[[1]]))

#################################################
# C. Further clean the corpus and create the TDM
#################################################
# set the control parameters
# 1. remove numbers, punctuations, and whitespace
# 2. minDocFreq: minimal frequency of word in a doc 
# 3. wordLengths: minimal legnth of word
# 4. weighting: defaulted weighting by the word frequency (TF), if weightTfIdf is to normalize the weighting
# weightTfIdf: https://www.rdocumentation.org/packages/tm/versions/0.6-2/topics/weightTfIdf
control<-list(removePunctuation=T,removeNumbers=T, stripWhitespace=T, 
              minDocFreq=5, wordLengths =c(2,Inf))
# 5. create the TDM
txt_tdm<-TermDocumentMatrix(text_corpus, control)  
# 6. further remove the sparse terms
txt_tdm <- removeSparseTerms(txt_tdm, 0.80)
## To check the newly generated tdm:
# txt_tdm 
# inspect(t(txt_tdm))

# find out the high frequency word
# findFreqTerms(txt_tdm, 250)
# find the correlated words with certain word
# findAssocs(txt_tdm, "反革命", 0.6) "反革命" means "anti-revolutionary"


#################################################
# D. Tidying term-document matrices (TDM) 
#   <create a dataframe>
#################################################
# Thanks to the tidytext library, we can easily transform tdm from and to a dataframe.
# https://cran.r-project.org/web/packages/tidytext/vignettes/tidying_casting.html

library(tidyverse)
# install.packages("tidytext")
library(tidytext)

df<-tidy(t(txt_tdm)) %>%
  mutate(paper_seq=as.numeric(document))  


# create a time sequence table for each document
dates<-seq(as.Date("1989-05-04"), as.Date("1989-07-04"), by=1) 
paper_seq<-seq(1, 62, by=1) 
date_doc<-data.frame(paper_seq, dates) 

# Use the above time sequence table as a reference, to inner join this table to the dataframe
df <- df %>%
  inner_join(date_doc,by=c("paper_seq"))

write_feather(df, "data/df.feather")


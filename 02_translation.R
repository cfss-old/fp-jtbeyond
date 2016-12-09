#install.packages("RYandexTranslate")
library(RYandexTranslate)


##################################################################################
# store the user name and key in local .Rprofile:
# 1) make sure .Rprofile end with a blank line
# 2) makse sure the .gitignore contains .Rprofile, and not sync it to the Github
# 3) make a note in Readme for this. 
###################################################################################

#api_key<-getOption("API_key")
api_key<-"trnsl.1.1.20161115T200845Z.846d2e44de5c376f.d200e21033fe9831123e55d9cf62469cf004e516"

# Function: translateCnWords ~iterate the term column in df to get the corresponding English translation
translateCnWords<-function(data){
  En_term<-list()
  for (cnword in data){
    enword<-RYandexTranslate::translate(api_key, text=cnword, lang="zh-en")
    En_term<-append(En_term, enword$text)
  }
  En_term<-unlist(En_term)
}


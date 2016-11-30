#install.packages("RYandexTranslate")
library(RYandexTranslate)


api_key<-"trnsl.1.1.20161115T200845Z.846d2e44de5c376f.d200e21033fe9831123e55d9cf62469cf004e516"

#testing code:
#data<-detect_language(api_key,text="how are you?")
#data

# Function: translateCnWords ~iterate the term column in df to get the corresponding English translation
translateCnWords<-function(data){
  En_term<-list()
  for (cnword in data){
    enword<-RYandexTranslate::translate(api_key, text=cnword, lang="zh-en")
    En_term<-append(En_term, enword$text)
  }
  En_term<-unlist(En_term)
}


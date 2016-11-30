library(feather)
library(tidyverse)
library(tidytext)
# install.packages("topicmodels")
library(topicmodels)


# devtools::install_github("hadley/ggplot2")
# need to install the latest version of ggplot2 to solve the confliction between the coord_flip
# and the scales="free"


# call the translation function in my script
source("translation.R")

# conversion of the tidy data to  two DTM before crackdown and after crackdown
crk<-as.Date("1989-06-03")

dtm_before<- read_feather("data/df.feather") %>%
  filter (term !="　　") %>%
  filter(dates < crk) %>%
  cast_dtm(dates, term, count)  

dtm_after<- read_feather("data/df.feather") %>%
  filter (term !="　　") %>%
  filter(dates > crk) %>%
  cast_dtm(dates, term, count)
# do the LDA simulation for both of two periods with k=3 (arbitary)
before_lda <- LDA(dtm_before, k = 3, control = list(seed = 1234))
after_lda <- LDA(dtm_after, k = 3, control = list(seed = 1234))
# tidy the LDA model
before_lda_td<-tidy(before_lda )
after_lda_td<-tidy(after_lda )

######################################
# plot the top terms
######################################
# the topic model before the crackdown
top_terms_before <- before_lda_td %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(En_term=translateCnWords(term))

top_terms_before %>%
  mutate(En_term = reorder(En_term, beta)) %>%
  ggplot(aes(En_term, beta, fill = factor(topic))) +
  geom_bar(alpha = 0.8, width=0.5, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales="free")+
  coord_flip()+
  labs(title="Topic Model for People`s Daily before crackdown from May to June in 1989",
       x="term", y="beta")
png("upload.png", width = 800, height =400)
ggsave("graph/top_terms_before.png")

########################################
# the topic model after the crackdown
top_terms_after <- after_lda_td %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(En_term=translateCnWords(term))

top_terms_after %>%
  mutate(En_term = reorder(En_term, beta)) %>%
  ggplot(aes(En_term, beta, fill = factor(topic))) +
  geom_bar(alpha = 0.8, width=0.5, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales="free")+
  coord_flip()+
  labs(title="Topic Model for People`s Daily after crackdown from May to June in 1989",
       x="term", y="beta")
png("upload.png", width = 800, height =400)
ggsave("graph/top_terms_after.png")
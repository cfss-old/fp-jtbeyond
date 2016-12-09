library(feather)
library(tidyverse)
library(tm)
library(NLP)
library(tidytext)
library(topicmodels)

df<-read_feather("data/df") %>%
  select(Datetime, term, count) %>%
  group_by (Datetime, term) %>%
  summarise (count=sum(count)) %>%
  ungroup()


# wordlist1=c("动乱", "暴乱")
# 
# df %>%
#   filter(Datetime >= as.Date("1989-04-01") & Datetime <= as.Date("1990-06-04")) %>%
#   filter (term %in% wordlist1)%>%
#   mutate (en_term=ifelse(term=="动乱", "Unrest", "Riots")) %>%
#   complete(Datetime, en_term, fill =list (count=0)) %>%
#   ggplot(aes(x=Datetime, y=count, color=en_term)) +
#   theme_bw()+
#   geom_line()+
#   labs(title="Word Counts of in People`s Daily during 1989 May to June", 
#        subtitle="Unrest vs. Riots",
#        x="date", y="count (per day)")+
#   theme(legend.title=element_blank(), legend.position="bottom")
# 
# 
# wordlist2=c("反革命", "稳定")
# 
# df %>%
#   filter(Datetime >= as.Date("1989-05-01") & Datetime <= as.Date("1990-06-04")) %>%
#   filter (term %in% wordlist2)%>%
#   mutate (en_term=ifelse(term=="稳定", "Stability", "the Counter-revolution")) %>%
#   complete(Datetime, en_term, fill =list (count=0)) %>%
#   ggplot(aes(x=Datetime, y=count, color=en_term)) +
#   theme_bw()+
#   geom_line()+
#   labs(title="Word Counts of in People`s Daily during 1989 May to June", 
#        subtitle="the Counter-revolution vs. Stability",
#        x="date", y="count (per day)")+
#   theme(legend.title=element_blank(), legend.position="bottom")
# 
# 
#  
# 
# wordlist3=c("资产阶级自由化", "安定团结")
# 
# df %>%
#   filter(Datetime >= as.Date("1986-10-01") & Datetime <= as.Date("1987-12-01")) %>%
#   filter (term %in% wordlist3) %>%
#   mutate (en_term=ifelse(term=="资产阶级自由化", "bourgeois liberalism", "stability and unity")) %>%
#   complete(Datetime, en_term, fill =list (count=0)) %>%
#   ggplot(aes(x=Datetime, y=count, color=en_term)) +
#   theme_bw()+
#   geom_line()+
#   labs(title="Word Counts of in People`s Daily during 1989 May to June", 
#        subtitle="Bourgeois Liberalism vs. Stability and Unity",
#        x="date", y="Count")+
#   theme(legend.title=element_blank(), legend.position="bottom") 
# 
# 
# 
# 
# wordlist4 = c("建设", "四项")
# 
# df %>%
#   filter(Datetime >= as.Date("1986-12-01") & Datetime <= as.Date("1987-12-01")) %>%
#   filter (term %in% wordlist4) %>%
#   mutate (en_term=ifelse(term=="建设", "Economic Construction", "Four Principles")) %>%
#   complete(Datetime, en_term, fill =list (count=0)) %>%
#   ggplot(aes(x=Datetime, y=count, color=en_term)) +
#   theme_bw()+
#   geom_line()+
#   labs(title="Word Counts of in People`s Daily during 1989 May to June", 
#        subtitle="Economic Construction and Four Principles",
#        x="date", y="Count")+
#   theme(legend.title=element_blank(), legend.position="bottom") 
# 
#   

        
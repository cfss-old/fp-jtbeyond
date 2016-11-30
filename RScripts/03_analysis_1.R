library(feather)
library(tidyverse)
library(tidytext)

# to call the translation function from the other R script
source("translation.R")


###############################################
# Overall Word Frequency analysis
###############################################
df<- read_feather("data/df.feather") %>%
  filter (term !="　　")  # although in the tdm cleaning, has deleted some of the spaces, 
#there are still some spaces, here to remove all the whitespaces
# but do not not save this document, since the space is useful in reading in the data

  
# to calculate the word counts in whole period
word_counts<-df %>%
  group_by (term) %>%
  summarise(total=sum(count)) %>%
  arrange(desc(total)) %>%
  slice(1:20) %>%
  mutate(En_term=translateCnWords(term)) 
write_feather(word_counts, "data/word_counts.feather")

WordCounts<-ggplot (word_counts, aes(x=reorder(En_term,total) , y=total)) +
  geom_bar (stat="identity") +
  labs(title="Word Counts in People`s Daily from  May 4th to July 4th in 1989",x=NULL, y="Count")+
  coord_flip()
png("upload.png", width = 800, height =400)
ggsave("graph/WordCounts.png", plot=WordCounts)

# to calculate the word counts before crackdown
crk<-as.Date("1989-06-03")

before_crk<-df %>%
  filter(dates < crk) %>%
  group_by (term) %>%
  summarise(total=sum(count)) %>%
  arrange(desc(total)) 

after_crk<-df %>%
  filter(dates > crk) %>%
  group_by (term) %>%
  summarise(total=sum(count)) %>%
  arrange(desc(total)) 
  
  
CountBeforeCrk<-before_crk %>%
  slice(1:20) %>%
  mutate(En_term=translateCnWords(term)) %>%
  ggplot (aes(x=reorder(En_term,total) , y=total)) +
  geom_bar (stat="identity") +
  labs(title="Word Counts in People`s Daily before crackdown in 1989", 
       subtitle="Top 20 terms used",
       x=NULL, y="Count")+
  coord_flip()
png("upload.png", width = 800, height =400)
ggsave("graph/CountBeforeCrk.png", plot=CountBeforeCrk)
  
CountAfterCrk<-after_crk %>%
  slice (1:20) %>%
  mutate(En_term=translateCnWords(term)) %>%
  ggplot (aes(x=reorder(En_term,total) , y=total, fill="red")) +
  geom_bar (stat="identity") +
  labs(title="Word Counts in People`s Daily after crackdown in 1989", 
       subtitle="Top 20 terms used",
       x=NULL, y="Count")+
  coord_flip() 

png("upload.png", width = 800, height =400)
ggsave("graph/CountAfterCrk.png", plot=CountAfterCrk)
  
###############################################
#  Word Frequency analysis for specific words
###############################################  
  
# in this session, I will explore several key words in 1989 movement:

wordlist=c("动乱", "暴乱")

plot_riots<-df %>%
    filter (term %in% wordlist) %>%
    mutate (en_term=ifelse(term=="动乱", "unrest", "riots")) %>% # generate a new column for english word
    ggplot(aes(x=dates, y=count, color=en_term)) +
    geom_line()+
    labs(title="Word Counts of in People`s Daily during 1989 May to June", 
       subtitle="unrest and riots",
       x="date", y="Count")+
    theme(legend.title=element_blank()) 
png("upload.png", width = 600, height =400)
ggsave("graph/plot_riots.png", plot=plot_riots)

wordlist2=c("反革命","稳定")
plot_stability<-df %>%
  filter (term %in% wordlist2) %>%
  mutate (en_term=ifelse(term=="反革命", "counter-revolution", "stability")) %>% # generate a new column for english word
  ggplot(aes(x=dates, y=count, color=en_term)) +
  geom_line()+
  labs(title="Word Counts of in People`s Daily during 1989 May to June", 
       subtitle="counter-revolution and stability",
       x="date", y="Count")+
  theme(legend.title=element_blank())
png("upload.png", width = 600, height =400)
ggsave("graph/plot_stability.png", plot=plot_stability)

 
wordlist3=c("资产阶级自由化","社会主义")
plot_capitalism<-df %>%
  filter (term %in% wordlist3) %>%
  mutate (en_term=ifelse(term=="资产阶级自由化", "bourgeois liberalism", "socialism")) %>% # generate a new column for english word
  ggplot(aes(x=dates, y=count, color=en_term)) +
  geom_line()+
  labs(title="Word Counts of in People`s Daily during 1989 May to June", 
       subtitle="capitalism and socialism",
       x="date", y="Count")+
  theme(legend.title=element_blank()) 
 
png("upload.png", width = 600, height =400)
ggsave("graph/plot_capitalism.png", plot=plot_capitalism)

  
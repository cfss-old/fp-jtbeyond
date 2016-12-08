library(feather)
library(stringr)
#install.packages("tm") 
#install.packages("NLP") # "tm" is attached with library NLP
library(NLP)
library(tm) # text mining library
library(tidyverse)
# install.packages("tidytext")
library(tidytext)

# 3D plot by using the ploty
#library("devtools")
#install_github("ropensci/plotly")  # plotly is part of ropensci
#library(plotly)
# The instruction for ploty is here: https://plot.ly/r/getting-started/
#library(plot3D)

#install.packages("sparklyr")
#spark_install(version = "2.0.0")
#library(sparkly)
######################################################################################
# There are two easy method to deal with big dataset:
# a) sparklyr
# Note: spark needs you to install the java JDK
# http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html

# b) or instead of the library(data.frame), use the library(data.table)
# Dealing with big dataset by data.table
# https://www.analyticsvidhya.com/blog/2016/05/data-table-data-frame-work-large-data-sets/
######################################################################################
#sc <- spark_connect(master = "local", version = "2.0.0")



# Read in data from the dataframe/
data1986 <-read.csv("dataframe/tidydata_1986", colClasses=c("NULL",NA,NA))
# http://stackoverflow.com/questions/14527466/is-there-a-way-to-omit-the-first-column-when-reading-a-csv
data1987 <-read.csv("dataframe/tidydata_1987", colClasses=c("NULL",NA,NA))
data1988 <-read.csv("dataframe/tidydata_1988", colClasses=c("NULL",NA,NA))
data1989 <-read.csv("dataframe/tidydata_1989", colClasses=c("NULL",NA,NA))
data1990 <-read.csv("dataframe/tidydata_1990", colClasses=c("NULL",NA,NA))

######################################################################################
# Arbitrily categorize the terms in to "strong ideology (SI)," weak ideology (WI)"
# and "economic perfermance (EP)", and socre each months` articels
# based on these three  categories.
######################################################################################

df <- do.call ("rbind", list(data1986,data1987, data1988, data1989, data1990)) %>%
  mutate (Datetime = as.Date(date)) %>%
  separate (date,  c("Year", "Month", "Date"), sep="-") %>%
  unite (year_month, Year, Month, sep = "-") %>%
  drop_na(term, count)

SI <- c ( "资产阶级自由化", "精神污染", "阶级斗争", "四项",  "剥削阶级", "工人阶级", "无产阶级", "资本主义", "反社会主义",
          "帝国主义", "修正主义", "辩证唯物主义", "虚无主义", "共产主义", "共产主义者", "马克思主义", "马克思主义者", "革命英雄主义","官僚资本主义", "唯物主义", "历史唯物主义",
          "革命军人", "革命队伍", "革命斗志", "革命精神", "革命事业","革命化", "反革命")
WI <- c ("党的领导", "党的建设", "个人主义", "集体主义", "民主专政", "艰苦奋斗","愚公精神", 
         "安定团结", "团结奋斗", "团结一致", "紧密团结", "加强团结", "团结" ,
         "稳定","稳定性","社会安定", 
         "党风建设", "法制", "民主法制","法制建设", "法制化","法制观念","法制教育", "健全法制",
         "爱国主义", "爱国精神","爱国热情", 
         "民主监督","民主集中制")
EP <- c("四化建设", "基本建设", "城乡建设","改革开放", "对外开放", "改革建设",
        "科学技术", "生产力", "科技进步","科技兴农",
        "体制改革", "深化改革", "推进改革", "繁荣昌盛", "繁荣富强", "经济繁荣","经济效益","经济增长", "迅猛发展", "蓬勃发展")

#Note: to plot the ideological terms is very tricky, we cannot include those words themselves with very high
# frequency all over the time, such as "socialism"（社会主义）,if this one added into the SI,
# we will see actully SI will be the dominant terms all over five years
# In addition, we can not use very simple words, such as "construction" (建设), if put this word
# word in EP, then we will find EP will be very dominant besides in the period after the crackdown
# of the movement. 


df_score <- df %>%
  mutate (SI_score = ifelse (term %in% SI, count, 0)) %>%
  mutate (WI_score = ifelse (term %in% WI, count, 0)) %>%
  mutate (EP_score = ifelse (term %in% EP, count, 0)) 

# write this dataframe with scores in to a csv file and upload
# complete_df_and_scores <- df_score %>% 
#   select (Datetime, term, count, SI_score, WI_score, EP_score)
# write_csv(complete_df_and_scores, "dataframe/complete_dataset_with_scores")
##############################
# Monthly scores
##############################
df_monthly_score <-df_score %>%
  group_by (year_month) %>%
  summarise (SI = sum (SI_score), WI = sum (WI_score), EP = sum(EP_score), monthly_term =sum (count)) %>%
  # calculate in each month, the featured terms count for how many percentage in total vocabulary
  mutate (adj_SI= 100*(SI/monthly_term), adj_WI= 100*(WI/monthly_term), adj_EP = 100*(EP/monthly_term)) %>%
  # convert the year_month to XXXX-XX-01 as a datetime type variable for the convenience in the next step`s plotting
  mutate (year_month = as.Date(paste0 (year_month, "-01"))) %>%
  select (year_month, adj_SI, adj_WI, adj_EP) %>%
  gather (key, percent, -year_month)

##############################
# Plot1: Monthly_Score Plot
##############################

ggplot (df_monthly_score, aes(x = year_month, y = percent, color = key)) +
  theme_bw () + 
  # legend position, and rotate the x-axis` label
  theme(legend.position ="bottom", axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_line() +
  labs (title = "SI, WI and EP terms` percentage of each month`s frontpage in People`s Daily", 
        subtitle = "(January 1986 to December 1990)", 
        x = "months from 1986 to 1990",
        y = "weighted scores (in percentage)",
        color = NULL)+ # "color"=NULL to silent the legend title. 
  # change the x-axis`s label 
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 month", date_labels = "%Y-%m") +
  # change the label`s name
  scale_color_discrete(breaks=c("adj_SI", "adj_WI", "adj_EP"), label=c("Strong Ideology","Weak Ideology", "Economic Performance"))
  
png("upload.png", width = 600, height =400)
ggsave("graph/scores_acorss_years.png") 

##############################
# Daily scores
##############################                   
df_daily_score <-df_score %>%
  group_by (Datetime) %>%
  summarise (SI = sum (SI_score), WI = sum (WI_score), EP = sum(EP_score), daily_term =sum (count)) %>%
  # calculate in each month, the featured terms count for how many percentage in total vocabulary
  mutate (adj_SI= 100*(SI/daily_term), adj_WI= 100*(WI/daily_term), adj_EP = 100*(EP/daily_term)) %>%
  select (Datetime, adj_SI, adj_WI, adj_EP) %>%
  gather (key, percent, -Datetime)
  

##############################
# Daily scores plot
############################## 
ggplot (df_daily_score, aes(x = Datetime, y = percent, fill = key)) +
  theme_bw () + 
  # legend position, and rotate the x-axis` label
  theme(legend.position ="bottom", axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_bar(stat="identity") +
  labs (title = "SI, WI and EP terms` percentage of each day`s frontpage in People`s Daily", 
        subtitle = "(January 1986 to December 1990)", 
        x = "months from 1986 to 1990",
        y = "weighted scores (in percentage)",
        color = NULL)+ # "color"=NULL to silent the legend title. 
  # change the x-axis`s label 
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 month", date_labels = "%Y-%m") +
  # change the label`s name
  scale_fill_discrete(breaks=c("adj_SI", "adj_WI", "adj_EP"), label=c("Strong Ideology","Weak Ideology", "Economic Performance")) +
  

png("upload.png", width = 600, height =400)
ggsave("graph/daily_scores_acorss_years.png") 


# find through out 5 years, everyday mean for adj_SI=  0.2165333; adj_WI=0.4380292; adj_EP= 0.251179
# p <- plot_ly(df_daily_score, x = ~adj_EP, y = ~adj_WI, color = ~as.integer(year),
#              marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 20),
#              text = ~paste('Strong ideology:',adj_SI, 'Weak ideology:',adj_WI, 'Economic performance', adj_EP,
#                            '<br>Year:', year)) 

## Use plot3D to plot the Daily score
# x<-df_daily_score$adj_SI
# y<-df_daily_score$adj_WI
# z<-df_daily_score$adj_EP
# 
# 
# scatter3D(x, y, z, colvar = NULL, 
#           col.var = as.integer(df_daily_score$year),
#           col = c("green", "red", "blue", "yellow", "orange"), 
#           pch = 19, cex = 0.5,
#           bty = "g", colkey = FALSE, main ="SI, WI and EP in daily frontpage of People`s Daily (1986-1990)",
#           xlab =" Strong ideology",
#           ylab =" Weak ideology",
#           zlab =" Economic Performance", 
#           labels = c("1986", "1987", "1988", "1989", "1990"),
#           theta = 15, phi = 20)


  
setwd("C:/Users/User/Desktop")

df_results <-read.csv("results.csv")
library(dplyr)
library(ggplot2)
library(tidyr)

str(df_results)
df_results$date <-as.Date(df_results$date) #changing the date variable from factor to date

#create new col 'result' for home wins,away wins and draw
df_results$result[df_results$home_score > df_results$away_score] <- "Home Win" 

df_results$result[df_results$home_score < df_results$away_score] <- "Away Win"

df_results$result[is.na(df_results$result)]<-'Draw'

#filtering my data and selecting only those from World Cup
World_cup<- df_results %>% filter(tournament=='FIFA World Cup')

str(World_cup)

World_cup$total_goals<-World_cup$home_score +World_cup$away_score #create a col for total goals per match in WC


#calculating the mean and sum of total goals from 1994-2014
World_cup_2014 <- World_cup %>% filter(date>"2014-01-01" & date<"2014-12-12")
mean(World_cup_2014$total_goals) #2.67
sum(World_cup_2014$total_goals) #171

World_cup_2010<- World_cup %>% filter(date>'2010-01-01' & date<'2010-12-12')
mean(World_cup_2010$total_goals) #2.26
sum(World_cup_2010$total_goals) #145

World_cup_2006 <- World_cup %>% filter(date>"2006-01-01" & date<"2006-12-12")
mean(World_cup_2006$total_goals) #2.29
sum(World_cup_2006$total_goals) #147

World_cup_2002 <- World_cup %>% filter(date>"2002-01-01" & date<"2002-12-12")
mean(World_cup_2002$total_goals) #2.51
sum(World_cup_2002$total_goals) #161

World_cup_1998 <- World_cup %>% filter(date>"1998-01-01" & date<"1998-12-12")
mean(World_cup_1998$total_goals) #2.67
sum(World_cup_1998$total_goals) #171

World_cup_1994 <- World_cup %>% filter(date>"1994-01-01" & date<"1994-12-12")
mean(World_cup_1994$total_goals) #2.71
sum(World_cup_1994$total_goals) #141


#creating a dataframe with WC from 1994-2014 and avg,total goal per tournament
Tournament<- c("World Cup 1994","World Cup 1998","World Cup 2002","World Cup 2006","World Cup 2010","World Cup 2014")
Avg_Goal<-c(2.71,2.67,2.51,2.29,2.26,2.67)
Tot_goals<-c(141,171,161,147,145,171)
WC_Stats<-data.frame(Tournament,Avg_Goal,Tot_goals,stringsAsFactors = FALSE)
str(WC_Stats)

#create 2 graphs and combine them
library(gridExtra)
p1<-ggplot(WC_Stats,aes(Tournament,Tot_goals,group=1)) +
  geom_point() +
  geom_line() +
  xlab("WC from 1994 to 2014")+
  ylab("Total goals per tournament") +
  ggtitle("Graph of total goals in every WC since 1994")

p2<-ggplot(WC_Stats,aes(Tournament,Avg_Goal,group=1)) +
  geom_point() +
  geom_line()+
  ylab("Goals per Game")+
  ggtitle("Graph of average goals in every WC since 1994")

grid.arrange(p1,p2,ncol=2)

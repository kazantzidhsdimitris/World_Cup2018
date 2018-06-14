setwd("C:/Users/User/Desktop")

df_results <-read.csv("results.csv")
library(dplyr)
library(ggplot2)
library(tidyr)

str(df_results)
#create new col 'result' for home wins,away wins and draw
df_results$result[df_results$home_score > df_results$away_score] <- "Home Win" 
  
df_results$result[df_results$home_score < df_results$away_score] <- "Away Win"

df_results$result[is.na(df_results$result)]<-'Draw'

df_results<-df_results %>% unite(score,c("home_score","away_score"),sep = "-")



df_results$date <-as.Date(df_results$date) #changing the date variable from factor to date


v<-c("Portugal","Argentina","Brazil","Uruguay","Spain","England","France","Germany","Croatia","Belgium","Russia")


World_cup<- df_results %>% filter(tournament=='FIFA World Cup')


rand_score<-c("0-0","1-1","2-2","3-3","4-4","1-0","2-0","3-0","4-0","2-1","3-1","3-2","4-3","1-2","2-3","1-3","3-4","2-4")


#counting appearances of rand scores in WC
ggplot(subset(World_cup,score %in% rand_score),aes(score))+
  geom_bar()+
  ggtitle("Appearances of rand scores in WC")

#Spain as a home team in WC
df_Spain_home <-World_cup[World_cup$home_team=='Spain',]
nrow(df_Spain_home %>% filter(df_Spain_home$result=='Home Win')) / nrow(df_Spain_home) #0.41 perc win of Spain in the years 2017,2018
nrow(df_Spain_home %>% filter(df_Spain_home$result=='Draw')) / nrow(df_Spain_home) #0.24 perc draw of Spain
nrow(df_Spain_home %>% filter(df_Spain_home$result=='Away Win')) / nrow(df_Spain_home) #0.35 perc of losing

#Spain as an away team in WC
df_Spain_Away <-World_cup[World_cup$away_team=='Spain',]
nrow(df_Spain_Away %>% filter(result=='Away Win')) / nrow(df_Spain_Away) #0.57 perc Spain winning away
nrow(df_Spain_Away %>% filter(result=='Draw')) / nrow(df_Spain_Away) #0.17 perc for Draw
nrow(df_Spain_Away %>% filter(result=='Home Win')) / nrow(df_Spain_Away) #0.27 perc Spain losing away

mean(c(0.41,0.57)) #0.49 perc of wins for Spain
mean(c(0.24,0.17)) #0.20 perc of draws
mean(c(0.35,0.27)) #0.31 perc of losses

#Portugal as an away team
df_Port_Away <-World_cup[World_cup$away_team=='Portugal',]
nrow(df_Port_Away %>% filter(result=='Away Win')) / nrow(df_Port_Away) #0.33 perc Portugal winning 
nrow(df_Port_Away %>% filter(result=='Draw')) / nrow(df_Port_Away) #0.2 perc for Portugal draw
nrow(df_Port_Away %>% filter(result=='Home Win')) / nrow(df_Port_Away) #0.47 perc Portugal losing 

#Portugal as home team
df_Port_Home <-World_cup[World_cup$home_team=='Portugal',]
nrow(df_Port_Home %>% filter(result=='Home Win')) / nrow(df_Port_Home) #0.72 perc Portugal winning
nrow(df_Port_Home %>% filter(result=='Draw')) / nrow(df_Port_Home) # 0.1 perc Portugal draw
nrow(df_Port_Home %>% filter(result=='Away Win')) / nrow(df_Port_Home) #0.18 perc Portugal losing

mean(c(0.33,0.72)) #0.52 perc of wins for Portugal
mean(c(0.2,0.1)) #0.15 perc of draws
mean(c(0.47,0.18)) #0.33 perc of losing

#Argentina as away team
df_Arg_Away <-World_cup[World_cup$away_team=='Argentina',]
nrow(df_Arg_Away %>% filter(result=='Away Win')) / nrow(df_Arg_Away) #0.16 perc Arg winning away
nrow(df_Arg_Away %>% filter(result=='Draw')) / nrow(df_Arg_Away) #0.37 perc for Draw
nrow(df_Arg_Away %>% filter(result=='Home Win')) / nrow(df_Arg_Away) #0.47 perc Arg losing away

#Argentina as home team
df_Arg_Home <-World_cup[World_cup$home_team=='Argentina',]
nrow(df_Arg_Home %>% filter(result=='Home Win')) / nrow(df_Arg_Home) #0.67 perc Arg winning
nrow(df_Arg_Home %>% filter(result=='Draw')) / nrow(df_Arg_Home) # 0.12 perc Arg draw
nrow(df_Arg_Home %>% filter(result=='Away Win')) / nrow(df_Arg_Home) #0.21 perc Arg losing

mean(c(0.16,0.67)) #0.41 perc of wins for Arg
mean(c(0.37,0.12)) #0.25 perc of draws
mean(c(0.47,0.21)) #0.34 perc of losing

#Brazil as away team
df_Br_Away <-World_cup[World_cup$away_team=='Brazil',]
nrow(df_Br_Away %>% filter(result=='Away Win')) / nrow(df_Br_Away) #0.70 perc Br winning away
nrow(df_Br_Away %>% filter(result=='Draw')) / nrow(df_Br_Away) #0.13 perc for Draw
nrow(df_Br_Away %>% filter(result=='Home Win')) / nrow(df_Br_Away) #0.17 perc Br losing away

#Brazil as home team
df_Br_Home <-World_cup[World_cup$home_team=='Brazil',]
nrow(df_Br_Home %>% filter(result=='Home Win')) / nrow(df_Br_Home) #0.66 perc Br winning home
nrow(df_Br_Home %>% filter(result=='Draw')) / nrow(df_Br_Home) #0.18 perc for Draw
nrow(df_Br_Home %>% filter(result=='Away Win')) / nrow(df_Br_Home) #0.16 perc Br losing home

mean(c(0.7,0.66)) #0.68 win rate for Br
mean(c(0.13,0.18)) #0.16 draw rate for Br
mean(c(0.17,0.16)) #0.17 losing rate for Br

#Germany as away team
df_Ger_Away <-World_cup[World_cup$away_team=='Germany',]
nrow(df_Ger_Away %>% filter(result=='Away Win')) / nrow(df_Ger_Away) #0.58 perc Ger winning away
nrow(df_Ger_Away %>% filter(result=='Draw')) / nrow(df_Ger_Away) #0.13 perc for Draw
nrow(df_Ger_Away %>% filter(result=='Home Win')) / nrow(df_Ger_Away) #0.29 perc Ger losing away

#Germany as home team
df_Ger_Home <-World_cup[World_cup$home_team=='Germany',]
nrow(df_Ger_Home %>% filter(result=='Home Win')) / nrow(df_Ger_Home) #0.64 perc Ger winning home
nrow(df_Ger_Home %>% filter(result=='Draw')) / nrow(df_Ger_Home) #0.21 perc for Draw
nrow(df_Ger_Home %>% filter(result=='Away Win')) / nrow(df_Ger_Home) #0.21 perc Ger losing home

mean(c(0.58,0.64)) #0.61 win rate for Germany
mean(c(0.13,0.21)) #0.17 draw rate
mean(c(0.29,0.21)) #0.25 losing rate for Germany

#create dataframe with all the ratings we've found
Country<-c("Germany","Brazil","Argentina","Portugal","spain")
Home_Win_rate<-c(0.64,0.66,0.67,0.72,0.41)
Home_Draw_rate<-c(0.21,0.18,0.12,0.1,0.24)
Home_Lose_rate<-c(0.21,0.16,0.21,0.18,0.35)
Away_Win_rate<-c(0.58,0.7,0.16,0.33,0.57)
Away_Draw_rate<-c(0.13,0.13,0.37,0.2,0.17)
Away_Lose_rate<-c(0.29,0.17,0.47,0.47,0.27)
World_Cup_ratings<-data.frame(Country,Home_Win_rate,Away_Win_rate,Home_Draw_rate,Away_Draw_rate,Home_Lose_rate,Away_Lose_rate,stringsAsFactors = FALSE)

str(World_Cup_ratings)


ggplot(World_Cup_ratings,aes(Country,Home_Win_rate,group=1))+
  geom_point() +geom_line() +
  ggtitle("Win rate as home-teams for 5 Countries")

library(ggrepel)
library(gridExtra)
p1<-ggplot(World_Cup_ratings,aes(Home_Win_rate,Away_Win_rate))+
  geom_point(aes(color=Country),size=3)+
  geom_text_repel(aes(label=Country),size=3.5)+
  ggtitle("Home and Away win rates for 5 Countries")+
  xlab("Win rate as Home team")+
  ylab("Win rate as Away team")

p2<-ggplot(World_Cup_ratings,aes(Home_Draw_rate,Away_Draw_rate))+
  geom_point(aes(color=Country),size=3) +
  geom_text_repel(aes(label=Country),size=3.5)+
  ggtitle("Home and Away draw rates for 5 Countries")+
  xlab("Draw rate as Home team")+
  ylab("Draw rate as Away team")

p3<-ggplot(World_Cup_ratings,aes(Home_Lose_rate,Away_Lose_rate))+
  geom_point(aes(color=Country),size=3)+
  geom_text_repel(aes(label=Country),size=3.5)+
  ggtitle("Home and Away lose rates for 5 Countries")+
  xlab("Lose rate as Home team")+
  ylab("Lose rate as Away team")


grid.arrange(p1,p2,p3,ncol=1)

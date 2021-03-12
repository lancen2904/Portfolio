#######LOADING NESSCARY LIBRARY######
library(tidyverse)
library(dplyr)
library(plyr)
library(readxl)
library(data.table)
library(ggplot2)
library(hrbrthemes)
######LOAD THE DATA & ANALYSE DATA########

##We have 3 datasets
data<-read.csv("Slotmachine1.csv")
data2<-read.csv("Slotmachine2.csv")
data3<-read.csv("Slotmachine3.csv")
##If you can read the file please "GO TO SESSION ==> SET WORKING DIRECTORY THEN DIRECT TO THE ROOT FOLDER
#Count number of round that lose and win -So we can see the actual Winning and Losing Rate
count(data$Result) 
count(data2$Result)
count(data3$Result)
#Count the Awards
count(data$Reward)
count(data2$Reward)
count(data3$Reward)
# Base on the count:
#dataset1: the winning rate only 10% (90 loses and 10 wins over 100 rounds)
#dataset2: the winning rate only 11%  (16 loses and 2 wins over 18 rounds)
#dataset3: the winning rate only 18%  (31 loses and 7 wins over 38 rounds)

#Check the trending of the Coin. There are 2 scenario:
#1/ If player has huge amount of money since the beginning and play bet small amount.
#The first data set we have initial amount of 167000 and player bet 1000/round
ggplot(data, aes(x=data$Round, y=data$New.Coin.Count)) +
  geom_line(color ="#69b3a2", size =2, alpha=0.9, linetype=2)+
  theme_ipsum()+
  xlab("Round")+
  ylab("Coint Count")+
  ggtitle("Trend of Coin")

#2/ If player has smaller amount of money and bet same amount or adjust the bet amount:
ggplot(data2, aes(x=data2$Round, y=data2$New.Coin.Amount)) +
  geom_line(color ="#69b3a2", size =2, alpha=0.9, linetype=2)+
  theme_ipsum()+
  xlab("Round")+
  ylab("Coint Count")+
  ggtitle("Trend of Coin")
#3/ If player has smaller amount of money and bet the same amount of money from the beginning to the end
ggplot(data3, aes(x=data3$Round, y=data3$New.Coin.Count)) +
  geom_line(color ="#69b3a2", size =2, alpha=0.9, linetype=2)+
  theme_ipsum()+
  xlab("Round")+
  ylab("Coint Count")+
  ggtitle("Trend of Coin")
### The first graph shows that after 100th round, player ends up winning because he was lucky won 3 special award ( tripple 7, Free 10 rounds).
### However, the other graphs show that player ends up lose all of his money

####################CALCULATE THE CORRELATION BETWEEN NUMBER OF ROUNDS PLAY AND NEW COIN COUNTS#################
cor(data$Round, data$New.Coin.Count)
cor(data2$Round,data2$New.Coin.Amount)
cor(data3$Round,data3$New.Coin.Count)

####################CALCULATE THE POSIBILITY THAT PLAYER IS RUINED####################### 
#i:the initial amount of coin
#n:plays until it reach the amount Coin or Ruin
#p:probability of wining

slotmachine <-function(i,n,p){
  initial <-i
  while (initial>0 & initial < n) {
    run <-sample(c(-1000,1000),1,prob=c(1-p,p)) # Adjust x is the value we bet ==> -x amount we lose)
    initial <-initial + run
  }
  if(initial==0) return(1) else print(initial) # Return 1: Ruined.
}
i <-167000  #Modifying i: initial money 
n <-260000  #n: expected money (We want to see the possibility that player can reach n or ruin before that)
p <-1/2     #p: winning rate
trials <-1000
simlist <-replicate(trials,slotmachine(i,n,p))
mean(simlist)


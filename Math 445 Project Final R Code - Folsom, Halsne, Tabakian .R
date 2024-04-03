###Math 445 Final Project R Code - Eric Folsom, Eva Halsne, David Tabakian##
library(plyr)
library(dplyr)
library(readr)
library(car)
###In R studio set working directory to Source File Location###
data2021 <- read_csv("all2021.csv", ##initializing play by play data from
                     col_names = FALSE)#retrosheet for the 2021 season
fields <- read.csv("fields.csv")#Play by play data does not come wiith the headers we want
names(data2021) <- fields[, "Header"]#adding the column names that we want.
data2021$RUNS <- with(data2021, AWAY_SCORE_CT+HOME_SCORE_CT)
data2021$HALF.INNING <- with(data2021, #tells us what half inning the game is in
                             paste(GAME_ID, INN_CT, BAT_HOME_ID))#top or bottom
data2021$RUNS.SCORED <- with(data2021, (BAT_DEST_ID > 3) + #how many runs score as the result of the play
                               (RUN1_DEST_ID > 3) + (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3))
RUNS.SCORED.INNING <- aggregate(data2021$RUNS.SCORED,#count number of runs scored in an inning
                                list(HALF.INNING=data2021$HALF.INNING), sum)
RUNS.SCORED.START <- aggregate(data2021$RUNS.SCORED,#runs scored at start of an inning
                               list(HALF.INNING=data2021$HALF.INNING), "[", 1)
#maximum number of runs scored in an inning
MAX <- data.frame(HALF.INNING=RUNS.SCORED.START$HALF.INNING)
MAX$x <- RUNS.SCORED.INNING$x + RUNS.SCORED.START$x
data2021 <- merge(data2021, MAX)
N <- ncol(data2021)
names(data2021)[N] <- "MAX.RUNS"
data2021$RUNS.ROI <- with(data2021, MAX.RUNS - RUNS)
##This function returns the state of the game based on the position or runners
get.state <- function(runner1, runner2, runner3, outs){#and the number of outs
  runners <- paste(runner1, runner2, runner3, sep="")
  paste(runners, outs)
}


##getting runner positions from the data into a readable form for our function
RUNNER1 <- ifelse(as.character(data2021[ ,"BASE1_RUN_ID"]) == "", 0, 1)#get.state
RUNNER1[is.na(RUNNER1)] <- 0
RUNNER2 <- ifelse(as.character(data2021[ ,"BASE2_RUN_ID"]) == "", 0, 1)
RUNNER2[is.na(RUNNER2)] <- 0
RUNNER3 <- ifelse(as.character(data2021[ ,"BASE3_RUN_ID"]) == "", 0, 1)
RUNNER3[is.na(RUNNER3)] <- 0

##Obtaining the game states at the beginning and end of the plays
data2021$STATE <- get.state(RUNNER1, RUNNER2, RUNNER3, data2021$OUTS_CT)
NRUNNER1 <- with(data2021, as.numeric(RUN1_DEST_ID == 1 |
                                        BAT_DEST_ID == 1))
NRUNNER2 <- with(data2021, as.numeric(RUN1_DEST_ID == 2 |
                                        RUN2_DEST_ID == 2 | BAT_DEST_ID==2))
NRUNNER3 <- with(data2021, as.numeric(RUN1_DEST_ID == 3 |
                                        RUN2_DEST_ID == 3 | RUN3_DEST_ID == 3 | BAT_DEST_ID == 3))
NOUTS <- with(data2021, OUTS_CT + EVENT_OUTS_CT)
data2021$NEW.STATE <- get.state(NRUNNER1, NRUNNER2, NRUNNER3, NOUTS)

#creating a new data frame that only looks that the the ends of the innings
#this is so we can look at the states where there are 3 outs in an inning.
data2021 <- subset(data2021, (STATE != NEW.STATE) | (RUNS.SCORED > 0))
data.outs <- ddply(data2021, .(HALF.INNING), summarize,
                   Outs.Inning=sum(EVENT_OUTS_CT))
data2021 <- merge(data2021, data.outs)
data2021C <- subset(data2021, Outs.Inning == 3) 
data2021C <- subset(data2021, BAT_EVENT_FL == TRUE)
data2021C$NEW.STATE <- recode(data2021C$NEW.STATE,# recoding all the sates with 3 outs
"c('000 3', '100 3', '010 3', '001 3','110 3', '101 3', '011 3', '111 3')='3'")# to state 3

RUNS <- with(data2021C, aggregate(RUNS.ROI, list(STATE), mean))
RUNS$Outs <- substr(RUNS$Group, 5, 5)
RUNS <- RUNS[order(RUNS$Outs), ]
RUNS.2021 <-matrix(round(RUNS$x, 2), 8, 3)
dimnames(RUNS.2021)[[2]] <- c("0 outs", "1 out", "2 outs")
dimnames(RUNS.2021)[[1]] <- c("000", "001", "010", "011", "100", "101",
                              "110", "111")

#Constructing our transitional matrix
T.matrix <- with(data2021C, table(STATE, NEW.STATE))
P.matrix <- prop.table(T.matrix, 1)
P.matrix <- rbind(P.matrix, c(rep(0, 24), 1))
#our new transitional matrix with 25 game states

#Creating a function taht counts the number of runners and outs in a game state
count.runners.outs <- function(s)
  sum(as.numeric(strsplit(s,"")[[1]]), na.rm=TRUE)

#creating a runs matrix so we can see how many runs score as the result of the play
runners.outs <- sapply(dimnames(T.matrix)[[1]], count.runners.outs)[-25]
R <- outer(runners.outs + 1, runners.outs, FUN="-")
dimnames(R)[[1]] <- dimnames(T.matrix)[[1]][-25]
dimnames(R)[[2]] <- dimnames(T.matrix)[[1]][-25]
R <- cbind(R, rep(0, 24))#runs matrix

##This is our function that simulates the half inning. 
#inputs are the transitional matrix, runs matrix, and the starting state
#The function returns the number of runs scored in the simulated half inning
simulate.half.inning <- function(P, R, start=1){
  s <- start; path <- NULL; runs <- 0
  while(s < 25){
    s.new <- sample(1:25, 1, prob=P[s, ])
    path <- c(path, s.new)
    runs <- runs + R[s, s.new]
    s <- s.new
  }
  runs
}


##Simulating 10000 games of baseball by the average MLB team in 2021##
G<-10000
game.sim<-double(G)
for (i in 1:G){
  game.sim[i]<-sum(replicate(9,simulate.half.inning(T.matrix,R)))
}
mean(game.sim)
var(game.sim)
hist(game.sim)


#Obtaining transitional matricies for each MLB team based on 2021 data.
data2021C$HOME_TEAM_ID <- with(data2021C, substr(GAME_ID, 1, 3))
data2021C$BATTING.TEAM <- with(data2021C,
                               ifelse(BAT_HOME_ID == 0,
                                      as.character(AWAY_TEAM_ID),
                                      as.character(HOME_TEAM_ID)))
Team.T <- with(data2021C, table(BATTING.TEAM, STATE, NEW.STATE)) 
ANA.t<-Team.T['ANA', , ]
ARI.t<-Team.T['ARI', , ]
ATL.t<-Team.T['ATL', , ]
BAL.t<-Team.T['BAL', , ]
BOS.t<-Team.T['BOS', , ]
CHA.t<-Team.T['CHA', , ]
CHN.t<-Team.T['CHN', , ]
CIN.t<-Team.T['CIN', , ]
CLE.t<-Team.T['CLE', , ]
COL.t<-Team.T['COL', , ]
DET.t<-Team.T['DET', , ]
HOU.t<-Team.T['HOU', , ]
KCA.t<-Team.T['KCA', , ]
LAN.t<-Team.T['LAN', , ]
MIA.t<-Team.T['MIA', , ]
MIL.t<-Team.T['MIL', , ]
MIN.t<-Team.T['MIN', , ]
NYA.t<-Team.T['NYA', , ]
NYM.t<-Team.T['NYN', , ]
OAK.t<-Team.T['OAK', , ]
PHI.t<-Team.T['PHI', , ]
PIT.t<-Team.T['PIT', , ]
SDN.t<-Team.T['SDN', , ]
SEA.t<-Team.T['SEA', , ]
SFN.t<-Team.T['SFN', , ]
SLN.t<-Team.T['SLN', , ]
TBA.t<-Team.T['TBA', , ]
TEX.t<-Team.T['TEX', , ]
TOR.t<-Team.T['TOR', , ]
WAS.t<-Team.T['WAS', , ]

#########################################
######6/6/2022 game outcome prediction###
#########################################

B<-10000
par(mfrow=c(1,2))
#############
##ARI @ CIN##
#############

ARI.score<-double(B)
CIN.score<-double(B)
runs.ARI<-double(18)
runs.CIN<-double(18)
for(i in 1:B){
  
  for( j in 1:18){
    if(j %%2==0)
      runs.CIN[j]<-simulate.half.inning(CIN.t,R)
    
    else
      runs.ARI[j]<-simulate.half.inning(ARI.t,R)
  }
  ARI.score[i]<-sum(runs.ARI)
  CIN.score[i]<-sum(runs.CIN)
}
mean(CIN.score)
mean(ARI.score)

hist(ARI.score, main="Runs Scored Per Simulated Game - ARI", xlab="Runs")
hist(CIN.score, main="Runs Scored Per Simulated Game - CIN", xlab="Runs")

#Our simulation predicts that Cincinatti will beat Arizona with a score of 5-4
#Actual outcome: CIN 7 - ARI 0
#correct winner

#############
##TEX @ CLE##
#############

TEX.score<-double(B)
CLE.score<-double(B)
runs.TEX<-double(18)
runs.CLE<-double(18)
for(i in 1:B){
  
  for( j in 1:18){
    if(j %%2==0)
      runs.CLE[j]<-simulate.half.inning(CLE.t,R)
    
    else
      runs.TEX[j]<-simulate.half.inning(TEX.t,R)
  }
  TEX.score[i]<-sum(runs.TEX)
  CLE.score[i]<-sum(runs.CLE)
}
mean(CLE.score)
mean(TEX.score)

hist(CLE.score, main="Runs Scored Per Simulated Game - CLE", xlab="Runs")
hist(TEX.score, main="Runs Scored Per Simulated Game - TEX", xlab="Runs")

#Our simulation predicts that Cleveland will beat Texas with a score of 5-4
#Actual Outcome: Game Postponed
#N/A

#############
##TOR @ KCA##
#############

TOR.score<-double(B)
KCA.score<-double(B)
runs.TOR<-double(18)
runs.KCA<-double(18)
for(i in 1:B){
  
  for( j in 1:18){
    if(j %%2==0)
      runs.KCA[j]<-simulate.half.inning(KCA.t,R)
    
    else
      runs.TOR[j]<-simulate.half.inning(TOR.t,R)
  }
  TOR.score[i]<-sum(runs.TOR)
  KCA.score[i]<-sum(runs.KCA)
}

mean(KCA.score)
mean(TOR.score)

hist(KCA.score, main="Runs Scored Per Simulated Game - KCA", xlab="Runs")
hist(TOR.score, main="Runs Scored Per Simulated Game - TOR", xlab="Runs")

#Our simulation predicts that Toronto will beat Kansas City  by a score of 5-4
#Actual Outcome: TOR 8 - KCA 0
#correct winner

#############
##BOS @ ANA##
#############

BOS.score<-double(B)
ANA.score<-double(B)
runs.BOS<-double(18)
runs.ANA<-double(18)
for(i in 1:B){
  
  for( j in 1:18){
    if(j %%2==0)
      runs.ANA[j]<-simulate.half.inning(ANA.t,R)
    
    else
      runs.BOS[j]<-simulate.half.inning(BOS.t,R)
  }
  BOS.score[i]<-sum(runs.BOS)
  ANA.score[i]<-sum(runs.ANA)
}
mean(ANA.score)
mean(BOS.score)

hist(ANA.score, main="Runs Scored Per Simulated Game - ANA", xlab="Runs")
hist(BOS.score, main="Runs Scored Per Simulated Game - BOS", xlab="Runs")

#Our simulation predicts that Boston will beat Anaheim with a score of 5-4
#Actual outcome: BOS 1 - ANA 0
#correct winner

#############
##NYM @ SDN##
#############

NYM.score<-double(B)
SDN.score<-double(B)
runs.NYM<-double(18)
runs.SDN<-double(18)
for(i in 1:B){
  
  for( j in 1:18){
    if(j %%2==0)
      runs.SDN[j]<-simulate.half.inning(SDN.t,R)
    
    else
      runs.NYM[j]<-simulate.half.inning(NYM.t,R)
  }
  NYM.score[i]<-sum(runs.NYM)
  SDN.score[i]<-sum(runs.SDN)
}
mean(SDN.score)
mean(NYM.score)

hist(SDN.score, main="Runs Scored Per Simulated Game - SDN", xlab="Runs")
hist(NYM.score, main="Runs Scored Per Simulated Game - NYM", xlab="Runs")

#Our simulation predicts that San Diego will beat New York (N) with a score of 5-4
#Actual Outcome: NYM 11 - SDN 5
#incorrect

#############
##SEA @ HOU##
#############

SEA.score<-double(B)
HOU.score<-double(B)
runs.SEA<-double(18)
runs.HOU<-double(18)
for(i in 1:B){
  
  for( j in 1:18){
    if(j %%2==0)
      runs.HOU[j]<-simulate.half.inning(HOU.t,R)
    
    else
      runs.SEA[j]<-simulate.half.inning(SEA.t,R)
  }
  SEA.score[i]<-sum(runs.SEA)
  HOU.score[i]<-sum(runs.HOU)
}
mean(HOU.score)
mean(SEA.score)

hist(SEA.score, main = "Runs scored Per Simulated Game - SEA",xlab="Runs" )
hist(HOU.score, main="Runs Scored Per Simulated Game - HOU", xlab="Runs")
mean(SEA.score)
var(SEA.score)
quantile(SEA.score,0.025)
quantile(SEA.score,0.975)

#Our simulation predicts that Houston will beat Seattle with a score of 6-5
#Actual Results: SEA 7 - HOU 4 
#Incorrect

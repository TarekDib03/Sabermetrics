SS_2000 <- read.csv("Sabermetrics/2000_MLB_SS.csv", stringsAsFactors = F)
# Number of frecords in SS_2000
nrow(SS_2000)
# Order by SLG
SS_2000_ordered_SLG <- SS_2000[order(SS_2000$SLG),]
# Name of player with lowest SLG
SS_2000_ordered_SLG$Name[1]

# OPS
SS_2000$OPS <- SS_2000$OBP + SS_2000$SLG
# Subset of SS_2000 with OPS over 0.75
SS_0.75 <- subset(SS_2000, SS_2000$OPS > 0.75)
# Number of players with OPS over 0.75
nrow(SS_0.75)

''' Which shortstop with an OPS greater than .750 in 2000 created the most negative defense value 
(as measured by "Fld")?'''
SS_0.75$Name[which.min(SS_0.75$Fld)]

'''Now, using the SS_2000 data frame, first make a plot in ggplot2. Use the ggplot() function, 
passing in the name of the data frame, and also the aes function(). The two variables we will use 
are SB and Fld - pass them into aes() in that order, so that SB is the x variable and Fld is the y 
variable. Store your plot in a variable with a name of your choice. 

Then, add a geom_point onto your plot, with a color of your choice. Finally, add on a stat_smooth, 
using the lm method and the formula y~x.'''
library(ggplot2)
p <- ggplot(SS_2000, aes(SB, Fld))
p + geom_point(col="darkgreen") + stat_smooth(method = "lm", formula = y ~ x)

# Read the 2001 
SS_2001 <- read.csv("Sabermetrics/2001_MLB_SS.csv", stringsAsFactors = F)

''' Join the two dataframes: SS_2000 and SS_2001 on playerid using 1) the join function in the plyr 
library, 2) merge function in R base and 3) sqldf package'''
library(plyr)
SS_2000_2001 <- join(SS_2000, SS_2001, by = "playerid", type="inner")

# 2nd method: use merge function
SS_2000_2001_merge <- merge(SS_2000, SS_2001, by = "playerid")

# 3rd method: Use the sqldf library
library(sqldf)
SS_2000_2001_sqldf <- sqldf("select * from SS_2000 
                            join SS_2001 on SS_2000.playerid = SS_2001.playerid")

# Correlation between WRC+ in 2000 and WRC+ IN 2001
round(cor(SS_2000_2001_merge$wRC..x, SS_2000_2001_merge$wRC..y), 3)


# Take a subset of SS_2000 for OPS lower than 0.7
SS_0.7 <- subset(SS_2000, SS_2000$OPS < 0.7)
SS_0.7$Name[which.max(SS_0.7$WAR)]



#### Lahman Database
PitcherRegression <- read.csv("Sabermetrics/lahman_sandbox.csv", stringsAsFactors = F)
attach(PitcherRegression)
summary(PitcherRegression)

library(dplyr)

#Strikeout Rate Top 20 -> 2013 to 2014 (Smallest Regression to Mean)
head(KRate13,20)
boxplot(head(KRate13, 20), KRate14[(rank(-KRate13) <= 20)], col="tomato",
        main = "Regression to the Mean Example:\nTop 20 2013 KRate to 2014", 
        ylab = "KRate", ylim = c(0.10,.35))
summary(head(KRate13,20))
summary(KRate14[(rank(-KRate13) <= 20)])

#Strikeout Rate Bottom 20 -> 2013 to 2014
tail(KRate13,20)
boxplot(tail(KRate13, 20), KRate14[(rank(KRate13) <= 20)], col="tomato",
        main = "Regression to the Mean Example:\nBottom 20 2013 KRate to 2014", 
        ylab = "KRate", ylim = c(0.10,.35))
summary(tail(KRate13,20))
summary(KRate14[(rank(KRate13) <= 20)])

#Rearrange By ERA (Medium Regression to Mean)
PitcherRegression2 <- (arrange(PitcherRegression, ERA13))
attach(PitcherRegression2)

#ERA Top 20 -> 2013 to 2014

head(ERA13,20)
boxplot(head(ERA13, 20), ERA14[(rank(ERA13) <= 20)], col="tomato",
        main = "Regression to the Mean Example:\nTop 20 2013 ERA to 2014", 
        ylab = "ERA", ylim = c(1.5,6.5))
summary(head(ERA13,20))
summary(ERA14[(rank(ERA13) <= 20)])

#ERA Bottom 20 -> 2013 to 2014

tail(ERA13,20)
boxplot(tail(ERA13, 20), ERA14[(rank(-ERA13) <= 20)], col="tomato",
        main = "Regression to the Mean Example:\nBottom 20 2013 ERA to 2014", 
        ylab = "ERA", ylim = c(1.5,6.5))
summary(tail(ERA13,20))
summary(ERA14[(rank(-ERA13) <= 20)])

#Rearrange By BABIP (Most Regression to Mean)
PitcherRegression3 <- (arrange(PitcherRegression, BABIP13))
attach(PitcherRegression3)

#BABIP Top 20 -> 2013 to 2014

head(BABIP13,20)
boxplot(head(BABIP13, 20), BABIP14[(rank(BABIP13) <= 20)], col="tomato",
        main = "Regression to the Mean Example:\nTop 20 2013 BABIP to 2014", 
        ylab = "BABIP", ylim = c(.240,.360))
summary(head(BABIP13,20))
summary(BABIP14[(rank(BABIP13) <= 20)])

#BABIP Top 20 -> 2013 to 2014

tail(BABIP13,20)
boxplot(tail(BABIP13, 20), BABIP14[(rank(-BABIP13) <= 20)], col="tomato",
        main = "Regression to the Mean Example:\nBottom 20 2013 BABIP to 2014", 
        ylab = "BABIP", ylim = c(.240,.360))
summary(tail(BABIP13,20))
summary(BABIP14[(rank(-BABIP13) <= 20)])


'''Regression to the Mean does not forecast this pendulum type effect. Although we could have 
predicted Buchholz to regress from 2013 to 2014, we would have predicted regression towards the 
mean, not past the mean and towards the bottom of the league.'''

# SLG statistic
myFunction <- function(sin,doub,trip,homer,AB){
  return((sin+2*doub+3*trip+4*homer)/AB)}

'''We would like to write a function that takes in a vector of Home Runs and a threshold, and 
returns TRUE if at least 50% of the values in the vector return are greater than the threshold. 
For example, greater50(c(10,15,20),14) should return TRUE, while greater50(c(10,14,20),14) should 
return FALSE.'''

greater50 <- function(HRVector,threshold){
  count <- 0
  
  for(i in 1:length(HRVector)){
    count = count + ifelse(HRVector[i] > threshold, 1, 0)
    }
  fifty <- count/length(HRVector) >= .5
  return(fifty)
}


# Retrosheet database
retro <- read.csv("Sabermetrics/retrosheet_sandbox.csv", stringsAsFactors = F)
retro$weekend <- ifelse(retro$day == "Sat"|retro$day=="Sun", 1, 0)
# Fraction of games played on the weekend 
round(sum(retro$weekend)/nrow(retro), 3)

# Create a vector of which teams (home vs. visitor)win 
retro$home_win <- ifelse(retro$home_score > retro$visitor_score,1,0)

#create a function that returns W, G, and WP all time for a team
W_G_WP <- function(team,df){
  W <- 0
  G <- 0
  sub <- subset(df,home == team | visitor == team)
  for(i in 1:nrow(sub)){
    W <- W + ifelse(sub$home[i] == team, sub$home_win[i],
                    ifelse(sub$home_win[i] == 1,0,1))
    G <- G + 1
  }
  return(c(W,G,W/G))
}

# Example
W_G_WP("NYA",retro)



weekWP <- function(team,df){
  sub <- subset(df,home == team | visitor == team)
  weekend <- subset(sub,weekend == 1)
  week <- subset(sub,weekend == 0)
  weekWP <- W_G_WP(team,week)[3]
  weekendWP <- W_G_WP(team,weekend)[3]
  return(c(team, weekWP, weekendWP))
}

# 3 letter code of the team with the highest weekend winning percentage from 2000 - 2009?
for(i in 1:length(unique(retro$home))){ 
  print(weekWP(unique(retro$home)[i],retro))
}

# Sort retro by descending attendance
library(dplyr)
retro_attend_sorted <- arrange(retro, desc(attendance))
retro_attend_sorted$home[1]

# Most team with doubles
retro$home[which.max(retro$home_2b)]

fitlm <- lm(home_score ~ visitor_errors, data=retro)
summary(fitlm)
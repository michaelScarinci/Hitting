#Leaps is the package used for regsubfit() function. It gives us a summary of the best 
#combination of variables to use.
library(leaps)
#stats is the primary data file we will be using, the .csv was generated from fangraphs.com
stats <- read.csv("/Volumes/DAILY/Baseball/Hitting/2015.csv")

#Using the stats file we create the statistic runs accounted and then runs accounted per plate appearance
runsAccounted <- stats$R + stats$RBI - stats$HR
runsAccountedPA <- runsAccounted/stats$PA

#Easier access to the stats is created here through variables
wOBA <- stats$wOBA
AVG <- stats$AVG
ISO <- stats$ISO
OBP <- stats$OBP
WAR <- stats$WAR
wRC <- stats$wRC.

#Here runs accounted per plate appearance is attached the the stats data frame for reference in plots and analysis
stats$runsAccountedPA <- runsAccountedPA

#Each stat was plotted against runs accounted per plate appearance in exploratory analysis to 
#gain a better understanding of their relationship.
plot(wOBA,runsAccountedPA,xlab = "wOBA", ylab = "RAPA", main = "wOBA against RAPA")
plot(AVG,runsAccountedPA, xlab = "Batting Average", ylab = "RAPA",main = "AVG vs. RAPA")
plot(ISO,runsAccountedPA)
plot(OBP,runsAccountedPA)
plot(WAR,runsAccountedPA)
plot(wRC,runsAccountedPA)

#A model was created with each statistic to learn which had the best R^2 value. It was found that wOBA 
#had the best while Average had the worst R^2 value. The summary function gives us the neccessary information.
avgModel <- lm(runsAccountedPA~AVG)
summary(avgModel)
wobaModel <- lm(runsAccountedPA~wOBA)
summary(wobaModel)
isoModel <- lm(runsAccountedPA ~ ISO)
summary(isoModel)
obpModel <- lm(runsAccountedPA ~ OBP)
summary(obpModel)
warModel <- lm(runsAccountedPA ~ WAR)
summary(warModel)
wrcModel <- lm(runsAccountedPA ~ wRC)
summary(wrcModel)

#frame is a data frame which has only the stats being used in the model, this is used for the regsubfit function.
#The function regsubsets will give the best combination of statistics for whatever amount of variables we want to use in a future model.
frame <- data.frame(wOBA,AVG,ISO,OBP,WAR,wRC,runsAccountedPA)
regfit.full = regsubsets(runsAccountedPA~.,frame)
summary(regfit.full)

#These will give the rsquared and adjusted rsquared values for each amount of variables, from 1 to 6 variables used in a future model. 
#The variables used for each amount of variable are the ones with * in the summary(refit.full)
reg.summary$rsq
reg.summary$adjr2


#This will create a line plot with the rsquared values in green and the adjusted rsquared values in red
#This is how I chose to use five variables in the ideal model, as the max adjusted rsquared occured at 5 variables.
plot(reg.summary$rsq, type = 'l',xlab = "Number of Variables",ylab = "Value",
     main = "R Squared versus Adjusted R Squared",col = 'green')
lines(reg.summary$adjr2, type = 'l',col='red')
legend('topleft',c("R^2","Adj R^2"), col = c("green","red"),lty = 1)


#This plot was exported to microsoft paint to circle the 3 undervalues players. It gave a good visual of the
#relationship between wOBA and RAPA
plot(wOBA,runsAccountedPA,xlab = "wOBA",ylab = "Runs Accounted per PA",
     main = "wOBA and Runs Accounted per PA")
abline(wobaModel)


#This plotted the rsquared values of each stat with RAPA
values <- c(.1839,.595,.2993,.4233,.478,.3544)
rsquared <- c("Average","wOBA","OBP","ISO","wRC","WAR")
barplot(values,names = rsquared, xlab = "Stat",ylab = "R squared value", main = "R sqaured values of wOBA with RAPA",col = "blue")

#Here I used subsets to select the 3 value players as shown in the wOBA versus RAPA plot
valuePlayers <- subset(stats,wOBA > .380 & runsAccountedPA <.22)
lowerValue <- subset(stats, wOBA > .330 & runsAccountedPA < .17)
valuePlayers <- rbind(valuePlayers,lowerValue)
View(valuePlayers)


#This code creates a bar plot of the team runs for each value player and then at the end a bar plot of the average runs per team
# for the 2015 season. 
runs <- c(613,624,638,688)
names <- c("Reds","Mariners","Dodgers","League Average")
barplot(runs,names = names, xlab = "Team",ylab = "Runs Scored", main = "Team Runs for 2015",col = "blue")
text(.7,475,"613",col = 'white')
text(1.9,475,"624",col = 'white')
text(3.1,475,"638",col = 'white')
text(4.3,475,"688",col = 'white')

#Using the information of adjusted R squared and regsubfit() function the ideal model was created using the five statistics below.
bestModel <- lm(runsAccountedPA~wOBA+AVG+OBP+WAR+wRC)
summary(bestModel)

# The position in value player for each player was Votto= 5, Cruz = 22 and Pederson = 67. Using predict.lm I was able to find the confidence and prediction 
#intervals for each player as well as their fit values.
predict.lm(bestModel,interval = "predict")
predict.lm(bestModel,interval = "conf")

#Using the best model I was able to find fit RAPA and used that to multiply by the number of plate appearances to get a players fit runs accounted
# for the 2015 season. A data frame was then created using the actual runs accounted for 2015 and then the difference to create a stacked bar plot for
# each player.
Votto <- c(146,27)
Cruz <- c(139,23)
Pederson <- c(95,20)
projectedRuns <- data.frame(Votto,Cruz,Pederson)
barplot(as.matrix(projectedRuns),main = "Actual Runs Accounted versus Fit Runs Accounted",
        xlab = "Player",ylab = "Runs Accounted",col = c("blue","red"),legend.text = c("Actual","Fit"))

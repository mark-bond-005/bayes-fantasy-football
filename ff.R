# Try to distribute tau as gamma (0.00001, 0.00001)


nWeeks <- 13
#setwd("C:/Users/Mark Bond/Documents/Fantasy Football")
# Read the data in, make some obvious modifications
ff <- read.csv("ff.csv", stringsAsFactors = FALSE)
ff$actualWin <- ifelse(ff$actualOne > ff$actualTwo, 1, 2)
ff$projWin <- ifelse(ff$projOne > ff$projTwo, 1, 2)
ff$projDif <- ff$projOne - ff$projTwo

# Annoyingly, the data are side-by-side. Let's make a long dataset.
ID <- c(ff$teamOne, ff$teamTwo)
team <- as.data.frame(ID)
team$week <- c(ff$week, ff$week)
team$proj <- c(ff$projOne, ff$projTwo)
team$zProj <- (team$proj - mean(team$proj))/sd(team$proj)
team$actual <- c(ff$actualOne, ff$actualTwo)
team <- team[order(team$week, team$ID), ]
teamNames <- team[ 1:14, "ID"]

# We now fit a moderately terrible linear model.
awfulLm <- lm(team$actual ~ team$proj)
plot(team$proj, team$actual, xlab="Projected Team Scores", ylab = "Actual Team Scores")
curve(coef(awfulLm)[1] +coef(awfulLm)[2]*x, add = TRUE)

library(reshape)

# We create two separate long datasets for this, the "projected" dataset and the "actual" dataset
eZ <- team[ , 1:3]
eZ$variable <- paste( "proj", eZ$week, sep = "")
eZ$value <- eZ$proj
wide <- cast(eZ, ID ~ variable)

eZ2 <- team[ , -3]
eZ2$variable <- paste( "actual", eZ2$week, sep = "")
eZ2$value <- eZ2$actual
wide2 <- cast(eZ2, ID ~ variable)
#Now we merge these two datasets
ffWide <- merge(wide, wide2, by = "ID")

# Here is a frequentist HLM.
library(arm)
freqLm <- lmer(actual ~ zProj + (1 | ID), data = team)
summary(freqLm)
skill <- ranef(freqLm)

# Finally, we begin the Bayesian analysis.
library(R2WinBUGS)
teamId <- as.numeric(team$ID)
actual <- team$actual
zProj <- team$zProj
J <- length(unique(ID))
n <- length(team$actual)
# This data list calls the variables named in the previous 5 lines.
footData <- list("n", "J", "actual", "teamId", "zProj")
# Initial values
footInits <- function() {
  list(a = rnorm(J), b = rnorm(1), muA = rnorm(1),
       sigmaY = runif(1), sigmaA = runif(1))
  }
#Names the parameters we're interested in.
footParm <-c("a", "b", "muA", "sigmaY", "sigmaA", "varPctA", "varPctY")

# In addition to the lists and vectors defined above, we use "football.bug", a file we wrote with notepad. Go read it!
footBayes <- bugs(footData, footInits, footParm, "football3.bug", n.chains = 3, n.iter=10000, n.burnin=6000)
attach.bugs(footBayes)
aMultilvl <- rep(NA, J)
for(j in 1:J){ aMultilvl[j] <- median(a[ , j])}
bMultilvl <- median(b)

team$ability <- rep(aMultilvl, 6)

for(i in 1:length(teamNames)) {
  ff[ ff$teamOne == teamNames[i], "abilityOne"] <- aMultilvl[i]
  ff[ ff$teamTwo == teamNames[i], "abilityTwo"] <- aMultilvl[i]
}

ff$actualWin <- -1*ff$actualWin + 2 # Now a One indicates that teamOne wins, and a 0 indicates that teamTwo wins.
ff$abilDif <- ff$abilityOne - ff$abilityTwo
freqLogLm <- glm( actualWin ~ abilDif, data = ff, family = binomial)
plot(ff$actualWin ~ ff$abilDif, xlab = "Projected point spread", ylab = "Whether team one wins")
lines(ff$abilDif, freqLogLm$fitted, type="l", col="red")

qplot(team$actual, team$proj)
#fully Bayesian fixed-slope varying-intercept hierarchical linear model

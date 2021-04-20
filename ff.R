library(reshape2)

# Try to distribute tau as gamma (0.00001, 0.00001)

setwd("C:/Users/mab7986/Desktop/bayes_fantasy_football-master")
nWeeks <- 13
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

# Finally, we begin the Bayesian analysis.
library(R2WinBUGS)

teamId <- as.numeric(as.factor(team$ID))
actual <- team$actual
zProj <- team$zProj
J <- length(unique(ID))
n <- length(team$actual)
# This data list calls the variables named in the previous 5 lines.
footData <- list("n", "actual", "zProj")
footInits <- function(){
  list(intercept = rnorm(1), b = rnorm(1),
       sigmaY = runif(1))
}
footParm <- c("intercept", "b", "sigmaY")
footBayes_simple <- bugs(footData, footInits, footParm, "football.bug", n.chains = 3, n.iter=500, n.burnin=100, debug=TRUE)
print(footBayes_simple)

# Here is a frequentist HLM.
library(arm)
freqLm <- lmer(actual ~ zProj + (1 | ID), data = team)
summary(freqLm)
skill <- ranef(freqLm)

footData2 <- list("n", "J", "actual", "teamId", "zProj")
# Initial values
footInits2 <- function() {
  list(a = rnorm(J), b = rnorm(1), muA = rnorm(1),
       sigmaY = runif(1), sigmaA = runif(1))
  }
#Names the parameters we're interested in.
footParm2 <-c("a", "b", "muA", "sigmaY", "sigmaA", "varPctA", "varPctY")

# In addition to the lists and vectors defined above, we use "football.bug", a file we wrote with notepad. Go read it!
footBayes <- bugs(footData2, footInits2, footParm2, "football2.bug", n.chains = 3, n.iter=500, n.burnin=100, debug=TRUE)
print(footBayes)
attach.bugs(footBayes)
aMultilvl <- rep(NA, J)
for(j in 1:J){ aMultilvl[j] <- median(a[ , j])}
bMultilvl <- median(b)

footData3 <- footData2
footInits3 <- function() {
  list(a = rnorm(J), b = rnorm(J), muA = rnorm(1),
       sigmaY = runif(1), sigmaA = runif(1), muB =rnorm(1), sigmaB = runif(1))
  }
#Names the parameters we're interested in.
footParm3 <-c("a", "b", "muA", "muB", "sigmaY", "sigmaA", "varPctA", "varPctY")
footBayes_complex <- bugs(footData3, footInits3, footParm3, "football3.bug", n.chains = 3, n.iter=2000, n.burnin=500, debug=TRUE)
print(footBayes_complex)
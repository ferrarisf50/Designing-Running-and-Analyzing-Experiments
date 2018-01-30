###
## Scenario:   Comparing task completion times in design tools
##
## Statistics: ANOVA assumptions, data transformations, one-way ANOVA, 
##             post hoc comparisons, nonparametric tests
###

## Independent-samples t-test

# read in a data file with task completion times (min) from 2 design tools
designtime = read.csv("designtime.csv")
View(designtime)
designtime$Subject = factor(designtime$Subject) # convert to nominal factor
summary(designtime)

# view descriptive statistics by software
library(plyr)
ddply(designtime, ~ Tool, function(data) summary(data$Time))
ddply(designtime, ~ Tool, summarise, Time.mean=mean(Time), Time.sd=sd(Time))

# graph histograms and a boxplot
hist(designtime[designtime$Tool == "Illustrator",]$Time) # histogram
hist(designtime[designtime$Tool == "InDesign",]$Time) # histogram
plot(Time ~ Tool, data=designtime) # boxplot

# independent-samples t-test
t.test(Time ~ Tool, data=designtime, var.equal=TRUE)


## Testing ANOVA assumptions

# Shapiro-Wilk normality test on response
shapiro.test(designtime[designtime$Tool == "Illustrator",]$Time)
shapiro.test(designtime[designtime$Tool == "InDesign",]$Time)

# residuals
m = aov(Time ~ Tool, data=designtime) # fit model
shapiro.test(residuals(m)) # test residuals
qqnorm(residuals(m)); qqline(residuals(m)) # plot residuals

# tests for homoscedasticity (homogeneity of variance)
library(car)
leveneTest(Time ~ Tool, data=designtime, center=median) # Brown-Forsythe test

# Kolmogorov-Smirnov test for log-normality
# fit the distribution to a lognormal to estimate fit parameters
# then supply those to a K-S test with the lognormal distribution fn (see ?plnorm)
# see ?Distributions for many other named probability distributions
library(MASS)
fit = fitdistr(designtime[designtime$Tool == "Illustrator",]$Time, "lognormal")$estimate
ks.test(designtime[designtime$Tool == "Illustrator",]$Time, "plnorm", meanlog=fit[1], sdlog=fit[2], exact=TRUE)
fit = fitdistr(designtime[designtime$Tool == "InDesign",]$Time, "lognormal")$estimate
ks.test(designtime[designtime$Tool == "InDesign",]$Time, "plnorm", meanlog=fit[1], sdlog=fit[2], exact=TRUE)

## Data transformation

# create a new column in designtime defined as log(Time)
designtime$logTime = log(designtime$Time) # log transform
View(designtime) # verify
summary(designtime$logTime)

# explore for intuition-building
hist(designtime[designtime$Tool == "Illustrator",]$logTime) # histogram
hist(designtime[designtime$Tool == "InDesign",]$logTime) # histogram
plot(logTime ~ Tool, data=designtime) # boxplot


# view descriptive statistics by software
library(plyr)
ddply(designtime, ~ Tool, function(data) summary(data$logTime))
ddply(designtime, ~ Tool, summarise, logTime.mean=mean(logTime), logTime.sd=sd(logTime))

# Welch t-test for unequal variances handles
# the violation of homoscedasticity. but not
# the violation of normality.
t.test(logTime ~ Tool, data=designtime, var.equal=FALSE) # Welch t-test

## Nonparametric equivalent of independent-samples t-test

# Mann-Whitney U test
library(coin)
wilcox_test(Time ~ Tool, data=designtime, distribution="exact")

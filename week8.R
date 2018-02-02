###
##
## Statistics: Generalized Linear Models (GLM) for nominal logistic regression, 
##             ordinal logistic regression, and Poisson regression
###

# Generalized Linear Models (GLM) extend Linear Models (LM) for studies 
# with between-Ss factors to acommodate nominal (incl. binomial) or ordinal 
# responses, or with non-normal response distributions (e.g., Poisson, 
# exponential, gamma). All GLMs have a distribution and a link fn relating 
# their factors to their response. The GLM generalizes the LM, which is a 
# GLM with a normal distribution and "identity" link fn. See, e.g., 
# http://en.wikipedia.org/wiki/Generalized_linear_model

## GLM 1: Nominal logistic regression for preference responses
## -----  Multinomial distribution w/ logit link fn

# read  data showing preferences by sex
deviceprefssex = read.csv("deviceprefssex.csv") 
View(deviceprefssex)
deviceprefssex$Subject = factor(deviceprefssex$Subject) # convert to nominal factor
deviceprefssex$Disability = factor(deviceprefssex$Disability) # convert to nominal factor
summary(deviceprefssex)
plot(deviceprefssex[deviceprefssex$Sex == "M",]$Pref)
plot(deviceprefssex[deviceprefssex$Sex == "F",]$Pref)
plot(deviceprefssex[deviceprefssex$Pref == "trackball",]$Sex)
plot(deviceprefssex[deviceprefssex$Pref == "touchpad",]$Sex)

# binomial regression

library(car) # for Anova
contrasts(deviceprefssex$Sex) <- "contr.sum"
contrasts(deviceprefssex$Disability) <- "contr.sum"
# family parameter identifies both distribution and link fn
m = glm(Pref ~ Disability * Sex, data=deviceprefssex, family=binomial)
Anova(m, type=3)


# analyze Pref by Sex with multinomial logistic regression,
# also sometimes called nominal logistic regression
library(nnet) # for multinom
library(car) # for Anova
# set sum-to-zero contrasts for the Anova call
contrasts(deviceprefssex$Sex) <- "contr.sum"
contrasts(deviceprefssex$Disability) <- "contr.sum"
m = multinom(Pref ~ Disability * Sex, data=deviceprefssex) # multinomial logistic
Anova(m, type=3) # note: not "anova" from stats pkg
# note: if Pref had only had two response categories, we might use 
# binomial regression, which uses the same syntax as Poisson regression 
# below, but with family=binomial.

# recall our testing from before to see which preferences by males were
# significantly different from chance (answer: really liked C).
ma = binom.test(sum(deviceprefssex[deviceprefssex$Disability == "0" & deviceprefssex$Sex == "M",]$Pref == "trackball"), nrow(deviceprefssex[deviceprefssex$Disability == "0" & deviceprefssex$Sex == "M",]), p=1/2)
mb = binom.test(sum(deviceprefssex[deviceprefssex$Disability == "1" & deviceprefssex$Sex == "M",]$Pref == "touchpad"), nrow(deviceprefssex[deviceprefssex$Disability == "1" & deviceprefssex$Sex == "M",]), p=1/2)
# and for females, their preferences differed significantly from 
# chance for a different choice (answer: really disliked A).
fa = binom.test(sum(deviceprefssex[deviceprefssex$Disability == "0" & deviceprefssex$Sex == "F",]$Pref == "trackball"), nrow(deviceprefssex[deviceprefssex$Disability == "0" & deviceprefssex$Sex == "F",]), p=1/2)
fb = binom.test(sum(deviceprefssex[deviceprefssex$Disability == "1" & deviceprefssex$Sex == "F",]$Pref == "touchpad"), nrow(deviceprefssex[deviceprefssex$Disability == "1" & deviceprefssex$Sex == "F",]), p=1/2)
p.adjust(c(ma$p.value, mb$p.value, fa$p.value, fb$p.value), method="holm") # correct for multiple comparisons


#----------------

hwreco = read.csv("hwreco.csv") 
View(hwreco)
hwreco$Subject = factor(hwreco$Subject) # convert to nominal factor
summary(hwreco)
with(hwreco, interaction.plot(Recognizer, Hand, Errors, ylim=c(0, max(hwreco$Errors)))) # interaction?

# re-verify that these data are Poisson-distributed
library(fitdistrplus)
fit = fitdist(hwreco[hwreco$Recognizer == "A",]$Errors, "pois", discrete=TRUE)
gofstat(fit) # goodness-of-fit test
fit = fitdist(hwreco[hwreco$Recognizer == "B",]$Errors, "pois", discrete=TRUE)
gofstat(fit) # goodness-of-fit test
fit = fitdist(hwreco[hwreco$Recognizer == "C",]$Errors, "pois", discrete=TRUE)
gofstat(fit) # goodness-of-fit test

# analyze using Poisson regression
# set sum-to-zero contrasts for the Anova call
contrasts(hwreco$Recognizer) <- "contr.sum"
contrasts(hwreco$Hand) <- "contr.sum"
# family parameter identifies both distribution and link fn
m = glm(Errors ~ Recognizer * Hand, data=hwreco, family=poisson)
Anova(m, type=3)
qqnorm(residuals(m)); qqline(residuals(m)) # s'ok! Poisson regression makes no normality assumption


library(multcomp)
library(lsmeans)

summary(glht(m, lsm(pairwise ~ Recognizer * Hand)), test=adjusted(type="none"))
p.adjust(c(0.001925, 0.095955, 0.243171), method="holm")


#-------

df = read.csv("bookflights.csv")
View(df)
df$Subject = factor(df$Subject)
df$International = factor(df$International)
df$Ease = ordered(df$Ease)
summary(df)
df$Subject

with(df, interaction.plot(Website, International, as.numeric(Ease), ylim=c(0, max(df$Ease))))

library(MASS)
library(car)
contrasts(df$Website) <- "contr.sum"
contrasts(df$International) <- "contr.sum"
m = polr(Ease ~ Website * International, data=df, Hess=TRUE)
Anova(m, type=3)


summary(glht(m, lsm(pairwise ~ Website * International)), test=adjusted(type="none")) # this does not work!

summary(as.glht(pairs(lsmeans(m, pairwise ~ Website * International))), test=adjusted(type="none")) # this works!
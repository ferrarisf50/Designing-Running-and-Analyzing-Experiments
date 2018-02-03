###
## Scenario:   websearch3.csv  describes a study of the number of searches 
##             people did with various search engines to successfully find 
##             100 facts on the web.
##
## Statistics: Linear Mixed Models (LMM) and Generalized Linear Mixed 
##             Models (GLMM) extending LMs and GLMs, respectively, with 
##             random effects to handle within-Ss factors.
###

# Linear Mixed Models (LMM) do everything Linear Models (LM) do but
# can have both fixed and random effects. Random effects allow us
# to handle within-Ss factors by modeling "Subject" as a random
# effect. Generalized Linear Mixed Models (GLMM) do everything 
# Generalized Linear Models (GLM) do, but also can have both fixed 
# and random effects. LMMs and GLMMs are called "mixed effects 
# models." See https://en.wikipedia.org/wiki/Generalized_linear_mixed_model

## Linear Mixed Model (LMM) on Searches

websearch = read.csv("websearch3.csv")
View(websearch)
websearch$Subject = factor(websearch$Subject) # convert to nominal factor
websearch$Order = factor(websearch$Order) # convert to nominal factor
summary(websearch)

# explore the Searches data
library(plyr)
ddply(websearch, ~ Engine, function(data) summary(data$Searches))
ddply(websearch, ~ Engine, summarise, Searches.mean=mean(Searches), Searches.sd=sd(Searches))

# histograms, boxplot  
hist(websearch[websearch$Engine == "Google",]$Searches)
hist(websearch[websearch$Engine == "Yahoo",]$Searches)
hist(websearch[websearch$Engine == "Bing" ,]$Searches)
boxplot(Searches ~ Engine, data=websearch, xlab="Engine", ylab="Searches") # boxplots

# libraries for LMMs we'll use on Searches
library(lme4) # for lmer
library(lmerTest)
library(car) # for Anova

# set sum-to-zero contrasts for the Anova calls
contrasts(websearch$Engine) <- "contr.sum"
contrasts(websearch$Order) <- "contr.sum"

# Linear mixed model (LMM) on Searches by Engine
m = lmer(Searches ~ Engine + (1|Subject), data=websearch)
Anova(m, type=3, test.statistic="F")

# perform post hoc pairwise comparisons
library(multcomp) # for glht
summary(glht(m, mcp(Engine="Tukey")), test=adjusted(type="holm"))

#========

## Scenario:   socialvalue.csv  describes a study of the people viewing 
##             a positive or negative film clip before going onto social media 
##             and then judging the value of the first 100 posts they see there

## Linear Mixed Model (LMM) 

socialvalue = read.csv("socialvalue.csv")
View(socialvalue)
socialvalue$Subject = factor(socialvalue$Subject) # convert to nominal factor
socialvalue$SocialOrder = factor(socialvalue$SocialOrder) # convert to nominal factor
socialvalue$ClipOrder = factor(socialvalue$ClipOrder) # convert to nominal factor
summary(socialvalue)

# explore the data
library(plyr)
ddply(socialvalue, ~ Social * Clip, function(data) summary(data$Valued))
ddply(socialvalue, ~ Social * Clip, summarise, Valued.mean=mean(Valued), Valued.sd=sd(Valued))


# libraries for LMMs we'll use on Searches
library(lme4) # for lmer
library(lmerTest)
library(car) # for Anova

# set sum-to-zero contrasts for the Anova calls
contrasts(socialvalue$Social) <- "contr.sum"
contrasts(socialvalue$Clip) <- "contr.sum"

# Linear mixed model (LMM) on Searches by Engine
m = lmer(Valued ~ Social * Clip + (1|Subject), data=socialvalue)
Anova(m, type=3, test.statistic="F")

# two planned pairwise comparisons of how the film clips may 
# have influenced judgments about the value of social media. 
# The first question is whether on Facebook, the number of valued posts 
# was different after people saw a positive film clip versus a negative 
# film clip. The second question is whether on Twitter, the number of 
# valued posts was different after people saw a positive film clip versus 
# a negative film clip.

library(multcomp) # for glht
library(lsmeans) # for lsm
summary(glht(m, lsm(pairwise ~ Social * Clip)), test=adjusted(type="none"))
p.adjust(c(0.000225, 0.594397), method="holm")

#_________________________________________________

# teaser.csv describes a survey in which respondents recruited 
# online saw five different teaser trailers for upcoming movies 
# of different genres. Respondents simply indicated whether they 
# liked each teaser or not

teaser2 = read.csv("teaser.csv")
View(teaser2)
teaser2$Subject = factor(teaser2$Subject) # convert to nominal factor
teaser2$Order = factor(teaser2$Order) # convert to nominal factor
#teaser2$Liked = factor(teaser2$Liked) # convert to nominal factor
summary(teaser2)

# histograms, boxplot  
hist(teaser2[teaser2$Liked == "0",]$Teaser)
hist(teaser2[teaser2$Liked == "1",]$Teaser)

boxplot(Liked ~ Teaser, data=teaser2, xlab="Liked", ylab="Teaser") # boxplots

ddply(teaser2, ~ Teaser, function(data) summary(data$Liked))



## Generalized Linear Mixed Model (GLMM) on Liked

# libraries for GLMMs  we'll use on Liked
library(lme4) # for glmer
library(lmerTest)
library(car) # for Anova

# set sum-to-zero contrasts for the Anova call
contrasts(teaser2$Teaser) <- "contr.sum"
contrasts(teaser2$Order) <- "contr.sum"
contrasts(teaser2$Liked) <- "contr.sum"

m = glmer(Liked ~ Order + (1|Subject), data=teaser2, family=binomial)
Anova(m, type=3)

# main GLMM test on Liked
m = glmer(Liked ~ Teaser  + (1|Subject), data=teaser2, family=binomial, nAGQ=0)
Anova(m, type=3)

# perform post hoc pairwise comparisons
library(multcomp) # for glht
summary(glht(m, mcp(Teaser="Tukey")), test=adjusted(type="holm"))





#_________________________________________________
# vocab.csv describes a study in which 50 recent posts by men and women 
# on social media were analyzed for how many unique words they used, 
# i.e., the size of their operational vocabulary on social media. 
# The research question is how men's and women's vocabulary may differ 
# on each of three social media websites. 

vocab = read.csv("vocab.csv")
View(vocab)
vocab$Subject = factor(vocab$Subject) # convert to nominal factor
vocab$Order = factor(vocab$Order) # convert to nominal factor
#vocab$Liked = factor(vocab$Liked) # convert to nominal factor
summary(vocab)


# interaction plot
with(vocab, interaction.plot(Social, Sex, Vocab, ylim=c(0, max(vocab$Vocab)))) # interaction?

# Kolmogorov-Smirnov goodness-of-fit tests on Vocab for each 
# level of Social using exponential distributions
library(MASS)

fit = fitdistr(vocab[vocab$Social == "Facebook",]$Vocab, "exponential")$estimate
ks.test(vocab[vocab$Social == "Facebook",]$Vocab, "pexp", rate=fit[1], exact=TRUE)

fit = fitdistr(vocab[vocab$Social == "Twitter",]$Vocab, "exponential")$estimate
ks.test(vocab[vocab$Social == "Twitter",]$Vocab, "pexp", rate=fit[1], exact=TRUE)

fit = fitdistr(vocab[vocab$Social == "Gplus",]$Vocab, "exponential")$estimate
ks.test(vocab[vocab$Social == "Gplus",]$Vocab, "pexp", rate=fit[1], exact=TRUE)


#GLMM - test of order effects on Vocab to ensure counterbalancing worked

library(lme4)
library(lmerTest)
library(car)

contrasts(vocab$Sex) <- "contr.sum"
contrasts(vocab$Order) <- "contr.sum"

m = glmer(Vocab ~ Sex * Order + (1|Subject), data=vocab, family=Gamma(link="log"))
Anova(m, type=3)

#GLMM - test of Vocab by Sex and Social

m = glmer(Vocab ~ Sex * Social + (1|Subject), data=vocab, family=Gamma(link="log"))
Anova(m, type=3)

# post hoc pairwise comparisons among levels of Social adjusted Holm proc
library(multcomp)
summary(glht(m, mcp(Social="Tukey")), test=adjusted(type="holm"))
summary(glht(m, lsm(pairwise ~ Sex * Social)), test=adjusted(type="holm"))

#--------------------------------------------


df = read.csv("websearch3.csv")
View(df)

df$Subject = factor(df$Subject)
df$Effort = ordered(df$Effort)

summary(df)
df$Effort


# assuming df contains websearch3.csv
# assuming Subject has been recoded as nominal
# assuming Effort has been recoded as ordinal
library(ordinal)
library(RVAideMemoire)
df2 <- as.data.frame(df) # quirk
contrasts(df2$Engine) <- "contr.sum"
m = clmm(Effort ~ Engine + (1|Subject), data=df2)
Anova(m, type=3) # type ignored

# assuming code continuing from Q24
plot(as.numeric(Effort) ~ Engine, data=df2)
library(lme4)
library(multcomp)
m = lmer(as.numeric(Effort) ~ Engine + (1|Subject), data=df2)
summary(glht(m, mcp(Engine="Tukey")), test=adjusted(type="holm"))
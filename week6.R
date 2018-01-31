###
## Scenario:   Finding 100 distinct facts on the web using different search engines
##
## Statistics: Paired-samples t-test, one-way repeated measures ANOVA, 
##             Mauchly's test of sphericity, post hoc comparisons, nonparametrics
###

## Paired-samples t-test

# read in a data file with times (sec) to find a set of contacts
gglbng = read.csv("websearch2.csv")
View(gglbng)
gglbng$Subject
gglbng$Subject = factor(gglbng$Subject) # convert to a nominal factor
gglbng$Order = factor(gglbng$Order) # convert to a nominal factor
summary(gglbng)

# view descriptive statistics by Engine
library(plyr)
ddply(gglbng, ~ Engine, function(data) summary(data$Searches))
ddply(gglbng, ~ Engine, summarise, Searches.mean=mean(Searches), Searches.sd=sd(Searches))

# explore the Searches response
hist(gglbng[gglbng$Engine == "Google",]$Searches) # histogram
hist(gglbng[gglbng$Engine == "Bing",]$Searches) # histogram
plot(Searches ~ Engine, data=gglbng) # boxplot
plot(Effort ~ Engine, data=gglbng) # boxplot

# test for an order effect
library(reshape2)	
# for a paired-samples t-test we must use a wide-format table; most
# R fns do not require a wide-format table, but the dcast function
# offers a quick way to translate long-format into wide-format when
# we need it.
gglbng.wide.order = dcast(gglbng, Subject ~ Order, value.var="Searches") # go wide
View(gglbng.wide.order) # verify
t.test(gglbng.wide.order$"1", gglbng.wide.order$"2", paired=TRUE, var.equal=TRUE)

# paired-samples t-test
gglbng.wide.tech = dcast(gglbng, Subject ~ Engine, value.var="Searches") # go wide
View(gglbng.wide.tech)
t.test(gglbng.wide.tech$Google, gglbng.wide.tech$Bing, paired=TRUE, var.equal=TRUE)
plot(Searches ~ Engine, data=gglbng) # confirm

# Wilcoxon signed-rank test on Effort
library(coin)
wilcoxsign_test(Effort ~ Engine | Subject, data=gglbng, distribution="exact")
# note: the term afer the "|" indicates the within-Ss blocking term for matched pairs

## One-way repeated measures ANOVA

# read in a data file now with a third search engine, Yahoo
gglbngyh = read.csv("websearch3.csv")
View(gglbngyh)
gglbngyh$Subject = factor(gglbngyh$Subject) # convert to nominal factor
gglbngyh$Order = factor(gglbngyh$Order) # convert to nominal factor
summary(gglbngyh)

# view descriptive statistics by Engine
library(plyr)
ddply(gglbngyh, ~ Engine, function(data) summary(data$Searches))
ddply(gglbngyh, ~ Engine, summarise, Searches.mean=mean(Searches), Searches.sd=sd(Searches))

# graph histograms and boxplot
hist(gglbngyh[gglbngyh$Engine == "Google",]$Searches)
hist(gglbngyh[gglbngyh$Engine == "Bing",]$Searches)
hist(gglbngyh[gglbngyh$Engine == "Yahoo",]$Searches) # new one
plot(Searches ~ Engine, data=gglbngyh) # boxplot

# now test for an order effect 
# repeated measures ANOVA
library(ez)
m = ezANOVA(dv=Searches, within=Order, wid=Subject, data=gglbngyh)

# we then check the model for violations of sphericity. Sphericity is 
# the situation where the variances of the differences between all 
# combinations of levels of a within-Ss factor are equal. It always
# holds for within-Ss factors that have just 2 levels, but for 3+
# levels, sphericity can be tested with Mauchly's Test of Sphericity.
m$Mauchly # p<.05 indicates a violation

# if no violation, examine the uncorrected ANOVA in m$ANOVA. 
# if violation, instead look at m$Sphericity and use the 
# Greenhouse-Geisser correction, GGe.
m$ANOVA


# repeated measures ANOVA
library(ez)
# ez lets us specify the dependent variable (Searches), within-Ss 
# variables (Engine), and the variable that identifies 
# subjects (Subject).
m = ezANOVA(dv=Searches, within=Engine, wid=Subject, data=gglbngyh)

# we then check the model for violations of sphericity. Sphericity is 
# the situation where the variances of the differences between all 
# combinations of levels of a within-Ss factor are equal. It always
# holds for within-Ss factors that have just 2 levels, but for 3+
# levels, sphericity can be tested with Mauchly's Test of Sphericity.
m$Mauchly # p<.05 indicates a violation

# if no violation, examine the uncorrected ANOVA in m$ANOVA. 
# if violation, instead look at m$Sphericity and use the 
# Greenhouse-Geisser correction, GGe.
m$ANOVA

# manual post hoc pairwise comparisons with paired-samples t-tests
library(reshape2)	
gglbngyh.wide.engine = dcast(gglbngyh, Subject ~ Engine, value.var="Searches") # go wide
View(gglbngyh.wide.engine)
go.bi = t.test(gglbngyh.wide.engine$Google, gglbngyh.wide.engine$Bing, paired=TRUE)
go.ya = t.test(gglbngyh.wide.engine$Google, gglbngyh.wide.engine$Yahoo, paired=TRUE)
bi.ya = t.test(gglbngyh.wide.engine$Bing, gglbngyh.wide.engine$Yahoo, paired=TRUE)
p.adjust(c(go.bi$p.value, go.ya$p.value, bi.ya$p.value), method="holm")


# Friedman test on Effort
library(coin)
friedman_test(Effort ~ Engine | Subject, data=gglbngyh, distribution="asymptotic")

# manual post hoc Wilcoxon signed-rank test multiple comparisons
go.bi = wilcox.test(gglbngyh[gglbngyh$Engine == "Google",]$Effort, gglbngyh[gglbngyh$Engine == "Bing",]$Effort, paired=TRUE, exact=FALSE)
go.ya = wilcox.test(gglbngyh[gglbngyh$Engine == "Google",]$Effort, gglbngyh[gglbngyh$Engine == "Yahoo",]$Effort, paired=TRUE, exact=FALSE)
bi.ya = wilcox.test(gglbngyh[gglbngyh$Engine == "Bing",]$Effort, gglbngyh[gglbngyh$Engine == "Yahoo",]$Effort, paired=TRUE, exact=FALSE)
p.adjust(c(go.bi$p.value, go.ya$p.value, bi.ya$p.value), method="holm")
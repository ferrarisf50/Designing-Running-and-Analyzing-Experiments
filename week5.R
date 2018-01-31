alphabets = read.csv("alphabets.csv")
View(alphabets)
alphabets$Subject = factor(alphabets$Subject) # convert to nominal factor
summary(alphabets)

# view descriptive statistics by Alphabet
library(plyr)
ddply(alphabets, ~ Alphabet, function(data) summary(data$WPM))
ddply(alphabets, ~ Alphabet, summarise, WPM.mean=mean(WPM), WPM.sd=sd(WPM))

# explore new response distribution
hist(alphabets[alphabets$Alphabet == "EdgeWrite",]$WPM)
hist(alphabets[alphabets$Alphabet == "Graffiti",]$WPM)
hist(alphabets[alphabets$Alphabet == "Unistrokes",]$WPM) # new one
plot(WPM ~ Alphabet, data=alphabets) # boxplot

# test normality for 3 alphabets
shapiro.test(alphabets[alphabets$Alphabet == "EdgeWrite",]$WPM)
shapiro.test(alphabets[alphabets$Alphabet == "Graffiti",]$WPM)
shapiro.test(alphabets[alphabets$Alphabet == "Unistrokes",]$WPM)
m = aov(WPM ~ Alphabet, data=alphabets) # fit model
shapiro.test(residuals(m)) # test residuals
qqnorm(residuals(m)); qqline(residuals(m)) # plot residuals

# test homoscedasticity
library(car)
leveneTest(WPM ~ Alphabet, data=alphabets, center=median) # Brown-Forsythe test

# one-way ANOVA, suitable now to logWPM
m = aov(WPM ~ Alphabet, data=alphabets) # fit model
anova(m) # report anova

# post hoc independent-samples t-tests
plot(WPM ~ Alphabet, data=alphabets) # for convenience
library(multcomp)
summary(glht(m, mcp(Alphabet="Tukey")), test=adjusted(type="holm")) # Tukey means compare all pairs

## Nonparametric equivalent of one-way ANOVA

# Kruskal-Wallis test
library(coin)
kruskal_test(WPM ~ Alphabet, data=alphabets, distribution="asymptotic") # can't do exact with 3 levels
# for reporting Kruskal-Wallis as chi-square, we can get N with nrow(alphabets)

# manual post hoc Mann-Whitney U pairwise comparisons
gr.un = wilcox.test(alphabets[alphabets$Alphabet == "Graffiti",]$WPM, alphabets[alphabets$Alphabet == "Unistrokes",]$WPM, exact=FALSE)
un.ed = wilcox.test(alphabets[alphabets$Alphabet == "Unistrokes",]$WPM, alphabets[alphabets$Alphabet == "EdgeWrite",]$WPM, exact=FALSE)
ed.gr = wilcox.test(alphabets[alphabets$Alphabet == "EdgeWrite",]$WPM, alphabets[alphabets$Alphabet == "Graffiti",]$WPM, exact=FALSE)
p.adjust(c(gr.un$p.value, un.ed$p.value, ed.gr$p.value), method="holm")

# alternative approach is using PMCMR for nonparam pairwise comparisons
library(PMCMR)
posthoc.kruskal.conover.test(WPM ~ Alphabet, data=alphabets, p.adjust.method="holm") # Conover & Iman (1979)
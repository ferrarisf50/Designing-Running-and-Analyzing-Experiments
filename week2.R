
devprefs = read.csv("deviceprefs.csv")
View(devprefs)

devprefs$Subject = factor(devprefs$Subject) 
devprefs$Disability = factor(devprefs$Disability) 

summary(devprefs)
plot(devprefs$Pref)
plot(devprefs[devprefs$Disability == "1",]$Pref)
plot(devprefs[devprefs$Disability == "0",]$Pref)

prfs = xtabs( ~ Pref, data=devprefs)
chisq.test(prfs)

prfs.no.dis = xtabs( ~ Pref, data=devprefs[devprefs$Disability == "0",])
binom.test(prfs.no.dis)

prfs.dis = xtabs( ~ Pref, data=devprefs[devprefs$Disability == "1",])
binom.test(prfs.dis)

prfs2 = xtabs( ~ Pref + Disability, data=devprefs) # the '+' sign indicates two vars
chisq.test(prfs2)

library(RVAideMemoire)
G.test(prfs2)

fisher.test(prfs2)
